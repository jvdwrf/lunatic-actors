use std::{intrinsics::transmute, marker::PhantomData};

use lunatic::{
    process::{self, Process},
    Mailbox, Request,
};
use serde::{de::DeserializeOwned, Deserialize, Serialize};

//-------------------------------------
// Msg and Reply
//-------------------------------------

#[derive(Serialize, Deserialize)]
pub enum Msg<M>
where
    M: Serialize,
{
    Cast(M),
    Call(M),
}

impl<M> Msg<M> where M: Serialize {}

#[derive(Serialize, Deserialize, Debug)]
pub enum Reply<R>
where
    R: Serialize,
{
    Ok(R),
    Err(String),
    NoReply,
}

//-------------------------------------
// MyRequest
//-------------------------------------

#[derive(Serialize, Deserialize)]
#[serde(bound(deserialize = "Request<Msg<M>, Reply<R>>: Deserialize<'de>"))]
pub struct MyRequest<M: Serialize + DeserializeOwned, R: Serialize + DeserializeOwned>(
    Request<Msg<M>, Reply<R>>,
);

impl<M, R> MyRequest<M, R>
where
    M: Serialize + DeserializeOwned,
    R: Serialize + DeserializeOwned,
{
    pub(crate) fn msg(&self) -> &Msg<M> {
        self.0.data()
    }

    pub fn reply(self, reply: Reply<R>) {
        self.0.reply(reply);
    }
}

//-------------------------------------
// Context
//-------------------------------------

pub struct Context<M: Serialize + DeserializeOwned, R: Serialize + DeserializeOwned>(
    Mailbox<MyRequest<M, R>>,
);

impl<M, R> Context<M, R>
where
    M: Serialize + DeserializeOwned,
    R: Serialize + DeserializeOwned,
{
    pub(crate) fn receive(&self) -> MyRequest<M, R> {
        self.0.receive().unwrap()
    }

    pub(crate) fn new(mailbox: Mailbox<MyRequest<M, R>>) -> Self {
        unsafe { transmute(mailbox) }
    }
}

pub struct Pid<M: Serialize + DeserializeOwned, R: Serialize + DeserializeOwned>(
    Process<MyRequest<M, R>>,
);

//-------------------------------------
// Pid
//-------------------------------------

impl<M, R> Pid<M, R>
where
    M: Serialize + DeserializeOwned,
    R: Serialize + DeserializeOwned,
{
    pub fn cast(&self, data: M) {
        let _unused_reply = self.request(Msg::Cast(data));
    }

    pub fn call(&self, data: M) -> Reply<R> {
        self.request(Msg::Call(data))
    }

    fn request(&self, msg: Msg<M>) -> Reply<R> {
        self.transmute().request(msg).unwrap()
    }

    fn transmute(&self) -> &Process<Request<Msg<M>, Reply<R>>> {
        unsafe { transmute(self) }
    }

    fn new(process: Process<MyRequest<M, R>>) -> Self {
        unsafe { transmute(process) }
    }
}

//-------------------------------------
// GenServer Trait
//-------------------------------------

pub trait Handle<M, R>
where
    M: Serialize + DeserializeOwned + Clone,
    R: Serialize + DeserializeOwned,
    Self: Sized
{
    fn handle_call(&mut self, data: &M, ctx: &mut Context<M, R>) -> Reply<R>;

    fn handle_cast(&mut self, data: &M, ctx: &mut Context<M, R>);

    fn after_spawn(self, ctx: Context<M, R>) -> (Self, Context<M, R>) {
        (self, ctx)
    }

    fn before_spawn(self) -> Self {
        self
    }
}

//-------------------------------------
// GenServer
//-------------------------------------

#[derive(Serialize, Deserialize)]
pub struct GenServer<T, M, R>
where
    T: Handle<M, R>,
    M: Serialize + DeserializeOwned + Clone,
    R: Serialize + DeserializeOwned
{
    actor: T,
    m: PhantomData<M>,
    r: PhantomData<R>,
}

impl<T, M, R> GenServer<T, M, R>
where
    T: Handle<M, R>,
    M: Serialize + DeserializeOwned + Clone,
    R: Serialize + DeserializeOwned,
    Self: Serialize + DeserializeOwned
{
    pub fn new(actor: T) -> Self {
        Self {
            actor,
            m: PhantomData,
            r: PhantomData,
        }
    }

    pub fn start(mut self) -> Pid<M, R> {
        self.actor = self.actor.before_spawn();

        let process = process::spawn_with(self, Self::start_loop).unwrap();

        Pid::new(process)
    }

    fn start_loop(self, mailbox: Mailbox<MyRequest<M, R>>) {
        let (mut gen_server, mut ctx) = self.actor.after_spawn(Context::new(mailbox));

        loop {
            let request = ctx.receive();

            match request.msg() {
                Msg::Call(data) => {
                    let reply = gen_server.handle_call(data, &mut ctx);
                    request.reply(reply);
                }
                Msg::Cast(data) => {
                    let data = data.clone();
                    request.reply(Reply::NoReply);
                    gen_server.handle_cast(&data, &mut ctx);
                }
            }
        }
    }
}
