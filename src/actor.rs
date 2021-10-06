use std::intrinsics::transmute;

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

impl<M> Msg<M>
where
    M: Serialize,
{
    pub fn data(&self) -> &M {
        match self {
            Msg::Cast(data) => data,
            Msg::Call(data) => data,
        }
    }
}

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

    println!("{:?}", pid.call(MyMessage::Count));
    pub fn reply(self, reply: Reply<R>) {
        self.0.reply(reply);
    }
}

//-------------------------------------
// MyMailbox
//-------------------------------------

pub struct MyMailbox<M: Serialize + DeserializeOwned, R: Serialize + DeserializeOwned>(
    Mailbox<MyRequest<M, R>>,
);

impl<M, R> MyMailbox<M, R>
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
// GenServer
//-------------------------------------

pub trait GenServer<M, R>
where
    M: Serialize + DeserializeOwned,
    R: Serialize + DeserializeOwned,
    Self: Sized + Serialize + DeserializeOwned,
{
    fn init(self) -> Self {
        self
    }

    fn handle_call(&mut self, data: &M) -> Reply<R>;

    fn handle_cast(&mut self, data: &M);

    fn start(self) -> Pid<M, R> {
        self = self.init();

        let process = process::spawn_with(self, Self::start_loop).unwrap();

        Pid::new(process)
    }

    fn start_loop(mut self, mailbox: Mailbox<MyRequest<M, R>>) {
        let mailbox = MyMailbox::new(mailbox);

        loop {
            let request = mailbox.receive();

            match request.msg() {
                Msg::Cast(data) => {
                    let reply = self.handle_call(data);
                    request.reply(reply);
                }
                Msg::Call(data) => {
                    request.reply(Reply::NoReply);
                    self.handle_cast(data);
                }
            }
        }
    }
}
