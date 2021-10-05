use std::intrinsics::transmute;

use lunatic::{Mailbox, Request, process::{self, Process}};
use serde::{de::DeserializeOwned, Deserialize, Serialize};

//-------------------------------------
// Msg and Reply
//-------------------------------------

#[derive(Serialize, Deserialize)]
pub struct Msg<M>
where
    M: Serialize,
{
    to_reply: bool,
    data: M,
}

impl<M> Msg<M>
where
    M: Serialize,
{
    pub fn new_cast(data: M) -> Self {
        Msg {
            to_reply: false,
            data,
        }
    }

    pub fn new_call(data: M) -> Self {
        Msg {
            to_reply: true,
            data,
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
    M: Serialize + DeserializeOwned + Clone,
    R: Serialize + DeserializeOwned,
{
    pub fn data(&self) -> M {
        self.0.data().data.clone()
    }

    pub fn to_reply(&self) -> bool {
        self.0.data().to_reply
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
    M: Serialize + DeserializeOwned + Clone,
    R: Serialize + DeserializeOwned,
{
    pub(crate) fn receive(&self) -> MyRequest<M, R> {
        self.0.receive().unwrap()
    }

    pub(crate) fn new(mailbox: Mailbox<MyRequest<M, R>>) -> Self {
        unsafe{ transmute(mailbox) }
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
    M: Serialize + DeserializeOwned + Clone,
    R: Serialize + DeserializeOwned,
{
    pub fn cast(&self, data: M) {
        let msg = Msg::new_cast(data);
        let _unused_reply = self.request(msg);
    }

    pub fn call(&self, data: M) -> Reply<R> {
        let msg = Msg::new_call(data);
        self.request(msg)
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
    M: Serialize + DeserializeOwned + Clone,
    R: Serialize + DeserializeOwned,
    Self: Sized + Serialize + DeserializeOwned
{
    fn init(self) -> Self {
        self
    }

    fn handle_call(&mut self, data: M) -> Reply<R>;

    fn handle_cast(&mut self, data: M);

    fn start(self) -> Pid<M, R> {
        let gen_server = self.init();

        let process = process::spawn_with(gen_server, Self::start_loop).unwrap();

        let pid = Pid::new(process);

        pid
    }

    fn start_loop(mut self, mailbox: Mailbox<MyRequest<M, R>>) {
        let mailbox = MyMailbox::new(mailbox);

        loop {
            let msg = mailbox.receive();

            match msg.to_reply() {
                true => {
                    let reply = self.handle_call(msg.data());
                    msg.reply(reply);
                },
                false => {
                    let data = msg.data();
                    msg.reply(Reply::NoReply);
                    self.handle_cast(data);
                },
            }


        }
    }
}