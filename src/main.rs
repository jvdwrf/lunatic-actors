use actor::{Handle, Reply};
use lunatic::Mailbox;
use serde::{Deserialize, Serialize};

use crate::actor::GenServer;

mod actor;

#[lunatic::main]
fn main(_mailbox: Mailbox<String>) {
    test();
}

fn test() {
    let my_actor = MyActor { count: 0 };
    let pid = GenServer::new(my_actor).start();

    // casting 5 times with a ping
    // calling count should increment by 1 every time
    for _ in 1..5 {
        pid.cast(MyMessage::Ping);
        println!("{:?}", pid.call(MyMessage::Count));
    }

    // casting 5 times with invalid message
    // calling count should not increment
    for _ in 1..5 {
        pid.cast(MyMessage::Invalid);
        println!("{:?}", pid.call(MyMessage::Count));
    }

    // calling with invalid should get a noreply
    println!("{:?}", pid.call(MyMessage::Invalid));
    // count not updated
    println!("{:?}", pid.call(MyMessage::Count));
    // this time calling with pid instead of casting
    // result should be a pong message
    println!("{:?}", pid.call(MyMessage::Ping));
    // count should be incremented by 1 now
    println!("{:?}", pid.call(MyMessage::Count));
}

#[derive(Serialize, Deserialize, Debug)]
struct MyActor {
    count: u32,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
enum MyMessage {
    Ping,
    Count,
    Invalid,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
enum MyReply {
    Pong,
    Count(u32),
}

impl Handle<MyMessage, MyReply> for MyActor {
    fn handle_call(&mut self, data: &MyMessage, ctx: &mut actor::Context<MyMessage, MyReply>) -> actor::Reply<MyReply> {
        match data {
            MyMessage::Ping => {
                self.count += 1;
                Reply::Ok(MyReply::Pong)
            }
            MyMessage::Count => Reply::Ok(MyReply::Count(self.count)),
            MyMessage::Invalid => Reply::NoReply,
        }
    }

    fn handle_cast(&mut self, data: &MyMessage, ctx: &mut actor::Context<MyMessage, MyReply>) {
        match data {
            MyMessage::Ping => self.count += 1,
            _ => (),
        }
    }
}
