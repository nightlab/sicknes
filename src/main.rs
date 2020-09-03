#![allow(dead_code)]

extern crate piston_window;

#[macro_use]
extern crate bitflags;

#[macro_use]
extern crate log;

use piston_window::*;

mod cpu;
mod sys;
mod tests;

use clap::{App, crate_version, crate_authors, crate_description};
use std::{thread};
use std::time::{Instant, Duration};
use std::sync::mpsc::{channel, Sender, Receiver};
use std::panic;
use std::io;
use std::process;

use ws::listen;

use std::net::TcpListener;
use std::net::TcpStream;

use tungstenite::server::accept;

bitflags! {
    struct NesPadState: u32 {
        const A = 1 << 0;
        const B = 1 << 1;
        const SELECT = 1 << 2;
        const START = 1 << 3;
        const UP = 1 << 4;
        const DOWN = 1 << 5;
        const LEFT = 1 << 6;
        const RIGHT = 1 << 7;
    }
}

#[derive(Debug, Clone)]
enum DebugMsg {
    WantExit
}

#[derive(Debug, Clone)]
enum CoreMsg {
    WantExit,
    Echo(String)
}

#[derive(Debug, Clone)]
enum MainMsg {
    WantExit,
    InsertCatridge(String),
    UpdateInput(u32, u32, u32, u32)
}

fn t_core(core_tx: Sender<CoreMsg>, main_rx: Receiver<MainMsg>) {
    let mut machine : Box<dyn sys::Machine> = Box::new(sys::NES::new());
    println!("Creating machine \"{}\"...", machine.get_name());
    let mut now = Instant::now();
    let mut cycles: u32 = 0;
    loop {
        if machine.is_running() {
            machine.update();
        } else {
            thread::sleep(Duration::from_millis(10));
        }
        if now.elapsed().as_millis() >= 1000 {
            let x = machine.get_cycles();
            let d = x - cycles;
            println!("{} cycles/sec ({} fps)", d, d / 29780);
            cycles = x;
            if let Ok(msg) = main_rx.try_recv() {
                match msg {
                    MainMsg::InsertCatridge(filename) => {
                        if machine.insert_catridge(&filename) {
                            machine.reset();
                            machine.start();
                        } else {
                            core_tx.send(CoreMsg::WantExit).unwrap();
                            break;
                        }
                    }
                    MainMsg::UpdateInput(c1, c2, c3, c4) => {
                        println!("Controller State Change: {} {} {} {}", c1, c2, c3, c4);
                    }
                    MainMsg::WantExit => {
                        break;
                    }
                }
            }                
            now = Instant::now();
        }
    }
}

fn handle_connection(stream: TcpStream) {
    println!("Yay!");
    match accept(stream) {
        Ok(mut websocket) => {
            println!("Accepted!");
            match websocket.read_message() {
                Ok(m) => {
                    println!("DEBUG Message: {}", m);
                }
                Err(_) => {
                    println!("Message Error");
                }
            }
        }
        Err(e) => {
            println!("Error! Terminating... {}", e);
        }

    }                    
}

fn t_debug(core_tx: Sender<CoreMsg>, debug_rx: Receiver<DebugMsg>) {
    let server = TcpListener::bind("127.0.0.1:10222").unwrap();
    server.set_nonblocking(true).expect("Cannot set non-blocking");
    'outer: loop {
        if let Ok(msg) = debug_rx.try_recv() {
            match msg {
                DebugMsg::WantExit => {
                    break 'outer;
                }
            }
        }

        /*
        for result in server.incoming() {
            match result {
                Ok(stream) => {
                    println!("DEBUG: Incoming connection");
                    thread::spawn(|| handle_connection(stream));
                }
                Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => {
                    thread::sleep(Duration::from_millis(1));
                    continue 'outer;
                }
                Err(e) => panic!("IO error: {}", e),
            }
        } */       
    }
    /*for stream in server.incoming() {
        let mut websocket = accept(stream.unwrap()).unwrap();
        loop {
            let msg = websocket.read_message().unwrap();

            // We do not want to send back ping/pong messages.
            if msg.is_binary() || msg.is_text() {
                websocket.write_message(msg).unwrap();
                core_tx.send(CoreMsg::Echo("Juck".to_string())).unwrap();
            }
        }
        if let Ok(msg) = debug_rx.try_recv() {
            match msg {
                DebugMsg::WantExit => {
                    break;
                }
            }
        }
    }*/
}

fn main() {
    env_logger::init();

    let app_string = format!("sicknes {}", crate_version!());
    let args = App::new("sicknes")
        .version(crate_version!())
        .author(crate_authors!("\n"))
        .about(crate_description!())
        .arg("<rom> 'Load the ROM file in iNES format and boot'")
        .get_matches();

    let default_hook = panic::take_hook();
    panic::set_hook(Box::new(move |i| {
        default_hook(i);
        process::exit(1);
    }));

    let (core_tx, core_rx) = channel::<CoreMsg>();
    let (main_tx, main_rx) = channel::<MainMsg>();
    let (debug_tx, debug_rx) = channel::<DebugMsg>();
    let main_tx2 = core_tx.clone();
    let core_thread = thread::spawn(move|| t_core(core_tx, main_rx));
    let debug_thread = thread::spawn(move|| t_debug(main_tx2, debug_rx));

    let rom = args.value_of("rom").unwrap();
    main_tx.send(MainMsg::InsertCatridge(rom.to_string())).unwrap();

    let mut window: PistonWindow =
        WindowSettings::new(app_string, [640, 480])
        .exit_on_esc(true).build().unwrap();

    let mut pad1 = NesPadState::empty();
    let pad2 = NesPadState::empty();

    loop {
        if let Some(event) = window.next() {
            // press keyboard button
            if let Some(Button::Keyboard(key)) = event.press_args() {
                match key {
                    Key::A => { pad1.set(NesPadState::A, true) }
                    Key::S => { pad1.set(NesPadState::B, true) }
                    Key::Q => { pad1.set(NesPadState::START, true) }
                    Key::W => { pad1.set(NesPadState::SELECT, true) }
                    Key::Up => { pad1.set(NesPadState::UP, true) }
                    Key::Down => { pad1.set(NesPadState::DOWN, true) }
                    Key::Left => { pad1.set(NesPadState::LEFT, true) }
                    Key::Right => { pad1.set(NesPadState::RIGHT, true) }
                    _ => {}
                }
                main_tx.send(MainMsg::UpdateInput(pad1.bits(), pad2.bits(), 0, 0)).unwrap();
            };

            // release keyboard button
            if let Some(button) = event.release_args() {
                match button {
                    Button::Keyboard(key) => {
                        match key {
                            Key::A => { pad1.set(NesPadState::A, false) }
                            Key::S => { pad1.set(NesPadState::B, false) }
                            Key::Q => { pad1.set(NesPadState::START, false) }
                            Key::W => { pad1.set(NesPadState::SELECT, false) }
                            Key::Up => { pad1.set(NesPadState::UP, false) }
                            Key::Down => { pad1.set(NesPadState::DOWN, false) }
                            Key::Left => { pad1.set(NesPadState::LEFT, false) }
                            Key::Right => { pad1.set(NesPadState::RIGHT, false) }
                            _ => {}
                        }
                    },
                    Button::Mouse(_) => {},
                    Button::Controller(_) => {},
                    Button::Hat(_) => {}
                }
                main_tx.send(MainMsg::UpdateInput(pad1.bits(), pad2.bits(), 0, 0)).unwrap();
            };            

            // render
            if let Some(_) = event.render_args() {
                window.draw_2d(&event, |_context, graphics, _device| {
                    clear([0.0; 4], graphics);
                });
            }
        } else {
            main_tx.send(MainMsg::WantExit).unwrap();
            debug_tx.send(DebugMsg::WantExit).unwrap();
            core_thread.join().unwrap();
            debug_thread.join().unwrap();
            break;
        }
        if let Ok(msg) = core_rx.try_recv() {
            match msg {
                CoreMsg::WantExit => {
                    core_thread.join().unwrap();
                    debug_thread.join().unwrap();
                    break;
                }
                CoreMsg::Echo(s) => { println!("Nachricht: {}", s); }
            }
        }
    }
    println!("Have a nice day!");
}