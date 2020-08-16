extern crate piston_window;

#[macro_use]
extern crate bitflags;

use piston_window::*;

mod cpu;
mod sys;

use clap::{App, crate_version, crate_authors, crate_description};
use std::{thread, time};
use std::sync::mpsc::channel;

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
enum CoreMsg {
    WantExit
}

#[derive(Debug, Clone)]
enum MainMsg {
    WantExit,
    InsertCatridge(String),
    UpdateInput(u32, u32, u32, u32)
}

fn main() {
    let app_string = format!("sicknes {}", crate_version!());
    let args = App::new("sicknes")
        .version(crate_version!())
        .author(crate_authors!("\n"))
        .about(crate_description!())
        .arg("<rom> 'Load the ROM file in iNES format and boot'")
        .get_matches();

    let (core_tx, core_rx) = channel::<CoreMsg>();
    let (main_tx, main_rx) = channel::<MainMsg>();
    let core_thread = thread::spawn(move|| {
        let mut machine : Box<dyn sys::Machine> = Box::new(sys::NES::new());
        println!("Creating machine \"{}\"...", machine.get_name());
        loop {
            if machine.is_running() {
                machine.update();
            } else {
                thread::sleep(time::Duration::from_millis(10));
            }
            if let Ok(msg) = main_rx.try_recv() {
                match msg {
                    MainMsg::InsertCatridge(filename) => {
                        if machine.insert_catridge(&filename) {
                            machine.reset();
                            machine.run();
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
        }
    });    

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
            core_thread.join().unwrap();
            break;
        }
        
        if let Ok(msg) = core_rx.try_recv() {
            match msg {
                CoreMsg::WantExit => { core_thread.join().unwrap(); break; }
            }
        }
    }
    println!("Have a nice day!");
}