extern crate piston_window;

#[macro_use]
extern crate bitflags;

use piston_window::*;
use piston::input::*;

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

struct NES {
    name: &'static str,
    is_running: bool,
    step: bool
}

impl NES {
    fn new() -> NES {
        NES {
            is_running: false,
            name: "Nintendo Entertainment System (Famicom)",
            step: false
        }
    }
}

impl Machine for NES {
    fn get_name(&self) -> &'static str {
        self.name
    }
    fn reset(&mut self) {
        self.is_running = false;
    }
    fn run(&mut self) {
        self.is_running = true;
    }
    fn stop(&mut self) {
        self.is_running = false;
    }
    fn step(&mut self) {
        self.step = true;
    }
    fn insert_catridge(&mut self, filename: &str) {
        println!("Inserting catridge {}...", filename);
    }
}

trait Machine {
    fn get_name(&self) -> &'static str;

    fn reset(&mut self);
    fn run(&mut self);
    fn stop(&mut self);
    fn step(&mut self);

    fn insert_catridge(&mut self, filename: &str);
}

fn main() {
    let app_string = format!("sicknes {}", crate_version!());
    let args = App::new("sicknes")
        .version(crate_version!())
        .author(crate_authors!("\n"))
        .about(crate_description!())
        .arg("<rom> 'Load the ROM file in iNES format and boot'")
        .get_matches();

    let (_core_tx, core_rx) = channel::<CoreMsg>();
    let (main_tx, main_rx) = channel::<MainMsg>();
    let core_thread = thread::spawn(move|| {
        let mut machine : Box<dyn Machine> = Box::new(NES::new());
        println!("Creating machine \"{}\"...", machine.get_name());
        loop {
            if let Ok(msg) = main_rx.try_recv() {
                match msg {
                    MainMsg::InsertCatridge(filename) => {
                        machine.insert_catridge(&filename);
                    }
                    MainMsg::UpdateInput(c1, c2, c3, c4) => {
                        println!("Controller State Change: {} {} {} {}", c1, c2, c3, c4);
                    }
                    MainMsg::WantExit => { break; }
                }
            }
            thread::sleep(time::Duration::from_millis(1));
        }
    });    

    let rom = args.value_of("rom").unwrap();
    main_tx.send(MainMsg::InsertCatridge(rom.to_string())).unwrap();

    let mut window: PistonWindow =
        WindowSettings::new(app_string, [640, 480])
        .exit_on_esc(true).build().unwrap();

    let mut pad1 = NesPadState::empty();
    let mut pad2 = NesPadState::empty();

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
                    clear([1.0; 4], graphics);
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