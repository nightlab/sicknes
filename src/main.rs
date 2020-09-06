#![allow(dead_code)]

#[macro_use] extern crate bitflags;
#[macro_use] extern crate conrod_core;
extern crate conrod_glium;
extern crate conrod_winit;
extern crate find_folder;
extern crate winit;

mod cpu;
mod sys;
mod tests;

use clap::{App, crate_version, crate_authors, crate_description};
use std::{thread};
use std::time::{Instant, Duration};
use std::sync::mpsc::{channel, Sender, Receiver};
use std::{process, panic};

use image::{GenericImage, GenericImageView, ImageBuffer, RgbImage};
use conrod_core::{color, widget, Colorable, Positionable, Scalar, Labelable, Sizeable, Widget};
use conrod_core::widget::{Canvas,Tabs,Button};

use glium::Surface;

use std;
use glium;

pub struct GliumDisplayWinitWrapper(pub glium::Display);

impl conrod_winit::WinitWindow for GliumDisplayWinitWrapper {
    fn get_inner_size(&self) -> Option<(u32, u32)> {
        self.0.gl_window().get_inner_size().map(Into::into)
    }
    fn hidpi_factor(&self) -> f32 {
        self.0.gl_window().get_hidpi_factor() as _
    }
}

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

struct Fonts {
    regular: conrod_core::text::font::Id,
    italic: conrod_core::text::font::Id,
    bold: conrod_core::text::font::Id,
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

fn main() {
    env_logger::init();

    conrod_winit::conversion_fns!();

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
    let core_thread = thread::spawn(move|| t_core(core_tx, main_rx));

    let rom = args.value_of("rom").unwrap();
    main_tx.send(MainMsg::InsertCatridge(rom.to_string())).unwrap();

    let mut pad1 = NesPadState::empty();
    let pad2 = NesPadState::empty();

    const WIDTH: u32 = 1080;
    const HEIGHT: u32 = 500;

    let mut events_loop = glium::glutin::EventsLoop::new();
    let window = glium::glutin::WindowBuilder::new()
                .with_title(app_string)
                .with_dimensions((WIDTH, HEIGHT).into());
    let context = glium::glutin::ContextBuilder::new()
                .with_vsync(true)
                .with_multisampling(4);
    let display = glium::Display::new(window, context, &events_loop).unwrap();
    let display = GliumDisplayWinitWrapper(display);

    let mut ui = conrod_core::UiBuilder::new([WIDTH as f64, HEIGHT as f64]).build();

    let ids = Ids::new(ui.widget_id_generator());

    let assets = find_folder::Search::KidsThenParents(3, 5).for_folder("assets").unwrap();
    let noto_sans = assets.join("fonts/NotoSans");
    let fonts = Fonts {
        regular: ui.fonts.insert_from_file(noto_sans.join("NotoSans-Regular.ttf")).unwrap(),
        italic: ui.fonts.insert_from_file(noto_sans.join("NotoSans-Italic.ttf")).unwrap(),
        bold: ui.fonts.insert_from_file(noto_sans.join("NotoSans-Bold.ttf")).unwrap(),
    };
    ui.theme.font_id = Some(fonts.regular);

    let mut renderer = conrod_glium::Renderer::new(&display.0).unwrap();

    //let framebuffer: RgbImage = ImageBuffer::new(512, 512);
    //let framebuffer = Image::new().rect(square(0.0, 0.0, 200.0));

    let data: Vec<u32> = vec![0xFFEEAAFF;512*512];
    
    //let image = glium::texture::RawImage2d::from_raw_rgba(data, (512, 512));
    //let texture = glium::texture::Texture2d::new(&display, image).unwrap();

    let framebuffer = glium::Texture2d::empty_with_format(&display.0,
        glium::texture::UncompressedFloatFormat::U8U8U8U8,
        glium::texture::MipmapsOption::NoMipmap,
        512, 512).unwrap();
        framebuffer.as_surface().clear_color(0.0, 1.0, 0.0, 1.0);


    let mut image_map = conrod_core::image::Map::<glium::texture::Texture2d>::new();

    let mut fbmap = conrod_core::image::Map::<glium::texture::Texture2d>::new();
    let fbid = fbmap.insert(framebuffer);

    let mut last_update = std::time::Instant::now();
    let mut ui_needs_update = true;
    'main: loop {
        let sixteen_ms = std::time::Duration::from_millis(16);
        let duration_since_last_update = std::time::Instant::now().duration_since(last_update);
        if duration_since_last_update < sixteen_ms {
            std::thread::sleep(sixteen_ms - duration_since_last_update);
        }

        let mut events = Vec::new();
        events_loop.poll_events(|event| events.push(event));

        if events.is_empty() && !ui_needs_update {
            events_loop.run_forever(|event| {
                events.push(event);
                glium::glutin::ControlFlow::Break
            });
        }

        ui_needs_update = false;
        last_update = std::time::Instant::now();

        for event in events {
            if let Some(event) = convert_event(event.clone(), &display) {
                ui.handle_event(event);
            }
            match event {
                glium::glutin::Event::WindowEvent { event, .. } => match event {
                    glium::glutin::WindowEvent::CloseRequested | glium::glutin::WindowEvent::KeyboardInput {
                        input:
                            glium::glutin::KeyboardInput {
                                virtual_keycode: Some(glium::glutin::VirtualKeyCode::Escape),
                                ..
                            },
                        ..
                    } => break 'main,
                    _ => (),
                },
                _ => (),
            }
        }

        set_ui(ui.set_widgets(), &ids, &fonts, fbid);
        if let Some(primitives) = ui.draw_if_changed() {
            renderer.fill(&display.0, primitives, &image_map);
            let mut target = display.0.draw();
            target.clear_color(0.0, 0.0, 0.0, 1.0);
            renderer.draw(&display.0, &mut target, &image_map).unwrap();
            target.finish().unwrap();
        }
    }

    /*loop {
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
                CoreMsg::WantExit => {
                    core_thread.join().unwrap();
                    break;
                }
                CoreMsg::Echo(s) => { println!("Nachricht: {}", s); }
            }
        }
    }*/
    println!("Have a nice day!");
}

widget_ids! {
    struct Ids {
        master,
        left_col,
        right_col,
        left_text,
        middle_text,
        right_text,
        button_1,
        button_2,
        footer,
        header,
        body,
        tab_foo,
        tab_bar,
        tab_baz,
        tabs,
        foo_label,
        bar_label,
        baz_label,
        framebuffer,
        img1
    }
}

fn set_ui(ref mut ui: conrod_core::UiCell, ids: &Ids, fonts: &Fonts, i: conrod_core::image::Id) {

    Canvas::new().flow_down(&[
        (ids.header, Canvas::new().length(45.0).color(color::BLUE)),
        (ids.body, Canvas::new().flow_right(&[
            (ids.left_col, widget::Canvas::new().color(color::BLACK)),
            (ids.right_col, widget::Canvas::new().color(color::GREEN))
        ])),
        (ids.footer, Canvas::new().length(30.0).color(color::RED))
    ]).set(ids.master, ui);

    Tabs::new(&[(ids.tab_foo, "FLEISCH"), (ids.tab_bar, "BIER"), (ids.tab_baz, "WURST")])
        .wh_of(ids.right_col)
        .color(color::BLUE)
        .label_color(color::WHITE)
        .middle_of(ids.right_col)
        .set(ids.tabs, ui);
    fn text(text: widget::Text) -> widget::Text { text.color(color::WHITE).font_size(36) }
    text(widget::Text::new("Ein knuspriges Steak")).middle_of(ids.tab_foo).set(ids.foo_label, ui);
    text(widget::Text::new("Ein eiskaltes Pils")).middle_of(ids.tab_bar).set(ids.bar_label, ui);
    text(widget::Text::new("Ein dicker, saftiger Schinken")).middle_of(ids.tab_baz).set(ids.baz_label, ui);

    widget::Image::new(i).w_h(100.0,100.0).middle().set(ids.img1, ui);

    Button::new()
        .label_font_id(fonts.bold)
        .label("EXIT")
        .w_h(120.0, 35.0).
        middle_of(ids.header)
        .set(ids.button_1, ui);
}
