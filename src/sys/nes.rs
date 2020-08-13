use crate::sys;
use crate::cpu::Mos6502;
use crate::cpu::Cpu;

pub struct NES {
    name: &'static str,

    cpu: Mos6502,

    is_running: bool,
    step: bool
}

impl NES {
    pub fn new() -> NES {
        NES {
            cpu: Mos6502::new(),
            is_running: false,
            name: "Nintendo Entertainment System (Famicom)",
            step: false
        }
    }
}

impl sys::Machine for NES {
    fn get_name(&self) -> &'static str {
        self.name
    }
    fn reset(&mut self) {
        self.is_running = false;
        self.cpu.reset();
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
    fn update(&mut self) {
        println!("run");
    }
    fn insert_catridge(&mut self, filename: &str) {
        println!("Inserting catridge {}...", filename);
    }
}
