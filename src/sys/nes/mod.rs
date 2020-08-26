use crate::sys;
use crate::cpu::Mos6502;

pub mod cartridge;
pub use cartridge::*;

pub struct Bus {
    ram: [u8; 2048],
    cart: Option<NesCartridge>,
    mapper: Box<dyn MapperAccess>,
    bus_error: bool
}

pub struct NES {
    name: &'static str,

    cpu: Mos6502,
    bus: Bus,
    
    _clock_master: u32,
    _clock_cpu: u32,

    is_running: bool,

    pub is_breakpoint: bool,
    pub breakpoints: Vec<u16>,

    step: bool
}

impl NES {
    pub fn new() -> NES {
        let bus = Bus {
            ram: [0; 2048],
            mapper: Box::new(MapperDummy {}),
            cart: None,
            bus_error: false
        };
        NES {
            bus,
            cpu: Mos6502::new(true),
            is_running: false,
            is_breakpoint: false,
            breakpoints: Vec::new(),
            name: "Nintendo Entertainment System (Famicom)",
            _clock_master: 21441960,
            _clock_cpu: 1786830, // Master / 12
            step: false
        }
    }
}

impl sys::MemoryAccessA16D8 for Bus {
    fn read_u8(&mut self, address: u16) -> u8 {
        match address {
            0x0000..=0x1fff => {
                let ra: usize = (address & 0x07ff).into();
                self.ram[ra]
            }
            0x2000..=0x7fff => {
                println!("BUS ERROR: Invalid read @ {:#06x}", address);
                self.bus_error = true;
                0xff
            }
            0x8000..=0xffff => {
                self.mapper.read_u8(address)
            }
        }
    }
    fn write_u8(&mut self, address: u16, data: u8) {
        match address {
            0x0000..=0x1fff => {
                let ra: usize = (address & 0x07ff).into();
                self.ram[ra] = data;
            }
            0x2000..=0xffff => {
                println!("BUS ERROR: Invalid write {:#04x} @ {:#06x}", data, address);
                self.bus_error = true;
            }
        }
    }
}

impl sys::Machine for NES {
    fn get_name(&self) -> &'static str {
        self.name
    }

    fn reset(&mut self) {
        self.is_running = false;
        self.cpu.reset(&mut self.bus);
    }

    fn run(&mut self) {
        self.is_running = true;
        self.step = false;
    }

    fn stop(&mut self) {
        self.is_running = false;
    }

    fn step(&mut self) {
        self.is_running = true;
        self.step = true;
    }
    
    fn is_running(&self) -> bool {
        self.is_running
    }
    
    fn update(&mut self) {
        if self.bus.cart.is_none() {
            self.is_running = false;
            return;
        }
        self.cpu.step(&mut self.bus);
        if self.step {
            self.is_running = false;
        }
        if self.bus.bus_error {
            println!("Halting system! [ {} ]", self.cpu);
            self.is_running = false;
        }
    }
    
    fn insert_catridge(&mut self, filename: &str) -> bool {
        println!("Inserting catridge {}...", filename);
        let res = NesCartridge::load(filename);
        if res.is_err() {
            println!("Error loading \"{}\": {}", filename, res.err().unwrap());
            return false;
        }
        let (cart, mapper) = res.unwrap();
        self.bus.mapper = mapper;
        self.bus.cart = Some(cart);
        true
    }
}
