use crate::sys;
use crate::cpu::Mos6502;

pub mod cartridge;
pub use cartridge::*;

pub struct Bus {
    pub ram: [u8; 2048],
    pub apu: [u8; 16],
    pub ppu: [u8; 8],
    cart: Option<NesCartridge>,
    mapper: Box<dyn MapperAccess>,
    pub bus_error: bool
}

pub struct NES {
    name: &'static str,

    pub cpu: Mos6502,
    pub bus: Bus,
    
    _clock_master: u32,
    _clock_cpu: u32,

    is_running: bool,
}

impl NES {
    pub fn new() -> NES {
        let bus = Bus {
            ram: [0; 2048],
            apu: [0; 16],
            ppu: [0; 8],
            mapper: Box::new(MapperDummy {}),
            cart: None,
            bus_error: false
        };
        NES {
            bus,
            cpu: Mos6502::new(true),
            is_running: false,
            name: "Nintendo Entertainment System (Famicom)",
            _clock_master: 21441960,
            _clock_cpu: 1786830 // Master / 12
        }
    }
}

impl sys::MemoryAccessA16D8 for Bus {
    fn read_u8(&mut self, address: u16) -> u8 {
        match address {
            0x8000..=0xffff => {
                self.mapper.read_u8(address)
            }
            0x2000..=0x3fff => {
                let ra = (address & 0x7) as usize;
                //println!("READ PPU @ {:#06x} = {:#04x}", address, self.ppu[ra]);
                self.ppu[ra]
            }
            0x0000..=0x1fff => {
                let ra: usize = (address & 0x07ff).into();
                self.ram[ra]
            }
            0x4000..=0x401f => {
                let ra = (address & 0xf) as usize;
                //println!("READ APU @ {:#06x} = {:#04x}", address, self.apu[ra]);
                self.apu[ra]
            }
            0x4020..=0x7fff => {
                println!("BUS ERROR: Invalid read @ {:#06x}", address);
                self.bus_error = true;
                0xff
            }
        }
    }
    fn write_u8(&mut self, address: u16, data: u8) {
        match address {
            0x0000..=0x1fff => {
                let ra: usize = (address & 0x07ff).into();
                self.ram[ra] = data;
            }
            0x2000..=0x3fff => {
                let ra = (address & 0x7) as usize;
                println!("WRITE PPU @ {:#06x} = {:#04x}", address, data);
                self.ppu[ra] = data;
            }
            0x4000..=0x401f => {
                let ra = (address & 0xf) as usize;
                println!("WRITE APU @ {:#06x} = {:#04x}", address, data);
                self.apu[ra] = data;
            }
            0x4020..=0xffff => {
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

    fn get_cycles(&self) -> u32 {
        self.cpu.cycle
    }
    
    fn update(&mut self) {
        //self.cpu.step_debug(&mut self.bus);
        self.cpu.step(&mut self.bus);
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
