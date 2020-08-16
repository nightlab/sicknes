use crate::sys;
use crate::cpu::Mos6502;
use std::io;
use std::io::ErrorKind;
use std::io::prelude::*;
use std::path::Path;
use std::fs::File;

pub struct Bus {
    ram: [u8; 2048],
    cart: Option<NesCatridge>,
    bus_error: bool
}

#[derive(Debug)]
enum Mapper {
    NROM
}

impl Default for Mapper {
    fn default() -> Self { Mapper::NROM }
}

bitflags! {
    #[derive(Default)]
    struct Flags6: u8 {
        const VERTICAL = 1 << 0;
        const BBUPRGRAM = 1 << 1;
        const TRAINER = 1 << 2;
        const FOURSCREEN = 1 << 3;
    }
}


#[derive(Debug,Default)]
pub struct NesCatridge {
    prg: Vec<u8>,
    chr: Vec<u8>,
    trainer: Vec<u8>,
    mapper: Mapper,
    mapper_lo: u8,

    f_prg: u32,
    f_chr: u32,
    f_flags6: Flags6,
    f_flags7: u8,
    f_flags8: u8,
    f_flags9: u8,
    f_flags10: u8
}

impl NesCatridge {
    fn load(filename : &str) -> Result<NesCatridge, io::Error> {
        let mut is_zip = false;
        if let Some(ext) = Path::new(filename).extension() {
            let ext = ext.to_str().unwrap().to_lowercase();
            if ext == "zip" {
                is_zip = true;
            }
        }
        if is_zip {
            return Err(io::Error::new(ErrorKind::Other, "Zip-files are not supported yet"));
        }
        let mut f = File::open(filename)?;
        let mut buffer = Vec::new();
        f.read_to_end(&mut buffer)?;
        if buffer.len() < 16 {
            return Err(io::Error::new(ErrorKind::Other, "Invalid file"));
        }
        if buffer.get(0..4).unwrap() != [78,69,83,26] {
            return Err(io::Error::new(ErrorKind::Other, "Found no iNES header"));
        }
        let mut cart = NesCatridge {
            prg: Vec::new(),
            chr: Vec::new(),
            trainer: Vec::new(),
            mapper: Mapper::NROM,
            f_prg: buffer[5] as u32 * 16384,
            f_chr: buffer[6] as u32 * 8192,
            mapper_lo: (buffer[8] & 0xf0) | (buffer[7] >> 4),
            f_flags6: Flags6 { bits: buffer[7] },
            f_flags7: buffer[8],
            f_flags8: buffer[9],
            f_flags9: buffer[10],
            f_flags10: buffer[11]
        };
        let sz_exp = (16 + (cart.f_flags6.contains(Flags6::TRAINER) as u32 * 512) + cart.f_prg + cart.f_chr) as usize;
        if buffer.len() < sz_exp {
            return Err(io::Error::new(ErrorKind::Other, format!("Header damaged (expected filesize < {})", sz_exp)));
        }

        let mut offset: usize = 16;
        if cart.f_flags6.contains(Flags6::TRAINER) {
            offset = offset + 512;
            cart.trainer.copy_from_slice(buffer.get(16..offset).unwrap());
        }
        println!("{:?}", cart);
        Ok(cart)
    }
}

pub struct NES {
    name: &'static str,

    cpu: Mos6502,
    bus: Bus,
    
    _clock_master: u32,
    _clock_cpu: u32,

    is_running: bool,
    step: bool
}

impl NES {
    pub fn new() -> NES {
        let bus = Bus {
            ram: [0; 2048],
            cart: None,
            bus_error: false
        };
        NES {
            bus,
            cpu: Mos6502::new(),
            is_running: false,
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
            0x2000..=0xffff => {
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
        self.cpu.reset(&self.bus);
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
        let cart = NesCatridge::load(filename);
        if cart.is_err() {
            println!("Error loading \"{}\": {}", filename, cart.unwrap_err());
            return false;
        }
        self.bus.cart = Some(cart.unwrap());
        true
    }
}
