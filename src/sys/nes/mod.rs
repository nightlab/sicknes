use crate::cpu::Mos6502;
use crate::sys;

pub mod cartridge;
pub use cartridge::*;

pub mod ppu;
pub use ppu::*;

pub trait BusControl {
    fn update(&mut self, cycles: u32);
}

pub struct Bus {
    pub ram: [u8; 2048],

    pub apu: [u8; 16],
    pub ppu: PPU,

    cart: Option<NesCartridge>,
    mapper: Box<dyn MapperAccess>,
    pub bus_error: bool,
}

pub struct NES {
    name: &'static str,

    pub cpu: Mos6502,
    pub bus: Bus,

    ppu_rem: u32,

    ccs: u32,

    is_running: bool,
    startup: bool,
}

impl NES {
    pub fn new() -> NES {
        NES {
            bus: Bus::new(),
            cpu: Mos6502::new(true),
            is_running: false,
            name: "Nintendo Entertainment System (Famicom)",
            ppu_rem: 0,
            ccs: 0,
            startup: false,
        }
    }
}

impl Bus {
    fn new() -> Bus {
        Bus {
            ram: [0; 2048],
            apu: [0; 16],
            ppu: PPU::new(),
            mapper: Box::new(MapperDummy {}),
            cart: None,
            bus_error: false,
        }
    }
}

impl sys::MemoryAccessA16D8 for Bus {
    fn read_u8(&mut self, address: u16, dummy: bool) -> u8 {
        match address {
            0x8000..=0xffff => self.mapper.read_prg_u8(address),
            0x2000..=0x3fff => {
                if dummy {
                    return 0xff;
                }
                self.ppu.reg_read(&mut self.mapper, (address & 0x7) as u8)
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
                //println!("WRITE PPU @ {:#06x} = {:#04x}", address, data);
                self.ppu
                    .reg_write(&mut self.mapper, (address & 0x7) as u8, data);
            }
            0x4000..=0x401f => {
                let ra = (address & 0xf) as usize;
                //println!("WRITE APU @ {:#06x} = {:#04x}", address, data);
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

    fn start(&mut self) {
        self.is_running = true;
    }

    fn is_running(&self) -> bool {
        self.is_running
    }

    fn update(&mut self) {
        let cc;
        if self.bus.ppu.nmi_pending {
            cc = self.cpu.interrupt(true, &mut self.bus);
            self.bus.ppu.nmi_pending = false;
        } else {
            cc = self.cpu.step(&mut self.bus) as u32;
        }
        self.ccs = self.ccs + cc * 3;
        if self.startup {
            if self.ccs >= 88974 {
                self.startup = false;
                self.ccs = 0;
            }
        } else {
            if self.ccs >= 341 {
                self.ccs = self.ccs - 341;
                self.bus.ppu.update(&mut self.bus.mapper);
            }
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
