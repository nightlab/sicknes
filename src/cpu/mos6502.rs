extern crate bitflags;

use std::fmt;
use crate::sys;

bitflags! {
    #[derive(Default)]
    struct Status: u8 {
        const CARRY = 1 << 0;
        const ZERO = 1 << 1;
        const INT_DIS = 1 << 2;
        const DECIMAL = 1 << 3;
        const B1 = 1 << 4;
        const B2 = 1 << 5;
        const OVERFLOW = 1 << 6;
        const NEGATIVE = 1 << 7;
    }
}

#[derive(Debug)]
enum Addressing {
    Implicit,
    Accumulator,
    Immediate,
    Zeropage,
    Absolute,
    Relative,
    Indirect,
    ZpIndexedX,
    ZpIndexedY,
    AbsIndexedX,
    AbsIndexedY,
    IndexedIndirectX,
    IndexedIndirectY
}

pub struct Mos6502 {
    a: u8,
    x: u8,
    y: u8,
    s: u8,
    p: Status,
    pc: u16,
    cycle: u32
}

macro_rules! a_imm {
    ($this:ident, $bus:expr) => { $bus.read_u8($this.pc.wrapping_add(1)) }
}

macro_rules! a_abs16 {
    ($this:ident, $bus:expr) => { ($bus.read_u8($this.pc.wrapping_add(2)) as u16) << 8 | $bus.read_u8($this.pc.wrapping_add(1)) as u16 }
}

macro_rules! flags_nz {
    ($this:ident, $value:expr) => {
        $this.p.set(Status::ZERO, $value == 0);
        $this.p.set(Status::NEGATIVE, ($value & 0x80) == 0x80);
    }
}

impl fmt::Display for Mos6502 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
            "A={:02X} X={:02X} Y={:02X} S={:02X} FLAGS={:#04X}({:?}) PC={:#06X} CYCLE={}",
            self.a, self.x, self.y, self.s, self.p, self.p, self.pc, self.cycle
        )
    }
}

impl Mos6502 {
    pub fn new() -> Mos6502 { 
        Mos6502 {
            a: 0, x: 0, y: 0, s: 0,
            p: Status { bits: 0 },
            pc: 0, cycle: 0
        }
    }

    pub fn reset(&mut self, bus: &mut dyn sys::MemoryAccessA16D8) {
        self.a = 0;
        self.x = 1;
        self.y = 0;
        self.s = 0xfd;
        self.p.bits = 0x34;
        self.pc = (bus.read_u8(0xfffd) as u16) << 8 | bus.read_u8(0xfffc) as u16;
        self.pc = 0xC000;
        self.cycle = 0;
    }

    pub fn debug_op(&self, bus: &mut dyn sys::MemoryAccessA16D8, op: u8) -> (u16, String) {
        match op {
            0x4c /* JMP abs */ => { (3, format!("JMP ${:04X}", a_abs16!(self, bus))) }
            0x78 /* SEI */ => { (1, format!("SEI")) }
            0xd8 /* CLD */ => { (1, format!("CLD")) }
            0xa2 /* LDX #xx */ => { (2, format!("LDX #${:02X}", bus.read_u8(self.pc.wrapping_add(1)))) }
            _ => { panic!("Invalid opcode {:#04x} [ {} ]", op, self); }
        }
    }

    pub fn exec_op(&mut self, bus: &mut dyn sys::MemoryAccessA16D8, op: u8) -> u32 {
        match op {
            0x4c /* JMP abs */ => { self.pc = a_abs16!(self, bus); 3 }
            0x78 /* SEI */ => { self.p.insert(Status::INT_DIS); self.pc = self.pc.wrapping_add(1); 2 }
            0xd8 /* CLD */ => { self.p.remove(Status::DECIMAL); self.pc = self.pc.wrapping_add(1); 2 }
            0xa2 /* LDX imm */ => { self.x = a_imm!(self, bus); flags_nz!(self, self.x); self.pc = self.pc.wrapping_add(2); 2 }
            _ => { panic!("Invalid opcode {:#04x} [ {} ]", op, self); }
        }
    }
    
    pub fn step(&mut self, bus: &mut dyn sys::MemoryAccessA16D8) {
        let op = bus.read_u8(self.pc);
        let (_, s) = self.debug_op(bus, op);
        println!("{:#06X} {}", self.pc, s);
        self.cycle = self.cycle + self.exec_op(bus, op);
    }
}
