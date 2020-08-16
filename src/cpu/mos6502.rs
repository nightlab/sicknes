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

/*
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
}*/

pub struct Mos6502 {
    a: u8,
    x: u8,
    y: u8,
    s: u8,
    p: Status,
    pc: u16,
    cycle: u32
}

impl fmt::Display for Mos6502 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
            "A={:#04x}({}) X={:#04x}({}) Y={:#04x}({}) S={:#04x}({}) FLAGS={:#04x}({:?}) PC={:#06x} CYCLE={}",
            self.a, self.a, self.x, self.x, self.y, self.y, self.s, self.s, self.p, self.p, self.pc, self.cycle
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

    pub fn reset(&mut self, _bus: &dyn sys::MemoryAccessA16D8) {
        self.a = 0;
        self.x = 0;
        self.y = 0;
        self.s = 0xfd;
        self.p.bits = 0x34;
        self.pc = 0;
        self.cycle = 0;
    }
    
    pub fn step(&mut self, bus: &mut dyn sys::MemoryAccessA16D8) {
        bus.read_u8(self.pc);
        self.pc = self.pc.wrapping_add(1);
        self.cycle = self.cycle + 1;
    }
}
