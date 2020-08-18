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

macro_rules! o_load {
    ($this:ident, $bus:expr, $addressing:ident, $reg:ident, $len:expr) => {
        let _op = $addressing!($this, $bus);
        f_nz!($this, _op);
        $this.$reg = _op;
        t_upc!($this, $len);
    }
}

macro_rules! o_bit {
    ($this:ident, $bus:expr, $operand:ident, $len:expr) => {
        $this.p.set(Status::NEGATIVE, $operand & 0x80 == 0x80);
        $this.p.set(Status::OVERFLOW, $operand & 0x40 == 0x40);
        $this.p.set(Status::ZERO, ($this.a & $operand) == 0);
        t_upc!($this, $len);
    }
}

macro_rules! o_store_zp {
    ($this:ident, $bus:expr, $reg:ident, $len:expr) => {
        let adr = a_imm!($this, $bus) as u16;
        $bus.write_u8(adr, $this.$reg);
        t_upc!($this, $len);
    }
}

macro_rules! a_imm {
    ($this:ident, $bus:expr) => { $bus.read_u8($this.pc.wrapping_add(1)) }
}

macro_rules! a_abs16 {
    ($this:ident, $bus:expr) => { ($bus.read_u8($this.pc.wrapping_add(2)) as u16) << 8 | $bus.read_u8($this.pc.wrapping_add(1)) as u16 }
}

macro_rules! f_nz {
    ($this:ident, $value:expr) => {
        $this.p.set(Status::ZERO, $value == 0);
        $this.p.set(Status::NEGATIVE, ($value & 0x80) == 0x80);
    }
}

macro_rules! t_upc {
    ($this:ident, $len:expr) => { $this.pc = $this.pc.wrapping_add($len); }
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
        self.cycle = 7;
    }

    // dump opcode
    pub fn debug_op(&self, bus: &mut dyn sys::MemoryAccessA16D8, op: u8) -> (String, u16) {
        match op {
            0x4c /* JMP abs */ => { (format!("JMP ${:04X}", a_abs16!(self, bus)), 3 /*LEN*/) }
            
            0x78 /* SEI */ => { (format!("SEI"), 1 /*LEN*/) }
            0x38 /* SEC */ => { (format!("SEC"), 1 /*LEN*/) }
            0xf8 /* SED */ => { (format!("SED"), 1 /*LEN*/) }
            0x58 /* CLI */ => { (format!("CLI"), 1 /*LEN*/) }
            0x18 /* CLC */ => { (format!("CLC"), 1 /*LEN*/) }
            0xd8 /* CLD */ => { (format!("CLD"), 1 /*LEN*/) }
            0xb8 /* CLV */ => { (format!("CLV"), 1 /*LEN*/) }

            0x24 /* BIT zp */ => { let a = a_imm!(self, bus); (format!("BIT ${:02X} = ${:02X}", a, bus.read_u8(a as u16)), 2 /*LEN*/) }

            0xa9 /* LDA imm */ => { (format!("LDA #${:02X}", a_imm!(self, bus)), 2 /*LEN*/) }
            
            0xa2 /* LDX imm */ => { (format!("LDX #${:02X}", a_imm!(self, bus)), 2 /*LEN*/) }
            
            0xa0 /* LDY imm */ => { (format!("LDY #${:02X}", a_imm!(self, bus)), 2 /*LEN*/) }

            0x85 /* STA zp */ => { let a = a_imm!(self, bus); (format!("STA ${:02X} = ${:02X}", a, bus.read_u8(a as u16)), 2 /*LEN*/) }

            0x86 /* STX zp */ => { let a = a_imm!(self, bus); (format!("STX ${:02X} = ${:02X}", a, bus.read_u8(a as u16)), 2 /*LEN*/) }

            0x84 /* STY zp */ => { let a = a_imm!(self, bus); (format!("STY ${:02X} = ${:02X}", a, bus.read_u8(a as u16)), 2 /*LEN*/) }

            0x20 /* JSR abs */ => { (format!("JSR ${:04X}", a_abs16!(self, bus)), 3 /*LEN*/) }

            0xb0 /* BCS rel */ => { (format!("BCS ${:04X}", self.a_rel(bus)), 2 /*LEN*/) }
            0x90 /* BCC rel */ => { (format!("BCC ${:04X}", self.a_rel(bus)), 2 /*LEN*/) }
            0xf0 /* BEQ rel */ => { (format!("BEQ ${:04X}", self.a_rel(bus)), 2 /*LEN*/) }
            0xd0 /* BNE rel */ => { (format!("BNE ${:04X}", self.a_rel(bus)), 2 /*LEN*/) }
            0x70 /* BVS rel */ => { (format!("BVS ${:04X}", self.a_rel(bus)), 2 /*LEN*/) }
            0x50 /* BVC rel */ => { (format!("BVC ${:04X}", self.a_rel(bus)), 2 /*LEN*/) }

            0x30 /* BMI rel */ => { (format!("BMI ${:04X}", self.a_rel(bus)), 2 /*LEN*/) }
            0x10 /* BPL rel */ => { (format!("BPL ${:04X}", self.a_rel(bus)), 2 /*LEN*/) }

            0x60 /* NOP */ => { (format!("RTS"), 1 /*LEN*/) }

            0xea /* NOP */ => { (format!("NOP"), 1 /*LEN*/) }

            _ => { panic!("Invalid opcode {:#04x} [ {} ]", op, self); }
        }
    }

    // execute opcode
    pub fn exec_op(&mut self, bus: &mut dyn sys::MemoryAccessA16D8, op: u8) -> u32 {
        match op {
            0x4c /* JMP abs */ => { self.pc = a_abs16!(self, bus); 3 /*CYCLES*/ }

            0x78 /* SEI */ => { self.p.insert(Status::INT_DIS); t_upc!(self, 1 /*LEN*/); 2 /*CYCLES*/ }
            0x38 /* SEC */ => { self.p.insert(Status::CARRY); t_upc!(self, 1 /*LEN*/); 2 /*CYCLES*/ }
            0xf8 /* SED */ => { self.p.insert(Status::DECIMAL); t_upc!(self, 1 /*LEN*/); 2 /*CYCLES*/ }
            0x58 /* CLI */ => { self.p.remove(Status::INT_DIS); t_upc!(self, 1 /*LEN*/); 2 /*CYCLES*/ }
            0x18 /* CLC */ => { self.p.remove(Status::CARRY); t_upc!(self, 1 /*LEN*/); 2 /*CYCLES*/ }
            0xd8 /* CLD */ => { self.p.remove(Status::DECIMAL); t_upc!(self, 1 /*LEN*/); 2 /*CYCLES*/ }
            0xb8 /* CLV */ => { self.p.remove(Status::OVERFLOW); t_upc!(self, 1 /*LEN*/); 2 /*CYCLES*/ }

            0x24 /* BIT zp */ => { let op = self.a_zp(bus); o_bit!(self, bus, op, 2 /*LEN*/); 3 /*CYCLES*/ }

            0xa9 /* LDA imm */ => { o_load!(self, bus, a_imm, a, 2 /*LEN*/); 2 /*CYCLES*/ }

            0xa2 /* LDX imm */ => { o_load!(self, bus, a_imm, x, 2 /*LEN*/); 2 /*CYCLES*/ }

            0xa0 /* LDY imm */ => { o_load!(self, bus, a_imm, y, 2 /*LEN*/); 2 /*CYCLES*/ }

            0x85 /* STA zp */ => { o_store_zp!(self, bus, a, 2 /*LEN*/); 3 /*CYCLES*/ }

            0x86 /* STX zp */ => { o_store_zp!(self, bus, x, 2 /*LEN*/); 3 /*CYCLES*/ }

            0x84 /* STY zp */ => { o_store_zp!(self, bus, y, 2 /*LEN*/); 3 /*CYCLES*/ }

            0x20 /* JSR */ => {
                let ta = a_abs16!(self, bus);
                t_upc!(self, 2);
                bus.write_u8(0x100 + self.s as u16, (self.pc >> 8) as u8);
                self.s = self.s.wrapping_sub(1);
                bus.write_u8(0x100 + self.s as u16, (self.pc & 0xff) as u8);
                self.s = self.s.wrapping_sub(1);
                self.pc = ta;
                6
            }

            0xb0 /* BCS rel */ => { self.t_branch(bus, self.p.contains(Status::CARRY)) }
            0x90 /* BCC rel */ => { self.t_branch(bus, !self.p.contains(Status::CARRY)) }
            0xf0 /* BEQ rel */ => { self.t_branch(bus, self.p.contains(Status::ZERO)) }
            0xd0 /* BNE rel */ => { self.t_branch(bus, !self.p.contains(Status::ZERO)) }
            0x70 /* BVS rel */ => { self.t_branch(bus, self.p.contains(Status::OVERFLOW)) }
            0x50 /* BVC rel */ => { self.t_branch(bus, !self.p.contains(Status::OVERFLOW)) }
            0x30 /* BMI rel */ => { self.t_branch(bus, self.p.contains(Status::NEGATIVE)) }
            0x10 /* BPL rel */ => { self.t_branch(bus, !self.p.contains(Status::NEGATIVE)) }

            0xea /* NOP */ => { t_upc!(self, 1 /*LEN*/); 2 /*CYCLES*/ }

            0x60 /* RTS */ => {
                self.s = self.s.wrapping_add(1);
                let mut ra = bus.read_u8(0x100 + self.s as u16) as u16;
                self.s = self.s.wrapping_add(1);
                ra = ra | ((bus.read_u8(0x100 + self.s as u16) as u16) << 8);
                self.pc = ra.wrapping_add(1);
                6 /*CYCLES*/
            }

            _ => { panic!("Invalid opcode {:#04x} [ {} ]", op, self); }
        }
    }

    // relative addressing: sign extend and calc pc offset
    pub fn a_rel(&self, bus: &mut dyn sys::MemoryAccessA16D8) -> u16 {
        let offset = a_imm!(self, bus) as u16;
        self.pc.wrapping_add(2 + if offset & 0x80 == 0x80 { 0xff00 | offset } else { offset })
    }

    // zeropage addressing
    pub fn a_zp(&self, bus: &mut dyn sys::MemoryAccessA16D8) -> u8 {
        let a = bus.read_u8(self.pc.wrapping_add(1)) as u16;
        bus.read_u8(a)
    }

    // generic branch function
    pub fn t_branch(&mut self, bus: &mut dyn sys::MemoryAccessA16D8, jump: bool) -> u32 {
        if jump {
            let old_pc = self.pc;
            self.pc = self.a_rel(bus);
            if (self.pc & 0xFF00) == (old_pc & 0xFF00) {
                return 3;
            } else {
                return 4;
            }
        }
        t_upc!(self, 2);
        2
    }
    
    pub fn step(&mut self, bus: &mut dyn sys::MemoryAccessA16D8) {
        let op = bus.read_u8(self.pc);
        let (s, _) = self.debug_op(bus, op);
        println!("{:#06X} {}\t\t{}", self.pc, s, self.cycle);
        self.cycle = self.cycle + self.exec_op(bus, op);
    }
}
