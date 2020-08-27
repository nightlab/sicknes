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
        const BREAK = 1 << 4;
        const B = 1 << 5;
        const OVERFLOW = 1 << 6;
        const NEGATIVE = 1 << 7;
    }
}

pub struct Mos6502 {
    a: u8,
    x: u8,
    y: u8,
    s: u8,
    p: Status,
    pc: u16,
    cycle: u32,
    cfg_iops: bool
}

macro_rules! o_inc {
    ($this:ident, $reg:ident, $len:expr) => {
        $this.$reg = $this.$reg.wrapping_add(1);
        f_nz!($this, $this.$reg);
        t_upc!($this, $len);
    }
}

macro_rules! o_dec {
    ($this:ident, $reg:ident, $len:expr) => {
        $this.$reg = $this.$reg.wrapping_sub(1);
        f_nz!($this, $this.$reg);
        t_upc!($this, $len);
    }
}

macro_rules! o_store_zp {
    ($this:ident, $bus:expr, $reg:ident, $len:expr) => {
        let adr = $this.a_imm($bus) as u16;
        $bus.write_u8(adr, $this.$reg);
        t_upc!($this, $len);
    }
}

macro_rules! f_nz {
    ($this:ident, $value:expr) => {
        $this.p.set(Status::ZERO, $value == 0);
        $this.p.set(Status::NEGATIVE, ($value & 0x80) > 0);
    }
}

macro_rules! t_upc {
    ($this:ident, $len:expr) => { $this.pc = $this.pc.wrapping_add($len); }
}

impl fmt::Display for Mos6502 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
            "A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
            self.a, self.x, self.y, self.p, self.s
        )
    }
}

impl Mos6502 {
    pub fn new(cfg_iops: bool) -> Mos6502 { 
        Mos6502 {
            a: 0, x: 0, y: 0, s: 0,
            p: Status { bits: 0 },
            pc: 0, cycle: 0,
            cfg_iops
        }
    }

    pub fn reset(&mut self, bus: &mut dyn sys::MemoryAccessA16D8) {
        self.a = 0;
        self.x = 1;
        self.y = 0;
        self.s = 0xfd;
        self.p.bits = 0x24; // @todo bit 4 set after reset or not??? 
        self.pc = (bus.read_u8(0xfffd) as u16) << 8 | bus.read_u8(0xfffc) as u16;
        self.pc = 0xC000;
        self.cycle = 7;
    }

    // dump opcode
    pub fn debug_op(&self, bus: &mut dyn sys::MemoryAccessA16D8, op: u8) -> (String, u16) {
        match op {
            0x08 /* PHP */ => { (format!("PHP"), 1) }
            0x48 /* PHA */ => { (format!("PHA"), 1) }
            0x68 /* PLA */ => { (format!("PLA"), 1) }
            0x28 /* PLP */ => { (format!("PLP"), 1) }
            0x40 /* RTI */ => { (format!("RTI"), 1) }

            0x4c /* JMP abs */ => { self.dump_abs16(bus, "JMP") }
            0x6c /* JMP (abs) */ => { self.dump_abs_ind(bus, "JMP") }
            
            0x78 /* SEI */ => { (format!("SEI"), 1) }
            0x38 /* SEC */ => { (format!("SEC"), 1) }
            0xf8 /* SED */ => { (format!("SED"), 1) }
            0x58 /* CLI */ => { (format!("CLI"), 1) }
            0x18 /* CLC */ => { (format!("CLC"), 1) }
            0xd8 /* CLD */ => { (format!("CLD"), 1) }
            0xb8 /* CLV */ => { (format!("CLV"), 1) }

            0xe6 /* INC zp */ => { self.dump_zp(bus, "INC") }
            0xf6 /* INC zp,X */ => { self.dump_zp_x(bus, "INC") }
            0xee /* INC abs */ => { self.dump_abs16(bus, "INC") }
            0xfe /* INC abs,X */ => { self.dump_abs_x(bus, "INC") }
            0xe8 /* INX */ => { (format!("INX"), 1) }
            0xc8 /* INY */ => { (format!("INY"), 1) }

            0xc6 /* DEC zp */ => { self.dump_zp(bus, "DEC") }
            0xd6 /* INC zp,X */ => { self.dump_zp_x(bus, "DEC") }
            0xce /* DEC abs */ => { self.dump_abs16(bus, "DEC") }
            0xde /* DEC abs,X */ => { self.dump_abs_x(bus, "DEC") }
            0xca /* DEX */ => { (format!("DEX"), 1) }
            0x88 /* DEY */ => { (format!("DEY"), 1) }

            0xaa /* TAX */ => { (format!("TAX"), 1) }
            0xa8 /* TAY */ => { (format!("TAY"), 1) }
            0xba /* TSX */ => { (format!("TSX"), 1) }
            0x8a /* TXA */ => { (format!("TXA"), 1) }
            0x9a /* TXS */ => { (format!("TXS"), 1) }
            0x98 /* TYA */ => { (format!("TYA"), 1) }

            0x24 /* BIT zp */ => { self.dump_zp(bus, "BIT") }
            0x2c /* BIT abs */ => { self.dump_abs16(bus, "BIT") }

            0x29 /* AND imm */ => { self.dump_imm(bus, "AND") }
            0x25 /* AND zp */ => { self.dump_zp(bus, "AND") }
            0x35 /* AND zp,X */ => { self.dump_zp_x(bus, "AND") }
            0x2d /* AND abs */ => { self.dump_abs16(bus, "AND") }
            0x3d /* AND abs,X */ => { self.dump_abs_x(bus, "AND") }
            0x39 /* AND abs,Y */ => { self.dump_abs_y(bus, "AND") }
            0x21 /* AND (ind,X) */ => { self.dump_idx_x(bus, "AND") }
            0x31 /* AND (ind),Y */ => { self.dump_ind_y(bus, "AND") }
            
            0x09 /* ORA imm */ => { self.dump_imm(bus, "ORA") }
            0x05 /* ORA zp */ => { self.dump_zp(bus, "ORA") }
            0x15 /* ORA zp,X */ => { self.dump_zp_x(bus, "ORA") }
            0x0d /* ORA abs */ => { self.dump_abs16(bus, "ORA") }
            0x1d /* ORA abs,X */ => { self.dump_abs_x(bus, "ORA") }
            0x19 /* ORA abs,Y */ => { self.dump_abs_y(bus, "ORA") }
            0x01 /* ORA (ind,X) */ => { self.dump_idx_x(bus, "ORA") }
            0x11 /* ORA (ind),Y */ => { self.dump_ind_y(bus, "ORA") }
            
            0x49 /* EOR imm */ => { self.dump_imm(bus, "EOR") }
            0x45 /* EOR zp */ => { self.dump_zp(bus, "EOR") }
            0x55 /* EOR zp,X */ => { self.dump_zp_x(bus, "EOR") }
            0x4d /* EOR abs */ => { self.dump_abs16(bus, "EOR") }
            0x5d /* EOR abs,X */ => { self.dump_abs_x(bus, "EOR") }
            0x59 /* EOR abs,Y */ => { self.dump_abs_y(bus, "EOR") }
            0x41 /* EOR (ind,X) */ => { self.dump_idx_x(bus, "EOR") }
            0x51 /* EOR (ind),Y */ => { self.dump_ind_y(bus, "EOR") }

            0x0a /* ASL A */ => { (format!("ASL A"), 1) }
            0x06 /* ASL zp */ => { self.dump_zp(bus, "ASL") }
            0x16 /* ASL zp,X */ => { self.dump_zp_x(bus, "ASL") }
            0x0e /* ASL abs */ => { self.dump_abs16(bus, "ASL") }
            0x1e /* ASL abs,X */ => { self.dump_abs_x(bus, "ASL") }
            
            0x4a /* LSR A */ => { (format!("LSR A"), 1) }
            0x46 /* LSR zp */ => { self.dump_zp(bus, "LSR") }
            0x56 /* LSR zp,X */ => { self.dump_zp_x(bus, "LSR") }
            0x4e /* LSR abs */ => { self.dump_abs16(bus, "LSR") }
            0x5e /* LSR abs,X */ => { self.dump_abs_x(bus, "LSR") }

            0x6a /* ROR A */ => { (format!("ROR A"), 1) }
            0x66 /* ROR zp */ => { self.dump_zp(bus, "ROR") }
            0x76 /* ROR zp,X */ => { self.dump_zp_x(bus, "ROR") }
            0x6e /* ROR abs */ => { self.dump_abs16(bus, "ROR") }
            0x7e /* ROR abs,X */ => { self.dump_abs_x(bus, "ROR") }

            0x2a /* ROL A */ => { (format!("ROL A"), 1) }
            0x26 /* ROL zp */ => { self.dump_zp(bus, "ROL") }
            0x36 /* ROL zp,X */ => { self.dump_zp_x(bus, "ROL") }
            0x2e /* ROL abs */ => { self.dump_abs16(bus, "ROL") }
            0x3e /* ROL abs,X */ => { self.dump_abs_x(bus, "ROL") }
            
            0x69 /* ADC imm */ => { self.dump_imm(bus, "ADC") }
            0x65 /* ADC zp */ => { self.dump_zp(bus, "ADC") }
            0x75 /* ADC zp,X */ => { self.dump_zp_x(bus, "ADC") }
            0x6d /* ADC abs */ => { self.dump_abs16(bus, "ADC") }
            0x7d /* ADC abs,X */ => { self.dump_abs_x(bus, "ADC") }
            0x79 /* ADC abs,Y */ => { self.dump_abs_y(bus, "ADC") }
            0x61 /* ADC (ind,X) */ => { self.dump_idx_x(bus, "ADC") }
            0x71 /* ADC (ind),Y */ => { self.dump_ind_y(bus, "ADC") }

            0xc9 /* CMP imm */ => { self.dump_imm(bus, "CMP") }
            0xc5 /* CMP zp */ => { self.dump_zp(bus, "CMP") }
            0xd5 /* CMP zp,X */ => { self.dump_zp_x(bus, "CMP") }
            0xcd /* CMP abs */ => { self.dump_abs16(bus, "CMP") }
            0xdd /* CMP abs,X */ => { self.dump_abs_x(bus, "CMP") }
            0xd9 /* CMP abs,Y */ => { self.dump_abs_y(bus, "CMP") }
            0xc1 /* CMP (ind,X) */ => { self.dump_idx_x(bus, "CMP") }
            0xd1 /* CMP (ind),Y */ => { self.dump_ind_y(bus, "ADC") }

            0xc0 /* CPY imm */ => { self.dump_imm(bus, "CPY") }
            0xc4 /* CPY zp */ => { self.dump_zp(bus, "CPY") }
            0xcc /* CPY abs */ => { self.dump_abs16(bus, "CPY") }

            0xe0 /* CPX imm */ => { self.dump_imm(bus, "CPX") }
            0xe4 /* CPX zp */ => { self.dump_zp(bus, "CPX") }
            0xec /* CPX abs */ => { self.dump_abs16(bus, "CPX") }

            0xe9 /* SBC imm */ => { self.dump_imm(bus, "SBC") }
            0xe5 /* SBC zp */ => { self.dump_zp(bus, "SBC") }
            0xf5 /* SBC zp,X */ => { self.dump_zp_x(bus, "SBC") }
            0xed /* SBC abs */ => { self.dump_abs16(bus, "SBC") }
            0xfd /* SBC abs,X */ => { self.dump_abs_x(bus, "SBC") }
            0xf9 /* SBC abs,Y */ => { self.dump_abs_y(bus, "SBC") }
            0xe1 /* SBC (ind,X) */ => { self.dump_idx_x(bus, "SBC") }
            0xf1 /* SBC (ind),Y */ => { self.dump_ind_y(bus, "SBC") }

            0xa9 /* LDA imm */ => { self.dump_imm(bus, "LDA") }
            0xa5 /* LDA zp */ => { self.dump_zp(bus, "LDA") }
            0xb5 /* LDA zp,X */ => { self.dump_zp_x(bus, "LDA") }
            0xad /* LDA abs */ => { self.dump_abs16(bus, "LDA") }
            0xbd /* LDA abs,X */ => { self.dump_abs_x(bus, "LDA") }
            0xb9 /* LDA abs,Y */ => { self.dump_abs_y(bus, "LDA") }
            0xa1 /* LDA (ind,X) */ => { self.dump_idx_x(bus, "LDA") }
            0xb1 /* LDA (ind),Y */ => { self.dump_ind_y(bus, "LDA") }

            0xa2 /* LDX imm */ => { self.dump_imm(bus, "LDX") }
            0xa6 /* LDX zp */ => { self.dump_zp(bus, "LDX") }
            0xb6 /* LDX zp,Y */ => { self.dump_zp_y(bus, "LDX") }
            0xae /* LDX abs */ => { self.dump_abs16(bus, "LDX") }
            0xbe /* LDX abs,Y */ => { self.dump_abs_y(bus, "LDX") }
            
            0xa0 /* LDY imm */ => { self.dump_imm(bus, "LDY") }
            0xa4 /* LDY zp */ => { self.dump_zp(bus, "LDY") }
            0xb4 /* LDY zp,X */ => { self.dump_zp_x(bus, "LDY") }
            0xac /* LDY abs */ => { self.dump_abs16(bus, "LDY") }
            0xbc /* LDY abs,X */ => { self.dump_abs_x(bus, "LDY") }

            0x85 /* STA zp */ => { self.dump_zp(bus, "STA") }
            0x95 /* STA zp,X */ => { self.dump_zp_x(bus, "STA") }
            0x8d /* STA abs */ => { self.dump_abs16(bus, "STA") }
            0x9d /* STA abs,X */ => { self.dump_abs_x(bus, "STA") }
            0x99 /* STA abs,Y */ => { self.dump_abs_y(bus, "STA") }
            0x81 /* STA (ind,X) */ => { self.dump_idx_x(bus, "STA") }
            0x91 /* STA (ind),Y */ => { self.dump_ind_y(bus, "STA") }

            0x86 /* STX zp */ => { self.dump_zp(bus, "STX") }
            0x96 /* STX zp,Y */ => { self.dump_zp_y(bus, "STX") }
            0x8e /* STX abs */ => { self.dump_abs16(bus, "STX") }

            0x84 /* STY zp */ => { self.dump_zp(bus, "STY") }
            0x94 /* STY zp,X */ => { self.dump_zp_x(bus, "STY") }
            0x8c /* STY abs */ => { self.dump_abs16(bus, "STY") }

            0x20 /* JSR abs */ => { self.dump_abs16(bus, "JSR") }

            0xb0 /* BCS rel */ => { self.dump_rel(bus, "BCS") }
            0x90 /* BCC rel */ => { self.dump_rel(bus, "BCC") }
            0xf0 /* BEQ rel */ => { self.dump_rel(bus, "BEQ") }
            0xd0 /* BNE rel */ => { self.dump_rel(bus, "BNE") }
            0x70 /* BVS rel */ => { self.dump_rel(bus, "BVS") }
            0x50 /* BVC rel */ => { self.dump_rel(bus, "BVC") }
            0x30 /* BMI rel */ => { self.dump_rel(bus, "BMI") }
            0x10 /* BPL rel */ => { self.dump_rel(bus, "BPL") }

            0x60 /* NOP */ => { (format!("RTS"), 1) }

            0xea /* NOP */ => { (format!("NOP"), 1) }

            0x00 /* BRK */ => { (format!("BRK"), 1) }

            _ => { 
                // illegal opcodes
                if self.cfg_iops {
                    match op {
                        0x1a | 0x3a | 0x5a | 0x7a | 0xda | 0xfa /* NOP */ => { (format!("*NOP"), 1) }
                        0x80 /* NOP imm */ => { self.dump_imm(bus, "*NOP") }
                        0x04 | 0x44 | 0x64 /* NOP zp */ => { self.dump_zp(bus, "*NOP") }
                        0x14 | 0x34 | 0x54 | 0x74 | 0xd4 | 0xf4 /* NOP zp,X */ => { self.dump_zp_x(bus, "*NOP") }
                        0x0c /* NOP abs */ => { self.dump_abs16(bus, "*NOP") }
                        0x1c | 0x3c | 0x5c | 0x7c | 0xdc | 0xfc /* NOP abs,X */ => { self.dump_abs_x(bus, "*NOP") }

                        0xa7 /* LAX zp */ => { self.dump_zp(bus, "*LAX") }
                        0xb7 /* LAX zp,Y */ => { self.dump_zp_y(bus, "*LAX") }
                        0xaf /* LAX abs */ => { self.dump_abs16(bus, "*LAX") }
                        0xbf /* LAX abs,Y */ => { self.dump_abs_y(bus, "*LAX") }
                        0xa3 /* LAX (ind,X) */ => { self.dump_idx_x(bus, "*LAX") }
                        0xb3 /* LAX (ind),Y */ => { self.dump_ind_y(bus, "*LAX") }

                        0x87 /* SAX zp */ => { self.dump_zp(bus, "*SAX") }
                        0x8f /* SAX abs */ => { self.dump_abs16(bus, "*SAX") }
                        0x83 /* SAX (ind,X) */ => { self.dump_idx_x(bus, "*SAX") }
                        0x97 /* SAX zp,Y */ => { self.dump_zp_y(bus, "*SAX") }

                        0xeb /* SBC imm */ => { self.dump_imm(bus, "*SBC") }

                        0xc7 /* DCP zp */ => { self.dump_zp(bus, "*DCP") }
                        0xd7 /* DCP zp,X */ => { self.dump_zp_x(bus, "*DCP") }
                        0xcf /* DCP abs */ => { self.dump_abs16(bus, "*DCP") }
                        0xdf /* DCP abs,X */ => { self.dump_abs_x(bus, "*DCP") }
                        0xdb /* DCP abs,Y */ => { self.dump_abs_y(bus, "*DCP") }
                        0xc3 /* DCP (ind,X) */ => { self.dump_idx_x(bus, "*DCP") }
                        0xd3 /* DCP (ind),Y */ => { self.dump_ind_y(bus, "*DCP") }

                        0xe7 /* ISB zp */ => { self.dump_zp(bus, "*ISB") }
                        0xf7 /* ISB zp,X */ => { self.dump_zp_x(bus, "*ISB") }
                        0xef /* ISB abs */ => { self.dump_abs16(bus, "*ISB") }
                        0xff /* ISB abs,X */ => { self.dump_abs_x(bus, "*ISB") }
                        0xfb /* ISB abs,Y */ => { self.dump_abs_y(bus, "*ISB") }
                        0xe3 /* ISB (ind,X) */ => { self.dump_idx_x(bus, "*ISB") }
                        0xf3 /* ISB (ind),Y */ => { self.dump_ind_y(bus, "*ISB") }

                        0x07 /* SLO zp */ => { self.dump_zp(bus, "*SLO") }
                        0x17 /* SLO zp,X */ => { self.dump_zp_x(bus, "*SLO") }
                        0x0f /* SLO abs */ => { self.dump_abs16(bus, "*SLO") }
                        0x1f /* SLO abs,X */ => { self.dump_abs_x(bus, "*SLO") }
                        0x1b /* SLO abs,Y */ => { self.dump_abs_y(bus, "*SLO") }
                        0x03 /* SLO (ind,X) */ => { self.dump_idx_x(bus, "*SLO") }
                        0x13 /* SLO (ind),Y */ => { self.dump_ind_y(bus, "*SLO") }

                        0x27 /* RLA zp */ => { self.dump_zp(bus, "*RLA") }
                        0x37 /* RLA zp,X */ => { self.dump_zp_x(bus, "*RLA") }
                        0x2f /* RLA abs */ => { self.dump_abs16(bus, "*RLA") }
                        0x3f /* RLA abs,X */ => { self.dump_abs_x(bus, "*RLA") }
                        0x3b /* RLA abs,Y */ => { self.dump_abs_y(bus, "*RLA") }
                        0x23 /* RLA (ind,X) */ => { self.dump_idx_x(bus, "*RLA") }
                        0x33 /* RLA (ind),Y */ => { self.dump_ind_y(bus, "*RLA") }

                        _ => { panic!("Invalid opcode {:#04x} [ {} ]", op, self); }
                    }
                } else {
                    panic!("Invalid opcode {:#04x} [ {} ]", op, self);
                }
            }
        }
    }

    pub fn dump_zp(&self, bus: &mut dyn sys::MemoryAccessA16D8, mnemonic: &str) -> (String, u16) {
        let a = self.a_imm(bus);
        (format!("{} ${:02X} = {:02X}", mnemonic, a, bus.read_u8(a as u16)), 2)
    }

    pub fn dump_zp_x(&self, bus: &mut dyn sys::MemoryAccessA16D8, mnemonic: &str) -> (String, u16) {
        let base = bus.read_u8(self.pc.wrapping_add(1));
        let adr = base.wrapping_add(self.x);
        (format!("{} ${:02X},X @ ${:02X} = {:02X}", mnemonic, base, adr, bus.read_u8(adr as u16)), 2)
    }

    pub fn dump_zp_y(&self, bus: &mut dyn sys::MemoryAccessA16D8, mnemonic: &str) -> (String, u16) {
        let base = bus.read_u8(self.pc.wrapping_add(1));
        let adr = base.wrapping_add(self.y);
        (format!("{} ${:02X},X @ ${:02X} = {:02X}", mnemonic, base, adr, bus.read_u8(adr as u16)), 2)
    }

    pub fn dump_idx_x(&self, bus: &mut dyn sys::MemoryAccessA16D8, mnemonic: &str) -> (String, u16) {
        let z = self.a_imm(bus);
        let a = self.a_idx_x(bus) as u16;
        (format!("{} (${:02X},X) @ {:02X} = {:04X} = {:02X}", mnemonic, z, z.wrapping_add(self.x), a, bus.read_u8(a as u16)), 2)
    }

    pub fn dump_ind_y(&self, bus: &mut dyn sys::MemoryAccessA16D8, mnemonic: &str) -> (String, u16) {
        let zp_adr = bus.read_u8(self.pc.wrapping_add(1));
        let base = (bus.read_u8(zp_adr.wrapping_add(1) as u16) as u16) << 8 | bus.read_u8(zp_adr as u16) as u16;
        let adr = base.wrapping_add(self.y as u16);
        (format!("{} (${:02X}),Y = {:04X} @ {:04X} = {:02X}", mnemonic, zp_adr, base, adr, bus.read_u8(adr)), 2)
    }

    pub fn dump_abs16(&self, bus: &mut dyn sys::MemoryAccessA16D8, mnemonic: &str) -> (String, u16) {
        let a = self.a_abs16(bus);
        (format!("{} ${:04X} = {:02X}", mnemonic, a, bus.read_u8(a as u16)), 2)
    }

    pub fn dump_abs_x(&self, bus: &mut dyn sys::MemoryAccessA16D8, mnemonic: &str) -> (String, u16) {
        let base = self.a_abs16(bus);
        let adr = base.wrapping_add(self.y as u16);
        (format!("{} ${:04X},X @ {:04X} = {:02X}", mnemonic, base, adr, bus.read_u8(adr)), 3)
    }

    pub fn dump_abs_y(&self, bus: &mut dyn sys::MemoryAccessA16D8, mnemonic: &str) -> (String, u16) {
        let base = self.a_abs16(bus);
        let adr = base.wrapping_add(self.y as u16);
        (format!("{} ${:04X},Y @ {:04X} = {:02X}", mnemonic, base, adr, bus.read_u8(adr)), 3)
    }

    pub fn dump_abs_ind(&self, bus: &mut dyn sys::MemoryAccessA16D8, mnemonic: &str) -> (String, u16) {
        let base = self.a_abs16(bus);
        let target;
        if base & 0xff == 0xff {
            target = (bus.read_u8(base & 0xff00) as u16) << 8 | bus.read_u8(base) as u16;
        } else {
            target = (bus.read_u8(base + 1) as u16) << 8 | bus.read_u8(base) as u16;
        }
        (format!("{} (${:04X}) = {:04X}", mnemonic, base, target), 3)
    }

    pub fn dump_imm(&self, bus: &mut dyn sys::MemoryAccessA16D8, mnemonic: &str) -> (String, u16) {
        (format!("{} #${:02X}", mnemonic, self.a_imm(bus)), 2)
    }

    pub fn dump_rel(&self, bus: &mut dyn sys::MemoryAccessA16D8, mnemonic: &str) -> (String, u16) {
        (format!("{} ${:04X}", mnemonic, self.a_rel(bus)), 2)
    }

    // execute opcode
    pub fn exec_op(&mut self, bus: &mut dyn sys::MemoryAccessA16D8, op: u8) -> u32 {
        match op {
            0x08 /* PHP */ => { bus.write_u8(0x100 + self.s as u16, self.p.bits | 0x30); self.s = self.s.wrapping_sub(1); self.fin(1, 3) }
            0x48 /* PHA */ => { bus.write_u8(0x100 + self.s as u16, self.a); self.s = self.s.wrapping_sub(1); self.fin(1, 3) }
            0x68 /* PLA */ => { self.s = self.s.wrapping_add(1); self.a = bus.read_u8(0x100 + self.s as u16); f_nz!(self, self.a); self.fin(1, 4) }
            0x28 /* PLP */ => {
                self.s = self.s.wrapping_add(1);
                self.p.bits = bus.read_u8(0x100 + self.s as u16) | 0x20;
                self.p.remove(Status::BREAK);
                self.fin(1, 4)
            }
            
            0x40 /* RTI */ => {
                self.s = self.s.wrapping_add(1);
                self.p.bits = bus.read_u8(0x100 + self.s as u16) | 0x20;
                self.s = self.s.wrapping_add(1);
                let ra = bus.read_u8(0x100 + self.s as u16) as u16;
                self.s = self.s.wrapping_add(1);
                self.pc = ra | ((bus.read_u8(0x100 + self.s as u16) as u16) << 8);
                6 /*CYCLES*/
            }            

            0x4c /* JMP abs */ => { self.pc = self.a_abs16(bus); 3 /*CYCLES*/ }
            0x6c /* JMP (abs) */ => {
                let base = self.a_abs16(bus);
                if base & 0xff == 0xff {
                    self.pc = (bus.read_u8(base & 0xff00) as u16) << 8 | bus.read_u8(base) as u16;
                } else {
                    self.pc = (bus.read_u8(base + 1) as u16) << 8 | bus.read_u8(base) as u16;
                }
                5 /*CYCLES*/
            }

            0x78 /* SEI */ => { self.p.insert(Status::INT_DIS); self.fin(1, 2) }
            0x38 /* SEC */ => { self.p.insert(Status::CARRY); self.fin(1, 2) }
            0xf8 /* SED */ => { self.p.insert(Status::DECIMAL); self.fin(1, 2) }
            0x58 /* CLI */ => { self.p.remove(Status::INT_DIS); self.fin(1, 2) }
            0x18 /* CLC */ => { self.p.remove(Status::CARRY); self.fin(1, 2) }
            0xd8 /* CLD */ => { self.p.remove(Status::DECIMAL); self.fin(1, 2) }
            0xb8 /* CLV */ => { self.p.remove(Status::OVERFLOW); self.fin(1, 2) }

            0xe6 /* INC zp */ => { let (op, adr) = self.a_zp(bus); bus.write_u8(adr, self.o_inc(op)); self.fin(2, 5) }
            0xf6 /* INC zp,X */ => { let a = self.a_zp_x(bus); let op = bus.read_u8(a as u16); bus.write_u8(a as u16, self.o_inc(op)); self.fin(2, 6) }
            0xee /* INC abs */ => { let a = self.a_abs16(bus); let op = bus.read_u8(a); bus.write_u8(a, self.o_inc(op)); self.fin(3, 6) }
            0xfe /* INC abs,X */ => { let a = self.a_abs_x(bus).0; let op = bus.read_u8(a); bus.write_u8(a, self.o_inc(op)); self.fin(3, 7) }
            0xe8 /* INX */ => { o_inc!(self, x, 1 /*LEN*/); 2 /*CYCLES*/ }
            0xc8 /* INY */ => { o_inc!(self, y, 1 /*LEN*/); 2 /*CYCLES*/ }
            
            0xc6 /* DEC zp */ => { let (op, adr) = self.a_zp(bus); bus.write_u8(adr, self.o_dec(op)); self.fin(2, 5) }
            0xd6 /* DEC zp,X */ => { let a = self.a_zp_x(bus); let op = bus.read_u8(a as u16); bus.write_u8(a as u16, self.o_dec(op)); self.fin(2, 6) }
            0xce /* DEC abs */ => { let a = self.a_abs16(bus); let op = bus.read_u8(a); bus.write_u8(a, self.o_dec(op)); self.fin(3, 6) }
            0xde /* DEC abs,X */ => { let a = self.a_abs_x(bus).0; let op = bus.read_u8(a); bus.write_u8(a, self.o_dec(op)); self.fin(3, 7) }
            0xca /* DEX */ => { o_dec!(self, x, 1 /*LEN*/); 2 /*CYCLES*/ }
            0x88 /* DEY */ => { o_dec!(self, y, 1 /*LEN*/); 2 /*CYCLES*/ }

            0xaa /* TAX */ => { self.x = self.a; f_nz!(self, self.x); self.fin(1, 2) }
            0xa8 /* TAY */ => { self.y = self.a; f_nz!(self, self.y); self.fin(1, 2) }
            0xba /* TSX */ => { self.x = self.s; f_nz!(self, self.x); self.fin(1, 2) }
            0x8a /* TXA */ => { self.a = self.x; f_nz!(self, self.a); self.fin(1, 2) }
            0x9a /* TXS */ => { self.s = self.x; self.fin(1, 2) }
            0x98 /* TYA */ => { self.a = self.y; f_nz!(self, self.a); self.fin(1, 2) }

            0x24 /* BIT zp */ => { self.o_bit(self.a_zp(bus).0); self.fin(2, 3) }
            0x2c /* BIT abs */ => { let a = self.a_abs16(bus); self.o_bit(bus.read_u8(a)); self.fin(3, 4) }

            0x29 /* AND imm */ => { self.o_and(self.a_imm(bus)); self.fin(2, 2) }
            0x25 /* AND zp */ => { self.o_and(self.a_zp(bus).0); self.fin(2, 3) }
            0x35 /* AND zp,X */ => { let a = self.a_zp_x(bus); self.o_and(bus.read_u8(a as u16)); self.fin(2, 4) }
            0x2d /* AND abs */ => { let a = self.a_abs16(bus); self.o_and(bus.read_u8(a)); self.fin(3, 4) }
            0x3d /* AND abs,X */ => { let (a, xtra) = self.a_abs_x(bus); self.o_and(bus.read_u8(a)); self.fin(3, 4 + xtra) }
            0x39 /* AND abs,Y */ => { let (a, xtra) = self.a_abs_y(bus); self.o_and(bus.read_u8(a)); self.fin(3, 4 + xtra) }
            0x21 /* AND (ind,X) */ => { let a = self.a_idx_x(bus) as u16; self.o_and(bus.read_u8(a)); self.fin(2, 6) }
            0x31 /* AND (ind),Y */ => { let (a, xtra) = self.a_ind_y(bus); self.o_and(bus.read_u8(a)); self.fin(2, 5 + xtra) }

            0x09 /* ORA imm */ => { self.o_or(self.a_imm(bus)); self.fin(2, 2) }
            0x05 /* ORA zp */ => { self.o_or(self.a_zp(bus).0); self.fin(2, 3) }
            0x15 /* ORA zp,X */ => { let a = self.a_zp_x(bus); self.o_or(bus.read_u8(a as u16)); self.fin(2, 4) }
            0x0d /* ORA abs */ => { let a = self.a_abs16(bus); self.o_or(bus.read_u8(a)); self.fin(3, 4) }
            0x1d /* ORA abs,X */ => { let (a, xtra) = self.a_abs_x(bus); self.o_or(bus.read_u8(a)); self.fin(3, 4 + xtra) }
            0x19 /* ORA abs,Y */ => { let (a, xtra) = self.a_abs_y(bus); self.o_or(bus.read_u8(a)); self.fin(3, 4 + xtra) }
            0x01 /* ORA (ind,X) */ => { let a = self.a_idx_x(bus) as u16; self.o_or(bus.read_u8(a)); self.fin(2, 6) }
            0x11 /* ORA (ind),Y */ => { let (a, xtra) = self.a_ind_y(bus); self.o_or(bus.read_u8(a)); self.fin(2, 5 + xtra) }

            0x49 /* EOR imm */ => { self.o_xor(self.a_imm(bus)); self.fin(2, 2) }
            0x45 /* EOR zp */ => { self.o_xor(self.a_zp(bus).0); self.fin(2, 3) }
            0x55 /* EOR zp,X */ => { let a = self.a_zp_x(bus); self.o_xor(bus.read_u8(a as u16)); self.fin(2, 4) }
            0x4d /* EOR abs */ => { let a = self.a_abs16(bus); self.o_xor(bus.read_u8(a)); self.fin(3, 4) }
            0x5d /* EOR abs,X */ => { let (a, xtra) = self.a_abs_x(bus); self.o_xor(bus.read_u8(a)); self.fin(3, 4 + xtra) }
            0x59 /* EOR abs,Y */ => { let (a, xtra) = self.a_abs_y(bus); self.o_xor(bus.read_u8(a)); self.fin(3, 4 + xtra) }
            0x41 /* EOR (ind,X) */ => { let a = self.a_idx_x(bus) as u16; self.o_xor(bus.read_u8(a)); self.fin(2, 6) }
            0x51 /* EOR (ind),Y */ => { let (a, xtra) = self.a_ind_y(bus); self.o_xor(bus.read_u8(a)); self.fin(2, 5 + xtra) }

            0x0a /* ASL A */ => { self.a = self.o_asl(self.a); self.fin(1, 2) }
            0x06 /* ASL zp */ => { let (op, a) = self.a_zp(bus); bus.write_u8(a, self.o_asl(op)); self.fin(2, 5) }
            0x16 /* ASL zp,X */ => { let a = self.a_zp_x(bus); let op = bus.read_u8(a as u16); bus.write_u8(a as u16, self.o_asl(op)); self.fin(2, 6) }
            0x0e /* ASL abs */ => { let a = self.a_abs16(bus); let op = bus.read_u8(a); bus.write_u8(a, self.o_asl(op)); self.fin(3, 6) }
            0x1e /* ASL abs,X */ => { let a = self.a_abs_x(bus).0; let op = bus.read_u8(a); bus.write_u8(a, self.o_asl(op)); self.fin(3, 7) }
            
            0x4a /* LSR A */ => { self.a = self.o_lsr(self.a); self.fin(1, 2) }
            0x46 /* LSR zp */ => { let (op, a) = self.a_zp(bus); bus.write_u8(a, self.o_lsr(op)); self.fin(2, 5) }
            0x56 /* LSR zp,X */ => { let a = self.a_zp_x(bus); let op = bus.read_u8(a as u16); bus.write_u8(a as u16, self.o_lsr(op)); self.fin(2, 6) }
            0x4e /* LSR abs */ => { let a = self.a_abs16(bus); let op = bus.read_u8(a); bus.write_u8(a, self.o_lsr(op)); self.fin(3, 6) }
            0x5e /* LSR abs,X */ => { let a = self.a_abs_x(bus).0; let op = bus.read_u8(a); bus.write_u8(a, self.o_lsr(op)); self.fin(3, 7) }

            0x6a /* ROR A */ => { self.a = self.o_ror(self.a); self.fin(1, 2) }
            0x66 /* ROR zp */ => { let (op, adr) = self.a_zp(bus); bus.write_u8(adr, self.o_ror(op)); self.fin(2, 5) }
            0x76 /* ROR zp,X */ => { let a = self.a_zp_x(bus); let op = bus.read_u8(a as u16); bus.write_u8(a as u16, self.o_ror(op)); self.fin(2, 6) }
            0x6e /* ROR abs */ => { let a = self.a_abs16(bus); let op = bus.read_u8(a); bus.write_u8(a, self.o_ror(op)); self.fin(3, 6) }
            0x7e /* ROR abs,X */ => { let a = self.a_abs_x(bus).0; let op = bus.read_u8(a); bus.write_u8(a, self.o_ror(op)); self.fin(3, 7) }

            0x2a /* ROL A */ => { self.a = self.o_rol(self.a); self.fin(1, 2) }
            0x26 /* ROL zp */ => { let (op, adr) = self.a_zp(bus); bus.write_u8(adr, self.o_rol(op)); self.fin(2, 5) }
            0x36 /* ROL zp,X */ => { let a = self.a_zp_x(bus); let op = bus.read_u8(a as u16); bus.write_u8(a as u16, self.o_rol(op)); self.fin(2, 6) }
            0x2e /* ROL abs */ => { let a = self.a_abs16(bus); let op = bus.read_u8(a); bus.write_u8(a, self.o_rol(op)); self.fin(3, 6) }
            0x3e /* ROL abs,X */ => { let a = self.a_abs_x(bus).0; let op = bus.read_u8(a); bus.write_u8(a, self.o_rol(op)); self.fin(3, 7) }

            0x69 /* ADC imm */ => { self.a = self.o_adc(self.a, self.a_imm(bus)); self.fin(2, 2) }
            0x65 /* ADC zp */ => { self.a = self.o_adc(self.a, self.a_zp(bus).0); self.fin(2, 3) }
            0x75 /* ADC zp,X */ => { let a = self.a_zp_x(bus); self.a = self.o_adc(self.a, bus.read_u8(a as u16)); self.fin(2, 4) }
            0x6d /* ADC abs */ => { let a = self.a_abs16(bus); self.a = self.o_adc(self.a, bus.read_u8(a)); self.fin(3, 4) }
            0x7d /* ADC abs,X */ => { let (a, xtra) = self.a_abs_x(bus); self.a = self.o_adc(self.a, bus.read_u8(a)); self.fin(3, 4 + xtra) }
            0x79 /* ADC abs,Y */ => { let (a, xtra) = self.a_abs_y(bus); self.a = self.o_adc(self.a, bus.read_u8(a)); self.fin(3, 4 + xtra) }
            0x61 /* ADC (ind,X) */ => { let a = self.a_idx_x(bus) as u16; self.a = self.o_adc(self.a, bus.read_u8(a)); self.fin(2, 6) }
            0x71 /* ADC (ind),Y */ => { let (a, xtra) = self.a_ind_y(bus); self.a = self.o_adc(self.a, bus.read_u8(a)); self.fin(2, 5 + xtra) }

            0xc9 /* CMP imm */ => { self.o_cmp(self.a, self.a_imm(bus)); self.fin(2, 2) }
            0xc5 /* CMP zp */ => { self.o_cmp(self.a, self.a_zp(bus).0); self.fin(2, 3) }
            0xd5 /* CMP zp,X */ => { let a = self.a_zp_x(bus); self.o_cmp(self.a, bus.read_u8(a as u16)); self.fin(2, 4) }
            0xcd /* CMP abs */ => { let a = self.a_abs16(bus); self.o_cmp(self.a, bus.read_u8(a)); self.fin(3, 4) }
            0xdd /* CMP abs,X */ => { let (a, xtra) = self.a_abs_x(bus); self.o_cmp(self.a, bus.read_u8(a)); self.fin(3, 4 + xtra) }
            0xd9 /* CMP abs,Y */ => { let (a, xtra) = self.a_abs_y(bus); self.o_cmp(self.a, bus.read_u8(a)); self.fin(3, 4 + xtra) }
            0xc1 /* CMP (ind,X) */ => { let a = self.a_idx_x(bus) as u16; self.o_cmp(self.a, bus.read_u8(a)); self.fin(2, 6) }
            0xd1 /* CMP (ind),Y */ => { let (a, xtra) = self.a_ind_y(bus); self.o_cmp(self.a, bus.read_u8(a)); self.fin(2, 5 + xtra) }

            0xc0 /* CPY imm */ => { self.o_cmp(self.y, self.a_imm(bus)); self.fin(2, 2) }
            0xc4 /* CPY zp */ => { self.o_cmp(self.y, self.a_zp(bus).0); self.fin(2, 3) }
            0xcc /* CPY abs */ => { let a = self.a_abs16(bus); self.o_cmp(self.y, bus.read_u8(a)); self.fin(3, 4) }
            
            0xe0 /* CPX imm */ => { self.o_cmp(self.x, self.a_imm(bus)); self.fin(2, 2) }
            0xe4 /* CPX zp */ => { self.o_cmp(self.x, self.a_zp(bus).0); self.fin(2, 3) }
            0xec /* CPX abs */ => { let a = self.a_abs16(bus); self.o_cmp(self.x, bus.read_u8(a)); self.fin(3, 4) }

            0xe9 /* SBC imm */ => { self.a = self.o_adc(self.a, !self.a_imm(bus)); self.fin(2, 2) }
            0xe5 /* SBC zp */ => { self.a = self.o_adc(self.a, !self.a_zp(bus).0); self.fin(2, 3) }
            0xf5 /* SBC zp,X */ => { let a = self.a_zp_x(bus); self.a = self.o_adc(self.a, !bus.read_u8(a as u16)); self.fin(2, 4) }
            0xed /* SBC abs */ => { let a = self.a_abs16(bus); self.a = self.o_adc(self.a, !bus.read_u8(a)); self.fin(3, 4) }
            0xfd /* SBC abs,X */ => { let (a, xtra) = self.a_abs_x(bus); self.a = self.o_adc(self.a, !bus.read_u8(a)); self.fin(3, 4 + xtra) }
            0xf9 /* SBC abs,Y */ => { let (a, xtra) = self.a_abs_y(bus); self.a = self.o_adc(self.a, !bus.read_u8(a)); self.fin(3, 4 + xtra) }
            0xe1 /* SBC (ind,X) */ => { let a = self.a_idx_x(bus) as u16; self.a = self.o_adc(self.a, !bus.read_u8(a)); self.fin(2, 6) }
            0xf1 /* SBC (ind),Y */ => { let (a, xtra) = self.a_ind_y(bus); self.a = self.o_adc(self.a, !bus.read_u8(a)); self.fin(2, 5 + xtra) }

            0xa9 /* LDA imm */ => { self.a = self.a_imm(bus); f_nz!(self, self.a); self.fin(2, 2) }
            0xa5 /* LDA zp */ => { self.a = self.a_zp(bus).0; f_nz!(self, self.a); self.fin(2, 3) }
            0xb5 /* LDA zp,X */ => { let a = self.a_zp_x(bus); self.a = bus.read_u8(a as u16); f_nz!(self, self.a); self.fin(2, 4) }
            0xad /* LDA abs */ => { let a = self.a_abs16(bus); self.a = bus.read_u8(a); f_nz!(self, self.a); self.fin(3, 4) }
            0xbd /* LDA abs,X */ => { let (a, xtra) = self.a_abs_x(bus); self.a = bus.read_u8(a); f_nz!(self, self.a); self.fin(3, 4 + xtra) }
            0xb9 /* LDA abs,Y */ => { let (a, xtra) = self.a_abs_y(bus); self.a = bus.read_u8(a); f_nz!(self, self.a); self.fin(3, 4 + xtra) }
            0xa1 /* LDA (ind,X) */ => { let a = self.a_idx_x(bus) as u16; self.a = bus.read_u8(a); f_nz!(self, self.a); self.fin(2, 6) }
            0xb1 /* LDA (ind),Y */ => { let (a, xtra) = self.a_ind_y(bus); self.a = bus.read_u8(a); f_nz!(self, self.a); self.fin(2, 5 + xtra) }

            0xa2 /* LDX imm */ => { self.x = self.a_imm(bus); f_nz!(self, self.x); self.fin(2, 2) }
            0xa6 /* LDX zp */ => { self.x = self.a_zp(bus).0; f_nz!(self, self.x); self.fin(2, 3) }
            0xb6 /* LDX zp,Y */ => { let a = self.a_zp_y(bus); self.x = bus.read_u8(a as u16); f_nz!(self, self.x); self.fin(2, 4) }
            0xae /* LDX abs */ => { let a = self.a_abs16(bus); self.x = bus.read_u8(a); f_nz!(self, self.x); self.fin(3, 4) }
            0xbe /* LDX abs,Y */ => { let (a, xtra) = self.a_abs_y(bus); self.x = bus.read_u8(a); f_nz!(self, self.x); self.fin(3, 4 + xtra) }

            0xa0 /* LDY imm */ => { self.y = self.a_imm(bus); f_nz!(self, self.y); self.fin(2, 2) }
            0xa4 /* LDY zp */ => { self.y = self.a_zp(bus).0; f_nz!(self, self.y); self.fin(2, 3) }
            0xb4 /* LDY zp,X */ => { let a = self.a_zp_x(bus); self.y = bus.read_u8(a as u16); f_nz!(self, self.y); self.fin(2, 4) }
            0xac /* LDY abs */ => { let a = self.a_abs16(bus); self.y = bus.read_u8(a); f_nz!(self, self.y); self.fin(3, 4) }
            0xbc /* LDY abs,X */ => { let (a, xtra) = self.a_abs_x(bus); self.y = bus.read_u8(a); f_nz!(self, self.y); self.fin(3, 4 + xtra) }

            0x85 /* STA zp */ => { o_store_zp!(self, bus, a, 2 /*LEN*/); 3 /*CYCLES*/ }
            0x95 /* STA zp,X */ => { let a = self.a_zp_x(bus); bus.write_u8(a as u16, self.a); self.fin(2, 4) }
            0x8d /* STA abs */ => { let a = self.a_abs16(bus); bus.write_u8(a, self.a); self.fin(3, 4) }
            0x9d /* STA abs,X */ => { let a = self.a_abs_x(bus).0; bus.write_u8(a, self.a); self.fin(3, 5) }
            0x99 /* STA abs,Y */ => { let a = self.a_abs_y(bus).0; bus.write_u8(a, self.a); self.fin(3, 5) }
            0x81 /* STA (ind,X) */ => { let a = self.a_idx_x(bus) as u16; bus.write_u8(a, self.a); self.fin(2, 6) }
            0x91 /* STA (ind),Y */ => { let a = self.a_ind_y(bus).0; bus.write_u8(a, self.a); self.fin(2, 6) }

            0x86 /* STX zp */ => { o_store_zp!(self, bus, x, 2 /*LEN*/); 3 /*CYCLES*/ }
            0x96 /* STX zp,Y */ => { let a = self.a_zp_y(bus); bus.write_u8(a as u16, self.x); self.fin(2, 4) }
            0x8e /* STX abs */ => { let a = self.a_abs16(bus); bus.write_u8(a, self.x); self.fin(3, 4) }

            0x84 /* STY zp */ => { o_store_zp!(self, bus, y, 2 /*LEN*/); 3 /*CYCLES*/ }
            0x94 /* STY zp,X */ => { let a = self.a_zp_x(bus); bus.write_u8(a as u16, self.y); self.fin(2, 4) }
            0x8c /* STY abs */ => { let a = self.a_abs16(bus); bus.write_u8(a, self.y); self.fin(3, 4) }

            0x20 /* JSR */ => {
                let ta = self.a_abs16(bus);
                t_upc!(self, 2);
                bus.write_u8(0x100 + self.s as u16, (self.pc >> 8) as u8);
                self.s = self.s.wrapping_sub(1);
                bus.write_u8(0x100 + self.s as u16, (self.pc & 0xff) as u8);
                self.s = self.s.wrapping_sub(1);
                self.pc = ta;
                6 /*CYCLES*/
            }

            0xb0 /* BCS rel */ => { self.t_branch(bus, self.p.contains(Status::CARRY)) }
            0x90 /* BCC rel */ => { self.t_branch(bus, !self.p.contains(Status::CARRY)) }
            0xf0 /* BEQ rel */ => { self.t_branch(bus, self.p.contains(Status::ZERO)) }
            0xd0 /* BNE rel */ => { self.t_branch(bus, !self.p.contains(Status::ZERO)) }
            0x70 /* BVS rel */ => { self.t_branch(bus, self.p.contains(Status::OVERFLOW)) }
            0x50 /* BVC rel */ => { self.t_branch(bus, !self.p.contains(Status::OVERFLOW)) }
            0x30 /* BMI rel */ => { self.t_branch(bus, self.p.contains(Status::NEGATIVE)) }
            0x10 /* BPL rel */ => { self.t_branch(bus, !self.p.contains(Status::NEGATIVE)) }

            0xea /* NOP */ => { self.fin(1, 2) }

            0x60 /* RTS */ => {
                self.s = self.s.wrapping_add(1);
                let mut ra = bus.read_u8(0x100 + self.s as u16) as u16;
                self.s = self.s.wrapping_add(1);
                ra = ra | ((bus.read_u8(0x100 + self.s as u16) as u16) << 8);
                self.pc = ra.wrapping_add(1);
                6 /*CYCLES*/
            }

            0x00 /* BRK */ => {
                bus.read_u8(self.pc.wrapping_add(1)); // dummy read
                self.pc = self.pc.wrapping_add(2);
                bus.write_u8(0x100 + self.s as u16, (self.pc >> 8) as u8);
                self.s = self.s.wrapping_sub(1);
                bus.write_u8(0x100 + self.s as u16, (self.pc & 0xff) as u8);
                self.s = self.s.wrapping_sub(1);
                bus.write_u8(0x100 + self.s as u16, self.p.bits | 0x30);
                self.s = self.s.wrapping_sub(1);
                self.pc = (bus.read_u8(0xffff) as u16) << 8 | bus.read_u8(0xfffe) as u16;
                //panic!("BRK not validated. Here is your chance.");
                7 /*CYCLES*/
            }

            _ => { 
                if self.cfg_iops {
                    // illegal opcodes
                    match op {
                        0x1a | 0x3a | 0x5a | 0x7a | 0xda | 0xfa /* NOP */ => { self.fin(1, 2) }
                        0x80 /* NOP imm */ => { self.a_imm(bus); self.fin(2, 2) }
                        0x04 | 0x44 | 0x64 /* NOP zp */ => { self.a_zp(bus); self.fin(2, 3) }
                        0x0c /* NOP abs */ => { let a = self.a_abs16(bus); bus.read_u8(a); self.fin(3, 4) }
                        0x1c | 0x3c | 0x5c | 0x7c | 0xdc | 0xfc /* NOP abs,X */ => { let (_, xtra) = self.a_abs_x(bus); self.fin(3, 4 + xtra) }
                        0x14 | 0x34 | 0x54 | 0x74 | 0xd4 | 0xf4 /* NOP zp,X */ => { self.a_zp_x(bus); self.fin(2, 4) }

                        0xa7 /* LAX zp */ => { let a = self.a_imm(bus) as u16; self.oi_lax(bus, a, 2, 3) }
                        0xb7 /* LAX zp,Y */ => { let a = self.a_zp_y(bus) as u16; self.oi_lax(bus, a, 2, 4) }
                        0xaf /* LAX abs */ => { let a = self.a_abs16(bus); self.oi_lax(bus, a, 3, 4) }
                        0xbf /* LAX abs,Y */ => { let (a, xtra) = self.a_abs_y(bus); self.oi_lax(bus, a, 3, 4 + xtra) }
                        0xa3 /* LAX (ind,X) */ => { let a = self.a_idx_x(bus) as u16; self.oi_lax(bus, a, 2, 6) }
                        0xb3 /* LAX (ind),Y */ => { let (a, xtra) = self.a_ind_y(bus); self.oi_lax(bus, a, 2, 5 + xtra) }

                        0x87 /* SAX zp */ => { let a = self.a_imm(bus); bus.write_u8(a as u16, self.a & self.x); self.fin(2, 3) }
                        0x8f /* SAX abs */ => { let a = self.a_abs16(bus); bus.write_u8(a, self.a & self.x); self.fin(3, 4) }
                        0x83 /* SAX (ind,X) */ => { let a = self.a_idx_x(bus) as u16; bus.write_u8(a, self.a & self.x); self.fin(2, 6) }
                        0x97 /* SAX zp,Y */ => { let a = self.a_zp_y(bus); bus.write_u8(a as u16, self.a & self.x); self.fin(2, 4) }

                        0xeb /* SBC imm */ => { self.a = self.o_adc(self.a, !self.a_imm(bus)); self.fin(2, 2) }

                        0xc7 /* DCP zp */ => { let a = self.a_imm(bus); self.oi_dcp(bus, a as u16); self.fin(2, 5) }
                        0xd7 /* DCP zp,X */ => { let a = self.a_zp_x(bus); self.oi_dcp(bus, a as u16); self.fin(2, 6) }                        
                        0xcf /* DCP abs */ => { let a = self.a_abs16(bus); self.oi_dcp(bus, a); self.fin(3, 6) }
                        0xdf /* DCP abs,X */ => { let (a, _) = self.a_abs_x(bus); self.oi_dcp(bus, a); self.fin(3, 7) }                        
                        0xdb /* DCP abs,Y */ => { let (a, _) = self.a_abs_y(bus); self.oi_dcp(bus, a); self.fin(3, 7) }
                        0xc3 /* DCP (ind,X) */ => { let a = self.a_idx_x(bus) as u16; self.oi_dcp(bus, a); self.fin(2, 8) }
                        0xd3 /* DCP (ind),Y */ => { let (a, _) = self.a_ind_y(bus); self.oi_dcp(bus, a); self.fin(2, 8) }

                        0xe7 /* ISB zp */ => { let a = self.a_imm(bus) as u16; self.oi_isb(bus, a); self.fin(2, 5) }
                        0xf7 /* ISB zp,X */ => { let a = self.a_zp_x(bus); self.oi_isb(bus, a as u16); self.fin(2, 6) }                        
                        0xef /* ISB abs */ => { let a = self.a_abs16(bus); self.oi_isb(bus, a); self.fin(3, 6) }
                        0xff /* ISB abs,X */ => { let a = self.a_abs_x(bus).0; self.oi_isb(bus, a); self.fin(3, 7) }                        
                        0xfb /* ISB abs,Y */ => { let a = self.a_abs_y(bus).0; self.oi_isb(bus, a); self.fin(3, 7) }
                        0xe3 /* ISB (ind,X) */ => { let a = self.a_idx_x(bus) as u16; self.oi_isb(bus, a); self.fin(2, 8) }
                        0xf3 /* ISB (ind),Y */ => { let a = self.a_ind_y(bus).0; self.oi_isb(bus, a); self.fin(2, 8) }

                        0x07 /* SLO zp */ => { let a = self.a_imm(bus) as u16; self.oi_slo(bus, a); self.fin(2, 5) }
                        0x17 /* SLO zp,X */ => { let a = self.a_zp_x(bus); self.oi_slo(bus, a as u16); self.fin(2, 6) }                        
                        0x0f /* SLO abs */ => { let a = self.a_abs16(bus); self.oi_slo(bus, a); self.fin(3, 6) }
                        0x1f /* SLO abs,X */ => { let a = self.a_abs_x(bus).0; self.oi_slo(bus, a); self.fin(3, 7) }                        
                        0x1b /* SLO abs,Y */ => { let a = self.a_abs_y(bus).0; self.oi_slo(bus, a); self.fin(3, 7) }
                        0x03 /* SLO (ind,X) */ => { let a = self.a_idx_x(bus) as u16; self.oi_slo(bus, a); self.fin(2, 8) }
                        0x13 /* SLO (ind),Y */ => { let a = self.a_ind_y(bus).0; self.oi_slo(bus, a); self.fin(2, 8) }

                        0x27 /* RLA zp */ => { let a = self.a_imm(bus) as u16; self.oi_rla(bus, a); self.fin(2, 5) }
                        0x37 /* RLA zp,X */ => { let a = self.a_zp_x(bus); self.oi_rla(bus, a as u16); self.fin(2, 6) }                        
                        0x2f /* RLA abs */ => { let a = self.a_abs16(bus); self.oi_rla(bus, a); self.fin(3, 6) }
                        0x3f /* RLA abs,X */ => { let a = self.a_abs_x(bus).0; self.oi_rla(bus, a); self.fin(3, 7) }                        
                        0x3b /* RLA abs,Y */ => { let a = self.a_abs_y(bus).0; self.oi_rla(bus, a); self.fin(3, 7) }
                        0x23 /* RLA (ind,X) */ => { let a = self.a_idx_x(bus) as u16; self.oi_rla(bus, a); self.fin(2, 8) }
                        0x33 /* RLA (ind),Y */ => { let a = self.a_ind_y(bus).0; self.oi_rla(bus, a); self.fin(2, 8) }

                        _ => { panic!("Invalid opcode {:#04x} [ {} ]", op, self); }
                    }
                } else {
                    panic!("Invalid opcode {:#04x} [ {} ]", op, self);
                }
            }
        }
    }

    // lda + ldx
    pub fn oi_lax(&mut self, bus: &mut dyn sys::MemoryAccessA16D8, adr: u16, size: u16, cycles: u32) -> u32 {
        self.a = bus.read_u8(adr as u16);
        self.x = self.a;
        f_nz!(self, self.a);
        self.pc = self.pc.wrapping_add(size);
        cycles
    }

    // dec + cmp
    pub fn oi_dcp(&mut self, bus: &mut dyn sys::MemoryAccessA16D8, adr: u16) {
        let op = bus.read_u8(adr).wrapping_sub(1); // DEC
        bus.write_u8(adr as u16, op); // STORE
        self.o_cmp(self.a, op); // CMP
    }

    // inc + sbc
    pub fn oi_isb(&mut self, bus: &mut dyn sys::MemoryAccessA16D8, adr: u16) {
        let op = bus.read_u8(adr).wrapping_add(1); // DEC
        bus.write_u8(adr as u16, op); // STORE
        self.a = self.o_adc(self.a, !op); // SBC
    }

    // asl + ora
    pub fn oi_slo(&mut self, bus: &mut dyn sys::MemoryAccessA16D8, adr: u16) {
        let op = bus.read_u8(adr);
        self.p.set(Status::CARRY, op & 0x80 > 0);
        let res = op << 1;
        bus.write_u8(adr as u16, res);
        self.o_or(res);
    }

    // rol + and
    pub fn oi_rla(&mut self, bus: &mut dyn sys::MemoryAccessA16D8, adr: u16) {
        let op = bus.read_u8(adr);
        let res;
        if self.p.contains(Status::CARRY) {
            res = (op << 1) | 0x01;
        } else {
            res = op << 1;
        }
        self.p.set(Status::CARRY, op & 0x80 > 0);
        bus.write_u8(adr as u16, res);
        self.o_and(res);
    }

    // relative addressing: sign extend and calc pc offset
    pub fn a_rel(&self, bus: &mut dyn sys::MemoryAccessA16D8) -> u16 {
        let offset = bus.read_u8(self.pc.wrapping_add(1)) as u16;
        self.pc.wrapping_add(2 + if offset & 0x80 == 0x80 { 0xff00 | offset } else { offset })
    }

    // zeropage addressing
    pub fn a_imm(&self, bus: &mut dyn sys::MemoryAccessA16D8) -> u8 {
        bus.read_u8(self.pc.wrapping_add(1))
    }

    // absolute addressing (address)
    pub fn a_abs16(&self, bus: &mut dyn sys::MemoryAccessA16D8) -> u16 {
        (bus.read_u8(self.pc.wrapping_add(2)) as u16) << 8 | bus.read_u8(self.pc.wrapping_add(1)) as u16
    }

    // absolute x-indexed
    // returns: address + extra cycle for page boundary
    pub fn a_abs_x(&self, bus: &mut dyn sys::MemoryAccessA16D8) -> (u16, u32) {
        let base = (bus.read_u8(self.pc.wrapping_add(2)) as u16) << 8 | bus.read_u8(self.pc.wrapping_add(1)) as u16;
        let adr = base.wrapping_add(self.x as u16);
        (adr, ((adr & 0xff00) != (base & 0xff00)) as u32)
    }

    // absolute y-indexed
    // returns: address + extra cycle for page boundary
    pub fn a_abs_y(&self, bus: &mut dyn sys::MemoryAccessA16D8) -> (u16, u32) {
        let base = (bus.read_u8(self.pc.wrapping_add(2)) as u16) << 8 | bus.read_u8(self.pc.wrapping_add(1)) as u16;
        let adr = base.wrapping_add(self.y as u16);
        (adr, ((adr & 0xff00) != (base & 0xff00)) as u32)
    }
    
    // zeropage addressing (operand + address)
    // returns: value + address
    pub fn a_zp(&self, bus: &mut dyn sys::MemoryAccessA16D8) -> (u8, u16) {
        let adr = bus.read_u8(self.pc.wrapping_add(1)) as u16;
        (bus.read_u8(adr), adr)
    }

    // zeropage x-indexed addressing
    pub fn a_zp_x(&self, bus: &mut dyn sys::MemoryAccessA16D8) -> u8 {
        bus.read_u8(self.pc.wrapping_add(1)).wrapping_add(self.x)
    }

    // zeropage y-indexed addressing
    pub fn a_zp_y(&self, bus: &mut dyn sys::MemoryAccessA16D8) -> u8 {
        bus.read_u8(self.pc.wrapping_add(1)).wrapping_add(self.y)
    }

    // indexed indirect x
    // returns: address
    pub fn a_idx_x(&self, bus: &mut dyn sys::MemoryAccessA16D8) -> u16 {
        let base: u8 = bus.read_u8(self.pc.wrapping_add(1)).wrapping_add(self.x);
        (bus.read_u8(base.wrapping_add(1) as u16) as u16) << 8 | bus.read_u8(base as u16) as u16
    }

    // indirect indexed y
    // returns: address + extra cycle for page boundary
    pub fn a_ind_y(&self, bus: &mut dyn sys::MemoryAccessA16D8) -> (u16, u32) {
        let zp_adr = bus.read_u8(self.pc.wrapping_add(1));
        let base = (bus.read_u8(zp_adr.wrapping_add(1) as u16) as u16) << 8 | bus.read_u8(zp_adr as u16) as u16;
        let adr = base.wrapping_add(self.y as u16);
        (adr, ((adr & 0xff00) != (base & 0xff00)) as u32)
    }    

    // binary mode "add with carry"
    pub fn o_adc(&mut self, reg: u8, operand: u8) -> u8 {
        let res16 = reg as u16 + operand as u16 + self.p.contains(Status::CARRY) as u16;
        let res = (res16 & 0xff) as u8;
        self.p.set(Status::OVERFLOW, ((!(reg ^ operand)) & (reg ^ res)) & 0x80 == 0x80);
        self.p.set(Status::CARRY, res16 > 0xff);
        self.p.set(Status::ZERO, res == 0);
        self.p.set(Status::NEGATIVE, (res & 0x80) == 0x80);
        res
    }

    // binary mode "add with carry"
    pub fn o_cmp(&mut self, reg: u8, operand: u8) {
        let (res, ovf) = reg.overflowing_sub(operand);
        self.p.set(Status::CARRY, ovf == false);
        self.p.set(Status::ZERO, res == 0);
        self.p.set(Status::NEGATIVE, (res & 0x80) == 0x80);
    }

    // rotate one bit right
    pub fn o_ror(&mut self, operand: u8) -> u8 {
        let res;
        if self.p.contains(Status::CARRY) {
            res = (operand >> 1) | 0x80;
            self.p.set(Status::NEGATIVE, true);
        } else {
            res = operand >> 1;
            self.p.set(Status::NEGATIVE, false);
        }
        self.p.set(Status::CARRY, operand & 0x01 > 0);
        self.p.set(Status::ZERO, res == 0);
        res
    }

    // rotate one bit left
    pub fn o_rol(&mut self, operand: u8) -> u8 {
        let res;
        if self.p.contains(Status::CARRY) {
            res = (operand << 1) | 0x01;
        } else {
            res = operand << 1;
        }
        self.p.set(Status::CARRY, operand & 0x80 > 0);
        self.p.set(Status::NEGATIVE, res & 0x80 > 0);
        self.p.set(Status::ZERO, res == 0);
        res
    }

    // test bit in accumulator
    pub fn o_bit(&mut self, operand: u8) {
        self.p.set(Status::NEGATIVE, operand & 0x80 > 0);
        self.p.set(Status::OVERFLOW, operand & 0x40 > 0);
        self.p.set(Status::ZERO, (operand & self.a) == 0);
    }

    // or with accumulator
    pub fn o_or(&mut self, operand: u8) {
        self.a = self.a | operand;
        f_nz!(self, self.a);
    }

    // xor with accumulator
    pub fn o_xor(&mut self, operand: u8) {
        self.a = self.a ^ operand;
        f_nz!(self, self.a);
    }

    // and with accumulator
    pub fn o_and(&mut self, operand: u8) {
        self.a = self.a & operand;
        f_nz!(self, self.a);
    }

    // increment
    pub fn o_inc(&mut self, operand: u8) -> u8 {
        let res = operand.wrapping_add(1);
        self.p.set(Status::ZERO, res == 0);
        self.p.set(Status::NEGATIVE, res & 0x80 > 0);
        res
    }

    // decrement
    pub fn o_dec(&mut self, operand: u8) -> u8 {
        let res = operand.wrapping_sub(1);
        self.p.set(Status::ZERO, res == 0);
        self.p.set(Status::NEGATIVE, res & 0x80 > 0);
        res
    }

    // shift one bit right
    pub fn o_lsr(&mut self, operand: u8) -> u8 {
        let res = operand >> 1;
        self.p.set(Status::CARRY, operand & 0x01 > 0);
        self.p.set(Status::ZERO, res == 0);
        self.p.remove(Status::NEGATIVE);
        res
    }

    // shift one bit left
    pub fn o_asl(&mut self, operand: u8) -> u8 {
        let res = operand << 1;
        self.p.set(Status::CARRY, operand & 0x80 > 0);
        self.p.set(Status::ZERO, res == 0);
        self.p.set(Status::NEGATIVE, res & 0x80 > 0);
        res
    }

    // increment pc and return cycle count
    pub fn fin(&mut self, size: u16, cycles: u32) -> u32 {
        self.pc = self.pc.wrapping_add(size);
        cycles
    }
    
    // generic branch function
    pub fn t_branch(&mut self, bus: &mut dyn sys::MemoryAccessA16D8, jump: bool) -> u32 {
        if jump {
            let old_pc = self.pc.wrapping_add(2);
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
        println!("{:04X}            {:<32} {}             CYC:{}", self.pc, s, self, self.cycle);
        self.cycle = self.cycle + self.exec_op(bus, op);
        /*if self.pc == 0xe1a3 {
            panic!("BREAKPOINT");
        }*/
    }
}
