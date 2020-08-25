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

macro_rules! o_and {
    ($this:ident, $bus:expr, $addressing:ident, $len:expr) => {
        let _op = $addressing!($this, $bus);
        $this.a = $this.a & _op;
        f_nz!($this, $this.a);
        t_upc!($this, $len);
    }
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
macro_rules! o_or {
    ($this:ident, $bus:expr, $addressing:ident, $len:expr) => {
        let _op = $addressing!($this, $bus);
        $this.a = $this.a | _op;
        f_nz!($this, $this.a);
        t_upc!($this, $len);
    }
}

macro_rules! o_xor {
    ($this:ident, $bus:expr, $addressing:ident, $len:expr) => {
        let _op = $addressing!($this, $bus);
        $this.a = $this.a ^ _op;
        f_nz!($this, $this.a);
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
            "A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
            self.a, self.x, self.y, self.p, self.s
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
        self.p.bits = 0x24; // @todo bit 4 set after reset or not??? 
        self.pc = (bus.read_u8(0xfffd) as u16) << 8 | bus.read_u8(0xfffc) as u16;
        self.pc = 0xC000;
        self.cycle = 7;
    }

    // dump opcode
    pub fn debug_op(&self, bus: &mut dyn sys::MemoryAccessA16D8, op: u8) -> (String, u16) {
        match op {
            0x08 /* PHP */ => { (format!("PHP"), 1 /*LEN*/) }
            0x48 /* PHA */ => { (format!("PHA"), 1 /*LEN*/) }
            0x68 /* PLA */ => { (format!("PLA"), 1 /*LEN*/) }
            0x28 /* PLP */ => { (format!("PLP"), 1 /*LEN*/) }
            0x40 /* RTI */ => { (format!("RTI"), 1 /*LEN*/) }

            0x4c /* JMP abs */ => { (format!("JMP ${:04X}", self.a_abs16(bus)), 3 /*LEN*/) }
            
            0x78 /* SEI */ => { (format!("SEI"), 1 /*LEN*/) }
            0x38 /* SEC */ => { (format!("SEC"), 1 /*LEN*/) }
            0xf8 /* SED */ => { (format!("SED"), 1 /*LEN*/) }
            0x58 /* CLI */ => { (format!("CLI"), 1 /*LEN*/) }
            0x18 /* CLC */ => { (format!("CLC"), 1 /*LEN*/) }
            0xd8 /* CLD */ => { (format!("CLD"), 1 /*LEN*/) }
            0xb8 /* CLV */ => { (format!("CLV"), 1 /*LEN*/) }

            0xe8 /* INX */ => { (format!("INX"), 1 /*LEN*/) }
            0xc8 /* INY */ => { (format!("INY"), 1 /*LEN*/) }
            0xca /* DEX */ => { (format!("DEX"), 1 /*LEN*/) }
            0x88 /* DEY */ => { (format!("DEY"), 1 /*LEN*/) }
            0xaa /* TAX */ => { (format!("TAX"), 1 /*LEN*/) }
            0xa8 /* TAY */ => { (format!("TAY"), 1 /*LEN*/) }
            0xba /* TSX */ => { (format!("TSX"), 1 /*LEN*/) }
            0x8a /* TXA */ => { (format!("TXA"), 1 /*LEN*/) }
            0x9a /* TXS */ => { (format!("TXS"), 1 /*LEN*/) }
            0x98 /* TYA */ => { (format!("TYA"), 1 /*LEN*/) }

            0x24 /* BIT zp */ => { let a = a_imm!(self, bus); (format!("BIT ${:02X} = {:02X}", a, bus.read_u8(a as u16)), 2 /*LEN*/) }

            0x29 /* AND imm */ => { (format!("AND #${:02X}", a_imm!(self, bus)), 2 /*LEN*/) }
            
            0x09 /* ORA imm */ => { (format!("ORA #${:02X}", a_imm!(self, bus)), 2 /*LEN*/) }
            
            0x49 /* EOR imm */ => { (format!("EOR #${:02X}", a_imm!(self, bus)), 2 /*LEN*/) }

            0x0a /* ASL A */ => { (format!("ASL A"), 1 /*LEN*/) }

            0x4a /* LSR A */ => { (format!("LSR A"), 1 /*LEN*/) }

            0x6a /* ROR A */ => { (format!("ROR A"), 1 /*LEN*/) }

            0x2a /* ROL A */ => { (format!("ROL A"), 1 /*LEN*/) }
            
            0x69 /* ADC imm */ => { (format!("ADC #${:02X}", a_imm!(self, bus)), 2 /*LEN*/) }

            0xc9 /* CMP imm */ => { (format!("CMP #${:02X}", a_imm!(self, bus)), 2 /*LEN*/) }
            0xc0 /* CPY imm */ => { (format!("CPY #${:02X}", a_imm!(self, bus)), 2 /*LEN*/) }
            0xe0 /* CPX imm */ => { (format!("CPX #${:02X}", a_imm!(self, bus)), 2 /*LEN*/) }

            0xe9 /* SBC imm */ => { (format!("SBC #${:02X}", a_imm!(self, bus)), 2 /*LEN*/) }

            0xa9 /* LDA imm */ => { (format!("LDA #${:02X}", a_imm!(self, bus)), 2 /*LEN*/) }
            0xad /* LDA abs */ => { (format!("LDA #${:04X}", a_imm!(self, bus)), 2 /*LEN*/) }
            0xa5 /* LDA zp */ => { let a = a_imm!(self, bus); (format!("LDA ${:02X} = {:02X}", a, bus.read_u8(a as u16)), 2 /*LEN*/) }
            0xa1 /* LDA (op, X) */ => {
                let z = a_imm!(self, bus);
                let a = self.a_ind_x(bus) as u16;
                (format!("LDA (${:02X},X) @ {:02X} = {:04X} = {:02X}", z, z.wrapping_add(self.x), a, bus.read_u8(a as u16)), 2 /*LEN*/)
            }
            
            0xa2 /* LDX imm */ => { (format!("LDX #${:02X}", a_imm!(self, bus)), 2 /*LEN*/) }
            0xae /* LDX abs */ => { (format!("LDX #${:04X}", a_imm!(self, bus)), 2 /*LEN*/) }
            0xa6 /* LDX zp */ => { let a = a_imm!(self, bus); (format!("LDX ${:02X} = {:02X}", a, bus.read_u8(a as u16)), 2 /*LEN*/) }
            
            0xa0 /* LDY imm */ => { (format!("LDY #${:02X}", a_imm!(self, bus)), 2 /*LEN*/) }
            0xac /* LDY abs */ => { (format!("LDY #${:04X}", a_imm!(self, bus)), 2 /*LEN*/) }
            0xa4 /* LDY zp */ => { let a = a_imm!(self, bus); (format!("LDY ${:02X} = {:02X}", a, bus.read_u8(a as u16)), 2 /*LEN*/) }

            0x85 /* STA zp */ => { let a = a_imm!(self, bus); (format!("STA ${:02X} = {:02X}", a, bus.read_u8(a as u16)), 2 /*LEN*/) }
            0x8d /* STA abs */ => { let a = self.a_abs16(bus); (format!("STA ${:04X} = {:02X}", a, bus.read_u8(a as u16)), 2 /*LEN*/) }
            0x81 /* STA (op, X) */ => {
                let z = a_imm!(self, bus);
                let a = self.a_ind_x(bus) as u16;
                (format!("STA (${:02X},X) @ {:02X} = {:04X} = {:02X}", z, z.wrapping_add(self.x), a, bus.read_u8(a as u16)), 2 /*LEN*/)
            }

            0x86 /* STX zp */ => { let a = a_imm!(self, bus); (format!("STX ${:02X} = {:02X}", a, bus.read_u8(a as u16)), 2 /*LEN*/) }
            0x8e /* STX abs */ => { let a = self.a_abs16(bus); (format!("STX ${:04X} = {:02X}", a, bus.read_u8(a as u16)), 2 /*LEN*/) }

            0x84 /* STY zp */ => { let a = a_imm!(self, bus); (format!("STY ${:02X} = {:02X}", a, bus.read_u8(a as u16)), 2 /*LEN*/) }
            0x8c /* STY abs */ => { let a = self.a_abs16(bus); (format!("STY ${:04X} = {:02X}", a, bus.read_u8(a as u16)), 2 /*LEN*/) }

            0x20 /* JSR abs */ => { (format!("JSR ${:04X}", self.a_abs16(bus)), 3 /*LEN*/) }

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
            0x08 /* PHP */ => {
                bus.write_u8(0x100 + self.s as u16, self.p.bits | 0x30);
                self.s = self.s.wrapping_sub(1);
                t_upc!(self, 1 /*LEN*/);
                3 /*CYCLES*/
            }
            0x48 /* PHA */ => { bus.write_u8(0x100 + self.s as u16, self.a); self.s = self.s.wrapping_sub(1); t_upc!(self, 1 /*LEN*/); 3 /*CYCLES*/ }
            0x68 /* PLA */ => { self.s = self.s.wrapping_add(1); self.a = bus.read_u8(0x100 + self.s as u16); f_nz!(self, self.a); t_upc!(self, 1 /*LEN*/); 4 /*CYCLES*/ }
            0x28 /* PLP */ => {
                self.s = self.s.wrapping_add(1);
                self.p.bits = bus.read_u8(0x100 + self.s as u16) | 0x20;
                t_upc!(self, 1 /*LEN*/);
                4 /*CYCLES*/
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

            0x78 /* SEI */ => { self.p.insert(Status::INT_DIS); t_upc!(self, 1 /*LEN*/); 2 /*CYCLES*/ }
            0x38 /* SEC */ => { self.p.insert(Status::CARRY); t_upc!(self, 1 /*LEN*/); 2 /*CYCLES*/ }
            0xf8 /* SED */ => { self.p.insert(Status::DECIMAL); t_upc!(self, 1 /*LEN*/); 2 /*CYCLES*/ }
            0x58 /* CLI */ => { self.p.remove(Status::INT_DIS); t_upc!(self, 1 /*LEN*/); 2 /*CYCLES*/ }
            0x18 /* CLC */ => { self.p.remove(Status::CARRY); t_upc!(self, 1 /*LEN*/); 2 /*CYCLES*/ }
            0xd8 /* CLD */ => { self.p.remove(Status::DECIMAL); t_upc!(self, 1 /*LEN*/); 2 /*CYCLES*/ }
            0xb8 /* CLV */ => { self.p.remove(Status::OVERFLOW); t_upc!(self, 1 /*LEN*/); 2 /*CYCLES*/ }

            0xe8 /* INX */ => { o_inc!(self, x, 1 /*LEN*/); 2 /*CYCLES*/ }
            0xc8 /* INY */ => { o_inc!(self, y, 1 /*LEN*/); 2 /*CYCLES*/ }
            0xca /* DEX */ => { o_dec!(self, x, 1 /*LEN*/); 2 /*CYCLES*/ }
            0x88 /* DEY */ => { o_dec!(self, y, 1 /*LEN*/); 2 /*CYCLES*/ }
            0xaa /* TAX */ => { self.x = self.a; f_nz!(self, self.x); t_upc!(self, 1 /*LEN*/); 2 /*CYCLES*/ }
            0xa8 /* TAY */ => { self.y = self.a; f_nz!(self, self.y); t_upc!(self, 1 /*LEN*/); 2 /*CYCLES*/ }
            0xba /* TSX */ => { self.x = self.s; f_nz!(self, self.x); t_upc!(self, 1 /*LEN*/); 2 /*CYCLES*/ }
            0x8a /* TXA */ => { self.a = self.x; f_nz!(self, self.a); t_upc!(self, 1 /*LEN*/); 2 /*CYCLES*/ }
            0x9a /* TXS */ => { self.s = self.x; t_upc!(self, 1 /*LEN*/); 2 /*CYCLES*/ }
            0x98 /* TYA */ => { self.a = self.y; f_nz!(self, self.a); t_upc!(self, 1 /*LEN*/); 2 /*CYCLES*/ }

            // check this! v-flag on different addressing modes!!!
            0x24 /* BIT zp */ => { let o = self.a_zp(bus); o_bit!(self, bus, o, 2 /*LEN*/); 3 /*CYCLES*/ }

            0x29 /* AND imm */ => { o_and!(self, bus, a_imm, 2 /*LEN*/); 2 /*CYCLES*/ }

            0x09 /* ORA imm */ => { o_or!(self, bus, a_imm, 2 /*LEN*/); 2 /*CYCLES*/ }

            0x49 /* EOR imm */ => { o_xor!(self, bus, a_imm, 2 /*LEN*/); 2 /*CYCLES*/ }

            0x0a /* ASL A */ => { self.a = self.o_asl(self.a); t_upc!(self, 1 /*LEN*/); 2 /*CYCLES*/ }

            0x4a /* LSR A */ => { self.a = self.o_lsr(self.a); t_upc!(self, 1 /*LEN*/); 2 /*CYCLES*/ }

            0x6a /* ROR A */ => { self.a = self.o_ror(self.a); t_upc!(self, 1 /*LEN*/); 2 /*CYCLES*/ }

            0x2a /* ROL A */ => { self.a = self.o_rol(self.a); t_upc!(self, 1 /*LEN*/); 2 /*CYCLES*/ }

            // unverified
            0x69 /* ADC imm */ => { let o = self.a_imm(bus); self.a = self.o_adc(self.a, o); t_upc!(self, 2 /*LEN*/); 2 /*CYCLES*/ }

            // unverified
            0xc9 /* CMP imm */ => { let o = self.a_imm(bus); self.o_cmp(self.a, o); t_upc!(self, 2 /*LEN*/); 2 /*CYCLES*/ }
            0xc0 /* CPY imm */ => { let o = self.a_imm(bus); self.o_cmp(self.y, o); t_upc!(self, 2 /*LEN*/); 2 /*CYCLES*/ }
            0xe0 /* CPX imm */ => { let o = self.a_imm(bus); self.o_cmp(self.x, o); t_upc!(self, 2 /*LEN*/); 2 /*CYCLES*/ }

            0xe9 /* SBC imm */ => { let o = self.a_imm(bus); self.a = self.o_adc(self.a, !o); t_upc!(self, 2 /*LEN*/); 2 /*CYCLES*/ }

            0xa9 /* LDA imm */ => { o_load!(self, bus, a_imm, a, 2 /*LEN*/); 2 /*CYCLES*/ }
            0xad /* LDA abs */ => { let a = self.a_abs16(bus); self.a = bus.read_u8(a); f_nz!(self, self.a); t_upc!(self, 3); 4 /*CYCLES*/ }
            0xa5 /* LDA zp */ => { let a = self.a_zp(bus) as u16; self.a = bus.read_u8(a); f_nz!(self, self.a); t_upc!(self, 2); 3 /*CYCLES*/ }
            0xa1 /* LDA (op, X) */ => { let a = self.a_ind_x(bus) as u16; self.a = bus.read_u8(a); f_nz!(self, self.a); t_upc!(self, 2); 6 /*CYCLES*/ }

            0xa2 /* LDX imm */ => { o_load!(self, bus, a_imm, x, 2 /*LEN*/); 2 /*CYCLES*/ }
            0xae /* LDX abs */ => { let a = self.a_abs16(bus); self.x = bus.read_u8(a); f_nz!(self, self.x); t_upc!(self, 3); 4 /*CYCLES*/ }
            0xa6 /* LDX zp */ => { let a = self.a_zp(bus) as u16; self.x = bus.read_u8(a); f_nz!(self, self.x); t_upc!(self, 2); 3 /*CYCLES*/ }

            0xa0 /* LDY imm */ => { o_load!(self, bus, a_imm, y, 2 /*LEN*/); 2 /*CYCLES*/ }
            0xac /* LDY abs */ => { let a = self.a_abs16(bus); self.y = bus.read_u8(a); f_nz!(self, self.y); t_upc!(self, 3); 4 /*CYCLES*/ }
            0xa4 /* LDY zp */ => { let a = self.a_zp(bus) as u16; self.y = bus.read_u8(a); f_nz!(self, self.y); t_upc!(self, 2); 3 /*CYCLES*/ }

            0x85 /* STA zp */ => { o_store_zp!(self, bus, a, 2 /*LEN*/); 3 /*CYCLES*/ }
            0x8d /* STA abs */ => { let a = self.a_abs16(bus); bus.write_u8(a, self.a); t_upc!(self, 3); 4 /*CYCLES*/ }
            0x81 /* STA (op, X) */ => { let a = self.a_ind_x(bus) as u16; bus.write_u8(a, self.a); t_upc!(self, 2); 6 /*CYCLES*/ }

            0x86 /* STX zp */ => { o_store_zp!(self, bus, x, 2 /*LEN*/); 3 /*CYCLES*/ }
            0x8e /* STX abs */ => { let a = self.a_abs16(bus); bus.write_u8(a, self.x); t_upc!(self, 3); 4 /*CYCLES*/ }

            0x84 /* STY zp */ => { o_store_zp!(self, bus, y, 2 /*LEN*/); 3 /*CYCLES*/ }
            0x8c /* STY abs */ => { let a = self.a_abs16(bus); bus.write_u8(a, self.y); t_upc!(self, 3); 4 /*CYCLES*/ }

            0x20 /* JSR */ => {
                let ta = self.a_abs16(bus);
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
    pub fn a_imm(&self, bus: &mut dyn sys::MemoryAccessA16D8) -> u8 {
        bus.read_u8(self.pc.wrapping_add(1))
    }

    // absolute addressing
    pub fn a_abs16(&self, bus: &mut dyn sys::MemoryAccessA16D8) -> u16 {
        (bus.read_u8(self.pc.wrapping_add(2)) as u16) << 8 | bus.read_u8(self.pc.wrapping_add(1)) as u16
    }

    // zeropage addressing
    pub fn a_zp(&self, bus: &mut dyn sys::MemoryAccessA16D8) -> u8 {
        let a = bus.read_u8(self.pc.wrapping_add(1)) as u16;
        bus.read_u8(a)
    }

    // indexed indirect x
    pub fn a_ind_x(&self, bus: &mut dyn sys::MemoryAccessA16D8) -> u16 {
        let base: u8 = bus.read_u8(self.pc.wrapping_add(1)).wrapping_add(self.x);
        (bus.read_u8(base.wrapping_add(1) as u16) as u16) << 8 | bus.read_u8(base as u16) as u16
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
        /*if self.pc == 0xC804 {
            panic!("BREAKPOINT");
        }*/
        self.cycle = self.cycle + self.exec_op(bus, op);
    }
}
