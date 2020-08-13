use crate::cpu;

#[derive(Default)]
pub struct Mos6502 {
    a: u8,
    x: u8,
    y: u8,
    s: u8,
    p: u8,
    pc: u16
}

impl Mos6502 {
    pub fn new() -> Mos6502 { Default::default() }
}

impl cpu::Cpu for Mos6502 {
    fn reset(&mut self) {
        self.a = 0;
        self.x = 0;
        self.y = 0;
        self.s = 0xFD;
        self.p = 0x34;
        self.pc = 0;
    }
}
