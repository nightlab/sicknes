pub mod mos6502;

pub trait Cpu {
    fn reset(&mut self);
}

pub use mos6502::Mos6502;
