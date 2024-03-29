pub mod nes;

pub trait Machine {
    fn get_name(&self) -> &'static str;

    fn reset(&mut self);
    fn start(&mut self);
    fn update(&mut self);
    fn is_running(&self) -> bool;

    fn get_cycles(&self) -> u32;

    fn insert_catridge(&mut self, filename: &str) -> bool;
}

pub trait MemoryAccessA16D8 {
    fn read_u8(&mut self, address: u16, dummy: bool) -> u8;
    fn write_u8(&mut self, address: u16, data: u8);
}

pub use nes::NES;
