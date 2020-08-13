pub mod nes;

pub trait Machine {
    fn get_name(&self) -> &'static str;

    fn reset(&mut self);
    fn run(&mut self);
    fn stop(&mut self);
    fn step(&mut self);
    fn update(&mut self);
    
    fn insert_catridge(&mut self, filename: &str);
}

pub use nes::NES;
