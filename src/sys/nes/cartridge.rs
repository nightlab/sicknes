use std::io;
use std::io::ErrorKind;
use std::io::prelude::*;
use std::path::Path;
use std::fs::File;

pub struct MapperNROM {}

#[derive(Debug)]
pub enum Mapper {
    NROM
}

impl Default for Mapper {
    fn default() -> Self { Mapper::NROM }
}

bitflags! {
    #[derive(Default)]
    pub struct Flags6: u8 {
        const VERTICAL = 1 << 0;
        const BBUPRGRAM = 1 << 1;
        const TRAINER = 1 << 2;
        const FOURSCREEN = 1 << 3;
    }
}

pub trait MapperAccess {
    fn read_u8(&mut self, address: u16) -> u8;
    fn write_u8(&mut self, address: u16, data: u8);
}

impl MapperAccess for MapperNROM {
    fn read_u8(&mut self, address: u16) -> u8 {
        println!("sehr geil");
        return 0;
    }
    fn write_u8(&mut self, address: u16, data: u8) {
    }
}

#[derive(Default)]
pub struct NesCartridge {
    pub prg: Vec<u8>,
    pub chr: Vec<u8>,
    pub trainer: Vec<u8>,
    pub mapper: Mapper,
    pub mapper_lo: u8,

    pub access: Option<Box<dyn MapperAccess>>,

    pub f_prg: u32,
    pub f_chr: u32,
    pub f_flags6: Flags6,
    pub f_flags7: u8,
    pub f_flags8: u8,
    pub f_flags9: u8,
    pub f_flags10: u8
}

impl NesCartridge {
    pub fn load(filename : &str) -> Result<NesCartridge, io::Error> {
        let mut is_zip = false;
        if let Some(ext) = Path::new(filename).extension() {
            let ext = ext.to_str().unwrap().to_lowercase();
            if ext == "zip" {
                is_zip = true;
            }
        }
        if is_zip {
            return Err(io::Error::new(ErrorKind::Other, "Zip-files are not supported yet"));
        }
        let mut f = File::open(filename)?;
        let mut buffer = Vec::new();
        f.read_to_end(&mut buffer)?;
        if buffer.len() < 16 {
            return Err(io::Error::new(ErrorKind::Other, "Invalid file"));
        }
        if buffer.get(0..4).unwrap() != [78,69,83,26] {
            return Err(io::Error::new(ErrorKind::Other, "Found no iNES header"));
        }
        let mut cart = NesCartridge {
            prg: Vec::new(),
            chr: Vec::new(),
            trainer: Vec::new(),
            mapper: Mapper::NROM,
            f_prg: buffer[5] as u32 * 16384,
            f_chr: buffer[6] as u32 * 8192,
            mapper_lo: (buffer[8] & 0xf0) | (buffer[7] >> 4),
            f_flags6: Flags6 { bits: buffer[7] },
            f_flags7: buffer[8],
            f_flags8: buffer[9],
            f_flags9: buffer[10],
            f_flags10: buffer[11],
            access: None
        };
        let sz_exp = (16 + (cart.f_flags6.contains(Flags6::TRAINER) as u32 * 512) + cart.f_prg + cart.f_chr) as usize;
        if buffer.len() < sz_exp {
            return Err(io::Error::new(ErrorKind::Other, format!("Header damaged (expected filesize < {})", sz_exp)));
        }
        let mut offset: usize = 16;
        if cart.f_flags6.contains(Flags6::TRAINER) {
            offset = offset + 512;
            cart.trainer.copy_from_slice(buffer.get(16..offset).unwrap());
        }
        let t = offset + cart.f_prg as usize;
        cart.prg.resize(cart.f_prg as usize, 0);
        cart.prg.copy_from_slice(buffer.get(offset..t).unwrap());
        match cart.mapper_lo {
            0 => {
                cart.mapper = Mapper::NROM;
                cart.access = Some(Box::new(MapperNROM {}));
            }
            _ => {
                return Err(io::Error::new(ErrorKind::Other, format!("Unsupported mapper #{}", cart.mapper_lo)));
            }
        }
        Ok(cart)
    }
}