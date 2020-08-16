use std::io;
use std::io::ErrorKind;
use std::io::prelude::*;
use std::path::Path;
use std::fs::File;

pub struct CatridgeData {
    pub prg: Vec<u8>,
    pub chr: Vec<u8>
}

pub trait MapperAccess {
    fn read_u8(&self, address: u16) -> u8;
    fn write_u8(&self, address: u16, data: u8);
}

pub struct MapperDummy {
}

impl MapperAccess for MapperDummy {
    fn read_u8(&self, _address: u16) -> u8 { return 0; }
    fn write_u8(&self, _address: u16, _data: u8) { }
}

pub struct MapperNROM {
    data: Box<CatridgeData>
}

impl MapperAccess for MapperNROM {
    fn read_u8(&self, address: u16) -> u8 {
        return self.data.prg[(address & 0x7fff) as usize];
    }
    fn write_u8(&self, _address: u16, _data: u8) {
    }
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

pub struct NesCartridge {
    pub trainer: Vec<u8>,
    pub mapper_num: u8,
    pub f_prg: u32,
    pub f_chr: u32,
    pub f_flags6: Flags6,
    pub f_flags7: u8,
    pub f_flags8: u8,
    pub f_flags9: u8,
    pub f_flags10: u8
}

impl NesCartridge {
    pub fn load(filename : &str) -> Result<(NesCartridge, Box<dyn MapperAccess>), io::Error> {
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

        let f_flags6 = Flags6 { bits: buffer[7] };
        let f_prg = buffer[5] as u32 * 16384;
        let f_chr = buffer[6] as u32 * 8192;
        let mapper_num = (buffer[8] & 0xf0) | (buffer[7] >> 4);
        let mut prg = Vec::<u8>::new();
        let chr = Vec::<u8>::new();
        let mut trainer = Vec::<u8>::new();

        let sz_exp = (16 + (f_flags6.contains(Flags6::TRAINER) as u32 * 512) + f_prg + f_chr) as usize;
        if buffer.len() < sz_exp {
            return Err(io::Error::new(ErrorKind::Other, format!("Header damaged (expected filesize < {})", sz_exp)));
        }
        let mut offset: usize = 16;
        if f_flags6.contains(Flags6::TRAINER) {
            offset = offset + 512;
            trainer.copy_from_slice(buffer.get(16..offset).unwrap());
        }
        let t = offset + f_prg as usize;

        let mapper;
        let pd = buffer.get(offset..t).unwrap();
        prg.extend_from_slice(pd);
        if mapper_num == 0 && f_prg == 16384 {
            prg.extend_from_slice(pd);
        }
        match mapper_num {
            0 => {
                mapper = Box::new(MapperNROM {
                    data: Box::new(CatridgeData { prg, chr })
                });
            }
            _ => {
                return Err(io::Error::new(ErrorKind::Other, format!("Unsupported mapper #{}", mapper_num)));
            }
        }

        let cart = NesCartridge {
            trainer,
            f_prg,
            f_chr,
            mapper_num,
            f_flags6,
            f_flags7: buffer[8],
            f_flags8: buffer[9],
            f_flags9: buffer[10],
            f_flags10: buffer[11]
        };

        Ok((cart, mapper))
    }
}