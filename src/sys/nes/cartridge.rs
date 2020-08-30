use std::io;
use std::io::ErrorKind;
use std::io::prelude::*;
use std::path::Path;
use std::fs::File;

#[derive(Debug)]
enum Mirroring {
    VERTICAL,
    HORIZONTAL,
    FOURSCREEN,
    SINGLE
}

pub struct CatridgeData {
    mirroring: Mirroring,
    pub prg: Vec<u8>,
    pub chr: Vec<u8>
}

pub trait MapperAccess {
    fn read_prg_u8(&mut self, address: u16) -> u8;
    fn write_prg_u8(&mut self, address: u16, data: u8);
    fn read_chr_u8(&mut self, vram: &[u8], address: u16) -> u8;
    fn write_chr_u8(&mut self, vram: &mut [u8], address: u16, data: u8);
}

pub struct MapperDummy {
}

impl MapperAccess for MapperDummy {
    fn read_prg_u8(&mut self, _address: u16) -> u8 { return 0; }
    fn write_prg_u8(&mut self, _address: u16, _data: u8) { }
    fn read_chr_u8(&mut self, _vram: &[u8], _address: u16) -> u8 { return 0; }
    fn write_chr_u8(&mut self, _vram: &mut [u8], _address: u16, _data: u8) { }
}

pub struct MapperNROM {
    data: Box<CatridgeData>
}

impl MapperAccess for MapperNROM {
    fn read_prg_u8(&mut self, address: u16) -> u8 {
        return self.data.prg[(address & 0x7fff) as usize];
    }

    fn write_prg_u8(&mut self, address: u16, data: u8) {
        panic!("NROM: INVALID WRITE ON CPU-BUS @ {:#06x} = {:#04x}", address, data);
    }

    fn read_chr_u8(&mut self, ciram: &[u8], address: u16) -> u8 {
        match address {
            // maps to chr rom
            0x0000..=0x1FFF => {
                return self.data.chr[address as usize];
            }

            // maps to ppu vram
            0x2000..=0x3EFF => {
                let a = (address - 0x2000) & 0x0fff;
                match self.data.mirroring {
                    Mirroring::VERTICAL => {
                        return ciram[(a & 0x7ff) as usize];
                    }
                    Mirroring::HORIZONTAL => {
                        if a >= 0x800 {
                            return ciram[(0x400 + (a & 0x3ff)) as usize];
                        } else {
                            return ciram[(a & 0x3ff) as usize];
                        }
                    }
                    Mirroring::FOURSCREEN => { panic!("Four Screen Mirroring not supported by this mapper"); }
                    Mirroring::SINGLE => { panic!("Single Screen Mirroring not supported by this mapper"); }
                }
            }

            _ => { panic!("NROM: INVALID READ ON PPU-BUS @ {:#06x}", address); }
        }
    }

    fn write_chr_u8(&mut self, ciram: &mut [u8], address: u16, data: u8) {
        match address {
            // maps to chr rom
            0x0000..=0x1FFF => {
                panic!("NROM: WRITE ROM ON PPU-BUS @ {:#06x} = {:#04x}", address, data);
            }

            // maps to ppu vram
            0x2000..=0x3EFF => {
                let a = (address - 0x2000) & 0x0fff;
                match self.data.mirroring {
                    Mirroring::VERTICAL => {
                        ciram[(a & 0x7ff) as usize] = data;
                    }
                    Mirroring::HORIZONTAL => {
                        if a >= 0x800 {
                            ciram[(0x400 + (a & 0x3ff)) as usize] = data;
                        } else {
                            ciram[(a & 0x3ff) as usize] = data;
                        }
                    }
                    Mirroring::FOURSCREEN => { panic!("Four Screen Mirroring not supported by this mapper"); }
                    Mirroring::SINGLE => { panic!("Single Screen Mirroring not supported by this mapper"); }
                }
            }

            _ => { panic!("NROM: INVALID WRITE ON PPU-BUS @ {:#06x} = {:#04x}", address, data); }
        }
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

        let f_flags6 = Flags6 { bits: buffer[6] };
        let f_prg = buffer[4] as u32 * 16384;
        let f_chr = buffer[5] as u32 * 8192;
        let mapper_num = (buffer[7] & 0xf0) | (buffer[6] >> 4);
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

        let t1 = offset + f_prg as usize;
        let t2 = t1 + f_chr as usize;
        let mapper;

        // load prg data
        let mut prg = Vec::<u8>::new();
        let pd = buffer.get(offset..t1).unwrap();
        prg.extend_from_slice(pd);
        if mapper_num == 0 && f_prg == 16384 {
            prg.extend_from_slice(pd);
        }

        // load chr data
        let mut chr = Vec::<u8>::new();
        let pd = buffer.get(t1..t2).unwrap();
        chr.extend_from_slice(pd);

        match mapper_num {
            0 => {
                let mirroring: Mirroring;
                if f_flags6.contains(Flags6::FOURSCREEN) {
                    mirroring = Mirroring::FOURSCREEN;
                } else {
                    if f_flags6.contains(Flags6::VERTICAL) {
                        mirroring = Mirroring::VERTICAL;
                    } else {
                        mirroring = Mirroring::HORIZONTAL;
                    }
                }
                println!("Found NROM (PRG:{} CHR:{} MAPPER:{} MIRRORING:{:?})", f_prg, f_chr, mapper_num, mirroring);
                mapper = Box::new(MapperNROM {
                    data: Box::new(CatridgeData { mirroring, prg, chr })
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