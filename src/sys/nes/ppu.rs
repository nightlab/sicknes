use crate::sys::nes::MapperAccess;

bitflags! {
    #[derive(Default)]
    pub struct PPUMASK: u8 {
        const GREYSCALE = 1 << 0;
        const SHOW_BG8 = 1 << 1;
        const SHOW_SP8 = 1 << 2;
        const SHOW_BG = 1 << 3;
        const SHOW_SP = 1 << 4;
        const RED_EMP = 1 << 5;
        const GREEN_EMP = 1 << 6;
        const BLUE_EMP = 1 << 7;
    }
}

pub struct PPU {
    pub ciram: [u8; 2048],
    pub palette: [u8; 32],

    pub v: u16,
    pub t: u16,
    pub x: u8,
    pub w: u8,

    pub nmi_pending: bool,

    nt_address: u16,
    sp_pt_adr: u16,
    bg_pt_adr: u16,
    nmi: bool,
    vrinc: u16,

    greyscale: bool,
    show_bg8: bool,
    show_sp8: bool,
    show_bg: bool,
    show_sp: bool,
    emp_red: bool,
    emp_green: bool,
    emp_blue: bool,

    startup: bool,
    rendering: bool,

    pub ppuctrl: u8,
    pub ppumask: PPUMASK,
    pub ppustatus: u8,
    pub oamaddr: u8,
    pub oamdata: u8,
    pub ppudata: u8,

    pub scanline: i16,
    pub cycle: u32
}

impl PPU {
    pub fn new() -> PPU { 
        PPU {
            ciram: [0; 2048],
            palette: [0; 32],

            ppuctrl: 0,
            ppumask: PPUMASK { bits: 0 },
            ppustatus: 0,
            oamaddr: 0,
            oamdata: 0,
            ppudata: 0,

            rendering: false,

            nmi_pending: false,

            nt_address: 0x2000,
            vrinc: 1,
            sp_pt_adr: 0x0000,
            bg_pt_adr: 0x0000,
            nmi: false,

            greyscale: false,
            show_bg8: false,
            show_sp8: false,
            show_bg: false,
            show_sp: false,
            emp_red: false,
            emp_green: false,
            emp_blue: false,

            startup: true,

            v: 0,
            t: 0,
            x: 0,
            w: 0,

            scanline: 261,
            cycle: 0
        }
    }

    pub fn reg_write(&mut self, mapper: &mut Box<dyn MapperAccess>, reg: u8, value: u8) {
        match reg {
            0 => {
                if self.startup {
                    return;
                }
                self.t = (self.t & 0x73FF) | ((value & 3) as u16) << 10; // update temporary register
                self.ppuctrl = value & 0xbf;

                self.nt_address = 0x2000 + ((value & 3) as u16) * 0x400;
                self.vrinc = if value & 0x4 > 0 { 32 } else { 1 };
                self.nmi = value & 0x80 > 0;
            }
            1 => {
                if self.startup {
                    return;
                }
                self.ppumask.bits = value;

                self.greyscale = value & 0x1 > 0;
                self.show_bg8 = value & 0x2 > 0;
                self.show_sp8 = value & 0x4 > 0;
                self.show_bg = value & 0x8 > 0;
                self.show_sp = value & 0x10 > 0;
                self.emp_red = value & 0x20 > 0;
                self.emp_green = value & 0x40 > 0;
                self.emp_blue = value & 0x80 > 0;
            }
            2 => { panic!("Write {} on PPUSTATUS", value); }
            3 => { self.oamaddr = value; }
            4 => { self.oamdata = value; }
            5 => {
                if self.startup {
                    return;
                }
                if self.w == 0 {
                    self.x = value & 0x7;
                    self.t = (self.t & 0x7FE0) | (value >> 3) as u16;
                    self.w = 1;
                } else {
                    self.t = (self.t & 0xc1f) | (((value & 0xf8) as u16) << 2) | (((value & 0x7) as u16) << 12);
                    self.w = 0;
                }
            }
            6 => {
                if self.startup {
                    return;
                }
                if self.w == 0 {
                    self.t = (self.t & 0x00ff) | (((value & 0x3f) as u16) << 8);
                    self.w = 1;
                } else {
                    self.t = (self.t & 0xff00) | value as u16;
                    self.v = self.t;
                    self.w = 0;
                }
            }
            7 => {
                //println!("PPU@{:#06X}={:#04X}", self.v, value);
                if self.rendering && (self.show_bg || self.show_sp) {
                    panic!("WARNING: Write PPUDATA @ {:#06x} while rendering! [{}.{}]", self.v, self.scanline, self.cycle);
                }
                if self.v >= 0x3f00 {
                    self.palette[(self.v & 0x1f) as usize] = value;
                } else {
                    mapper.write_chr_u8(&mut self.ciram, self.v, value);
                }
                self.v = (self.v + self.vrinc) & 0x7FFF;
            }
            _ => { panic!("Impossible PPU write on register {}", reg); }
        }
    }

    pub fn reg_read(&mut self, mapper: &mut Box<dyn MapperAccess>, reg: u8) -> u8 {
        match reg {
            0 => { self.ppuctrl }
            1 => { self.ppumask.bits }
            2 => {
                let old = self.ppustatus;
                self.ppustatus = self.ppustatus & 0x7f; // reset vblank
                self.w = 0; // reset scroll/adr latch
                return old;
            }
            3 => { self.oamaddr }
            4 => { self.oamdata }
            5 => { panic!("Read on PPUSCROLL"); }
            6 => { panic!("Read on PPUADDR"); }
            7 => {
                if self.rendering && (self.show_bg || self.show_sp) {
                    panic!("WARNING: Read PPUDATA @ {:#06x} while rendering! [{}.{}]", self.v, self.scanline, self.cycle);
                }
                println!("Read on PPUDATA @ {:#06x}", self.v);
                let res;
                if self.v >= 0x3f00 {
                    res = self.palette[(self.v & 0x1f) as usize];
                } else {
                    res = mapper.read_chr_u8(&mut self.ciram, self.v);
                }
                self.v = (self.v + self.vrinc) & 0x7FFF;
                res
            }
            _ => { panic!("Impossible PPU write on register {}", reg); }
        }
    }

    pub fn clock(&mut self) {
        self.cycle = self.cycle + 1;
        if self.cycle == 341 {
            self.cycle = 0;
            self.scanline = self.scanline + 1;
            if self.scanline == 262 {
                self.scanline = 0;
            }
        }
    }

    pub fn update(&mut self, _mapper: &Box<dyn MapperAccess>, ec: u32) {
        if self.startup {
            if self.cycle > 29658*3 {
                self.startup = false;
                self.cycle = 0;
                self.scanline = 0;
            } else {
                self.cycle = self.cycle + ec;
            }
            return;
        }
        for _ in 0..ec {
            match self.scanline {
                // render
                0..=239 => {
                }

                // idle
                240..=260 => {
                    if self.scanline == 241 && self.cycle == 1 {
                        self.rendering = false;
                        self.ppustatus = self.ppustatus | 0x80; // set vblank
                        if self.nmi {
                            self.nmi_pending = true;
                        }
                    }
                }

                // prefetch
                261 => {
                    if self.cycle == 1 {
                        self.ppustatus = self.ppustatus & 0x7f; // reset vblank
                    }
                    if self.cycle == 340 {
                        self.rendering = true;
                    }                    
                }

                _ => { panic!("Invalid scanline {}", self.scanline); }
            }
            self.clock();
        }
    }

    pub fn start_dma(&mut self, _page: u8) {

    }
}


