#[cfg(test)]

use crate::sys;
use crate::sys::{NES, Machine, MemoryAccessA16D8};

use std::path::{Path, PathBuf};

fn res_nes() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("resources").join("nes")
}

#[test]
fn rom_cpu_nestest() {
    let mut machine : NES = sys::NES::new();

    let filename = res_nes().join("nestest.nes");
    assert!(machine.insert_catridge(filename.to_str().unwrap()));

    machine.cpu.reset(&mut machine.bus);

    // write nop @ 0x0500
    machine.bus.write_u8(0x0500, 0xea);

    // start test rom @ c000 (subroutine) and push return address 0x500-1 on stack
    machine.bus.write_u8(0x01fd, 0x04);
    machine.bus.write_u8(0x01fc, 0xff);
    machine.cpu.s = 0xfb;
    machine.cpu.pc = 0xc000;

    // run cpu
    while !machine.bus.bus_error && machine.cpu.pc != 0x0500 {
        machine.cpu.step(&mut machine.bus);
    }

    // get result from ram
    let rr1 = machine.bus.read_u8(2, false);
    let rr2 = machine.bus.read_u8(3, false);

    // compare results
    let regs = format!("{}", machine.cpu);
    assert!(!machine.bus.bus_error, "Bus error");
    assert_eq!(machine.cpu.pc, 0x0500, "PC doesn't match");
    assert_eq!(rr1, 0, "Error in result byte 1");
    assert_eq!(rr2, 0, "Error in result byte 2");
    assert_eq!(regs, "A:00 X:FF Y:15 P:27 SP:FD", "Registers not matching");
    assert_eq!(machine.cpu.cycle, 26560, "Wrong number of cycles");
}
