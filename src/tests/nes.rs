#[cfg(test)]

use crate::sys;

use std::path::{Path, PathBuf};

fn res_nes() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("resources").join("nes")
}

#[test]
fn rom_cpu_nestest() {
    let mut machine : Box<dyn sys::Machine> = Box::new(sys::NES::new());
    let filename = res_nes().join("nestest.nes");
    assert!(machine.insert_catridge(filename.to_str().unwrap()));
    machine.reset();
    machine.run();
    while machine.is_running() {
        machine.update();
    }
    assert_eq!(8, 8);
}
