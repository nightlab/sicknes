# sickNES

*Experimental NES emulator written in Rust*

This started as a project to figure out how to write emulators in Rust. This is not my first emulator I wrote (most of them in C/C++), so I was curious how it performs in different scenarios. I did not implement PPU/APU yet but basic mapper support is included along with a complete CPU implementation. Nestest ROM passes.

To build:

    # cargo build --release

To run:

    # cargo run --release resources/nes/nestest.nes

Run test:

    # cargo test
