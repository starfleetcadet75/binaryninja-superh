use binaryninja::{
    architecture::{register_architecture, ArchitectureExt, CustomArchitectureHandle},
    callingconvention::ConventionBuilder,
    custombinaryview::{BinaryViewType, BinaryViewTypeExt},
    platform::Platform,
    Endianness,
};
use log::{trace, LevelFilter};

use crate::arch::SuperhArch;

mod arch;
mod flags;
mod instructions;
mod lifter;
mod registers;

bitflags::bitflags! {
    pub struct SuperhVersion: u8 {
        const SH1 = 0b0000_0001;
        const SH2 = 0b0000_0010;
        const SH3 = 0b0000_0011;
        const SH4 = 0b0000_0100;
        const SH4A = 0b0000_0101;
        const SH2A = 0b0000_0110;
        const DSP = 0b0000_1000;
        const SH2E = 0b0000_1001;
        const SH3E = 0b0000_1010;
    }
}

#[no_mangle]
#[allow(non_snake_case)]
pub extern "C" fn CorePluginInit() -> bool {
    binaryninja::logger::init(LevelFilter::Trace).expect("failed to setup logging");

    trace!("Registering SuperH architectures");
    let sh4_arch = register_architecture(
        "sh4",
        |custom_handle: CustomArchitectureHandle<SuperhArch>, core_arch| SuperhArch {
            handle: core_arch,
            custom_handle,
            endian: Endianness::LittleEndian,
            isa_version: SuperhVersion::SH4,
        },
    );

    let sh4b_arch = register_architecture(
        "sh4b",
        |custom_handle: CustomArchitectureHandle<SuperhArch>, core_arch| SuperhArch {
            handle: core_arch,
            custom_handle,
            endian: Endianness::BigEndian,
            isa_version: SuperhVersion::SH4,
        },
    );

    trace!("Registering SuperH binary views");
    if let Ok(bvt) = BinaryViewType::by_name("ELF") {
        bvt.register_arch(42, Endianness::LittleEndian, sh4_arch);
        bvt.register_arch(42, Endianness::BigEndian, sh4b_arch);
    }

    if let Ok(bvt) = BinaryViewType::by_name("PE") {
        bvt.register_arch(422, Endianness::LittleEndian, sh4_arch);
        bvt.register_arch(422, Endianness::BigEndian, sh4b_arch);
    }

    trace!("Registering SuperH calling conventions");
    register_calling_conventions(sh4_arch);
    register_calling_conventions(sh4b_arch);

    trace!("Registering SuperH platforms");
    let platform = Platform::new(sh4_arch, "linux-sh4");
    platform.register_os("linux");
    register_linux_syscall_calling_convention(&platform);

    let platform = Platform::new(sh4b_arch, "linux-sh4b");
    platform.register_os("linux");
    register_linux_syscall_calling_convention(&platform);

    true
}

// Calling conventions based on:
// * https://www.st.com/resource/en/reference_manual/CD17839242-.pdf (page 9)
// * https://llvm-gcc-renesas.com/manuals/SH-ABI-Specification.html
// * https://devblogs.microsoft.com/oldnewthing/20190820-00/?p=102792
// * https://devblogs.microsoft.com/oldnewthing/20190805-00/?p=102749
fn register_calling_conventions(arch: &SuperhArch) {
    let calling_convention = ConventionBuilder::new(arch)
        .caller_saved_registers(&[
            "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "fr0", "fr1", "fr2", "fr3", "fr4",
            "fr5", "fr6", "fr7", "fr8", "fr9", "fr10", "fr11", "mach", "macl", "pr", "fpul",
        ])
        .int_arg_registers(&["r4", "r5", "r6", "r7"])
        .float_arg_registers(&["fr4", "fr5", "fr6", "fr7", "fr8", "fr9", "fr10", "fr11"])
        .return_int_reg("r0")
        .return_hi_int_reg("r1")
        .return_float_reg("fr0")
        .global_pointer_reg("r12")
        .register("default");

    arch.set_default_calling_convention(&calling_convention);
    arch.set_cdecl_calling_convention(&calling_convention);
    arch.set_fastcall_calling_convention(&calling_convention);
    arch.set_stdcall_calling_convention(&calling_convention);
}

// Based on https://elixir.bootlin.com/linux/latest/source/arch/sh/kernel/entry-common.S#L233
fn register_linux_syscall_calling_convention(platform: &Platform) {
    let calling_convention = ConventionBuilder::new(&platform.arch())
        .caller_saved_registers(&[
            "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "fr0", "fr1", "fr2", "fr3", "fr4",
            "fr5", "fr6", "fr7", "fr8", "fr9", "fr10", "fr11", "mach", "macl", "pr", "fpul",
        ])
        .int_arg_registers(&["r3", "r4", "r5", "r6", "r7", "r0", "r1", "r2"])
        .is_eligible_for_heuristics(false)
        .return_int_reg("r0")
        .register("linux-syscall");
    platform.set_syscall_convention(&calling_convention);
}
