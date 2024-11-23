use binaryninja::architecture::Register;
use log::warn;
use strum_macros::{Display, EnumCount as EnumCountMacro, EnumIter, FromRepr};

use crate::{registers::SuperhRegister, SuperhVersion};

type TextToken = binaryninja::disassembly::InstructionTextToken;
type TextContent = binaryninja::disassembly::InstructionTextTokenContents;

#[derive(Clone, Copy, Debug, Display, Eq, PartialEq, EnumCountMacro, EnumIter, FromRepr)]
pub enum Operation {
    Add,
    Addc,
    Addv,
    And,
    Band,
    Bandnot,
    Bclr,
    Bf,
    Bld,
    Bldnot,
    Bor,
    Bornot,
    Bra,
    Braf,
    Bset,
    Bsr,
    Bsrf,
    Bst,
    Bt,
    Bxor,
    Clips,
    Clipu,
    Clrmac,
    Clrs,
    Clrt,
    Cmp,
    Dcf,
    Dct,
    Div0s,
    Div0u,
    Div1,
    Divs,
    Divu,
    Dmuls,
    Dmulu,
    Dt,
    Exts,
    Extu,
    Fabs,
    Fadd,
    Fcmp,
    Fcnvds,
    Fcnvsd,
    Fdiv,
    Fipr,
    Fldi0,
    Fldi1,
    Flds,
    Float,
    Fmac,
    Fmov,
    Fmul,
    Fneg,
    Fpchg,
    Frchg,
    Fsca,
    Fschg,
    Fsqrt,
    Fsrra,
    Fsts,
    Fsub,
    Ftrc,
    Ftrv,
    Icbi,
    Jmp,
    Jsr,
    Ldbank,
    Ldc,
    Ldre,
    Ldrs,
    Lds,
    Ldtlb,
    Mac,
    Mov,
    Mova,
    Movca,
    Movco,
    Movi20,
    Movi20s,
    Movli,
    Movml,
    Movmu,
    Movrt,
    Movs,
    Movt,
    Movu,
    Movua,
    Movx,
    Movy,
    Mul,
    Mulr,
    Muls,
    Mulu,
    Neg,
    Negc,
    Nop,
    Nopx,
    Nopy,
    Not,
    Nott,
    Ocbi,
    Ocbp,
    Ocbwb,
    Or,
    Pabs,
    Padd,
    Paddc,
    Pand,
    Pclr,
    Pcmp,
    Pcopy,
    Pdec,
    Pdmsb,
    Pinc,
    Plds,
    Pmuls,
    Pneg,
    Por,
    Pref,
    Prefi,
    Prnd,
    Psha,
    Pshl,
    Psts,
    Psub,
    Psubc,
    Pxor,
    Resbank,
    Rotcl,
    Rotcr,
    Rotl,
    Rotr,
    Rte,
    Rts,
    Rtv,
    Setrc,
    Sets,
    Sett,
    Shad,
    Shal,
    Shar,
    Shld,
    Shll,
    Shll16,
    Shll2,
    Shll8,
    Shlr,
    Shlr16,
    Shlr2,
    Shlr8,
    Sleep,
    Stbank,
    Stc,
    Sts,
    Sub,
    Subc,
    Subv,
    Swap,
    Synco,
    Tas,
    Trapa,
    Tst,
    Xor,
    Xtrct,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LengthSuffix {
    Byte,
    Word,
    Long,
    Single,
    Double,
}

impl LengthSuffix {
    pub fn size(&self) -> usize {
        match *self {
            LengthSuffix::Byte => 1,
            LengthSuffix::Word => 2,
            LengthSuffix::Long => 4,
            LengthSuffix::Single => 4,
            LengthSuffix::Double => 8,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Conditional {
    Eq,
    Ge,
    Gt,
    Hi,
    Hs,
    Pl,
    Pz,
    Str,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum OperandFlag {
    PreDec,
    PostInc,
    None,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Operand {
    Reg(SuperhRegister),
    Imm(i16),
    Address(u64),
    // "@r0"
    DerefReg(SuperhRegister, OperandFlag),
    // "@(r0,gbr)"
    DerefRegReg(SuperhRegister, SuperhRegister),
    // "@(#4,gbr)"
    DerefRegImm(SuperhRegister, u16),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Instruction {
    pub operation: Operation,
    pub length_suffix: Option<LengthSuffix>,
    pub cond: Option<Conditional>,
    pub operands: Vec<Operand>,
    pub delay_slot: bool,
    pub privileged: bool,
    pub version: SuperhVersion,
}

struct Context {
    instr_word: u16,
    address: u64,
    // Floating point values can be read/written in either 4 or 8 byte
    // values depending on the setting of the FPSCR_SZ flag.
    fpscr_sz: bool,
    // Most of the floating point instructions can perform either single
    // or double precision calculations depending on the FPSCR_PR flag.
    fpscr_pr: bool,
}

impl Instruction {
    pub fn decompose(
        instr_word: u16,
        address: u64,
        isa_version: SuperhVersion,
    ) -> Option<Instruction> {
        // The FPSCR register determines how certain floating point instructions
        // are decoded. For now, this context remains fixed using the default
        // power-on reset modes for SZ and PR.
        let ctx = Context {
            instr_word,
            address,
            fpscr_sz: false,
            fpscr_pr: false,
        };

        Self::decode(&ctx).and_then(|instr| {
            // Validate that the instruction is supported by
            // the current processor version.
            if instr.version.contains(isa_version) {
                Some(instr)
            } else {
                warn!(
                    "Instruction {} at {:#x} not supported by current SuperH version",
                    instr.operation, address
                );
                None
            }
        })
    }

    fn decode(ctx: &Context) -> Option<Instruction> {
        let n1 = (ctx.instr_word >> 0xc) & 0xf;
        let n2 = (ctx.instr_word >> 0x8) & 0xf;
        let n3 = (ctx.instr_word >> 0x4) & 0xf;
        let n4 = ctx.instr_word & 0xf;

        Some(match (n1, n2, n3, n4) {
            // 0110nnnnmmmm0011 "mov Rm,Rn"
            (0b0110, nnnn, mmmm, 0b0011) => Instruction {
                operation: Operation::Mov,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 1110nnnniiiiiiii "mov #imm,Rn"
            (0b1110, nnnn, _, _) => Instruction {
                operation: Operation::Mov,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Imm((ctx.instr_word & 0xff) as i16),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 11000111dddddddd "mova @(disp,PC),R0"
            (0b1100, 0b0111, _, _) => Instruction {
                operation: Operation::Mova,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Address(
                        ((ctx.address & 0xffff_fffc) + 4)
                            .wrapping_add(((ctx.instr_word & 0xff) as u64) << 2),
                    ),
                    Operand::Reg(SuperhRegister::R0),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 1001nnnndddddddd "mov.w @(disp,PC),Rn"
            (0b1001, nnnn, _, _) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Word),
                cond: None,
                operands: vec![
                    Operand::Address(
                        (ctx.address + 4).wrapping_add(((ctx.instr_word & 0xff) as u64) << 1),
                    ),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 1101nnnndddddddd "mov.l @(disp,PC),Rn"
            (0b1101, nnnn, _, _) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::Address(
                        ((ctx.address & 0xffff_fffc) + 4)
                            .wrapping_add(((ctx.instr_word & 0xff) as u64) << 2),
                    ),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0110nnnnmmmm0000 "mov.b @Rm,Rn"
            (0b0110, nnnn, mmmm, 0b0000) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Byte),
                cond: None,
                operands: vec![
                    Operand::DerefReg(SuperhRegister::new_gpr(mmmm.into()), OperandFlag::None),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0110nnnnmmmm0001 "mov.w @Rm,Rn"
            (0b0110, nnnn, mmmm, 0b0001) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Word),
                cond: None,
                operands: vec![
                    Operand::DerefReg(SuperhRegister::new_gpr(mmmm.into()), OperandFlag::None),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0110nnnnmmmm0010 "mov.l @Rm,Rn"
            (0b0110, nnnn, mmmm, 0b0010) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::DerefReg(SuperhRegister::new_gpr(mmmm.into()), OperandFlag::None),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0010nnnnmmmm0000 "mov.b Rm,@Rn"
            (0b0010, nnnn, mmmm, 0b0000) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Byte),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::DerefReg(SuperhRegister::new_gpr(nnnn.into()), OperandFlag::None),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0010nnnnmmmm0001 "mov.w Rm,@Rn"
            (0b0010, nnnn, mmmm, 0b0001) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Word),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::DerefReg(SuperhRegister::new_gpr(nnnn.into()), OperandFlag::None),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0010nnnnmmmm0010 "mov.l Rm,@Rn"
            (0b0010, nnnn, mmmm, 0b0010) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::DerefReg(SuperhRegister::new_gpr(nnnn.into()), OperandFlag::None),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0110nnnnmmmm0100 "mov.b @Rm+,Rn"
            (0b0110, nnnn, mmmm, 0b0100) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Byte),
                cond: None,
                operands: vec![
                    Operand::DerefReg(SuperhRegister::new_gpr(mmmm.into()), OperandFlag::PostInc),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0110nnnnmmmm0101 "mov.w @Rm+,Rn"
            (0b0110, nnnn, mmmm, 0b0101) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Word),
                cond: None,
                operands: vec![
                    Operand::DerefReg(SuperhRegister::new_gpr(mmmm.into()), OperandFlag::PostInc),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0110nnnnmmmm0110 "mov.l @Rm+,Rn"
            (0b0110, nnnn, mmmm, 0b0110) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::DerefReg(SuperhRegister::new_gpr(mmmm.into()), OperandFlag::PostInc),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0010nnnnmmmm0100 "mov.b Rm,@-Rn"
            (0b0010, nnnn, mmmm, 0b0100) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Byte),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::DerefReg(SuperhRegister::new_gpr(nnnn.into()), OperandFlag::PreDec),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0010nnnnmmmm0101 "mov.w Rm,@-Rn"
            (0b0010, nnnn, mmmm, 0b0101) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Word),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::DerefReg(SuperhRegister::new_gpr(nnnn.into()), OperandFlag::PreDec),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0010nnnnmmmm0110 "mov.l Rm,@-Rn"
            (0b0010, nnnn, mmmm, 0b0110) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::DerefReg(SuperhRegister::new_gpr(nnnn.into()), OperandFlag::PreDec),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 10000100mmmmdddd "mov.b @(disp,Rm),R0"
            (0b1000, 0b0100, mmmm, _) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Byte),
                cond: None,
                operands: vec![
                    Operand::DerefRegImm(SuperhRegister::new_gpr(mmmm.into()), n4),
                    Operand::Reg(SuperhRegister::R0),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 10000101mmmmdddd "mov.w @(disp,Rm),R0"
            (0b1000, 0b0101, mmmm, _) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Word),
                cond: None,
                operands: vec![
                    Operand::DerefRegImm(SuperhRegister::new_gpr(mmmm.into()), n4),
                    Operand::Reg(SuperhRegister::R0),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0101nnnnmmmmdddd "mov.l @(disp,Rm),Rn"
            (0b0101, nnnn, mmmm, _) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::DerefRegImm(SuperhRegister::new_gpr(mmmm.into()), n4),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 10000000nnnndddd "mov.b R0,@(disp,Rn)"
            (0b1000, 0b0000, nnnn, _) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Byte),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::R0),
                    Operand::DerefRegImm(SuperhRegister::new_gpr(nnnn.into()), n4),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 10000001nnnndddd "mov.w R0,@(disp,Rn)"
            (0b1000, 0b0001, nnnn, _) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Word),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::R0),
                    Operand::DerefRegImm(SuperhRegister::new_gpr(nnnn.into()), n4),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0001nnnnmmmmdddd "mov.l Rm,@(disp,Rn)"
            (0b0001, nnnn, mmmm, _) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::DerefRegImm(SuperhRegister::new_gpr(nnnn.into()), n4),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000nnnnmmmm1100 "mov.b @(R0,Rm),Rn"
            (0b0000, nnnn, mmmm, 0b1100) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Byte),
                cond: None,
                operands: vec![
                    Operand::DerefRegReg(SuperhRegister::R0, SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000nnnnmmmm1101 "mov.w @(R0,Rm),Rn"
            (0b0000, nnnn, mmmm, 0b1101) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Word),
                cond: None,
                operands: vec![
                    Operand::DerefRegReg(SuperhRegister::R0, SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000nnnnmmmm1110 "mov.l @(R0,Rm),Rn"
            (0b0000, nnnn, mmmm, 0b1110) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::DerefRegReg(SuperhRegister::R0, SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000nnnnmmmm0100 "mov.b Rm,@(R0,Rn)"
            (0b0000, nnnn, mmmm, 0b0100) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Byte),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::DerefRegReg(SuperhRegister::R0, SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000nnnnmmmm0101 "mov.w Rm,@(R0,Rn)"
            (0b0000, nnnn, mmmm, 0b0101) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Word),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::DerefRegReg(SuperhRegister::R0, SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000nnnnmmmm0110 "mov.l Rm,@(R0,Rn)"
            (0b0000, nnnn, mmmm, 0b0110) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::DerefRegReg(SuperhRegister::R0, SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 11000100dddddddd "mov.b @(disp,GBR),R0"
            (0b1100, 0b0100, _, _) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Byte),
                cond: None,
                operands: vec![
                    Operand::DerefRegImm(SuperhRegister::Gbr, ctx.instr_word & 0xff),
                    Operand::Reg(SuperhRegister::R0),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 11000101dddddddd "mov.w @(disp,GBR),R0"
            (0b1100, 0b0101, _, _) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Word),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::R0),
                    Operand::DerefRegImm(SuperhRegister::Gbr, ctx.instr_word & 0xff),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 11000110dddddddd "mov.l @(disp,GBR),R0"
            (0b1100, 0b0110, _, _) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::DerefRegImm(SuperhRegister::Gbr, ctx.instr_word & 0xff),
                    Operand::Reg(SuperhRegister::R0),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 11000000dddddddd "mov.b R0,@(disp,GBR)"
            (0b1100, 0b0000, _, _) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Byte),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::R0),
                    Operand::DerefRegImm(SuperhRegister::Gbr, ctx.instr_word & 0xff),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 11000001dddddddd "mov.w R0,@(disp,GBR)"
            (0b1100, 0b0001, _, _) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Word),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::R0),
                    Operand::DerefRegImm(SuperhRegister::Gbr, ctx.instr_word & 0xff),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 11000010dddddddd "mov.l R0,@(disp,GBR)"
            (0b1100, 0b0010, _, _) => Instruction {
                operation: Operation::Mov,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::R0),
                    Operand::DerefRegImm(SuperhRegister::Gbr, ctx.instr_word & 0xff),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000nnnn00101001 "movt Rn"
            (0b0000, nnnn, 0b0010, 0b1001) => Instruction {
                operation: Operation::Movt,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Reg(SuperhRegister::new_gpr(nnnn.into()))],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0110nnnnmmmm1000 "swap.b Rm,Rn"
            (0b0110, nnnn, mmmm, 0b1000) => Instruction {
                operation: Operation::Swap,
                length_suffix: Some(LengthSuffix::Byte),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0110nnnnmmmm1001 "swap.w Rm,Rn"
            (0b0110, nnnn, mmmm, 0b1001) => Instruction {
                operation: Operation::Swap,
                length_suffix: Some(LengthSuffix::Word),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0010nnnnmmmm1101 "xtrct Rm,Rn"
            (0b0010, nnnn, mmmm, 0b1101) => Instruction {
                operation: Operation::Xtrct,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0011nnnnmmmm1100 "add Rm,Rn"
            (0b0011, nnnn, mmmm, 0b1100) => Instruction {
                operation: Operation::Add,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0111nnnniiiiiiii "add #imm,Rn"
            (0b0111, nnnn, _, _) => Instruction {
                operation: Operation::Add,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Imm((ctx.instr_word & 0xff) as i16),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0011nnnnmmmm1110 "addc Rm,Rn"
            (0b0011, nnnn, mmmm, 0b1110) => Instruction {
                operation: Operation::Addc,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0011nnnnmmmm1111 "addv Rm,Rn"
            (0b0011, nnnn, mmmm, 0b1111) => Instruction {
                operation: Operation::Addv,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 10001000iiiiiiii "cmp/eq #imm,R0"
            (0b1000, 0b1000, _, _) => Instruction {
                operation: Operation::Cmp,
                length_suffix: None,
                cond: Some(Conditional::Eq),
                operands: vec![
                    Operand::Imm((ctx.instr_word & 0xff) as i16),
                    Operand::Reg(SuperhRegister::R0),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0011nnnnmmmm0000 "cmp/eq Rm,Rn"
            (0b0011, nnnn, mmmm, 0b0000) => Instruction {
                operation: Operation::Cmp,
                length_suffix: None,
                cond: Some(Conditional::Eq),
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0011nnnnmmmm0010 "cmp/hs Rm,Rn"
            (0b0011, nnnn, mmmm, 0b0010) => Instruction {
                operation: Operation::Cmp,
                length_suffix: None,
                cond: Some(Conditional::Hs),
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0011nnnnmmmm0011 "cmp/ge Rm,Rn"
            (0b0011, nnnn, mmmm, 0b0011) => Instruction {
                operation: Operation::Cmp,
                length_suffix: None,
                cond: Some(Conditional::Ge),
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0011nnnnmmmm0110 "cmp/hi Rm,Rn"
            (0b0011, nnnn, mmmm, 0b0110) => Instruction {
                operation: Operation::Cmp,
                length_suffix: None,
                cond: Some(Conditional::Hi),
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0011nnnnmmmm0111 "cmp/gt Rm,Rn"
            (0b0011, nnnn, mmmm, 0b0111) => Instruction {
                operation: Operation::Cmp,
                length_suffix: None,
                cond: Some(Conditional::Gt),
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnn00010101 "cmp/pl Rn"
            (0b0100, nnnn, 0b0001, 0b0101) => Instruction {
                operation: Operation::Cmp,
                length_suffix: None,
                cond: Some(Conditional::Pl),
                operands: vec![Operand::Reg(SuperhRegister::new_gpr(nnnn.into()))],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnn00010001 "cmp/pz Rn"
            (0b0100, nnnn, 0b0001, 0b0001) => Instruction {
                operation: Operation::Cmp,
                length_suffix: None,
                cond: Some(Conditional::Pz),
                operands: vec![Operand::Reg(SuperhRegister::new_gpr(nnnn.into()))],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0010nnnnmmmm1100 "cmp/str Rm,Rn"
            (0b0010, nnnn, mmmm, 0b1100) => Instruction {
                operation: Operation::Cmp,
                length_suffix: None,
                cond: Some(Conditional::Str),
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0010nnnnmmmm0111 "div0s Rm,Rn"
            (0b0010, nnnn, mmmm, 0b0111) => Instruction {
                operation: Operation::Div0s,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000000000011001 "div0u"
            (0b0000, 0b0000, 0b0001, 0b1001) => Instruction {
                operation: Operation::Div0u,
                length_suffix: None,
                cond: None,
                operands: vec![],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0011nnnnmmmm0100 "div1 Rm,Rn"
            (0b0011, nnnn, mmmm, 0b0100) => Instruction {
                operation: Operation::Div1,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0011nnnnmmmm1101 "dmuls.l Rm,Rn"
            (0b0011, nnnn, mmmm, 0b1101) => Instruction {
                operation: Operation::Dmuls,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0011nnnnmmmm0101 "dmulu.l Rm,Rn"
            (0b0011, nnnn, mmmm, 0b0101) => Instruction {
                operation: Operation::Dmulu,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnn00010000 "dt Rn"
            (0b0100, nnnn, 0b0001, 0b0000) => Instruction {
                operation: Operation::Dt,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Reg(SuperhRegister::new_gpr(nnnn.into()))],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0110nnnnmmmm1110 "exts.b Rm,Rn"
            (0b0110, nnnn, mmmm, 0b1110) => Instruction {
                operation: Operation::Exts,
                length_suffix: Some(LengthSuffix::Byte),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0110nnnnmmmm1111 "exts.w Rm,Rn"
            (0b0110, nnnn, mmmm, 0b1111) => Instruction {
                operation: Operation::Exts,
                length_suffix: Some(LengthSuffix::Word),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0110nnnnmmmm1100 "extu.b Rm,Rn"
            (0b0110, nnnn, mmmm, 0b1100) => Instruction {
                operation: Operation::Extu,
                length_suffix: Some(LengthSuffix::Byte),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0110nnnnmmmm1101 "extu.w Rm,Rn"
            (0b0110, nnnn, mmmm, 0b1101) => Instruction {
                operation: Operation::Extu,
                length_suffix: Some(LengthSuffix::Word),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000nnnnmmmm1111 "mac.l @Rm+,@Rn+"
            (0b0000, nnnn, mmmm, 0b1111) => Instruction {
                operation: Operation::Mac,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::DerefReg(SuperhRegister::new_gpr(mmmm.into()), OperandFlag::PostInc),
                    Operand::DerefReg(SuperhRegister::new_gpr(nnnn.into()), OperandFlag::PostInc),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnnmmmm1111 "mac.w @Rm+,@Rn+"
            (0b0100, nnnn, mmmm, 0b1111) => Instruction {
                operation: Operation::Mac,
                length_suffix: Some(LengthSuffix::Word),
                cond: None,
                operands: vec![
                    Operand::DerefReg(SuperhRegister::new_gpr(mmmm.into()), OperandFlag::PostInc),
                    Operand::DerefReg(SuperhRegister::new_gpr(nnnn.into()), OperandFlag::PostInc),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000nnnnmmmm0111 "mul.l Rm,Rn"
            (0b0000, nnnn, mmmm, 0b0111) => Instruction {
                operation: Operation::Mul,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0010nnnnmmmm1111 "muls.w Rm,Rn"
            (0b0010, nnnn, mmmm, 0b1111) => Instruction {
                operation: Operation::Muls,
                length_suffix: Some(LengthSuffix::Word),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0010nnnnmmmm1110 "mulu.w Rm,Rn"
            (0b0010, nnnn, mmmm, 0b1110) => Instruction {
                operation: Operation::Mulu,
                length_suffix: Some(LengthSuffix::Word),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0110nnnnmmmm1011 "neg Rm,Rn"
            (0b0110, nnnn, mmmm, 0b1011) => Instruction {
                operation: Operation::Neg,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0110nnnnmmmm1010 "negc Rm,Rn"
            (0b0110, nnnn, mmmm, 0b1010) => Instruction {
                operation: Operation::Negc,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0011nnnnmmmm1000 "sub Rm,Rn"
            (0b0011, nnnn, mmmm, 0b1000) => Instruction {
                operation: Operation::Sub,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0011nnnnmmmm1010 "subc Rm,Rn"
            (0b0011, nnnn, mmmm, 0b1010) => Instruction {
                operation: Operation::Subc,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0011nnnnmmmm1011 "subv Rm,Rn"
            (0b0011, nnnn, mmmm, 0b1011) => Instruction {
                operation: Operation::Subv,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0010nnnnmmmm1001 "and Rm,Rn"
            (0b0010, nnnn, mmmm, 0b1001) => Instruction {
                operation: Operation::And,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 11001001iiiiiiii "and #imm,R0"
            (0b1100, 0b1001, _, _) => Instruction {
                operation: Operation::And,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Imm((ctx.instr_word & 0xff) as i16),
                    Operand::Reg(SuperhRegister::R0),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 11001101iiiiiiii "and.b #imm,@(R0,GBR)"
            (0b1100, 0b1101, _, _) => Instruction {
                operation: Operation::And,
                length_suffix: Some(LengthSuffix::Byte),
                cond: None,
                operands: vec![
                    Operand::Imm((ctx.instr_word & 0xff) as i16),
                    Operand::DerefRegReg(SuperhRegister::R0, SuperhRegister::Gbr),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0110nnnnmmmm0111 "not Rm,Rn"
            (0b0110, nnnn, mmmm, 0b0111) => Instruction {
                operation: Operation::Not,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0010nnnnmmmm1011 "or Rm,Rn"
            (0b0010, nnnn, mmmm, 0b1011) => Instruction {
                operation: Operation::Or,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 11001011iiiiiiii "or #imm,R0"
            (0b1100, 0b1011, _, _) => Instruction {
                operation: Operation::Or,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Imm((ctx.instr_word & 0xff) as i16),
                    Operand::Reg(SuperhRegister::R0),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 11001111iiiiiiii "or.b #imm,@(R0,GBR)"
            (0b1100, 0b1111, _, _) => Instruction {
                operation: Operation::Or,
                length_suffix: Some(LengthSuffix::Byte),
                cond: None,
                operands: vec![
                    Operand::Imm((ctx.instr_word & 0xff) as i16),
                    Operand::DerefRegReg(SuperhRegister::R0, SuperhRegister::Gbr),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnn00011011 "tas.b @Rn"
            (0b0100, nnnn, 0b0001, 0b1011) => Instruction {
                operation: Operation::Tas,
                length_suffix: Some(LengthSuffix::Byte),
                cond: None,
                operands: vec![Operand::DerefReg(
                    SuperhRegister::new_gpr(nnnn.into()),
                    OperandFlag::None,
                )],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0010nnnnmmmm1000 "tst Rm,Rn"
            (0b0010, nnnn, mmmm, 0b1000) => Instruction {
                operation: Operation::Tst,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 11001000iiiiiiii "tst #imm,R0"
            (0b1100, 0b1000, _, _) => Instruction {
                operation: Operation::Tst,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Imm((ctx.instr_word & 0xff) as i16),
                    Operand::Reg(SuperhRegister::R0),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 11001100iiiiiiii "tst.b #imm,@(R0,GBR)"
            (0b1100, 0b1100, _, _) => Instruction {
                operation: Operation::Tst,
                length_suffix: Some(LengthSuffix::Byte),
                cond: None,
                operands: vec![
                    Operand::Imm((ctx.instr_word & 0xff) as i16),
                    Operand::DerefRegReg(SuperhRegister::R0, SuperhRegister::Gbr),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0010nnnnmmmm1010 "xor Rm,Rn"
            (0b0010, nnnn, mmmm, 0b1010) => Instruction {
                operation: Operation::Xor,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 11001010iiiiiiii "xor #imm,R0"
            (0b1100, 0b1010, _, _) => Instruction {
                operation: Operation::Xor,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Imm((ctx.instr_word & 0xff) as i16),
                    Operand::Reg(SuperhRegister::R0),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 11001110iiiiiiii "xor.b #imm,@(R0,GBR)"
            (0b1100, 0b1110, _, _) => Instruction {
                operation: Operation::Xor,
                length_suffix: Some(LengthSuffix::Byte),
                cond: None,
                operands: vec![
                    Operand::Imm((ctx.instr_word & 0xff) as i16),
                    Operand::DerefRegReg(SuperhRegister::R0, SuperhRegister::Gbr),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnn00100100 "rotcl Rn"
            (0b0100, nnnn, 0b0010, 0b0100) => Instruction {
                operation: Operation::Rotcl,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Reg(SuperhRegister::new_gpr(nnnn.into()))],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnn00100101 "rotcr Rn"
            (0b0100, nnnn, 0b0010, 0b0101) => Instruction {
                operation: Operation::Rotcr,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Reg(SuperhRegister::new_gpr(nnnn.into()))],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnn00000100 "rotl Rn"
            (0b0100, nnnn, 0b0000, 0b0100) => Instruction {
                operation: Operation::Rotl,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Reg(SuperhRegister::new_gpr(nnnn.into()))],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnn00000101 "rotr Rn"
            (0b0100, nnnn, 0b0000, 0b0101) => Instruction {
                operation: Operation::Rotr,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Reg(SuperhRegister::new_gpr(nnnn.into()))],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnnmmmm1100 "shad Rm,Rn"
            (0b0100, nnnn, mmmm, 0b1100) => Instruction {
                operation: Operation::Shad,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnn00100000 "shal Rn"
            (0b0100, nnnn, 0b0010, 0b0000) => Instruction {
                operation: Operation::Shal,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Reg(SuperhRegister::new_gpr(nnnn.into()))],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnn00100001 "shar Rn"
            (0b0100, nnnn, 0b0010, 0b0001) => Instruction {
                operation: Operation::Shar,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Reg(SuperhRegister::new_gpr(nnnn.into()))],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnnmmmm1101 "shld Rm,Rn"
            (0b0100, nnnn, mmmm, 0b1101) => Instruction {
                operation: Operation::Shld,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnn00000000 "shll Rn"
            (0b0100, nnnn, 0b0000, 0b0000) => Instruction {
                operation: Operation::Shll,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Reg(SuperhRegister::new_gpr(nnnn.into()))],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnn00001000 "shll2 Rn"
            (0b0100, nnnn, 0b0000, 0b1000) => Instruction {
                operation: Operation::Shll2,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Reg(SuperhRegister::new_gpr(nnnn.into()))],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnn00011000 "shll8 Rn"
            (0b0100, nnnn, 0b0001, 0b1000) => Instruction {
                operation: Operation::Shll8,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Reg(SuperhRegister::new_gpr(nnnn.into()))],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnn00101000 "shll16 Rn"
            (0b0100, nnnn, 0b0010, 0b1000) => Instruction {
                operation: Operation::Shll16,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Reg(SuperhRegister::new_gpr(nnnn.into()))],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnn00000001 "shlr Rn"
            (0b0100, nnnn, 0b0000, 0b0001) => Instruction {
                operation: Operation::Shlr,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Reg(SuperhRegister::new_gpr(nnnn.into()))],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnn00001001 "shlr2 Rn"
            (0b0100, nnnn, 0b0000, 0b1001) => Instruction {
                operation: Operation::Shlr2,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Reg(SuperhRegister::new_gpr(nnnn.into()))],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnn00011001 "shlr8 Rn"
            (0b0100, nnnn, 0b0001, 0b1001) => Instruction {
                operation: Operation::Shlr8,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Reg(SuperhRegister::new_gpr(nnnn.into()))],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnn00101001 "shlr16 Rn"
            (0b0100, nnnn, 0b0010, 0b1001) => Instruction {
                operation: Operation::Shlr16,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Reg(SuperhRegister::new_gpr(nnnn.into()))],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 10001011dddddddd "bf label"
            (0b1000, 0b1011, _, _) => Instruction {
                operation: Operation::Bf,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Address(disp8(
                    (ctx.instr_word & 0xff) as u8,
                    ctx.address as u32,
                ) as u64)],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 10001111dddddddd "bf/s label"
            (0b1000, 0b1111, _, _) => Instruction {
                operation: Operation::Bf,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Address(disp8(
                    (ctx.instr_word & 0xff) as u8,
                    ctx.address as u32,
                ) as u64)],
                delay_slot: true,
                privileged: false,
                version: SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 10001001dddddddd "bt label"
            (0b1000, 0b1001, _, _) => Instruction {
                operation: Operation::Bt,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Address(disp8(
                    (ctx.instr_word & 0xff) as u8,
                    ctx.address as u32,
                ) as u64)],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 10001101dddddddd "bt/s label"
            (0b1000, 0b1101, _, _) => Instruction {
                operation: Operation::Bt,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Address(disp8(
                    (ctx.instr_word & 0xff) as u8,
                    ctx.address as u32,
                ) as u64)],
                delay_slot: true,
                privileged: false,
                version: SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 1010dddddddddddd "bra label"
            (0b1010, _, _, _) => Instruction {
                operation: Operation::Bra,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Address(
                    disp12(ctx.instr_word, ctx.address as u32) as u64
                )],
                delay_slot: true,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000mmmm00100011 "braf Rm"
            (0b0000, mmmm, 0b0010, 0b0011) => Instruction {
                operation: Operation::Braf,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Reg(SuperhRegister::new_gpr(mmmm.into()))],
                delay_slot: true,
                privileged: false,
                version: SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 1011dddddddddddd "bsr label"
            (0b1011, _, _, _) => Instruction {
                operation: Operation::Bsr,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Address(
                    disp12(ctx.instr_word, ctx.address as u32) as u64
                )],
                delay_slot: true,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000mmmm00000011 "bsrf Rm"
            (0b0000, mmmm, 0b0000, 0b0011) => Instruction {
                operation: Operation::Bsrf,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Reg(SuperhRegister::new_gpr(mmmm.into()))],
                delay_slot: true,
                privileged: false,
                version: SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100mmmm00101011 "jmp @Rm"
            (0b0100, mmmm, 0b0010, 0b1011) => Instruction {
                operation: Operation::Jmp,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::DerefReg(
                    SuperhRegister::new_gpr(mmmm.into()),
                    OperandFlag::None,
                )],
                delay_slot: true,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100mmmm00001011 "jsr @Rm"
            (0b0100, mmmm, 0b0000, 0b1011) => Instruction {
                operation: Operation::Jsr,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::DerefReg(
                    SuperhRegister::new_gpr(mmmm.into()),
                    OperandFlag::None,
                )],
                delay_slot: true,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000000000001011 "rts"
            (0b0000, 0b0000, 0b0000, 0b1011) => Instruction {
                operation: Operation::Rts,
                length_suffix: None,
                cond: None,
                operands: vec![],
                delay_slot: true,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000000000101000 "clrmac"
            (0b0000, 0b0000, 0b0010, 0b1000) => Instruction {
                operation: Operation::Clrmac,
                length_suffix: None,
                cond: None,
                operands: vec![],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000000001001000 "clrs"
            (0b0000, 0b0000, 0b0100, 0b1000) => Instruction {
                operation: Operation::Clrs,
                length_suffix: None,
                cond: None,
                operands: vec![],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH3 | SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 0000000000001000 "clrt"
            (0b0000, 0b0000, 0b0000, 0b1000) => Instruction {
                operation: Operation::Clrt,
                length_suffix: None,
                cond: None,
                operands: vec![],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100mmmm00001110 "ldc Rm,SR"
            (0b0100, mmmm, 0b0000, 0b1110) => Instruction {
                operation: Operation::Ldc,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::SR),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100mmmm00000111 "ldc.l @Rm+,SR"
            (0b0100, mmmm, 0b0000, 0b0111) => Instruction {
                operation: Operation::Ldc,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::DerefReg(SuperhRegister::new_gpr(mmmm.into()), OperandFlag::PostInc),
                    Operand::Reg(SuperhRegister::SR),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100mmmm00011110 "ldc Rm,GBR"
            (0b0100, mmmm, 0b0001, 0b1110) => Instruction {
                operation: Operation::Ldc,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::Gbr),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100mmmm00010111 "ldc.l @Rm+,GBR"
            (0b0100, mmmm, 0b0001, 0b0111) => Instruction {
                operation: Operation::Ldc,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::DerefReg(SuperhRegister::new_gpr(mmmm.into()), OperandFlag::PostInc),
                    Operand::Reg(SuperhRegister::Gbr),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100mmmm00101110 "ldc Rm,VBR"
            (0b0100, mmmm, 0b0010, 0b1110) => Instruction {
                operation: Operation::Ldc,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::Vbr),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100mmmm00100111 "ldc.l @Rm+,VBR"
            (0b0100, mmmm, 0b0010, 0b0111) => Instruction {
                operation: Operation::Ldc,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::DerefReg(SuperhRegister::new_gpr(mmmm.into()), OperandFlag::PostInc),
                    Operand::Reg(SuperhRegister::Vbr),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100mmmm00111010 "ldc Rm,SGR"
            (0b0100, mmmm, 0b0011, 0b1010) => Instruction {
                operation: Operation::Ldc,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::Sgr),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH4A,
            },
            // 0100mmmm00110110 "ldc.l @Rm+,SGR"
            (0b0100, mmmm, 0b0011, 0b0110) => Instruction {
                operation: Operation::Ldc,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::DerefReg(SuperhRegister::new_gpr(mmmm.into()), OperandFlag::PostInc),
                    Operand::Reg(SuperhRegister::Sgr),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH4A,
            },
            // 0100mmmm00111110 "ldc Rm,SSR"
            (0b0100, mmmm, 0b0011, 0b1110) => Instruction {
                operation: Operation::Ldc,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::Ssr),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH3 | SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 0100mmmm00110111 "ldc.l @Rm+,SSR"
            (0b0100, mmmm, 0b0011, 0b0111) => Instruction {
                operation: Operation::Ldc,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::DerefReg(SuperhRegister::new_gpr(mmmm.into()), OperandFlag::PostInc),
                    Operand::Reg(SuperhRegister::Ssr),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH3 | SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 0100mmmm01001110 "ldc Rm,SPC"
            (0b0100, mmmm, 0b0100, 0b1110) => Instruction {
                operation: Operation::Ldc,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::Spc),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH3 | SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 0100mmmm01000111 "ldc.l @Rm+,SPC"
            (0b0100, mmmm, 0b0100, 0b0111) => Instruction {
                operation: Operation::Ldc,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::DerefReg(SuperhRegister::new_gpr(mmmm.into()), OperandFlag::PostInc),
                    Operand::Reg(SuperhRegister::Spc),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH3 | SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 0100mmmm11111010 "ldc Rm,DBR"
            (0b0100, mmmm, 0b1111, 0b1010) => Instruction {
                operation: Operation::Ldc,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::Dbr),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 0100mmmm11110110 "ldc.l @Rm+,DBR"
            (0b0100, mmmm, 0b1111, 0b0110) => Instruction {
                operation: Operation::Ldc,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::DerefReg(SuperhRegister::new_gpr(mmmm.into()), OperandFlag::PostInc),
                    Operand::Reg(SuperhRegister::Dbr),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 0100mmmm1nnn1110 "ldc Rm,Rn_BANK"
            (0b0100, mmmm, nnnn, 0b1110) if (nnnn & 0b1000) == 0b1000 => Instruction {
                operation: Operation::Ldc,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_bank_reg((nnnn & 0b0111).into())),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH3 | SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 0100mmmm1nnn0111 "ldc.l @Rm+,Rn_BANK"
            (0b0100, mmmm, nnnn, 0b0111) if (nnnn & 0b1000) == 0b1000 => Instruction {
                operation: Operation::Ldc,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::DerefReg(SuperhRegister::new_gpr(mmmm.into()), OperandFlag::PostInc),
                    Operand::Reg(SuperhRegister::new_bank_reg((nnnn & 0b0111).into())),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH3 | SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 0100mmmm00001010 "lds Rm,MACH"
            (0b0100, mmmm, 0b0000, 0b1010) => Instruction {
                operation: Operation::Lds,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::Mach),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100mmmm00000110 "lds.l @Rm+,MACH"
            (0b0100, mmmm, 0b0000, 0b0110) => Instruction {
                operation: Operation::Lds,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::DerefReg(SuperhRegister::new_gpr(mmmm.into()), OperandFlag::PostInc),
                    Operand::Reg(SuperhRegister::Mach),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100mmmm00011010 "lds Rm,MACL"
            (0b0100, mmmm, 0b0001, 0b1010) => Instruction {
                operation: Operation::Lds,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::Macl),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100mmmm00010110 "lds.l @Rm+,MACL"
            (0b0100, mmmm, 0b0001, 0b0110) => Instruction {
                operation: Operation::Lds,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::DerefReg(SuperhRegister::new_gpr(mmmm.into()), OperandFlag::PostInc),
                    Operand::Reg(SuperhRegister::Macl),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100mmmm00101010 "lds Rm,PR"
            (0b0100, mmmm, 0b0010, 0b1010) => Instruction {
                operation: Operation::Lds,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::PR),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100mmmm00100110 "lds.l @Rm+,PR"
            (0b0100, mmmm, 0b0010, 0b0110) => Instruction {
                operation: Operation::Lds,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::DerefReg(SuperhRegister::new_gpr(mmmm.into()), OperandFlag::PostInc),
                    Operand::Reg(SuperhRegister::PR),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000000000111000 "ldtlb"
            (0b0000, 0b0000, 0b0011, 0b1000) => Instruction {
                operation: Operation::Ldtlb,
                length_suffix: None,
                cond: None,
                operands: vec![],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH3 | SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 0000nnnn11000011 "movca.l R0,@Rn"
            (0b0000, nnnn, 0b1100, 0b0011) => Instruction {
                operation: Operation::Movca,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::R0),
                    Operand::DerefReg(SuperhRegister::new_gpr(nnnn.into()), OperandFlag::None),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 0000000000001001 "nop"
            (0b0000, 0b0000, 0b0000, 0b1001) => Instruction {
                operation: Operation::Nop,
                length_suffix: None,
                cond: None,
                operands: vec![],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000nnnn10010011 "ocbi @Rn"
            (0b0000, nnnn, 0b1001, 0b0011) => Instruction {
                operation: Operation::Ocbi,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::DerefReg(
                    SuperhRegister::new_gpr(nnnn.into()),
                    OperandFlag::None,
                )],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 0000nnnn10100011 "ocbp @Rn"
            (0b0000, nnnn, 0b1010, 0b0011) => Instruction {
                operation: Operation::Ocbp,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::DerefReg(
                    SuperhRegister::new_gpr(nnnn.into()),
                    OperandFlag::None,
                )],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 0000nnnn10110011 "ocbwb @Rn"
            (0b0000, nnnn, 0b1011, 0b0011) => Instruction {
                operation: Operation::Ocbwb,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::DerefReg(
                    SuperhRegister::new_gpr(nnnn.into()),
                    OperandFlag::None,
                )],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 0000nnnn10000011 "pref @Rn"
            (0b0000, nnnn, 0b1000, 0b0011) => Instruction {
                operation: Operation::Pref,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::DerefReg(
                    SuperhRegister::new_gpr(nnnn.into()),
                    OperandFlag::None,
                )],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000000000101011 "rte"
            (0b0000, 0b0000, 0b0010, 0b1011) => Instruction {
                operation: Operation::Rte,
                length_suffix: None,
                cond: None,
                operands: vec![],
                delay_slot: true,
                privileged: true,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000000001011000 "sets"
            (0b0000, 0b0000, 0b0101, 0b1000) => Instruction {
                operation: Operation::Sets,
                length_suffix: None,
                cond: None,
                operands: vec![],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH3 | SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 0000000000011000 "sett"
            (0b0000, 0b0000, 0b0001, 0b1000) => Instruction {
                operation: Operation::Sett,
                length_suffix: None,
                cond: None,
                operands: vec![],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000000000011011 "sleep"
            (0b0000, 0b0000, 0b0001, 0b1011) => Instruction {
                operation: Operation::Sleep,
                length_suffix: None,
                cond: None,
                operands: vec![],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000nnnn00000010 "stc SR,Rn"
            (0b0000, nnnn, 0b0000, 0b0010) => Instruction {
                operation: Operation::Stc,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::SR),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnn00000011 "stc.l SR,@-Rn"
            (0b0100, nnnn, 0b0000, 0b0011) => Instruction {
                operation: Operation::Stc,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::SR),
                    Operand::DerefReg(SuperhRegister::new_gpr(nnnn.into()), OperandFlag::PreDec),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000nnnn00010010 "stc GBR,Rn"
            (0b0000, nnnn, 0b0001, 0b0010) => Instruction {
                operation: Operation::Stc,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::Gbr),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnn00010011 "stc.l GBR,@-Rn"
            (0b0100, nnnn, 0b0001, 0b0011) => Instruction {
                operation: Operation::Stc,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::Gbr),
                    Operand::DerefReg(SuperhRegister::new_gpr(nnnn.into()), OperandFlag::PreDec),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000nnnn00100010 "stc VBR,Rn"
            (0b0000, nnnn, 0b0010, 0b0010) => Instruction {
                operation: Operation::Stc,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::Vbr),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnn00100011 "stc.l VBR,@-Rn"
            (0b0100, nnnn, 0b0010, 0b0011) => Instruction {
                operation: Operation::Stc,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::Vbr),
                    Operand::DerefReg(SuperhRegister::new_gpr(nnnn.into()), OperandFlag::PreDec),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000nnnn00111010 "stc SGR,Rn"
            (0b0000, nnnn, 0b0011, 0b1010) => Instruction {
                operation: Operation::Stc,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::Sgr),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 0100nnnn00110010 "stc.l SGR,@-Rn"
            (0b0100, nnnn, 0b0011, 0b0010) => Instruction {
                operation: Operation::Stc,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::Sgr),
                    Operand::DerefReg(SuperhRegister::new_gpr(nnnn.into()), OperandFlag::PreDec),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 0000nnnn00110010 "stc SSR,Rn"
            (0b0000, nnnn, 0b0011, 0b0010) => Instruction {
                operation: Operation::Stc,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::Ssr),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH3 | SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 0100nnnn00110011 "stc.l SSR,@-Rn"
            (0b0100, nnnn, 0b0011, 0b0011) => Instruction {
                operation: Operation::Stc,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::Ssr),
                    Operand::DerefReg(SuperhRegister::new_gpr(nnnn.into()), OperandFlag::PreDec),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH3 | SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 0000nnnn01000010 "stc SPC,Rn"
            (0b0000, nnnn, 0b0100, 0b0010) => Instruction {
                operation: Operation::Stc,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::Spc),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH3 | SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 0100nnnn01000011 "stc.l SPC,@-Rn"
            (0b0100, nnnn, 0b0100, 0b0011) => Instruction {
                operation: Operation::Stc,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::Spc),
                    Operand::DerefReg(SuperhRegister::new_gpr(nnnn.into()), OperandFlag::PreDec),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH3 | SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 0000nnnn11111010 "stc DBR,Rn"
            (0b0000, nnnn, 0b1111, 0b1010) => Instruction {
                operation: Operation::Stc,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::Dbr),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 0100nnnn11110010 "stc.l DBR,@-Rn"
            (0b0100, nnnn, 0b1111, 0b0010) => Instruction {
                operation: Operation::Stc,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::Dbr),
                    Operand::DerefReg(SuperhRegister::new_gpr(nnnn.into()), OperandFlag::PreDec),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 0000nnnn1mmm0010 "stc Rm_BANK,Rn"
            (0b0000, nnnn, mmmm, 0b0010) if (mmmm & 0b1000) == 0b1000 => Instruction {
                operation: Operation::Stc,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_bank_reg(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH3 | SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 0100nnnn1mmm0011 "stc.l Rm_BANK,@-Rn"
            (0b0100, nnnn, mmmm, 0b0011) if (mmmm & 0b1000) == 0b1000 => Instruction {
                operation: Operation::Stc,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_bank_reg(mmmm.into())),
                    Operand::DerefReg(SuperhRegister::new_gpr(nnnn.into()), OperandFlag::PreDec),
                ],
                delay_slot: false,
                privileged: true,
                version: SuperhVersion::SH3 | SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 0000nnnn00001010 "sts MACH,Rn"
            (0b0000, nnnn, 0b0000, 0b1010) => Instruction {
                operation: Operation::Sts,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::Mach),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnn00000010 "sts.l MACH,@-Rn"
            (0b0100, nnnn, 0b0000, 0b0010) => Instruction {
                operation: Operation::Sts,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::Mach),
                    Operand::DerefReg(SuperhRegister::new_gpr(nnnn.into()), OperandFlag::PreDec),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000nnnn00011010 "sts MACL,Rn"
            (0b0000, nnnn, 0b0001, 0b1010) => Instruction {
                operation: Operation::Sts,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::Macl),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnn00010010 "sts.l MACL,@-Rn"
            (0b0100, nnnn, 0b0001, 0b0010) => Instruction {
                operation: Operation::Sts,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::Macl),
                    Operand::DerefReg(SuperhRegister::new_gpr(nnnn.into()), OperandFlag::PreDec),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000nnnn00101010 "sts PR,Rn"
            (0b0000, nnnn, 0b0010, 0b1010) => Instruction {
                operation: Operation::Sts,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::PR),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnn00100010 "sts.l PR,@-Rn"
            (0b0100, nnnn, 0b0010, 0b0010) => Instruction {
                operation: Operation::Sts,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::PR),
                    Operand::DerefReg(SuperhRegister::new_gpr(nnnn.into()), OperandFlag::PreDec),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 11000011iiiiiiii "trapa #imm"
            (0b1100, 0b0011, _, _) => Instruction {
                operation: Operation::Trapa,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Imm((ctx.instr_word & 0xff) as i16)],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH1
                    | SuperhVersion::SH2
                    | SuperhVersion::SH3
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 1111nnnnmmmm1100 "fmov FRm,FRn"
            (0b1111, nnnn, mmmm, 0b1100) => {
                if !ctx.fpscr_sz {
                    Instruction {
                        operation: Operation::Fmov,
                        length_suffix: None,
                        cond: None,
                        operands: vec![
                            Operand::Reg(SuperhRegister::new_fr(mmmm.into())),
                            Operand::Reg(SuperhRegister::new_fr(nnnn.into())),
                        ],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH2E
                            | SuperhVersion::SH3E
                            | SuperhVersion::SH4
                            | SuperhVersion::SH4A
                            | SuperhVersion::SH2A,
                    }
                } else {
                    let nnn = nnnn >> 1;
                    let mmm = mmmm >> 1;
                    match ((nnnn & 0b0001), (mmmm & 0b0001)) {
                        // 1111nnn0mmm01100 "fmov DRm,DRn"
                        (0b0000, 0b0000) => Instruction {
                            operation: Operation::Fmov,
                            length_suffix: None,
                            cond: None,
                            operands: vec![
                                Operand::Reg(SuperhRegister::new_dr(mmm.into())),
                                Operand::Reg(SuperhRegister::new_dr(nnn.into())),
                            ],
                            delay_slot: false,
                            privileged: false,
                            version: SuperhVersion::SH4 | SuperhVersion::SH4A | SuperhVersion::SH2A,
                        },
                        // 1111nnn1mmm01100 "fmov DRm,XDn"
                        (0b0001, 0b0000) => Instruction {
                            operation: Operation::Fmov,
                            length_suffix: None,
                            cond: None,
                            operands: vec![],
                            delay_slot: false,
                            privileged: false,
                            version: SuperhVersion::SH4 | SuperhVersion::SH4A,
                        },
                        // 1111nnn0mmm11100 "fmov XDm,DRn"
                        (0b0000, 0b0001) => Instruction {
                            operation: Operation::Fmov,
                            length_suffix: None,
                            cond: None,
                            operands: vec![],
                            delay_slot: false,
                            privileged: false,
                            version: SuperhVersion::SH4 | SuperhVersion::SH4A,
                        },
                        // 1111nnn1mmm11100 "fmov XDm,XDn"
                        (0b0001, 0b0001) => Instruction {
                            operation: Operation::Fmov,
                            length_suffix: None,
                            cond: None,
                            operands: vec![],
                            delay_slot: false,
                            privileged: false,
                            version: SuperhVersion::SH4 | SuperhVersion::SH4A,
                        },
                        _ => unreachable!(),
                    }
                }
            }
            // 1111nnnnmmmm1000 "fmov.s @Rm,FRn"
            (0b1111, nnnn, mmmm, 0b1000) => {
                if !ctx.fpscr_sz {
                    Instruction {
                        operation: Operation::Fmov,
                        length_suffix: Some(LengthSuffix::Single),
                        cond: None,
                        operands: vec![
                            Operand::DerefReg(
                                SuperhRegister::new_gpr(mmmm.into()),
                                OperandFlag::None,
                            ),
                            Operand::Reg(SuperhRegister::new_fr(nnnn.into())),
                        ],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH2E
                            | SuperhVersion::SH3E
                            | SuperhVersion::SH4
                            | SuperhVersion::SH4A
                            | SuperhVersion::SH2A,
                    }
                } else {
                    // let nnn = nnnn >> 1;
                    match nnnn & 0b0001 {
                        // 1111nnn0mmmm1000 "fmov.d @Rm,DRn"
                        0b0000 => Instruction {
                            operation: Operation::Fmov,
                            length_suffix: Some(LengthSuffix::Double),
                            cond: None,
                            operands: vec![],
                            delay_slot: false,
                            privileged: false,
                            version: SuperhVersion::SH4 | SuperhVersion::SH4A | SuperhVersion::SH2A,
                        },
                        // 1111nnn1mmmm1000 "fmov.d @Rm,XDn"
                        0b0001 => Instruction {
                            operation: Operation::Fmov,
                            length_suffix: Some(LengthSuffix::Double),
                            cond: None,
                            operands: vec![],
                            delay_slot: false,
                            privileged: false,
                            version: SuperhVersion::SH4 | SuperhVersion::SH4A,
                        },
                        _ => unreachable!(),
                    }
                }
            }
            // 1111nnnnmmmm1010 "fmov.s FRm,@Rn"
            (0b1111, nnnn, mmmm, 0b1010) => {
                if !ctx.fpscr_sz {
                    Instruction {
                        operation: Operation::Fmov,
                        length_suffix: Some(LengthSuffix::Single),
                        cond: None,
                        operands: vec![
                            Operand::Reg(SuperhRegister::new_fr(mmmm.into())),
                            Operand::DerefReg(
                                SuperhRegister::new_gpr(nnnn.into()),
                                OperandFlag::None,
                            ),
                        ],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH2E
                            | SuperhVersion::SH3E
                            | SuperhVersion::SH4
                            | SuperhVersion::SH4A
                            | SuperhVersion::SH2A,
                    }
                } else {
                    // let mmm = mmmm >> 1;
                    match mmmm & 0b0001 {
                        // 1111nnnnmmm01010 "fmov.d DRm,@Rn"
                        0b0000 => Instruction {
                            operation: Operation::Fmov,
                            length_suffix: Some(LengthSuffix::Double),
                            cond: None,
                            operands: vec![],
                            delay_slot: false,
                            privileged: false,
                            version: SuperhVersion::SH4 | SuperhVersion::SH4A | SuperhVersion::SH2A,
                        },
                        // 1111nnnnmmm11010 "fmov.d XDm,@Rn"
                        0b0001 => Instruction {
                            operation: Operation::Fmov,
                            length_suffix: Some(LengthSuffix::Double),
                            cond: None,
                            operands: vec![],
                            delay_slot: false,
                            privileged: false,
                            version: SuperhVersion::SH4 | SuperhVersion::SH4A,
                        },
                        _ => unreachable!(),
                    }
                }
            }
            // 1111nnnnmmmm1001 "fmov.s @Rm+,FRn"
            (0b1111, nnnn, mmmm, 0b1001) => {
                if !ctx.fpscr_sz {
                    Instruction {
                        operation: Operation::Fmov,
                        length_suffix: Some(LengthSuffix::Single),
                        cond: None,
                        operands: vec![
                            Operand::DerefReg(
                                SuperhRegister::new_gpr(mmmm.into()),
                                OperandFlag::PostInc,
                            ),
                            Operand::Reg(SuperhRegister::new_fr(nnnn.into())),
                        ],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH2E
                            | SuperhVersion::SH3E
                            | SuperhVersion::SH4
                            | SuperhVersion::SH4A
                            | SuperhVersion::SH2A,
                    }
                } else {
                    // let nnn = nnnn >> 1;
                    match nnnn & 0b0001 {
                        // 1111nnn0mmmm1001 "fmov.d @Rm+,DRn"
                        0b0000 => Instruction {
                            operation: Operation::Fmov,
                            length_suffix: Some(LengthSuffix::Double),
                            cond: None,
                            operands: vec![],
                            delay_slot: false,
                            privileged: false,
                            version: SuperhVersion::SH4 | SuperhVersion::SH4A | SuperhVersion::SH2A,
                        },
                        // 1111nnn1mmmm1001 "fmov.d @Rm+,XDn"
                        0b0001 => Instruction {
                            operation: Operation::Fmov,
                            length_suffix: Some(LengthSuffix::Double),
                            cond: None,
                            operands: vec![],
                            delay_slot: false,
                            privileged: false,
                            version: SuperhVersion::SH4 | SuperhVersion::SH4A,
                        },
                        _ => unreachable!(),
                    }
                }
            }
            // 1111nnnnmmmm1011 "fmov.s FRm,@-Rn"
            (0b1111, nnnn, mmmm, 0b1011) => {
                if !ctx.fpscr_sz {
                    Instruction {
                        operation: Operation::Fmov,
                        length_suffix: Some(LengthSuffix::Single),
                        cond: None,
                        operands: vec![
                            Operand::Reg(SuperhRegister::new_fr(mmmm.into())),
                            Operand::DerefReg(
                                SuperhRegister::new_gpr(nnnn.into()),
                                OperandFlag::PreDec,
                            ),
                        ],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH2E
                            | SuperhVersion::SH3E
                            | SuperhVersion::SH4
                            | SuperhVersion::SH4A
                            | SuperhVersion::SH2A,
                    }
                } else {
                    // let mmm = mmmm >> 1;
                    match mmmm & 0b0001 {
                        // 1111nnnnmmm01011 "fmov.d DRm,@-Rn"
                        0b0000 => Instruction {
                            operation: Operation::Fmov,
                            length_suffix: Some(LengthSuffix::Double),
                            cond: None,
                            operands: vec![],
                            delay_slot: false,
                            privileged: false,
                            version: SuperhVersion::SH4 | SuperhVersion::SH4A | SuperhVersion::SH2A,
                        },
                        // 1111nnnnmmm11011 "fmov.d XDm,@-Rn"
                        0b0001 => Instruction {
                            operation: Operation::Fmov,
                            length_suffix: Some(LengthSuffix::Double),
                            cond: None,
                            operands: vec![],
                            delay_slot: false,
                            privileged: false,
                            version: SuperhVersion::SH4 | SuperhVersion::SH4A,
                        },
                        _ => unreachable!(),
                    }
                }
            }
            // 1111nnnnmmmm0110 "fmov.s @(R0,Rm),FRn"
            (0b1111, nnnn, mmmm, 0b0110) => {
                if !ctx.fpscr_sz {
                    Instruction {
                        operation: Operation::Fmov,
                        length_suffix: Some(LengthSuffix::Single),
                        cond: None,
                        operands: vec![
                            Operand::DerefRegReg(
                                SuperhRegister::R0,
                                SuperhRegister::new_gpr(mmmm.into()),
                            ),
                            Operand::Reg(SuperhRegister::new_fr(nnnn.into())),
                        ],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH2E
                            | SuperhVersion::SH3E
                            | SuperhVersion::SH4
                            | SuperhVersion::SH4A
                            | SuperhVersion::SH2A,
                    }
                } else {
                    // let nnn = nnnn >> 1;
                    match nnnn & 0b0001 {
                        // 1111nnn0mmmm0110 "fmov.d @(R0,Rm),DRn"
                        0b0000 => Instruction {
                            operation: Operation::Fmov,
                            length_suffix: Some(LengthSuffix::Double),
                            cond: None,
                            operands: vec![],
                            delay_slot: false,
                            privileged: false,
                            version: SuperhVersion::SH4 | SuperhVersion::SH4A | SuperhVersion::SH2A,
                        },
                        // 1111nnn1mmmm0110 "fmov.d @(R0,Rm),XDn"
                        0b0001 => Instruction {
                            operation: Operation::Fmov,
                            length_suffix: Some(LengthSuffix::Double),
                            cond: None,
                            operands: vec![],
                            delay_slot: false,
                            privileged: false,
                            version: SuperhVersion::SH4 | SuperhVersion::SH4A,
                        },
                        _ => unreachable!(),
                    }
                }
            }
            // 1111nnnnmmmm0111 "fmov.s FRm,@(R0,Rn)"
            (0b1111, nnnn, mmmm, 0b0111) => {
                if !ctx.fpscr_sz {
                    Instruction {
                        operation: Operation::Fmov,
                        length_suffix: Some(LengthSuffix::Single),
                        cond: None,
                        operands: vec![
                            Operand::Reg(SuperhRegister::new_fr(mmmm.into())),
                            Operand::DerefRegReg(
                                SuperhRegister::R0,
                                SuperhRegister::new_gpr(nnnn.into()),
                            ),
                        ],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH2E
                            | SuperhVersion::SH3E
                            | SuperhVersion::SH4
                            | SuperhVersion::SH4A
                            | SuperhVersion::SH2A,
                    }
                } else {
                    // let mmm = mmmm >> 1;
                    match mmmm & 0b0001 {
                        // 1111nnnnmmm00111 "fmov.d DRm,@(R0,Rn)"
                        0b0000 => Instruction {
                            operation: Operation::Fmov,
                            length_suffix: Some(LengthSuffix::Double),
                            cond: None,
                            operands: vec![],
                            delay_slot: false,
                            privileged: false,
                            version: SuperhVersion::SH4 | SuperhVersion::SH4A | SuperhVersion::SH2A,
                        },
                        // 1111nnnnmmm10111 "fmov.d XDm,@(R0,Rn)"
                        0b0001 => Instruction {
                            operation: Operation::Fmov,
                            length_suffix: Some(LengthSuffix::Double),
                            cond: None,
                            operands: vec![],
                            delay_slot: false,
                            privileged: false,
                            version: SuperhVersion::SH4 | SuperhVersion::SH4A,
                        },
                        _ => unreachable!(),
                    }
                }
            }
            // 1111nnnn10001101 "fldi0 FRn"
            (0b1111, nnnn, 0b1000, 0b1101) => Instruction {
                operation: Operation::Fldi0,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Reg(SuperhRegister::new_fr(nnnn.into()))],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH2E
                    | SuperhVersion::SH3E
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 1111nnnn10011101 "fldi1 FRn"
            (0b1111, nnnn, 0b1001, 0b1101) => Instruction {
                operation: Operation::Fldi1,
                length_suffix: None,
                cond: None,
                operands: vec![Operand::Reg(SuperhRegister::new_fr(nnnn.into()))],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH2E
                    | SuperhVersion::SH3E
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 1111mmmm00011101 "flds FRm,FPUL"
            (0b1111, mmmm, 0b0001, 0b1101) => Instruction {
                operation: Operation::Flds,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_fr(mmmm.into())),
                    Operand::Reg(SuperhRegister::Fpul),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH2E
                    | SuperhVersion::SH3E
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 1111nnnn00001101 "fsts FPUL,FRn"
            (0b1111, nnnn, 0b0000, 0b1101) => Instruction {
                operation: Operation::Fsts,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::Fpul),
                    Operand::Reg(SuperhRegister::new_fr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH2E
                    | SuperhVersion::SH3E
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            (0b1111, nnnn, 0b0101, 0b1101) => {
                // let nnn = nnnn >> 1;
                match (ctx.fpscr_pr, nnnn & 0b0001) {
                    // 1111nnn001011101 "fabs DRn"
                    (true, 0b0000) => Instruction {
                        operation: Operation::Fabs,
                        length_suffix: None,
                        cond: None,
                        operands: vec![],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH4 | SuperhVersion::SH4A | SuperhVersion::SH2A,
                    },
                    // 1111nnnn01011101 "fabs FRn"
                    _ => Instruction {
                        operation: Operation::Fabs,
                        length_suffix: None,
                        cond: None,
                        operands: vec![Operand::Reg(SuperhRegister::new_fr(nnnn.into()))],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH2E
                            | SuperhVersion::SH3E
                            | SuperhVersion::SH4
                            | SuperhVersion::SH4A
                            | SuperhVersion::SH2A,
                    },
                }
            }
            (0b1111, nnnn, 0b0100, 0b1101) => {
                // let nnn = nnnn >> 1;
                match (ctx.fpscr_pr, nnnn & 0b0001) {
                    // 1111nnn001001101 "fneg DRn"
                    (true, 0b0000) => Instruction {
                        operation: Operation::Fneg,
                        length_suffix: None,
                        cond: None,
                        operands: vec![Operand::Reg(SuperhRegister::new_dr(nnnn.into()))],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH4 | SuperhVersion::SH4A | SuperhVersion::SH2A,
                    },
                    // 1111nnnn01001101 "fneg FRn"
                    _ => Instruction {
                        operation: Operation::Fneg,
                        length_suffix: None,
                        cond: None,
                        operands: vec![Operand::Reg(SuperhRegister::new_fr(nnnn.into()))],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH2E
                            | SuperhVersion::SH3E
                            | SuperhVersion::SH4
                            | SuperhVersion::SH4A
                            | SuperhVersion::SH2A,
                    },
                }
            }
            (0b1111, nnnn, mmmm, 0b0000) => {
                // let nnn = nnnn >> 1;
                // let mmm = mmmm >> 1;
                match (ctx.fpscr_pr, nnnn & 0b0001, mmmm & 0b0001) {
                    // 1111nnn0mmm00000 "fadd DRm,DRn"
                    (true, 0b0000, 0b0000) => Instruction {
                        operation: Operation::Fadd,
                        length_suffix: None,
                        cond: None,
                        operands: vec![],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH4 | SuperhVersion::SH4A | SuperhVersion::SH2A,
                    },
                    // 1111nnnnmmmm0000 "fadd FRm,FRn"
                    _ => Instruction {
                        operation: Operation::Fadd,
                        length_suffix: None,
                        cond: None,
                        operands: vec![
                            Operand::Reg(SuperhRegister::new_fr(mmmm.into())),
                            Operand::Reg(SuperhRegister::new_fr(nnnn.into())),
                        ],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH2E
                            | SuperhVersion::SH3E
                            | SuperhVersion::SH4
                            | SuperhVersion::SH4A
                            | SuperhVersion::SH2A,
                    },
                }
            }
            (0b1111, nnnn, mmmm, 0b0001) => {
                // let nnn = nnnn >> 1;
                // let mmm = mmmm >> 1;
                match (ctx.fpscr_pr, nnnn & 0b0001, mmmm & 0b0001) {
                    // 1111nnn0mmm00001 "fsub DRm,DRn"
                    (true, 0b0000, 0b0000) => Instruction {
                        operation: Operation::Fsub,
                        length_suffix: None,
                        cond: None,
                        operands: vec![],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH4 | SuperhVersion::SH4A | SuperhVersion::SH2A,
                    },
                    // 1111nnnnmmmm0001 "fsub FRm,FRn"
                    _ => Instruction {
                        operation: Operation::Fsub,
                        length_suffix: None,
                        cond: None,
                        operands: vec![
                            Operand::Reg(SuperhRegister::new_fr(mmmm.into())),
                            Operand::Reg(SuperhRegister::new_fr(nnnn.into())),
                        ],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH2E
                            | SuperhVersion::SH3E
                            | SuperhVersion::SH4
                            | SuperhVersion::SH4A
                            | SuperhVersion::SH2A,
                    },
                }
            }
            (0b1111, nnnn, mmmm, 0b0010) => {
                // let nnn = nnnn >> 1;
                // let mmm = mmmm >> 1;
                match (ctx.fpscr_pr, nnnn & 0b0001, mmmm & 0b0001) {
                    // 1111nnn0mmm00010 "fmul DRm,DRn"
                    (true, 0b0000, 0b0000) => Instruction {
                        operation: Operation::Fmul,
                        length_suffix: None,
                        cond: None,
                        operands: vec![],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH4 | SuperhVersion::SH4A | SuperhVersion::SH2A,
                    },
                    // 1111nnnnmmmm0010 "fmul FRm,FRn"
                    _ => Instruction {
                        operation: Operation::Fmul,
                        length_suffix: None,
                        cond: None,
                        operands: vec![
                            Operand::Reg(SuperhRegister::new_fr(mmmm.into())),
                            Operand::Reg(SuperhRegister::new_fr(nnnn.into())),
                        ],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH2E
                            | SuperhVersion::SH3E
                            | SuperhVersion::SH4
                            | SuperhVersion::SH4A
                            | SuperhVersion::SH2A,
                    },
                }
            }
            // 1111nnnnmmmm1110 "fmac FR0,FRm,FRn"
            (0b1111, nnnn, mmmm, 0b1110) => Instruction {
                operation: Operation::Fmac,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::Fr0),
                    Operand::Reg(SuperhRegister::new_fr(mmmm.into())),
                    Operand::Reg(SuperhRegister::new_fr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH2E
                    | SuperhVersion::SH3E
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            (0b1111, nnnn, mmmm, 0b0011) => {
                // let nnn = nnnn >> 1;
                // let mmm = mmmm >> 1;
                match (ctx.fpscr_pr, nnnn & 0b0001, mmmm & 0b0001) {
                    // 1111nnn0mmm00011 "fdiv DRm,DRn"
                    (true, 0b0000, 0b0000) => Instruction {
                        operation: Operation::Fdiv,
                        length_suffix: None,
                        cond: None,
                        operands: vec![],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH4 | SuperhVersion::SH4A | SuperhVersion::SH2A,
                    },
                    // 1111nnnnmmmm0011 "fdiv FRm,FRn"
                    _ => Instruction {
                        operation: Operation::Fdiv,
                        length_suffix: None,
                        cond: None,
                        operands: vec![
                            Operand::Reg(SuperhRegister::new_fr(mmmm.into())),
                            Operand::Reg(SuperhRegister::new_fr(nnnn.into())),
                        ],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH2E
                            | SuperhVersion::SH3E
                            | SuperhVersion::SH4
                            | SuperhVersion::SH4A
                            | SuperhVersion::SH2A,
                    },
                }
            }
            (0b1111, nnnn, 0b0110, 0b1101) => {
                // let nnn = nnnn >> 1;
                match (ctx.fpscr_pr, nnnn & 0b0001) {
                    // 1111nnn001101101 "fsqrt DRn"
                    (true, 0b0000) => Instruction {
                        operation: Operation::Fsqrt,
                        length_suffix: None,
                        cond: None,
                        operands: vec![],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH4 | SuperhVersion::SH4A | SuperhVersion::SH2A,
                    },
                    // 1111nnnn01101101 "fsqrt FRn"
                    _ => Instruction {
                        operation: Operation::Fsqrt,
                        length_suffix: None,
                        cond: None,
                        operands: vec![Operand::Reg(SuperhRegister::new_fr(nnnn.into()))],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH3E
                            | SuperhVersion::SH4
                            | SuperhVersion::SH4A
                            | SuperhVersion::SH2A,
                    },
                }
            }
            (0b1111, nnnn, mmmm, 0b0100) => {
                // let nnn = nnnn >> 1;
                // let mmm = mmmm >> 1;
                match (ctx.fpscr_pr, nnnn & 0b0001, mmmm & 0b0001) {
                    // 1111nnn0mmm00100 "fcmp/eq DRm,DRn"
                    (true, 0b0000, 0b0000) => Instruction {
                        operation: Operation::Fcmp,
                        length_suffix: None,
                        cond: Some(Conditional::Eq),
                        operands: vec![],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH4 | SuperhVersion::SH4A | SuperhVersion::SH2A,
                    },
                    // 1111nnnnmmmm0100 "fcmp/eq FRm,FRn"
                    _ => Instruction {
                        operation: Operation::Fcmp,
                        length_suffix: None,
                        cond: Some(Conditional::Eq),
                        operands: vec![
                            Operand::Reg(SuperhRegister::new_fr(mmmm.into())),
                            Operand::Reg(SuperhRegister::new_fr(nnnn.into())),
                        ],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH2E
                            | SuperhVersion::SH3E
                            | SuperhVersion::SH4
                            | SuperhVersion::SH4A
                            | SuperhVersion::SH2A,
                    },
                }
            }
            // 1111nnnnmmmm0101 "fcmp/gt FRm,FRn"
            (0b1111, nnnn, mmmm, 0b0101) => {
                // let nnn = nnnn >> 1;
                // let mmm = mmmm >> 1;
                match (ctx.fpscr_pr, nnnn & 0b0001, mmmm & 0b0001) {
                    // 1111nnn0mmm00101 "fcmp/gt DRm,DRn"
                    (true, 0b0000, 0b0000) => Instruction {
                        operation: Operation::Fcmp,
                        length_suffix: None,
                        cond: Some(Conditional::Gt),
                        operands: vec![],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH4 | SuperhVersion::SH4A | SuperhVersion::SH2A,
                    },
                    _ => Instruction {
                        operation: Operation::Fcmp,
                        length_suffix: None,
                        cond: Some(Conditional::Gt),
                        operands: vec![
                            Operand::Reg(SuperhRegister::new_fr(mmmm.into())),
                            Operand::Reg(SuperhRegister::new_fr(nnnn.into())),
                        ],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH2E
                            | SuperhVersion::SH3E
                            | SuperhVersion::SH4
                            | SuperhVersion::SH4A
                            | SuperhVersion::SH2A,
                    },
                }
            }
            (0b1111, nnnn, 0b0010, 0b1101) => {
                // let nnn = nnnn >> 1;
                match (ctx.fpscr_pr, nnnn & 0b0001) {
                    // 1111nnn000101101 "float FPUL,DRn"
                    (true, 0b0000) => Instruction {
                        operation: Operation::Float,
                        length_suffix: None,
                        cond: None,
                        operands: vec![],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH4 | SuperhVersion::SH4A | SuperhVersion::SH2A,
                    },
                    // 1111nnnn00101101 "float FPUL,FRn"
                    _ => Instruction {
                        operation: Operation::Float,
                        length_suffix: None,
                        cond: None,
                        operands: vec![
                            Operand::Reg(SuperhRegister::Fpul),
                            Operand::Reg(SuperhRegister::new_fr(nnnn.into())),
                        ],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH2E
                            | SuperhVersion::SH3E
                            | SuperhVersion::SH4
                            | SuperhVersion::SH4A
                            | SuperhVersion::SH2A,
                    },
                }
            }
            (0b1111, mmmm, 0b0011, 0b1101) => {
                // let mmm = mmmm >> 1;
                match (ctx.fpscr_pr, mmmm & 0b0001) {
                    // 1111mmm000111101 "ftrc DRm,FPUL"
                    (true, 0b0000) => Instruction {
                        operation: Operation::Ftrc,
                        length_suffix: None,
                        cond: None,
                        operands: vec![],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH4 | SuperhVersion::SH4A | SuperhVersion::SH2A,
                    },
                    // 1111mmmm00111101 "ftrc FRm,FPUL"
                    _ => Instruction {
                        operation: Operation::Ftrc,
                        length_suffix: None,
                        cond: None,
                        operands: vec![
                            Operand::Reg(SuperhRegister::new_fr(mmmm.into())),
                            Operand::Reg(SuperhRegister::Fpul),
                        ],
                        delay_slot: false,
                        privileged: false,
                        version: SuperhVersion::SH2E
                            | SuperhVersion::SH3E
                            | SuperhVersion::SH4
                            | SuperhVersion::SH4A
                            | SuperhVersion::SH2A,
                    },
                }
            }
            // 1111nnmm11101101 "fipr FVm,FVn"
            (0b1111, _nnmm, 0b1110, 0b1101) => Instruction {
                operation: Operation::Fipr,
                length_suffix: None,
                cond: None,
                operands: vec![
                    // Operand::Reg(ShRegister::new_fv(nnmm & 0b0011)),
                    // Operand::Reg(ShRegister::new_fv(nnmm >> 2)),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 1111nn0111111101 "ftrv XMTRX,FVn"
            (0b1111, nn, 0b1111, 0b1101) if (nn & 0b0011) == 0b0001 => Instruction {
                operation: Operation::Ftrv,
                length_suffix: None,
                cond: None,
                operands: vec![
                    // (nn >> 2)
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 1111mmm010111101 "fcnvds DRm,FPUL"
            (0b1111, mmmm, 0b1011, 0b1101) if (mmmm & 0b0001) == 0b0000 => Instruction {
                operation: Operation::Fcnvds,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_dr((mmmm >> 1).into())),
                    Operand::Reg(SuperhRegister::Fpul),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH4 | SuperhVersion::SH4A | SuperhVersion::SH2A,
            },
            // 1111nnn010101101 "fcnvsd FPUL,DRn"
            (0b1111, nnnn, 0b1010, 0b1101) if (nnnn & 0b0001) == 0b0000 => Instruction {
                operation: Operation::Fcnvsd,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::Fpul),
                    Operand::Reg(SuperhRegister::new_dr((nnnn >> 1).into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH4 | SuperhVersion::SH4A | SuperhVersion::SH2A,
            },
            // 0100mmmm01101010 "lds Rm,FPSCR"
            (0b0100, mmmm, 0b0110, 0b1010) => Instruction {
                operation: Operation::Lds,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::Fpscr),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH2E
                    | SuperhVersion::SH3E
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000nnnn01101010 "sts FPSCR,Rn"
            (0b0000, nnnn, 0b0110, 0b1010) => Instruction {
                operation: Operation::Sts,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::Fpscr),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH2E
                    | SuperhVersion::SH3E
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100mmmm01100110 "lds.l @Rm+,FPSCR"
            (0b0100, mmmm, 0b0110, 0b0110) => Instruction {
                operation: Operation::Lds,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::DerefReg(SuperhRegister::new_gpr(mmmm.into()), OperandFlag::PostInc),
                    Operand::Reg(SuperhRegister::Fpscr),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH2E
                    | SuperhVersion::SH3E
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnn01100010 "sts.l FPSCR,@-Rn"
            (0b0100, nnnn, 0b0110, 0b0010) => Instruction {
                operation: Operation::Sts,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::Fpscr),
                    Operand::DerefReg(SuperhRegister::new_gpr(nnnn.into()), OperandFlag::PreDec),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH2E
                    | SuperhVersion::SH3E
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100mmmm01011010 "lds Rm,FPUL"
            (0b0100, mmmm, 0b0101, 0b1010) => Instruction {
                operation: Operation::Lds,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::new_gpr(mmmm.into())),
                    Operand::Reg(SuperhRegister::Fpul),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH2E
                    | SuperhVersion::SH3E
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0000nnnn01011010 "sts FPUL,Rn"
            (0b0000, nnnn, 0b0101, 0b1010) => Instruction {
                operation: Operation::Sts,
                length_suffix: None,
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::Fpul),
                    Operand::Reg(SuperhRegister::new_gpr(nnnn.into())),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH2E
                    | SuperhVersion::SH3E
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100mmmm01010110 "lds.l @Rm+,FPUL"
            (0b0100, mmmm, 0b0101, 0b0110) => Instruction {
                operation: Operation::Lds,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::DerefReg(SuperhRegister::new_gpr(mmmm.into()), OperandFlag::PostInc),
                    Operand::Reg(SuperhRegister::Fpul),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH2E
                    | SuperhVersion::SH3E
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 0100nnnn01010010 "sts.l FPUL,@-Rn"
            (0b0100, nnnn, 0b0101, 0b0010) => Instruction {
                operation: Operation::Sts,
                length_suffix: Some(LengthSuffix::Long),
                cond: None,
                operands: vec![
                    Operand::Reg(SuperhRegister::Fpul),
                    Operand::DerefReg(SuperhRegister::new_gpr(nnnn.into()), OperandFlag::PreDec),
                ],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH2E
                    | SuperhVersion::SH3E
                    | SuperhVersion::SH4
                    | SuperhVersion::SH4A
                    | SuperhVersion::SH2A,
            },
            // 1111101111111101 "frchg"
            (0b1111, 0b1011, 0b1111, 0b1101) => Instruction {
                operation: Operation::Frchg,
                length_suffix: None,
                cond: None,
                operands: vec![],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH4 | SuperhVersion::SH4A,
            },
            // 1111001111111101 "fschg"
            (0b1111, 0b0011, 0b1111, 0b1101) => Instruction {
                operation: Operation::Fschg,
                length_suffix: None,
                cond: None,
                operands: vec![],
                delay_slot: false,
                privileged: false,
                version: SuperhVersion::SH4 | SuperhVersion::SH4A | SuperhVersion::SH2A,
            },
            _ => return None,
        })
    }

    pub fn disassemble(&self) -> Vec<TextToken> {
        let mut tokens = vec![];

        let mut mnemonic = self.operation.to_string().to_lowercase();
        if let Some(suffix) = &self.length_suffix {
            mnemonic.push_str(match suffix {
                LengthSuffix::Byte => ".b",
                LengthSuffix::Word => ".w",
                LengthSuffix::Long => ".l",
                LengthSuffix::Single => ".s",
                LengthSuffix::Double => ".d",
            });
        }

        if self.delay_slot && (self.operation == Operation::Bt || self.operation == Operation::Bf) {
            mnemonic.push_str("/s");
        }

        if let Some(cond) = &self.cond {
            mnemonic.push_str(match cond {
                Conditional::Eq => "/eq",
                Conditional::Ge => "/ge",
                Conditional::Gt => "/gt",
                Conditional::Hi => "/hi",
                Conditional::Hs => "/hs",
                Conditional::Pl => "/pl",
                Conditional::Pz => "/pz",
                Conditional::Str => "/str",
            });
        }

        let pad_len = 10usize.saturating_sub(mnemonic.len());
        tokens.push(TextToken::new(&mnemonic, TextContent::Instruction));

        if !self.operands.is_empty() {
            tokens.push(TextToken::new(
                &format!("{:1$}", " ", pad_len),
                TextContent::Text,
            ));
        }

        for (i, operand) in self.operands.iter().enumerate() {
            match operand {
                Operand::Reg(reg) => {
                    tokens.push(TextToken::new(
                        &format!("{}", reg.name()),
                        TextContent::Register,
                    ));
                }
                Operand::Imm(imm) => {
                    tokens.push(TextToken::new("#", TextContent::Text));

                    // TODO: There still seems to be an issue getting the immediate
                    // to appear as negative.
                    let i = *imm;
                    let i = if i < -9 {
                        format!("-{:#x}", -i)
                    } else if i < 0 {
                        format!("-{}", -i)
                    } else if i < 10 {
                        format!("{i}")
                    } else {
                        format!("{i:#x}")
                    };

                    tokens.push(TextToken::new(&i, TextContent::Integer(*imm as i64 as u64)));
                }
                Operand::Address(addr) => {
                    if self.operation == Operation::Mov {
                        tokens.push(TextToken::new(
                            &format!("{addr:#x}"),
                            TextContent::PossibleAddress(*addr),
                        ));
                    } else {
                        tokens.push(TextToken::new(
                            &format!("{addr:#x}"),
                            TextContent::CodeRelativeAddress(*addr),
                        ));
                    }
                }
                Operand::DerefReg(reg, flag) => {
                    tokens.push(TextToken::new("@", TextContent::BeginMemoryOperand));

                    if *flag == OperandFlag::PreDec {
                        tokens.push(TextToken::new("-", TextContent::Text));
                    }

                    tokens.push(TextToken::new(
                        &format!("{}", reg.name()),
                        TextContent::Register,
                    ));

                    if *flag == OperandFlag::PostInc {
                        tokens.push(TextToken::new("+", TextContent::Text));
                    }
                }
                Operand::DerefRegReg(rn, rm) => {
                    tokens.extend([
                        TextToken::new("@(", TextContent::BeginMemoryOperand),
                        TextToken::new(&format!("{}", rn.name()), TextContent::Register),
                        TextToken::new(",", TextContent::OperandSeparator),
                        TextToken::new(&format!("{}", rm.name()), TextContent::Register),
                        TextToken::new(")", TextContent::EndMemoryOperand),
                    ]);
                }
                Operand::DerefRegImm(reg, imm) => {
                    tokens.extend([
                        TextToken::new("@(", TextContent::BeginMemoryOperand),
                        TextToken::new(&format!("{imm}"), TextContent::Integer(*imm as u64)),
                        TextToken::new(",", TextContent::OperandSeparator),
                        TextToken::new(&format!("{}", reg.name()), TextContent::Register),
                        TextToken::new(")", TextContent::EndMemoryOperand),
                    ]);
                }
            }

            if i != self.operands.len() - 1 {
                tokens.push(TextToken::new(", ", TextContent::OperandSeparator));
            }
        }

        tokens
    }
}

/// Handles calculation of a 8-bit sign extension.
#[inline]
fn disp8(disp: u8, address: u32) -> u32 {
    let mut disp = disp as u32;
    if (disp & 0x80) != 0 {
        disp |= 0xffff_ff00;
    }
    address.wrapping_add((disp << 1).wrapping_add(4))
}

/// Handles calculation of a 12-bit sign extension for BRA and BSR instructions.
#[inline]
fn disp12(disp: u16, address: u32) -> u32 {
    let mut disp = (disp & 0x0fff) as u32;
    if (disp & 0x800) != 0 {
        disp |= 0xffff_f000;
    }
    address.wrapping_add((disp << 1).wrapping_add(4))
}
