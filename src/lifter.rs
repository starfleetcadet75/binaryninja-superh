use binaryninja::llil;
use binaryninja::llil::Label;

use crate::flags::{FlagWrite, SuperhFlag};
use crate::instructions::Conditional;
use crate::{
    instructions::{Instruction, Operand, OperandFlag, Operation},
    registers::SuperhRegister,
    SuperhArch, SuperhVersion,
};

impl Instruction {
    pub fn lift(&self, il: &mut llil::Lifter<SuperhArch>, address: u64, max_width: usize) {
        match self.operation {
            Operation::Add => {
                if let Operand::Reg(right) = self.operands[1] {
                    match self.operands[0] {
                        Operand::Reg(left) => {
                            il.set_reg(max_width, right, il.add(max_width, left, right))
                                .append();
                        }
                        Operand::Imm(imm) => {
                            il.set_reg(
                                max_width,
                                right,
                                il.add(max_width, right, il.sx(max_width, imm)),
                            )
                            .append();
                        }
                        _ => unreachable!(),
                    }
                }
            }
            Operation::Addc => {
                if let Operand::Reg(rm) = self.operands[0] {
                    if let Operand::Reg(rn) = self.operands[1] {
                        il.set_reg(
                            max_width,
                            rn,
                            il.adc(max_width, rm, rn, il.flag(SuperhFlag::T))
                                .with_flag_write(FlagWrite::T),
                        )
                        .append();
                    }
                }
            }
            Operation::Addv => {
                if let Operand::Reg(rm) = self.operands[0] {
                    if let Operand::Reg(rn) = self.operands[1] {
                        il.set_reg(
                            max_width,
                            rn,
                            il.add_overflow(max_width, rm, rn)
                                .with_flag_write(FlagWrite::T),
                        )
                        .append();
                    }
                }
            }
            Operation::And => match self.operands[0] {
                Operand::Reg(rm) => {
                    if let Operand::Reg(rn) = self.operands[1] {
                        il.set_reg(max_width, rn, il.and(max_width, rn, rm))
                            .append();
                    }
                }
                Operand::Imm(imm) => match self.operands[1] {
                    Operand::Reg(left) => {
                        let right = il.zx(max_width, imm);
                        il.set_reg(max_width, left, il.and(max_width, left, right))
                            .append();
                    }
                    Operand::DerefRegReg(r1, r2) => {
                        let right = il.load(1, il.add(max_width, r1, r2));
                        let left = il.zx(max_width, imm);
                        let result = il.and(max_width, left, right);
                        il.store(1, il.add(max_width, r1, r2), result).append();
                    }
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            Operation::Band => il.unimplemented().append(),
            Operation::Bandnot => il.unimplemented().append(),
            Operation::Bclr => il.unimplemented().append(),
            Operation::Bf => {
                if let Operand::Address(true_target) = self.operands[0] {
                    // Take the branch if T flag is not set.
                    let false_target = address.wrapping_add(2);

                    let mut new_true_label = Label::new();
                    let mut new_false_label = Label::new();

                    let true_label = il.label_for_address(true_target);
                    let false_label = il.label_for_address(false_target);

                    il.if_expr(
                        il.not(max_width, il.flag(SuperhFlag::T)),
                        true_label.unwrap_or(&new_true_label),
                        false_label.unwrap_or(&new_false_label),
                    )
                    .append();

                    if true_label.is_none() {
                        il.mark_label(&mut new_true_label);
                    }

                    il.goto(true_label.unwrap_or(&new_true_label)).append();

                    if false_label.is_none() {
                        il.mark_label(&mut new_false_label);
                    }
                }
            }
            Operation::Bld => il.unimplemented().append(),
            Operation::Bldnot => il.unimplemented().append(),
            Operation::Bor => il.unimplemented().append(),
            Operation::Bornot => il.unimplemented().append(),
            Operation::Bra => {
                if let Operand::Address(target) = self.operands[0] {
                    match il.label_for_address(target) {
                        Some(label) => il.goto(label),
                        None => il.jump(il.const_ptr(target)),
                    }
                    .append();
                }
            }
            Operation::Braf => {
                if let Operand::Reg(rm) = self.operands[0] {
                    let target = il.add(max_width, rm, address.wrapping_add(4));
                    il.jump(target).append();
                }
            }
            Operation::Bset => il.unimplemented().append(),
            Operation::Bsr => {
                if let Operand::Address(target) = self.operands[0] {
                    il.set_reg(max_width, SuperhRegister::PR, address.wrapping_add(4));

                    match il.label_for_address(target) {
                        Some(label) => il.goto(label),
                        None => il.call(il.const_ptr(target)),
                    }
                    .append();
                }
            }
            Operation::Bsrf => {
                if let Operand::Reg(rm) = self.operands[0] {
                    il.set_reg(max_width, SuperhRegister::PR, address.wrapping_add(4));

                    let target = il.add(max_width, rm, address.wrapping_add(4));
                    il.call(target).append();
                }
            }
            Operation::Bst => il.unimplemented().append(),
            Operation::Bt => {
                if let Operand::Address(true_target) = self.operands[0] {
                    // Take the branch if T flag is set.
                    let false_target = address.wrapping_add(2);

                    let mut new_true_label = Label::new();
                    let mut new_false_label = Label::new();

                    let true_label = il.label_for_address(true_target);
                    let false_label = il.label_for_address(false_target);

                    il.if_expr(
                        il.flag(SuperhFlag::T),
                        true_label.unwrap_or(&new_true_label),
                        false_label.unwrap_or(&new_false_label),
                    )
                    .append();

                    if true_label.is_none() {
                        il.mark_label(&mut new_true_label);
                    }

                    il.goto(true_label.unwrap_or(&new_true_label)).append();

                    if false_label.is_none() {
                        il.mark_label(&mut new_false_label);
                    }
                }
            }
            Operation::Bxor => il.unimplemented().append(),
            Operation::Clips => il.unimplemented().append(),
            Operation::Clipu => il.unimplemented().append(),
            Operation::Clrmac => {
                il.set_reg(max_width, SuperhRegister::Mach, 0).append();
                il.set_reg(max_width, SuperhRegister::Macl, 0).append();
            }
            Operation::Clrs => il.unimplemented().append(),
            Operation::Clrt => il.unimplemented().append(),
            Operation::Cmp => {
                let left = match self.operands[0] {
                    Operand::Reg(reg) => il.reg(max_width, reg),
                    Operand::Imm(imm) => il.sx(max_width, il.const_int(1, imm as u64)).build(),
                    _ => unreachable!(),
                };

                let right = match self.cond.unwrap() {
                    Conditional::Pl | Conditional::Pz => il.const_int(1, 0),
                    _ => match self.operands[1] {
                        Operand::Reg(reg) => il.reg(max_width, reg),
                        _ => unreachable!(),
                    },
                };

                match self.cond.unwrap() {
                    Conditional::Eq => {
                        // This instruction sets the T-bit if the value of R0 is equal to the sign-extended 8-bit
                        // immediate s, otherwise it clears the T-bit.
                        il.set_flag(
                            SuperhFlag::T,
                            il.cmp_e(max_width, left, right)
                                .with_flag_write(FlagWrite::T),
                        )
                        .append();
                    }
                    Conditional::Ge => {
                        // This instruction sets the T-bit if the signed value of Rn is greater than or equal to
                        // the signed value of Rm, otherwise it clears the T-bit.
                        il.set_flag(
                            SuperhFlag::T,
                            il.cmp_sge(max_width, left, right)
                                .with_flag_write(FlagWrite::T),
                        )
                        .append();
                    }
                    Conditional::Gt => {
                        // This instruction sets the T-bit if the signed value of Rn is greater than the signed
                        // value of Rm, otherwise it clears the T-bit.
                        il.set_flag(
                            SuperhFlag::T,
                            il.cmp_sgt(max_width, left, right)
                                .with_flag_write(FlagWrite::T),
                        )
                        .append();
                    }
                    Conditional::Hi => {
                        // This instruction sets the T-bit if the unsigned value of Rn is greater than the
                        // unsigned value of Rm, otherwise it clears the T-bit.
                        il.set_flag(
                            SuperhFlag::T,
                            il.cmp_ugt(max_width, left, right)
                                .with_flag_write(FlagWrite::T),
                        )
                        .append();
                    }
                    Conditional::Hs => {
                        // This instruction sets the T-bit if the unsigned value of Rn is greater than or equal to
                        // the unsigned value of Rm, otherwise it clears the T-bit.
                        il.set_flag(
                            SuperhFlag::T,
                            il.cmp_uge(max_width, left, right)
                                .with_flag_write(FlagWrite::T),
                        )
                        .append();
                    }
                    Conditional::Pl => {
                        // This instruction sets the T-bit if the signed value of Rn is greater than 0, otherwise
                        // it clears the T-bit.
                        il.set_flag(
                            SuperhFlag::T,
                            il.cmp_sgt(max_width, left, right)
                                .with_flag_write(FlagWrite::T),
                        )
                        .append();
                    }
                    Conditional::Pz => {
                        // This instruction sets the T-bit if the signed value of Rn is greater than or equal to 0,
                        // otherwise it clears the T-bit.
                        il.set_flag(
                            SuperhFlag::T,
                            il.cmp_sge(max_width, left, right)
                                .with_flag_write(FlagWrite::T),
                        )
                        .append();
                    }
                    Conditional::Str => {
                        // This instruction sets the T-bit if any byte in Rn has the same value as the
                        // corresponding byte in Rm, otherwise it clears the T-bit.
                        il.unimplemented().append()
                    }
                };
            }
            Operation::Dcf => il.unimplemented().append(),
            Operation::Dct => il.unimplemented().append(),
            Operation::Div0s => il.unimplemented().append(),
            Operation::Div0u => {
                il.set_flag(SuperhFlag::M, il.const_int(1, 0)).append();
                il.set_flag(SuperhFlag::Q, il.const_int(1, 0)).append();
                il.set_flag(SuperhFlag::T, il.const_int(1, 0)).append();
            }
            Operation::Div1 => il.unimplemented().append(),
            Operation::Divs => il.unimplemented().append(),
            Operation::Divu => il.unimplemented().append(),
            Operation::Dmuls => il.unimplemented().append(),
            Operation::Dmulu => il.unimplemented().append(),
            Operation::Dt => {
                if let Operand::Reg(reg) = self.operands[0] {
                    il.sub(max_width, reg, il.const_int(1, 1)).append();
                    il.set_flag(SuperhFlag::T, il.cmp_e(max_width, reg, il.const_int(1, 0)))
                        .append();
                }
            }
            Operation::Exts => il.unimplemented().append(),
            Operation::Extu => il.unimplemented().append(),
            Operation::Fabs => il.unimplemented().append(),
            Operation::Fadd => il.unimplemented().append(),
            Operation::Fcmp => il.unimplemented().append(),
            Operation::Fcnvds => il.unimplemented().append(),
            Operation::Fcnvsd => il.unimplemented().append(),
            Operation::Fdiv => il.unimplemented().append(),
            Operation::Fipr => il.unimplemented().append(),
            Operation::Fldi0 => il.unimplemented().append(),
            Operation::Fldi1 => il.unimplemented().append(),
            Operation::Flds => il.unimplemented().append(),
            Operation::Float => il.unimplemented().append(),
            Operation::Fmac => il.unimplemented().append(),
            Operation::Fmov => il.unimplemented().append(),
            Operation::Fmul => il.unimplemented().append(),
            Operation::Fneg | Operation::Neg => {
                if let Operand::Reg(reg) = self.operands[0] {
                    il.set_reg(max_width, reg, il.neg(max_width, reg)).append();
                }
            }
            Operation::Fpchg => {
                il.set_reg(
                    max_width,
                    SuperhRegister::Fpscr,
                    il.xor(max_width, SuperhRegister::Fpscr, 0x00080000),
                )
                .append();
            }
            Operation::Frchg => {
                il.set_reg(
                    max_width,
                    SuperhRegister::Fpscr,
                    il.xor(max_width, SuperhRegister::Fpscr, 0x00200000),
                )
                .append();
            }
            Operation::Fsca => il.unimplemented().append(),
            Operation::Fschg => {
                il.set_reg(
                    max_width,
                    SuperhRegister::Fpscr,
                    il.xor(max_width, SuperhRegister::Fpscr, 0x00100000),
                )
                .append();
            }
            Operation::Fsqrt => il.unimplemented().append(),
            Operation::Fsrra => il.unimplemented().append(),
            Operation::Fsts => il.unimplemented().append(),
            Operation::Fsub => il.unimplemented().append(),
            Operation::Ftrc => il.unimplemented().append(),
            Operation::Ftrv => il.unimplemented().append(),
            Operation::Icbi => il.unimplemented().append(),
            Operation::Jmp => {
                if let Operand::DerefReg(rm, _) = self.operands[0] {
                    il.jump(rm).append();
                }
            }
            Operation::Jsr => {
                if let Operand::DerefReg(rm, _) = self.operands[0] {
                    il.set_reg(max_width, SuperhRegister::PR, address.wrapping_add(4));
                    il.call(rm).append();
                }
            }
            Operation::Ldbank => il.unimplemented().append(),
            Operation::Ldc | Operation::Lds => {
                if let Operand::Reg(dest) = self.operands[1] {
                    match self.operands[0] {
                        Operand::Reg(src) => {
                            il.set_reg(max_width, dest, src).append();
                        }
                        Operand::DerefReg(src, flag) => {
                            let op_size = self.length_suffix.unwrap().size();
                            il.set_reg(max_width, dest, il.load(op_size, src)).append();

                            if flag == OperandFlag::PostInc {
                                il.set_reg(max_width, src, il.add(max_width, dest, op_size as i8))
                                    .append();
                            }
                        }
                        _ => unreachable!(),
                    }
                }
            }
            Operation::Ldre => il.unimplemented().append(),
            Operation::Ldrs => il.unimplemented().append(),
            Operation::Ldtlb => il.unimplemented().append(),
            Operation::Mac => il.unimplemented().append(),
            Operation::Mov => match self.operands[0] {
                Operand::Reg(src) => match self.operands[1] {
                    Operand::Reg(dest) => {
                        il.set_reg(max_width, dest, src).append();
                    }
                    Operand::DerefReg(dest, flag) => {
                        let op_size = self.length_suffix.unwrap().size();

                        if flag == OperandFlag::PreDec {
                            il.set_reg(max_width, dest, il.sub(max_width, dest, op_size as i8))
                                .append();
                        }

                        il.store(op_size, dest, src).append();

                        if flag == OperandFlag::PostInc {
                            il.set_reg(max_width, dest, il.add(max_width, dest, op_size as i8))
                                .append();
                        }
                    }
                    Operand::DerefRegReg(rn, rm) => {
                        let op_size = self.length_suffix.unwrap().size();
                        let dest = il.add(max_width, rn, rm);
                        il.store(op_size, dest, src).append();
                    }
                    Operand::DerefRegImm(dest, imm) => {
                        let op_size = self.length_suffix.unwrap().size();
                        let dest = il.add(max_width, dest, imm * op_size as u16);
                        il.store(op_size, dest, src).append();
                    }
                    _ => unreachable!(),
                },
                Operand::Imm(imm) => {
                    if let Operand::Reg(dest) = self.operands[1] {
                        il.set_reg(max_width, dest, il.sx(max_width, imm)).append();
                    }
                }
                Operand::Address(ptr) => {
                    let op_size = self.length_suffix.unwrap().size();

                    if let Operand::Reg(dest) = self.operands[1] {
                        il.set_reg(max_width, dest, il.sx(max_width, il.load(op_size, ptr)))
                            .append();
                    }
                }
                Operand::DerefReg(reg, flag) => {
                    let op_size = self.length_suffix.unwrap().size();

                    if flag == OperandFlag::PreDec {
                        il.set_reg(max_width, reg, il.sub(max_width, reg, op_size as i8))
                            .append();
                    }

                    if let Operand::Reg(dest) = self.operands[1] {
                        il.set_reg(max_width, dest, il.sx(max_width, il.load(op_size, reg)))
                            .append();
                    }

                    if flag == OperandFlag::PostInc {
                        il.set_reg(max_width, reg, il.add(max_width, reg, op_size as i8))
                            .append();
                    }
                }
                Operand::DerefRegReg(rn, rm) => {
                    let op_size = self.length_suffix.unwrap().size();
                    let src = il.add(max_width, rn, rm);

                    if let Operand::Reg(dest) = self.operands[1] {
                        il.set_reg(max_width, dest, il.sx(max_width, il.load(op_size, src)))
                            .append();
                    }
                }
                Operand::DerefRegImm(reg, imm) => {
                    let op_size = self.length_suffix.unwrap().size();
                    let src = il.add(max_width, reg, imm * op_size as u16);

                    if let Operand::Reg(dest) = self.operands[1] {
                        il.set_reg(max_width, dest, il.sx(max_width, il.load(op_size, src)))
                            .append();
                    }
                }
            },
            Operation::Mova => {
                if let Operand::Address(src) = self.operands[0] {
                    if let Operand::Reg(dest) = self.operands[1] {
                        il.set_reg(max_width, dest, src).append();
                    }
                }
            }
            Operation::Movca => il.unimplemented().append(),
            Operation::Movco => il.unimplemented().append(),
            Operation::Movi20 => il.unimplemented().append(),
            Operation::Movi20s => il.unimplemented().append(),
            Operation::Movli => il.unimplemented().append(),
            Operation::Movml => il.unimplemented().append(),
            Operation::Movmu => il.unimplemented().append(),
            Operation::Movrt => {
                if let Operand::Reg(reg) = self.operands[0] {
                    il.set_reg(max_width, reg, il.not(0, il.flag(SuperhFlag::T)))
                        .append();
                }
            }
            Operation::Movs => il.unimplemented().append(),
            Operation::Movt => {
                if let Operand::Reg(reg) = self.operands[0] {
                    il.set_reg(max_width, reg, il.flag(SuperhFlag::T)).append();
                }
            }
            Operation::Movu => il.unimplemented().append(),
            Operation::Movua => il.unimplemented().append(),
            Operation::Movx => il.unimplemented().append(),
            Operation::Movy => il.unimplemented().append(),
            Operation::Mul => il.unimplemented().append(),
            Operation::Mulr => il.unimplemented().append(),
            Operation::Muls => il.unimplemented().append(),
            Operation::Mulu => il.unimplemented().append(),
            Operation::Negc => il.unimplemented().append(),
            Operation::Nop | Operation::Nopx | Operation::Nopy => {
                il.nop().append();
            }
            Operation::Not => {
                if let Operand::Reg(src) = self.operands[0] {
                    if let Operand::Reg(dest) = self.operands[1] {
                        il.set_reg(max_width, dest, il.not(max_width, src)).append();
                    }
                }
            }
            Operation::Nott => il
                .set_flag(SuperhFlag::T, il.not(0, il.flag(SuperhFlag::T)))
                .append(),
            Operation::Ocbi => il.unimplemented().append(),
            Operation::Ocbp => il.unimplemented().append(),
            Operation::Ocbwb => il.unimplemented().append(),
            Operation::Or => {
                let left = match self.operands[0] {
                    Operand::Reg(reg) => il.reg(max_width, reg),
                    Operand::Imm(imm) => il.zx(max_width, il.const_int(1, imm as u64)).build(),
                    _ => unreachable!(),
                };

                let right = match self.operands[1] {
                    Operand::Reg(reg) => il.reg(max_width, reg),
                    Operand::DerefRegReg(r0, gbr) => il.load(1, il.add(max_width, r0, gbr)).build(),
                    _ => unreachable!(),
                };

                let value = il.or(max_width, left, right);
                match self.operands[1] {
                    Operand::Reg(reg) => il.set_reg(max_width, reg, value).append(),
                    Operand::DerefRegReg(r0, gbr) => {
                        il.store(1, il.add(max_width, r0, gbr), value).append()
                    }
                    _ => unreachable!(),
                };
            }
            Operation::Pabs => il.unimplemented().append(),
            Operation::Padd => il.unimplemented().append(),
            Operation::Paddc => il.unimplemented().append(),
            Operation::Pand => il.unimplemented().append(),
            Operation::Pclr => il.unimplemented().append(),
            Operation::Pcmp => il.unimplemented().append(),
            Operation::Pcopy => il.unimplemented().append(),
            Operation::Pdec => il.unimplemented().append(),
            Operation::Pdmsb => il.unimplemented().append(),
            Operation::Pinc => il.unimplemented().append(),
            Operation::Plds => il.unimplemented().append(),
            Operation::Pmuls => il.unimplemented().append(),
            Operation::Pneg => il.unimplemented().append(),
            Operation::Por => il.unimplemented().append(),
            Operation::Pref => il.unimplemented().append(),
            Operation::Prefi => il.unimplemented().append(),
            Operation::Prnd => il.unimplemented().append(),
            Operation::Psha => il.unimplemented().append(),
            Operation::Pshl => il.unimplemented().append(),
            Operation::Psts => il.unimplemented().append(),
            Operation::Psub => il.unimplemented().append(),
            Operation::Psubc => il.unimplemented().append(),
            Operation::Pxor => il.unimplemented().append(),
            Operation::Resbank => il.unimplemented().append(),
            Operation::Rotcl => il.unimplemented().append(),
            Operation::Rotcr => il.unimplemented().append(),
            Operation::Rotl => il.unimplemented().append(),
            Operation::Rotr => il.unimplemented().append(),
            Operation::Rte => {
                if self
                    .version
                    .contains(SuperhVersion::SH1 | SuperhVersion::SH2 | SuperhVersion::SH2A)
                {
                    il.set_reg(max_width, SuperhRegister::PC, il.pop(max_width))
                        .append();
                    il.set_reg(max_width, SuperhRegister::SR, il.pop(max_width))
                        .append();
                } else {
                    il.set_reg(max_width, SuperhRegister::SR, SuperhRegister::Ssr)
                        .append();
                    il.ret(SuperhRegister::Spc).append();
                }
            }
            Operation::Rts => il.ret(SuperhRegister::PR).append(),
            Operation::Rtv => {
                if let Operand::Reg(rm) = self.operands[0] {
                    il.set_reg(max_width, SuperhRegister::R0, rm).append();
                    il.ret(SuperhRegister::PR).append();
                }
            }
            Operation::Setrc => il.unimplemented().append(),
            Operation::Sets => il.set_flag(SuperhFlag::S, il.const_int(1, 1)).append(),
            Operation::Sett => il.set_flag(SuperhFlag::T, il.const_int(1, 1)).append(),
            Operation::Shad => il.unimplemented().append(),
            Operation::Shal => il.unimplemented().append(),
            Operation::Shar => il.unimplemented().append(),
            Operation::Shld => il.unimplemented().append(),
            Operation::Shll => il.unimplemented().append(),
            Operation::Shll16 => il.unimplemented().append(),
            Operation::Shll2 => il.unimplemented().append(),
            Operation::Shll8 => il.unimplemented().append(),
            Operation::Shlr => il.unimplemented().append(),
            Operation::Shlr16 => il.unimplemented().append(),
            Operation::Shlr2 => il.unimplemented().append(),
            Operation::Shlr8 => il.unimplemented().append(),
            Operation::Sleep => il.no_ret().append(),
            Operation::Stbank => il.unimplemented().append(),
            Operation::Stc | Operation::Sts => {
                if let Operand::Reg(src) = self.operands[0] {
                    match self.operands[1] {
                        Operand::Reg(dest) => {
                            il.set_reg(max_width, dest, src).append();
                        }
                        Operand::DerefReg(dest, flag) => {
                            let op_size = self.length_suffix.unwrap().size();

                            if flag == OperandFlag::PreDec {
                                il.set_reg(max_width, dest, il.sub(max_width, dest, op_size as i8))
                                    .append();
                            }

                            il.store(op_size, dest, src).append();
                        }
                        _ => unreachable!(),
                    }
                }
            }
            Operation::Sub => il.unimplemented().append(),
            Operation::Subc => il.unimplemented().append(),
            Operation::Subv => il.unimplemented().append(),
            Operation::Swap => il.unimplemented().append(),
            Operation::Synco => il.unimplemented().append(),
            Operation::Tas => {
                if let Operand::Reg(reg) = self.operands[0] {
                    il.set_flag(
                        SuperhFlag::T,
                        il.cmp_e(max_width, il.zx(1, il.load(1, reg)), il.const_int(1, 0)),
                    )
                    .append();
                    il.store(
                        1,
                        reg,
                        il.or(max_width, il.zx(1, il.load(1, reg)), il.const_int(1, 0x80)),
                    )
                    .append();
                }
            }
            Operation::Trapa => il.syscall().append(),
            Operation::Tst => {
                let left = match self.operands[0] {
                    Operand::Reg(reg) => il.reg(max_width, reg),
                    Operand::Imm(imm) => il.zx(max_width, il.const_int(1, imm as u64)).build(),
                    _ => unreachable!(),
                };

                let right = match self.operands[1] {
                    Operand::Reg(reg) => il.reg(max_width, reg),
                    Operand::DerefRegReg(r0, gbr) => il.load(1, il.add(max_width, r0, gbr)).build(),
                    _ => unreachable!(),
                };

                il.set_flag(
                    SuperhFlag::T,
                    il.not(max_width, il.and(max_width, left, right)),
                )
                .append();
            }
            Operation::Xor => {
                let left = match self.operands[0] {
                    Operand::Reg(reg) => il.reg(max_width, reg),
                    Operand::Imm(imm) => il.zx(max_width, il.const_int(1, imm as u64)).build(),
                    _ => unreachable!(),
                };

                let right = match self.operands[1] {
                    Operand::Reg(reg) => il.reg(max_width, reg),
                    Operand::DerefRegReg(r0, gbr) => il.load(1, il.add(max_width, r0, gbr)).build(),
                    _ => unreachable!(),
                };

                let value = il.xor(max_width, left, right);
                match self.operands[1] {
                    Operand::Reg(reg) => il.set_reg(max_width, reg, value).append(),
                    Operand::DerefRegReg(r0, gbr) => {
                        il.store(1, il.add(max_width, r0, gbr), value).append()
                    }
                    _ => unreachable!(),
                };
            }
            Operation::Xtrct => il.unimplemented().append(),
        }
    }
}
