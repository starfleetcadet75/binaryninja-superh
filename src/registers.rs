use std::borrow::Cow;

use binaryninja::{
    architecture::{ImplicitRegisterExtend, RegisterInfo},
    llil::{self, Liftable, LiftableWithSize, LiftedNonSSA},
};
use log::warn;
use strum_macros::{Display, EnumCount as EnumCountMacro, EnumIter, FromRepr};

use crate::arch::SuperhArch;

#[derive(Clone, Copy, Debug, Display, Eq, PartialEq, EnumCountMacro, EnumIter, FromRepr)]
pub enum SuperhRegister {
    // General-purpose registers.
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    // Banked registers.
    R0Bank0,
    R1Bank0,
    R2Bank0,
    R3Bank0,
    R4Bank0,
    R5Bank0,
    R6Bank0,
    R7Bank0,
    R0Bank1,
    R1Bank1,
    R2Bank1,
    R3Bank1,
    R4Bank1,
    R5Bank1,
    R6Bank1,
    R7Bank1,
    // Control registers.
    SR,
    Ssr,
    Spc,
    Gbr,
    Vbr,
    Sgr,
    Dbr,
    // System registers.
    Mach,
    Macl,
    PR,
    PC,
    Fpscr,
    Fpul,
    // Floating-point registers (single).
    Fr0,
    Fr1,
    Fr2,
    Fr3,
    Fr4,
    Fr5,
    Fr6,
    Fr7,
    Fr8,
    Fr9,
    Fr10,
    Fr11,
    Fr12,
    Fr13,
    Fr14,
    Fr15,
    // Floating-point registers (double).
    Dr0,
    Dr2,
    Dr4,
    Dr6,
    Dr8,
    Dr10,
    Dr12,
    Dr14,
}

impl SuperhRegister {
    pub fn new_gpr(id: usize) -> SuperhRegister {
        SuperhRegister::from_repr(id).unwrap()
    }

    pub fn new_bank_reg(id: usize) -> SuperhRegister {
        let bank_reg_offset = 16;
        SuperhRegister::from_repr(id + bank_reg_offset).unwrap()
    }

    pub fn new_fr(id: usize) -> SuperhRegister {
        let fp_offset = 45;
        SuperhRegister::from_repr(id + fp_offset).unwrap()
    }

    pub fn new_dr(id: usize) -> SuperhRegister {
        let dr_offset = 61;
        SuperhRegister::from_repr(id + dr_offset).unwrap()
    }
}

impl binaryninja::architecture::Register for SuperhRegister {
    type InfoType = Self;

    fn name(&self) -> Cow<str> {
        Cow::from(self.to_string().to_lowercase())
    }

    fn info(&self) -> Self::InfoType {
        *self
    }

    fn id(&self) -> u32 {
        *self as u32
    }
}

impl RegisterInfo for SuperhRegister {
    type RegType = Self;

    fn parent(&self) -> Option<Self::RegType> {
        None
    }

    fn size(&self) -> usize {
        match self {
            SuperhRegister::Dr0
            | SuperhRegister::Dr2
            | SuperhRegister::Dr4
            | SuperhRegister::Dr6
            | SuperhRegister::Dr8
            | SuperhRegister::Dr10
            | SuperhRegister::Dr12
            | SuperhRegister::Dr14 => 8,
            _ => 4,
        }
    }

    fn offset(&self) -> usize {
        0
    }

    fn implicit_extend(&self) -> ImplicitRegisterExtend {
        ImplicitRegisterExtend::NoExtend
    }
}

impl<'a> Liftable<'a, SuperhArch> for SuperhRegister {
    type Result = llil::ValueExpr;

    fn lift(
        il: &'a llil::Lifter<SuperhArch>,
        reg: Self,
    ) -> llil::Expression<'a, SuperhArch, llil::Mutable, llil::NonSSA<LiftedNonSSA>, Self::Result>
    {
        il.reg(reg.size(), reg)
    }
}

impl<'a> LiftableWithSize<'a, SuperhArch> for SuperhRegister {
    fn lift_with_size(
        il: &'a llil::Lifter<SuperhArch>,
        reg: Self,
        size: usize,
    ) -> llil::Expression<'a, SuperhArch, llil::Mutable, llil::NonSSA<LiftedNonSSA>, llil::ValueExpr>
    {
        if reg.size() < size {
            warn!(
                "il @ {:x} attempted to lift {} byte register as {} byte expr",
                il.current_address(),
                reg.size(),
                size
            );
        }

        let expr = il.reg(reg.size(), reg);
        if size < reg.size() {
            il.low_part(size, expr).build()
        } else {
            expr
        }
    }
}

impl From<SuperhRegister> for llil::Register<SuperhRegister> {
    fn from(reg: SuperhRegister) -> llil::Register<SuperhRegister> {
        llil::Register::ArchReg(reg)
    }
}
