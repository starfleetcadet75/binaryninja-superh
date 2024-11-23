use binaryninja::{
    architecture::{
        Architecture, BranchInfo, CoreArchitecture, CustomArchitectureHandle, FlagCondition,
        InstructionInfo, UnusedIntrinsic, UnusedRegisterStack, UnusedRegisterStackInfo,
    },
    disassembly::InstructionTextToken,
    llil, Endianness,
};
use log::warn;
use strum::IntoEnumIterator;

use crate::flags::{FlagClass, FlagGroup, FlagWrite};
use crate::{
    flags::SuperhFlag,
    instructions::{Instruction, Operation},
    SuperhVersion,
};
use crate::{instructions::Operand, registers::SuperhRegister};

pub struct SuperhArch {
    pub(crate) handle: CoreArchitecture,
    pub(crate) custom_handle: CustomArchitectureHandle<SuperhArch>,
    pub(crate) endian: Endianness,
    pub(crate) isa_version: SuperhVersion,
}

impl Architecture for SuperhArch {
    type Handle = CustomArchitectureHandle<Self>;

    type RegisterInfo = SuperhRegister;
    type Register = SuperhRegister;
    type RegisterStackInfo = UnusedRegisterStackInfo<Self::Register>;
    type RegisterStack = UnusedRegisterStack<Self::Register>;

    type Flag = SuperhFlag;
    type FlagWrite = FlagWrite;
    type FlagClass = FlagClass;
    type FlagGroup = FlagGroup;

    type Intrinsic = UnusedIntrinsic;

    fn endianness(&self) -> Endianness {
        self.endian
    }

    fn address_size(&self) -> usize {
        // 32-bit addresses.
        4
    }

    fn default_integer_size(&self) -> usize {
        // 32-bit addresses.
        4
    }

    fn instruction_alignment(&self) -> usize {
        // Align instructions to 16-bits.
        2
    }

    fn max_instr_len(&self) -> usize {
        // SuperH uses 16-bit fixed-length instructions which would be 2 bytes, however we
        // use 4 here due to the delay slots. In order to lift delay slot instructions in
        // the correct order, we must lift multiple instructions at once which requires
        // the data buffer passed to `instruction_lift()` to be large enough.
        // There are extensions for 32-bit instructions which would require this to expand.
        4
    }

    fn opcode_display_len(&self) -> usize {
        self.max_instr_len()
    }

    fn associated_arch_by_addr(&self, _address: &mut u64) -> CoreArchitecture {
        self.handle
    }

    fn instruction_info(&self, data: &[u8], address: u64) -> Option<InstructionInfo> {
        if data.len() < 2 {
            warn!(
                "Data passed to instruction_info is not large enough: address = {:08x}, len = {}",
                address,
                data.len()
            );
            return None;
        }

        // Safe unwrap is ensured by the above length check.
        let data = data[..2].try_into().unwrap();

        let instr_word = match self.endianness() {
            Endianness::LittleEndian => u16::from_le_bytes(data),
            Endianness::BigEndian => u16::from_be_bytes(data),
        };

        Instruction::decompose(instr_word, address, self.isa_version).map(|instr| {
            let mut result = InstructionInfo::new(2, instr.delay_slot as u8);

            match instr.operation {
                Operation::Bra => match instr.operands[0] {
                    Operand::Address(call_address) => {
                        result.add_branch(BranchInfo::Unconditional(call_address), None);
                    }
                    _ => unreachable!(),
                },
                Operation::Braf | Operation::Jmp => {
                    result.add_branch(BranchInfo::Indirect, None);
                }
                Operation::Bsr => match instr.operands[0] {
                    Operand::Address(call_address) => {
                        result.add_branch(BranchInfo::Call(call_address), None);
                    }
                    _ => unreachable!(),
                },
                // Operation::Jsr | Operation::Bsrf => {
                //     Jsr and Bsrf should not terminate a basic block.
                // }
                Operation::Bf | Operation::Bt => match instr.operands[0] {
                    Operand::Address(branch_address) => {
                        result.add_branch(BranchInfo::False(address.wrapping_add(2)), None);
                        result.add_branch(BranchInfo::True(branch_address), None);
                    }
                    _ => unreachable!(),
                },
                Operation::Rts | Operation::Rtv | Operation::Rte => {
                    result.add_branch(BranchInfo::FunctionReturn, None)
                }
                Operation::Trapa => result.add_branch(BranchInfo::SystemCall, None),
                _ => {} // Do nothing for non-control flow instructions.
            }

            result
        })
    }

    fn instruction_text(
        &self,
        data: &[u8],
        address: u64,
    ) -> Option<(usize, Vec<InstructionTextToken>)> {
        if data.len() < 2 {
            warn!(
                "Data passed to instruction_text is not large enough: address = {:08x}, len = {}",
                address,
                data.len()
            );
            return None;
        }

        // Safe unwrap is ensured by the above length check.
        let data = data[..2].try_into().unwrap();

        let instr_word = match self.endianness() {
            Endianness::LittleEndian => u16::from_le_bytes(data),
            Endianness::BigEndian => u16::from_be_bytes(data),
        };

        Instruction::decompose(instr_word, address, self.isa_version)
            .map(|instr| (2, instr.disassemble()))
    }

    fn instruction_llil(
        &self,
        data: &[u8],
        address: u64,
        il: &mut llil::Lifter<Self>,
    ) -> Option<(usize, bool)> {
        if data.len() < 2 {
            warn!(
                "Data passed to instruction_llil is not large enough: address = {:08x}, len = {}",
                address,
                data.len()
            );
            return None;
        }

        // Track the number of bytes consumed by lifting.
        let mut lifted_count = 2;

        // Safe unwrap is ensured by the above length check.
        let first_word = data[..2].try_into().unwrap();

        let instr_word = match self.endianness() {
            Endianness::LittleEndian => u16::from_le_bytes(first_word),
            Endianness::BigEndian => u16::from_be_bytes(first_word),
        };

        Instruction::decompose(instr_word, address, self.isa_version).map(|instr| {
            // To handle instructions that are executed from the delay slot,
            // lift the following instruction first so that it appears before the
            // control flow instruction in the IL.
            if instr.delay_slot {
                if data.len() < 4 {
                    warn!(
                        "Delay slot instruction found at {:#x} but data not large enough to lift: {:?}",
                        address, data
                    );
                }
                else {
                    // Safe unwrap is ensured by the first length check.
                    let second_word = data[2..4].try_into().unwrap();

                    let next_instr_word = match self.endianness() {
                        Endianness::LittleEndian => u16::from_le_bytes(second_word),
                        Endianness::BigEndian => u16::from_be_bytes(second_word),
                    };

                    lifted_count += Instruction::decompose(
                        next_instr_word,
                        address.wrapping_add(2),
                        self.isa_version,
                    )
                        .map(|next_instr| {
                            next_instr.lift(il, address.wrapping_add(2), self.default_integer_size());
                            2
                        })
                        .unwrap_or(0);
                }
            }

            instr.lift(il, address, self.default_integer_size());
            (lifted_count, true)
        })
    }

    fn flags_required_for_flag_condition(
        &self,
        _condition: FlagCondition,
        _class: Option<Self::FlagClass>,
    ) -> Vec<Self::Flag> {
        vec![]
    }

    fn flag_group_llil<'a>(
        &self,
        _group: Self::FlagGroup,
        _il: &'a mut llil::Lifter<Self>,
    ) -> Option<llil::LiftedExpr<'a, Self>> {
        None
    }

    fn registers_all(&self) -> Vec<Self::Register> {
        SuperhRegister::iter().collect()
    }

    fn registers_full_width(&self) -> Vec<Self::Register> {
        vec![
            SuperhRegister::R0,
            SuperhRegister::R1,
            SuperhRegister::R2,
            SuperhRegister::R3,
            SuperhRegister::R4,
            SuperhRegister::R5,
            SuperhRegister::R6,
            SuperhRegister::R7,
            SuperhRegister::R8,
            SuperhRegister::R9,
            SuperhRegister::R10,
            SuperhRegister::R11,
            SuperhRegister::R12,
            SuperhRegister::R13,
            SuperhRegister::R14,
            SuperhRegister::R15,
            SuperhRegister::Fr0,
            SuperhRegister::Fr1,
            SuperhRegister::Fr2,
            SuperhRegister::Fr3,
            SuperhRegister::Fr4,
            SuperhRegister::Fr5,
            SuperhRegister::Fr6,
            SuperhRegister::Fr7,
            SuperhRegister::Fr8,
            SuperhRegister::Fr9,
            SuperhRegister::Fr10,
            SuperhRegister::Fr11,
            SuperhRegister::Fr12,
            SuperhRegister::Fr13,
            SuperhRegister::Fr14,
            SuperhRegister::Fr15,
        ]
    }

    fn registers_global(&self) -> Vec<Self::Register> {
        vec![SuperhRegister::R12]
    }

    fn registers_system(&self) -> Vec<Self::Register> {
        vec![
            SuperhRegister::Mach,
            SuperhRegister::Macl,
            SuperhRegister::PR,
            SuperhRegister::PC,
            SuperhRegister::Fpscr,
            SuperhRegister::Fpul,
        ]
    }

    fn flags(&self) -> Vec<Self::Flag> {
        vec![SuperhFlag::T, SuperhFlag::S, SuperhFlag::M, SuperhFlag::Q]
    }

    fn flag_write_types(&self) -> Vec<Self::FlagWrite> {
        vec![]
    }

    fn flag_classes(&self) -> Vec<Self::FlagClass> {
        vec![]
    }

    fn flag_groups(&self) -> Vec<Self::FlagGroup> {
        vec![]
    }

    fn stack_pointer_reg(&self) -> Option<Self::Register> {
        Some(SuperhRegister::R15)
    }

    fn link_reg(&self) -> Option<Self::Register> {
        Some(SuperhRegister::PR)
    }

    fn register_from_id(&self, id: u32) -> Option<Self::Register> {
        SuperhRegister::from_repr(id as usize)
    }

    fn flag_from_id(&self, id: u32) -> Option<Self::Flag> {
        SuperhFlag::from_repr(id as usize)
    }

    fn flag_write_from_id(&self, _id: u32) -> Option<Self::FlagWrite> {
        None
    }

    fn flag_class_from_id(&self, _id: u32) -> Option<Self::FlagClass> {
        None
    }

    fn flag_group_from_id(&self, _id: u32) -> Option<Self::FlagGroup> {
        None
    }

    fn intrinsics(&self) -> Vec<Self::Intrinsic> {
        Vec::new()
    }

    fn intrinsic_from_id(&self, _id: u32) -> Option<Self::Intrinsic> {
        None
    }

    fn can_assemble(&self) -> bool {
        // LLVM does not have a SuperH target we can use.
        false
    }

    fn is_never_branch_patch_available(&self, _data: &[u8], _address: u64) -> bool {
        // let instr = match Instruction::decompose(instr_word, address, self.isa_version) {
        //     Some(instr) => instr,
        //     None => return false,
        // };

        // match op {
        //     Op::Beq(_) | Op::Bne(_) | Op::Blt(_) | Op::Bge(_) | Op::BltU(_) | Op::BgeU(_) => true,
        //     _ => false,
        // }
        false
    }

    fn is_always_branch_patch_available(&self, data: &[u8], addr: u64) -> bool {
        self.is_never_branch_patch_available(data, addr)
    }

    fn is_invert_branch_patch_available(&self, data: &[u8], addr: u64) -> bool {
        self.is_never_branch_patch_available(data, addr)
    }

    /// is_skip_and_return_zero_patch_available determines if the instruction data at addr is
    /// a call-like instruction that can be made into an instruction returns zero.
    fn is_skip_and_return_zero_patch_available(&self, _data: &[u8], _addr: u64) -> bool {
        // let op = match D::decode(addr, data) {
        //     Ok(Instr::Rv16(op)) => op,
        //     Ok(Instr::Rv32(op)) => op,
        //     _ => return false,
        // };

        // match op {
        //     Op::Jal(ref j) if j.rd().id() != 0 => true,
        //     Op::Jalr(ref j) if j.rd().id() != 0 => true,
        //     _ => false,
        // }

        false
    }

    fn is_skip_and_return_value_patch_available(&self, data: &[u8], addr: u64) -> bool {
        self.is_skip_and_return_zero_patch_available(data, addr)
    }

    fn convert_to_nop(&self, mut data: &mut [u8], _addr: u64) -> bool {
        if data.len() & 1 != 0 {
            // Cannot convert to nop if not aligned on 16 bit boundary.
            return false;
        }

        let nop_instr = match self.endianness() {
            Endianness::LittleEndian => &[0x09, 0x00],
            Endianness::BigEndian => &[0x00, 0x09],
        };

        while !data.is_empty() {
            data[0..2].copy_from_slice(nop_instr);
            data = data[2..].as_mut();
        }

        true
    }

    fn always_branch(&self, _data: &mut [u8], _addr: u64) -> bool {
        false
    }

    fn invert_branch(&self, _data: &mut [u8], _addr: u64) -> bool {
        false
    }

    fn skip_and_return_value(&self, _data: &mut [u8], _addr: u64, _value: u64) -> bool {
        false
    }

    fn handle(&self) -> Self::Handle {
        self.custom_handle
    }
}

impl AsRef<CoreArchitecture> for SuperhArch {
    fn as_ref(&self) -> &CoreArchitecture {
        &self.handle
    }
}
