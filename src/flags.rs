use std::{borrow::Cow, collections::HashMap};

use binaryninja::architecture::{FlagCondition, FlagRole};
use strum_macros::FromRepr;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, FromRepr)]
pub enum SuperhFlag {
    // T: Condition code flag
    T,
    // S: Multiply-accumulate saturation flag
    S,
    // M: Divide-step M flag
    M,
    // Q: Divide-step Q flag
    Q,
}

impl binaryninja::architecture::Flag for SuperhFlag {
    type FlagClass = Self;

    fn name(&self) -> Cow<str> {
        match self {
            SuperhFlag::T => Cow::from("T"),
            SuperhFlag::S => Cow::from("S"),
            SuperhFlag::M => Cow::from("M"),
            SuperhFlag::Q => Cow::from("Q"),
        }
    }

    fn role(&self, _class: Option<Self::FlagClass>) -> FlagRole {
        match self {
            SuperhFlag::T => FlagRole::ZeroFlagRole,
            SuperhFlag::S => FlagRole::SpecialFlagRole,
            SuperhFlag::M => FlagRole::SpecialFlagRole,
            SuperhFlag::Q => FlagRole::SpecialFlagRole,
        }
    }

    fn id(&self) -> u32 {
        *self as u32
    }
}

impl binaryninja::architecture::FlagWrite for SuperhFlag {
    type FlagType = Self;
    type FlagClass = Self;

    fn name(&self) -> Cow<str> {
        Cow::from("")
    }

    fn class(&self) -> Option<Self::FlagClass> {
        None
    }

    fn id(&self) -> u32 {
        *self as u32
    }

    fn flags_written(&self) -> Vec<Self::FlagType> {
        vec![]
    }
}

impl binaryninja::architecture::FlagClass for SuperhFlag {
    fn name(&self) -> Cow<str> {
        Cow::from("")
    }

    fn id(&self) -> u32 {
        *self as u32
    }
}

impl binaryninja::architecture::FlagGroup for SuperhFlag {
    type FlagType = Self;
    type FlagClass = Self;

    fn name(&self) -> Cow<str> {
        Cow::from("")
    }

    fn id(&self) -> u32 {
        *self as u32
    }

    fn flags_required(&self) -> Vec<Self::FlagType> {
        vec![]
    }

    fn flag_conditions(&self) -> HashMap<Self::FlagClass, FlagCondition> {
        HashMap::new()
    }
}
