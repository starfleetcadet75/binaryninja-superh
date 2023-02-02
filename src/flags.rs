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
    type FlagClass = FlagClass;

    fn name(&self) -> Cow<str> {
        match self {
            Self::T => Cow::from("T"),
            Self::S => Cow::from("S"),
            Self::M => Cow::from("M"),
            Self::Q => Cow::from("Q"),
        }
    }

    fn role(&self, _class: Option<Self::FlagClass>) -> FlagRole {
        match self {
            Self::T => FlagRole::SpecialFlagRole,
            Self::S => FlagRole::SpecialFlagRole,
            Self::M => FlagRole::SpecialFlagRole,
            Self::Q => FlagRole::SpecialFlagRole,
        }
    }

    fn id(&self) -> u32 {
        *self as u32
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum FlagWrite {
    None,
    T,
    S,
    M,
    Q,
}

impl binaryninja::architecture::FlagWrite for FlagWrite {
    type FlagType = SuperhFlag;
    type FlagClass = FlagClass;

    fn name(&self) -> Cow<str> {
        match self {
            Self::None => "".into(),
            Self::T => "t".into(),
            Self::S => "s".into(),
            Self::M => "m".into(),
            Self::Q => "q".into(),
        }
    }

    fn class(&self) -> Option<Self::FlagClass> {
        None
    }

    fn id(&self) -> u32 {
        *self as u32
    }

    fn flags_written(&self) -> Vec<Self::FlagType> {
        match self {
            Self::None => vec![],
            Self::T => vec![SuperhFlag::T],
            Self::S => vec![SuperhFlag::S],
            Self::M => vec![SuperhFlag::M],
            Self::Q => vec![SuperhFlag::Q],
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FlagClass {}

impl binaryninja::architecture::FlagClass for FlagClass {
    fn name(&self) -> Cow<str> {
        unimplemented!()
    }

    fn id(&self) -> u32 {
        unimplemented!()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum FlagGroup {}

impl binaryninja::architecture::FlagGroup for FlagGroup {
    type FlagType = SuperhFlag;
    type FlagClass = FlagClass;

    fn name(&self) -> Cow<str> {
        unimplemented!()
    }

    fn id(&self) -> u32 {
        unimplemented!()
    }

    fn flags_required(&self) -> Vec<Self::FlagType> {
        vec![]
    }

    fn flag_conditions(&self) -> HashMap<Self::FlagClass, FlagCondition> {
        HashMap::new()
    }
}
