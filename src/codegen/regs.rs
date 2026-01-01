use anyhow::{Result, bail};
use std::fmt;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Reg {
    X86 { reg: X86Reg, section: RegSection },
    X64 { reg: X64Reg, section: RegSection },
    Xmm { reg: XmmReg, section: RegSection },
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum RegSection {
    LowByte,
    HighByte,
    Word,
    Dword,
    Qword,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum X86Reg {
    // Generic registers
    Ax,
    Bx,
    Cx,
    Dx,
    // Base and stack pointer
    Bp,
    Sp,
    // Source and destination index
    Si,
    Di,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum X64Reg {
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum XmmReg {
    XMM0,
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7,
    XMM8,
    XMM9,
    XMM10,
    XMM11,
    XMM12,
    XMM13,
    XMM14,
    XMM15,
}

pub(super) const SYSTEM_V_GP_REGS: [Reg; 6] = [
    Reg::X86 {
        reg: X86Reg::Di,
        section: RegSection::Dword,
    },
    Reg::X86 {
        reg: X86Reg::Si,
        section: RegSection::Dword,
    },
    Reg::X86 {
        reg: X86Reg::Dx,
        section: RegSection::Dword,
    },
    Reg::X86 {
        reg: X86Reg::Cx,
        section: RegSection::Dword,
    },
    Reg::X64 {
        reg: X64Reg::R8,
        section: RegSection::Dword,
    },
    Reg::X64 {
        reg: X64Reg::R9,
        section: RegSection::Dword,
    },
];

pub(super) const SYSTEM_V_FP_REGS: [Reg; 8] = [
    Reg::Xmm {
        reg: XmmReg::XMM0,
        section: RegSection::Dword,
    },
    Reg::Xmm {
        reg: XmmReg::XMM1,
        section: RegSection::Dword,
    },
    Reg::Xmm {
        reg: XmmReg::XMM2,
        section: RegSection::Dword,
    },
    Reg::Xmm {
        reg: XmmReg::XMM3,
        section: RegSection::Dword,
    },
    Reg::Xmm {
        reg: XmmReg::XMM4,
        section: RegSection::Dword,
    },
    Reg::Xmm {
        reg: XmmReg::XMM5,
        section: RegSection::Dword,
    },
    Reg::Xmm {
        reg: XmmReg::XMM6,
        section: RegSection::Dword,
    },
    Reg::Xmm {
        reg: XmmReg::XMM7,
        section: RegSection::Dword,
    },
];

pub(super) const RBP: Reg = Reg::X86 {
    reg: X86Reg::Bp,
    section: RegSection::Qword,
};

pub(super) const RAX: Reg = Reg::X86 {
    reg: X86Reg::Ax,
    section: RegSection::Qword,
};

pub(super) const RDX: Reg = Reg::X86 {
    reg: X86Reg::Dx,
    section: RegSection::Qword,
};

impl RegSection {
    pub fn size(&self) -> usize {
        match self {
            RegSection::LowByte => 1,
            RegSection::HighByte => 1,
            RegSection::Word => 2,
            RegSection::Dword => 4,
            RegSection::Qword => 8,
        }
    }

    pub fn from_size(size: usize) -> Result<Self> {
        match size {
            _ if Self::LowByte.size() == size => Ok(Self::LowByte),
            _ if Self::HighByte.size() == size => Ok(Self::HighByte),
            _ if Self::Word.size() == size => Ok(Self::Word),
            _ if Self::Dword.size() == size => Ok(Self::Dword),
            _ if Self::Qword.size() == size => Ok(Self::Qword),
            _ => bail!("Could not convert size {size} to register"),
        }
    }
}

impl From<&X86Reg> for &str {
    fn from(value: &X86Reg) -> Self {
        match value {
            X86Reg::Ax => "ax",
            X86Reg::Bx => "bx",
            X86Reg::Cx => "cx",
            X86Reg::Dx => "dx",
            X86Reg::Bp => "bp",
            X86Reg::Sp => "sp",
            X86Reg::Si => "si",
            X86Reg::Di => "di",
        }
    }
}

impl From<&XmmReg> for usize {
    fn from(value: &XmmReg) -> Self {
        match value {
            XmmReg::XMM0 => 0,
            XmmReg::XMM1 => 1,
            XmmReg::XMM2 => 2,
            XmmReg::XMM3 => 3,
            XmmReg::XMM4 => 4,
            XmmReg::XMM5 => 5,
            XmmReg::XMM6 => 6,
            XmmReg::XMM7 => 7,
            XmmReg::XMM8 => 8,
            XmmReg::XMM9 => 9,
            XmmReg::XMM10 => 10,
            XmmReg::XMM11 => 11,
            XmmReg::XMM12 => 12,
            XmmReg::XMM13 => 13,
            XmmReg::XMM14 => 14,
            XmmReg::XMM15 => 15,
        }
    }
}

impl Reg {
    pub fn size(&self) -> usize {
        match self {
            Self::X86 { reg: _, section } => section.size(),
            Self::X64 { reg: _, section } => section.size(),
            Self::Xmm { reg: _, section } => section.size(),
        }
    }

    pub fn as_section(self, section: RegSection) -> Self {
        match self {
            Self::X86 { reg, .. } => Self::X86 { reg, section },
            Self::X64 { reg, .. } => Self::X64 { reg, section },
            Self::Xmm { reg, .. } => Self::Xmm { reg, section },
        }
    }
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::X86 { reg, section } => {
                let prefix = match section {
                    RegSection::Word => "",
                    RegSection::Dword => "e",
                    RegSection::Qword => "r",
                    _ => "",
                };
                let suffix = match section {
                    RegSection::LowByte => "l",
                    RegSection::HighByte => "h",
                    _ => "",
                };
                let reg_str = match reg {
                    reg @ X86Reg::Ax | reg @ X86Reg::Bx | reg @ X86Reg::Cx | reg @ X86Reg::Dx
                        if !suffix.is_empty() =>
                    {
                        &<&str>::from(reg)[..1]
                    }
                    reg => <&str>::from(reg),
                };

                write!(f, "{prefix}{reg_str}{suffix}")
            }
            Self::X64 { reg, section } => {
                let suffix = match section {
                    RegSection::LowByte => "b",
                    RegSection::HighByte => "h",
                    RegSection::Word => "w",
                    RegSection::Dword => "d",
                    RegSection::Qword => "",
                };
                write!(f, "r{}{}", usize::from(reg), suffix)
            }
            Self::Xmm { reg, .. } => {
                write!(f, "xmm{}", usize::from(reg))
            }
        }
    }
}

impl From<&X64Reg> for usize {
    fn from(value: &X64Reg) -> Self {
        match value {
            X64Reg::R8 => 8,
            X64Reg::R9 => 9,
            X64Reg::R10 => 10,
            X64Reg::R11 => 11,
            X64Reg::R12 => 12,
            X64Reg::R13 => 13,
            X64Reg::R14 => 14,
            X64Reg::R15 => 15,
        }
    }
}
