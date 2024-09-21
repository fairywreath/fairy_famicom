use std::fmt;

use lazy_static::lazy_static;

use crate::cpu::PSFlags;

use AddressingMode::{
    Absolute, AbsoluteX, AbsoluteY, Accumulator, Immediate, Implied, IndexedIndirect, Indirect,
    IndirectIndexed, Relative, ZeroPage, ZeroPageX, ZeroPageY,
};
use Operation::{
    ADC, AND, ASL, BCC, BCS, BEQ, BIT, BMI, BNE, BPL, BRK, BVC, BVS, CLC, CLD, CLI, CLV, CMP, CPX,
    CPY, DEC, DEX, DEY, EOR, INC, INX, INY, JMP, JSR, LDA, LDX, LDY, LSR, NOP, ORA, PHA, PHP, PLA,
    PLP, ROL, ROR, RTI, RTS, SBC, SEC, SED, SEI, STA, STX, STY, TAX, TAY, TSX, TXA, TXS, TYA, XXX,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(crate) enum Operation {
    XXX,
    ADC,
    AND,
    ASL,
    BCC,
    BCS,
    BVS,
    BEQ,
    BIT,
    BMI,
    BNE,
    BPL,
    BRK,
    BVC,
    CLC,
    CLD,
    CLI,
    CLV,
    CMP,
    CPX,
    CPY,
    DEC,
    DEX,
    DEY,
    EOR,
    INC,
    INX,
    INY,
    JMP,
    JSR,
    LDA,
    LDX,
    LDY,
    LSR,
    NOP,
    ORA,
    PHA,
    PHP,
    PLA,
    PLP,
    ROL,
    ROR,
    RTI,
    RTS,
    SBC,
    SEC,
    SED,
    SEI,
    STA,
    STX,
    STY,
    TAX,
    TAY,
    TSX,
    TXA,
    TXS,
    TYA,
}

#[derive(Default, Copy, Clone, Debug, PartialEq, Eq)]
pub(crate) enum AddressingMode {
    Absolute,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    AbsoluteX,
    AbsoluteY,
    #[default]
    Immediate,
    Relative,
    Implied,
    Indirect,
    IndexedIndirect,
    IndirectIndexed,
    Accumulator,
}

#[derive(Copy, Clone)]
pub(crate) struct Instruction {
    pub(crate) op_code: u8,
    pub(crate) operation: Operation,
    pub(crate) addressing_mode: AddressingMode,
    pub(crate) flags: PSFlags,
}

impl Instruction {
    fn new(
        op_code: u8,
        operation: Operation,
        addressing_mode: AddressingMode,
        flags: PSFlags,
    ) -> Self {
        Self {
            op_code,
            operation,
            addressing_mode,
            flags,
        }
    }
}

impl fmt::Debug for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Instruction")
            .field("op_code", &format_args!("{:#04x}", self.op_code)) // Hex format for op_code
            .field("operation", &self.operation)
            .field("addressing_mode", &self.addressing_mode)
            .field("flags", &self.flags)
            .finish()
    }
}

lazy_static! {
    pub(crate) static ref INSTRUCTIONS: Vec<Instruction> = vec![
           Instruction::new(0x00, BRK, Implied, PSFlags::empty()),
        Instruction::new(0x01, ORA, IndexedIndirect, PSFlags::Z | PSFlags::N),
        Instruction::new(0x02, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x03, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x04, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x05, ORA, ZeroPage, PSFlags::Z | PSFlags::N),
        Instruction::new(0x06, ASL, ZeroPage, PSFlags::C | PSFlags::N),
        Instruction::new(0x07, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x08, PHP, Implied, PSFlags::empty()),
        Instruction::new(0x09, ORA, Immediate, PSFlags::Z | PSFlags::N),
        Instruction::new(0x0a, ASL, Accumulator, PSFlags::C | PSFlags::N),
        Instruction::new(0x0b, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x0c, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x0d, ORA, Absolute, PSFlags::Z | PSFlags::N),
        Instruction::new(0x0e, ASL, Absolute, PSFlags::C | PSFlags::N),
        Instruction::new(0x0f, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x10, BPL, Relative, PSFlags::empty()),
        Instruction::new(0x11, ORA, IndirectIndexed, PSFlags::Z | PSFlags::N),
        Instruction::new(0x12, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x13, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x14, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x15, ORA, ZeroPageX, PSFlags::Z | PSFlags::N),
        Instruction::new(0x16, ASL, ZeroPageX, PSFlags::C | PSFlags::N),
        Instruction::new(0x17, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x18, CLC, Implied, PSFlags::C),
        Instruction::new(0x19, ORA, AbsoluteY, PSFlags::Z | PSFlags::N),
        Instruction::new(0x1a, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x1b, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x1c, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x1d, ORA, AbsoluteX, PSFlags::Z | PSFlags::N),
        Instruction::new(0x1e, ASL, AbsoluteX, PSFlags::C | PSFlags::N),
        Instruction::new(0x1f, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x20, JSR, Absolute, PSFlags::empty()),
        Instruction::new(0x21, AND, IndexedIndirect, PSFlags::Z | PSFlags::N),
        Instruction::new(0x22, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x23, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x24, BIT, ZeroPage, PSFlags::Z | PSFlags::V | PSFlags::N),
        Instruction::new(0x25, AND, ZeroPage, PSFlags::Z | PSFlags::N),
        Instruction::new(0x26, ROL, ZeroPage, PSFlags::C | PSFlags::N),
        Instruction::new(0x27, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x28, PLP, Implied, PSFlags::empty()),
        Instruction::new(0x29, AND, Immediate, PSFlags::Z | PSFlags::N),
        Instruction::new(0x2a, ROL, Accumulator, PSFlags::C | PSFlags::N),
        Instruction::new(0x2b, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x2c, BIT, Absolute, PSFlags::Z | PSFlags::V | PSFlags::N),
        Instruction::new(0x2d, AND, Absolute, PSFlags::Z | PSFlags::N),
        Instruction::new(0x2e, ROL, Absolute, PSFlags::C | PSFlags::N),
        Instruction::new(0x2f, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x30, BMI, Relative, PSFlags::empty()),
        Instruction::new(0x31, AND, IndirectIndexed, PSFlags::Z | PSFlags::N),
        Instruction::new(0x32, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x33, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x34, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x35, AND, ZeroPageX, PSFlags::Z | PSFlags::N),
        Instruction::new(0x36, ROL, ZeroPageX, PSFlags::C | PSFlags::N),
        Instruction::new(0x37, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x38, SEC, Implied, PSFlags::C),
        Instruction::new(0x39, AND, AbsoluteY, PSFlags::Z | PSFlags::N),
        Instruction::new(0x3a, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x3b, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x3c, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x3d, AND, AbsoluteX, PSFlags::Z | PSFlags::N),
        Instruction::new(0x3e, ROL, AbsoluteX, PSFlags::C | PSFlags::N),
        Instruction::new(0x3f, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x40, RTI, Implied, PSFlags::empty()),
        Instruction::new(0x41, EOR, IndexedIndirect, PSFlags::Z | PSFlags::N),
        Instruction::new(0x42, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x43, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x44, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x45, EOR, ZeroPage, PSFlags::Z | PSFlags::N),
        Instruction::new(0x46, LSR, ZeroPage, PSFlags::C | PSFlags::N),
        Instruction::new(0x47, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x48, PHA, Implied, PSFlags::empty()),
        Instruction::new(0x49, EOR, Immediate, PSFlags::Z | PSFlags::N),
        Instruction::new(0x4a, LSR, Accumulator, PSFlags::C | PSFlags::N),
        Instruction::new(0x4b, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x4c, JMP, Absolute, PSFlags::empty()),
        Instruction::new(0x4d, EOR, Absolute, PSFlags::Z | PSFlags::N),
        Instruction::new(0x4e, LSR, Absolute, PSFlags::C | PSFlags::N),
        Instruction::new(0x4f, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x50, BVC, Relative, PSFlags::empty()),
        Instruction::new(0x51, EOR, IndirectIndexed, PSFlags::Z | PSFlags::N),
        Instruction::new(0x52, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x53, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x54, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x55, EOR, ZeroPageX, PSFlags::Z | PSFlags::N),
        Instruction::new(0x56, LSR, ZeroPageX, PSFlags::C | PSFlags::N),
        Instruction::new(0x57, XXX, AddressingMode::default(), PSFlags::empty()),
          Instruction::new(0x58, CLI, Implied, PSFlags::I),
        Instruction::new(0x59, EOR, AbsoluteY, PSFlags::Z | PSFlags::N),
        Instruction::new(0x5a, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x5b, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x5c, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x5d, EOR, AbsoluteX, PSFlags::Z | PSFlags::N),
        Instruction::new(0x5e, LSR, AbsoluteX, PSFlags::C | PSFlags::N),
        Instruction::new(0x5f, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x60, RTS, Implied, PSFlags::empty()),
        Instruction::new(0x61, ADC, IndexedIndirect, PSFlags::Z | PSFlags::N),
        Instruction::new(0x62, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x63, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x64, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x65, ADC, ZeroPage, PSFlags::Z | PSFlags::N),
        Instruction::new(0x66, ROR, ZeroPage, PSFlags::C | PSFlags::N),
        Instruction::new(0x67, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x68, PLA, Implied, PSFlags::empty()),
        Instruction::new(0x69, ADC, Immediate, PSFlags::Z | PSFlags::N),
        Instruction::new(0x6a, ROR, Accumulator, PSFlags::C | PSFlags::N),
        Instruction::new(0x6b, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x6c, JMP, Indirect, PSFlags::empty()),
        Instruction::new(0x6d, ADC, Absolute, PSFlags::Z | PSFlags::N),
        Instruction::new(0x6e, ROR, Absolute, PSFlags::C | PSFlags::N),
        Instruction::new(0x6f, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x70, BVS, Relative, PSFlags::empty()),
        Instruction::new(0x71, ADC, IndirectIndexed, PSFlags::Z | PSFlags::N),
        Instruction::new(0x72, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x73, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x74, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x75, ADC, ZeroPageX, PSFlags::Z | PSFlags::N),
        Instruction::new(0x76, ROR, ZeroPageX, PSFlags::C | PSFlags::N),
        Instruction::new(0x77, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x78, SEI, Implied, PSFlags::I),
        Instruction::new(0x79, ADC, AbsoluteY, PSFlags::Z | PSFlags::N),
        Instruction::new(0x7a, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x7b, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x7c, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x7d, ADC, AbsoluteX, PSFlags::Z | PSFlags::N),
        Instruction::new(0x7e, ROR, AbsoluteX, PSFlags::C | PSFlags::N),
        Instruction::new(0x7f, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x80, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x81, STA, IndexedIndirect, PSFlags::empty()),
        Instruction::new(0x82, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x83, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x84, STY, ZeroPage, PSFlags::empty()),
        Instruction::new(0x85, STA, ZeroPage, PSFlags::empty()),
        Instruction::new(0x86, STX, ZeroPage, PSFlags::empty()),
        Instruction::new(0x87, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x88, DEY, Implied, PSFlags::Z | PSFlags::N),
        Instruction::new(0x89, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x8a, TXA, Implied, PSFlags::Z | PSFlags::N),
        Instruction::new(0x8b, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x8c, STY, Absolute, PSFlags::empty()),
        Instruction::new(0x8d, STA, Absolute, PSFlags::empty()),
        Instruction::new(0x8e, STX, Absolute, PSFlags::empty()),
        Instruction::new(0x8f, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x90, BCC, Relative, PSFlags::empty()),
        Instruction::new(0x91, STA, IndirectIndexed, PSFlags::empty()),
        Instruction::new(0x92, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x93, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x94, STY, ZeroPageX, PSFlags::empty()),
        Instruction::new(0x95, STA, ZeroPageX, PSFlags::empty()),
        Instruction::new(0x96, STX, ZeroPageY, PSFlags::empty()),
        Instruction::new(0x97, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x98, TYA, Implied, PSFlags::Z | PSFlags::N),
        Instruction::new(0x99, STA, AbsoluteY, PSFlags::empty()),
        Instruction::new(0x9a, TXS, Implied, PSFlags::empty()),
        Instruction::new(0x9b, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x9c, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x9d, STA, AbsoluteX, PSFlags::empty()),
        Instruction::new(0x9e, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0x9f, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xa0, LDY, Immediate, PSFlags::Z | PSFlags::N),
        Instruction::new(0xa1, LDA, IndexedIndirect, PSFlags::Z | PSFlags::N),
        Instruction::new(0xa2, LDX, Immediate, PSFlags::Z | PSFlags::N),
        Instruction::new(0xa3, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xa4, LDY, ZeroPage, PSFlags::Z | PSFlags::N),
        Instruction::new(0xa5, LDA, ZeroPage, PSFlags::Z | PSFlags::N),
        Instruction::new(0xa6, LDX, ZeroPage, PSFlags::Z | PSFlags::N),
        Instruction::new(0xa7, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xa8, TAY, Implied, PSFlags::Z | PSFlags::N),
        Instruction::new(0xa9, LDA, Immediate, PSFlags::Z | PSFlags::N),
        Instruction::new(0xaa, TAX, Implied, PSFlags::Z | PSFlags::N),
        Instruction::new(0xab, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xac, LDY, Absolute, PSFlags::Z | PSFlags::N),
        Instruction::new(0xad, LDA, Absolute, PSFlags::Z | PSFlags::N),
        Instruction::new(0xae, LDX, Absolute, PSFlags::Z | PSFlags::N),
        Instruction::new(0xaf, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xb0, BCS, Relative, PSFlags::empty()),
        Instruction::new(0xb1, LDA, IndirectIndexed, PSFlags::Z | PSFlags::N),
        Instruction::new(0xb2, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xb3, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xb4, LDY, ZeroPageX, PSFlags::Z | PSFlags::N),
        Instruction::new(0xb5, LDA, ZeroPageX, PSFlags::Z | PSFlags::N),
        Instruction::new(0xb6, LDX, ZeroPageY, PSFlags::Z | PSFlags::N),
        Instruction::new(0xb7, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xb8, CLV, Implied, PSFlags::empty()),
        Instruction::new(0xb9, LDA, AbsoluteY, PSFlags::Z | PSFlags::N),
        Instruction::new(0xba, TSX, Implied, PSFlags::Z | PSFlags::N),
        Instruction::new(0xbb, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xbc, LDY, AbsoluteX, PSFlags::Z | PSFlags::N),
        Instruction::new(0xbd, LDA, AbsoluteX, PSFlags::Z | PSFlags::N),
        Instruction::new(0xbe, LDX, AbsoluteY, PSFlags::Z | PSFlags::N),
        Instruction::new(0xbf, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xc0, CPY, Immediate, PSFlags::Z | PSFlags::N),
        Instruction::new(0xc1, CMP, IndexedIndirect, PSFlags::Z | PSFlags::N),
        Instruction::new(0xc2, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xc3, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xc4, CPY, ZeroPage, PSFlags::Z | PSFlags::N),
        Instruction::new(0xc5, CMP, ZeroPage, PSFlags::Z | PSFlags::N),
        Instruction::new(0xc6, DEC, ZeroPage, PSFlags::Z | PSFlags::N),
        Instruction::new(0xc7, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xc8, INY, Implied, PSFlags::Z | PSFlags::N),
        Instruction::new(0xc9, CMP, Immediate, PSFlags::Z | PSFlags::N),
        Instruction::new(0xca, DEX, Implied, PSFlags::Z | PSFlags::N),
        Instruction::new(0xcb, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xcc, CPY, Absolute, PSFlags::Z | PSFlags::N),
        Instruction::new(0xcd, CMP, Absolute, PSFlags::Z | PSFlags::N),
        Instruction::new(0xce, DEC, Absolute, PSFlags::Z | PSFlags::N),
        Instruction::new(0xcf, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xd0, BNE, Relative, PSFlags::empty()),
        Instruction::new(0xd1, CMP, IndirectIndexed, PSFlags::Z | PSFlags::N),
        Instruction::new(0xd2, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xd3, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xd4, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xd5, CMP, ZeroPageX, PSFlags::Z | PSFlags::N),
        Instruction::new(0xd6, DEC, ZeroPageX, PSFlags::Z | PSFlags::N),
        Instruction::new(0xd7, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xd8, CLD, Implied, PSFlags::empty()),
        Instruction::new(0xd9, CMP, AbsoluteY, PSFlags::Z | PSFlags::N),
        Instruction::new(0xda, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xdb, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xdc, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xdd, CMP, AbsoluteX, PSFlags::Z | PSFlags::N),
        Instruction::new(0xde, DEC, AbsoluteX, PSFlags::Z | PSFlags::N),
        Instruction::new(0xdf, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xe0, CPX, Immediate, PSFlags::Z | PSFlags::N),
        Instruction::new(0xe1, SBC, IndexedIndirect, PSFlags::Z | PSFlags::N),
        Instruction::new(0xe2, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xe3, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xe4, CPX, ZeroPage, PSFlags::Z | PSFlags::N),
        Instruction::new(0xe5, SBC, ZeroPage, PSFlags::Z | PSFlags::N),
        Instruction::new(0xe6, INC, ZeroPage, PSFlags::Z | PSFlags::N),
        Instruction::new(0xe7, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xe8, INX, Implied, PSFlags::Z | PSFlags::N),
        Instruction::new(0xe9, SBC, Immediate, PSFlags::Z | PSFlags::N),
        Instruction::new(0xea, NOP, Implied, PSFlags::empty()),
        Instruction::new(0xeb, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xec, CPX, Absolute, PSFlags::Z | PSFlags::N),
        Instruction::new(0xed, SBC, Absolute, PSFlags::Z | PSFlags::N),
        Instruction::new(0xee, INC, Absolute, PSFlags::Z | PSFlags::N),
        Instruction::new(0xef, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xf0, BEQ, Relative, PSFlags::empty()),
        Instruction::new(0xf1, SBC, IndirectIndexed, PSFlags::Z | PSFlags::N),
        Instruction::new(0xf2, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xf3, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xf4, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xf5, SBC, ZeroPageX, PSFlags::Z | PSFlags::N),
        Instruction::new(0xf6, INC, ZeroPageX, PSFlags::Z | PSFlags::N),
        Instruction::new(0xf7, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xf8, SED, Implied, PSFlags::empty()),
        Instruction::new(0xf9, SBC, AbsoluteY, PSFlags::Z | PSFlags::N),
        Instruction::new(0xfa, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xfb, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xfc, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
        Instruction::new(0xfd, SBC, AbsoluteX, PSFlags::Z | PSFlags::N),
        Instruction::new(0xfe, INC, AbsoluteX, PSFlags::Z | PSFlags::N),
        Instruction::new(0xff, XXX, AddressingMode::default(), PSFlags::empty()), // Dummy
    ];
}
