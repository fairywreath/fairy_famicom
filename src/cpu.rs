use anyhow::Result;
use bitflags::bitflags;

use crate::isa::{AddressingMode, Instruction, Operation, INSTRUCTIONS};

const MEM_SIZE: usize = 0xFFFF;
const MEM_ROM_OFFSET: usize = 0x8000;

bitflags! {
    /// Processor status flags.
     #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub(crate) struct PSFlags: u8 {
        /// Carry.
        const C = 0b00000001;
        /// Zero.
        const Z = 0b00000010;
        /// Interrupt Disable.
        const I = 0b00000100;
        /// Decimal.
        const D = 0b00001000;
        /// Break.
        const B = 0b0001000;
        /// Overflow.
        const V = 0b0100000;
        /// Negative.
        const N = 0b1000000;
    }
}

pub(crate) struct Cpu {
    /// Program counter.
    pc: u16,

    /// 256 byte stack allocated between $0100 and $01FF.
    sp: u8,

    /// Accumulator register used for airthmetic and logical operations.
    reg_a: u8,

    /// X index register used to hold counters or offsets for memory acceses. X is also used to
    /// read/write with the stack.
    reg_x: u8,

    /// Y index register is similar to X index register without stack access.
    reg_y: u8,

    /// Processor status flags, NV-BDIZC.
    flags: PSFlags,

    /// Virtual memory map.
    mem: [u8; MEM_SIZE],
}

impl Cpu {
    pub(crate) fn new() -> Self {
        log::info!("ISA instruction space size: {}", INSTRUCTIONS.len());
        Self {
            pc: 0,
            sp: 0,
            reg_a: 0,
            reg_x: 0,
            reg_y: 0,
            flags: PSFlags::empty(),
            mem: [0; MEM_SIZE],
        }
    }

    pub(crate) fn load_program(&mut self, program: Vec<u8>) -> Result<()> {
        self.mem[MEM_ROM_OFFSET..][..program.len()].copy_from_slice(&program);
        self.pc = MEM_ROM_OFFSET as _;

        Ok(())
    }

    pub(crate) fn run(&mut self) -> Result<()> {
        loop {
            let instruction = self.read_instruction();
            log::trace!("PC {:x}: {:?}", self.pc - 1, instruction);
            match instruction.operation {
                Operation::LDA => self.lda(instruction.addressing_mode),
                Operation::BRK => break,
                _ => {
                    log::warn!("Instruction {:?} not yet implemented!", instruction);
                    // todo!()
                }
            }
        }

        Ok(())
    }

    fn load_and_run(&mut self, program: Vec<u8>) {
        self.load_program(program).unwrap();
        self.run().unwrap();
    }

    fn mem_read_u8(&mut self, address: u16) -> u8 {
        let value = self.mem[address as usize];
        value
    }

    fn mem_read_u16(&mut self, address: u16) -> u16 {
        let lo = self.mem_read_u8(address) as u16;
        let hi = self.mem_read_u8(address + 1) as u16;
        (hi << 8) | lo
    }

    fn mem_write_u8(&mut self, address: u16, value: u8) {
        self.mem[address as usize] = value;
    }

    fn mem_write_u16(&mut self, address: u16, value: u16) {
        self.mem_write_u8(address, value as _);
        self.mem_write_u8(address + 1, (value >> 8) as _);
    }

    fn advance(&mut self, count: u16) {
        self.pc += count;
    }

    fn read_instruction(&mut self) -> Instruction {
        let instruction = INSTRUCTIONS[self.mem[self.pc as usize] as usize];
        self.advance(1);
        instruction
    }

    fn value_at_immediate(&mut self) -> u8 {
        let value = self.mem_read_u8(self.pc);
        self.advance(1);
        value
    }

    fn value_at_zero_page(&mut self) -> u8 {
        let address = self.mem_read_u8(self.pc);
        let value = self.mem_read_u8(address as _);
        self.advance(1);
        value
    }

    fn value_at_zero_page_x(&mut self) -> u8 {
        let zero_page_address = self.mem_read_u8(self.pc) as u16;
        let address = (zero_page_address + self.reg_x as u16) % 0xFF;
        let value = self.mem_read_u8(address as _);
        self.advance(1);
        value
    }

    fn value_at_zero_page_y(&mut self) -> u8 {
        let zero_page_address = self.mem_read_u8(self.pc) as u16;
        let address = zero_page_address + self.reg_y as u16;
        let value = self.mem_read_u8(address as _);
        self.advance(1);
        value
    }

    fn value_at_absolute(&mut self) -> u8 {
        let address = self.mem_read_u16(self.pc);
        let value = self.mem[address as usize];
        self.advance(2);
        value
    }

    fn value_at_absolute_x(&mut self) -> u8 {
        let address = self.mem_read_u16(self.pc) + self.reg_x as u16;
        let value = self.mem[address as usize];
        self.advance(2);
        value
    }

    fn value_at_absolute_y(&mut self) -> u8 {
        let address = self.mem_read_u16(self.pc) + self.reg_y as u16;
        let value = self.mem[address as usize];
        self.advance(2);
        value
    }

    fn value_at_relative(&mut self) -> u8 {
        let value = self.mem_read_u8(self.pc);
        self.advance(1);
        value
    }

    fn value_at_indirect(&mut self) -> u8 {
        let indirect_address = self.mem_read_u16(self.pc);

        let lo = self.mem_read_u8(indirect_address) as u16;
        let hi = self.mem_read_u8(indirect_address + 1) as u16;

        let address = (hi << 8) | lo;
        let value = self.mem_read_u8(address);

        self.advance(2);
        value
    }

    fn value_at_indexed_indirect(&mut self) -> u8 {
        let zero_page_address = self.mem_read_u8(self.pc) as u16;
        // XXX: Is a wrap around required for this zero page address?
        let indirect_address = zero_page_address + self.reg_x as u16;

        let lo = self.mem_read_u8(indirect_address) as u16;
        let hi = self.mem_read_u8(indirect_address + 1) as u16;
        let address = (hi << 8) | lo;
        let value = self.mem_read_u8(address);

        self.advance(1);
        value
    }

    fn value_at_indirect_indexed(&mut self) -> u8 {
        let lo = self.mem_read_u8(self.pc) as u16;
        let hi = self.mem_read_u8(self.pc + 1) as u16;
        let address = ((hi << 8) | lo) + self.reg_y as u16;
        let value = self.mem_read_u8(address);

        self.advance(1);
        value
    }

    fn is_negative(value: u8) -> bool {
        (value & 0b1000000) != 0
    }

    fn get_value(&mut self, addressing_mode: AddressingMode) -> u8 {
        match addressing_mode {
            AddressingMode::Immediate => self.value_at_immediate(),
            AddressingMode::ZeroPage => self.value_at_zero_page(),
            AddressingMode::ZeroPageX => self.value_at_zero_page_x(),
            AddressingMode::ZeroPageY => self.value_at_zero_page_y(),
            AddressingMode::Absolute => self.value_at_absolute(),
            AddressingMode::AbsoluteX => self.value_at_absolute_x(),
            AddressingMode::AbsoluteY => self.value_at_absolute_y(),
            AddressingMode::Indirect => self.value_at_indirect(),
            AddressingMode::IndexedIndirect => self.value_at_indexed_indirect(),
            AddressingMode::IndirectIndexed => self.value_at_indirect_indexed(),
            // XXX TODO: handle this properly.
            _ => todo!(),
        }
    }

    fn lda(&mut self, addressing_mode: AddressingMode) {
        let value = self.get_value(addressing_mode);
        self.reg_a = value;

        if self.reg_a == 0 {
            self.flags |= PSFlags::Z;
        }
        if Self::is_negative(self.reg_a) {
            self.flags |= PSFlags::N;
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = Cpu::new();
        cpu.load_and_run(vec![0xa9, 0x05, 0x00]);
        assert_eq!(cpu.reg_a, 0x05);
        assert!(!cpu.flags.contains(PSFlags::Z));
        assert!(!cpu.flags.contains(PSFlags::N));
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = Cpu::new();
        cpu.load_and_run(vec![0xa9, 0x00, 0x00]);
        assert_eq!(cpu.reg_a, 0x00);
        assert!(cpu.flags.contains(PSFlags::Z));
        assert!(!cpu.flags.contains(PSFlags::N));
    }
}
