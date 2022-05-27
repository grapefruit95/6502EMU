#include <stdint.h>
#include "bus.h"

/*
* C = Carry
* Z = Zero
* I = Interrupt
* D = Decimal mode
* B = Break
* U = Unused
* O = Overflow
* N = Negative
*/
enum conditionCodeBits{ C,Z,I,D,B,U,V,N };
uint8_t CC[8] = { 0,0,0,0,0,0,0,0 };

uint8_t a = 0x00, x = 0x00, y = 0x00, SP = 0x00, status = 0x00;
uint16_t PC = 0x0000;

//covert CC array to bitstring
void CC_to_status() {
	for (uint8_t i = 0; i < sizeof(CC); i++) {
		if (CC[i]) {
			status |= 0x01 << i;
		}
	}
}

void status_to_CC() {
	for (uint8_t i = 0; i < sizeof(CC); i++) {
		if (status & 0x01) {
			CC[i] = 1;
		}
		else {
			CC[i] = 0;
		}
		status = status >> 1;
	}
}

//Addressing Modes
uint8_t
IMP(), IMM(), ZP0(), ZPX(),
ZPY(), REL(), ABS(), ABX(),
ABY(), IND(), IZX(), IZY();

//Opcodes (56 of them + ILL for illegal opcodes)
uint8_t
ADC(), AND(), ASL(), BCC(), BCS(), BEQ(), BIT(), BMI(),
BNE(), BPL(), BRK(), BVC(), BVS(), CLC(), CLD(), CLI(),
CLV(), CMP(), CPX(), CPY(), DEC(), DEX(), DEY(), EOR(),
INC(), INX(), INY(), JMP(), JSR(), LDA(), LDX(), LDY(),
LSR(), NOP(), ORA(), PHA(), PHP(), PLA(), PLP(), ROL(),
ROR(), RTI(), RTS(), SBC(), SEC(), SED(), SEI(), STA(),
STX(), STY(), TAX(), TAY(), TSX(), TXA(), TXS(), TYA(),

ILL();

//Instructions
struct Instruction {
	char name[3];
	uint8_t(* operationPtr)();
	uint8_t (* addrModePtr)();
	uint8_t cycles;
};
typedef struct Instruction Instruction_t;

Instruction_t Instructions[256] = {
	{ "BRK", &BRK, &IMM, 7 },{ "ORA", &ORA, &IZX, 6 },{ "ILL", &ILL, &IMP, 2 },{ "ILL", &ILL, &IMP, 8 },{ "ILL", &NOP, &IMP, 3 },{ "ORA", &ORA, &ZP0, 3 },{ "ASL", &ASL, &ZP0, 5 },{ "ILL", &ILL, &IMP, 5 },{ "PHP", &PHP, &IMP, 3 },{ "ORA", &ORA, &IMM, 2 },{ "ASL", &ASL, &IMP, 2 },{ "ILL", &ILL, &IMP, 2 },{ "ILL", &NOP, &IMP, 4 },{ "ORA", &ORA, &ABS, 4 },{ "ASL", &ASL, &ABS, 6 },{ "ILL", &ILL, &IMP, 6 },
	{ "BPL", &BPL, &REL, 2 },{ "ORA", &ORA, &IZY, 5 },{ "ILL", &ILL, &IMP, 2 },{ "ILL", &ILL, &IMP, 8 },{ "ILL", &NOP, &IMP, 4 },{ "ORA", &ORA, &ZPX, 4 },{ "ASL", &ASL, &ZPX, 6 },{ "ILL", &ILL, &IMP, 6 },{ "CLC", &CLC, &IMP, 2 },{ "ORA", &ORA, &ABY, 4 },{ "ILL", &NOP, &IMP, 2 },{ "ILL", &ILL, &IMP, 7 },{ "ILL", &NOP, &IMP, 4 },{ "ORA", &ORA, &ABX, 4 },{ "ASL", &ASL, &ABX, 7 },{ "ILL", &ILL, &IMP, 7 },
	{ "JSR", &JSR, &ABS, 6 },{ "AND", &AND, &IZX, 6 },{ "ILL", &ILL, &IMP, 2 },{ "ILL", &ILL, &IMP, 8 },{ "BIT", &BIT, &ZP0, 3 },{ "AND", &AND, &ZP0, 3 },{ "ROL", &ROL, &ZP0, 5 },{ "ILL", &ILL, &IMP, 5 },{ "PLP", &PLP, &IMP, 4 },{ "AND", &AND, &IMM, 2 },{ "ROL", &ROL, &IMP, 2 },{ "ILL", &ILL, &IMP, 2 },{ "BIT", &BIT, &ABS, 4 },{ "AND", &AND, &ABS, 4 },{ "ROL", &ROL, &ABS, 6 },{ "ILL", &ILL, &IMP, 6 },
	{ "BMI", &BMI, &REL, 2 },{ "AND", &AND, &IZY, 5 },{ "ILL", &ILL, &IMP, 2 },{ "ILL", &ILL, &IMP, 8 },{ "ILL", &NOP, &IMP, 4 },{ "AND", &AND, &ZPX, 4 },{ "ROL", &ROL, &ZPX, 6 },{ "ILL", &ILL, &IMP, 6 },{ "SEC", &SEC, &IMP, 2 },{ "AND", &AND, &ABY, 4 },{ "ILL", &NOP, &IMP, 2 },{ "ILL", &ILL, &IMP, 7 },{ "ILL", &NOP, &IMP, 4 },{ "AND", &AND, &ABX, 4 },{ "ROL", &ROL, &ABX, 7 },{ "ILL", &ILL, &IMP, 7 },
	{ "RTI", &RTI, &IMP, 6 },{ "EOR", &EOR, &IZX, 6 },{ "ILL", &ILL, &IMP, 2 },{ "ILL", &ILL, &IMP, 8 },{ "ILL", &NOP, &IMP, 3 },{ "EOR", &EOR, &ZP0, 3 },{ "LSR", &LSR, &ZP0, 5 },{ "ILL", &ILL, &IMP, 5 },{ "PHA", &PHA, &IMP, 3 },{ "EOR", &EOR, &IMM, 2 },{ "LSR", &LSR, &IMP, 2 },{ "ILL", &ILL, &IMP, 2 },{ "JMP", &JMP, &ABS, 3 },{ "EOR", &EOR, &ABS, 4 },{ "LSR", &LSR, &ABS, 6 },{ "ILL", &ILL, &IMP, 6 },
	{ "BVC", &BVC, &REL, 2 },{ "EOR", &EOR, &IZY, 5 },{ "ILL", &ILL, &IMP, 2 },{ "ILL", &ILL, &IMP, 8 },{ "ILL", &NOP, &IMP, 4 },{ "EOR", &EOR, &ZPX, 4 },{ "LSR", &LSR, &ZPX, 6 },{ "ILL", &ILL, &IMP, 6 },{ "CLI", &CLI, &IMP, 2 },{ "EOR", &EOR, &ABY, 4 },{ "ILL", &NOP, &IMP, 2 },{ "ILL", &ILL, &IMP, 7 },{ "ILL", &NOP, &IMP, 4 },{ "EOR", &EOR, &ABX, 4 },{ "LSR", &LSR, &ABX, 7 },{ "ILL", &ILL, &IMP, 7 },
	{ "RTS", &RTS, &IMP, 6 },{ "ADC", &ADC, &IZX, 6 },{ "ILL", &ILL, &IMP, 2 },{ "ILL", &ILL, &IMP, 8 },{ "ILL", &NOP, &IMP, 3 },{ "ADC", &ADC, &ZP0, 3 },{ "ROR", &ROR, &ZP0, 5 },{ "ILL", &ILL, &IMP, 5 },{ "PLA", &PLA, &IMP, 4 },{ "ADC", &ADC, &IMM, 2 },{ "ROR", &ROR, &IMP, 2 },{ "ILL", &ILL, &IMP, 2 },{ "JMP", &JMP, &IND, 5 },{ "ADC", &ADC, &ABS, 4 },{ "ROR", &ROR, &ABS, 6 },{ "ILL", &ILL, &IMP, 6 },
	{ "BVS", &BVS, &REL, 2 },{ "ADC", &ADC, &IZY, 5 },{ "ILL", &ILL, &IMP, 2 },{ "ILL", &ILL, &IMP, 8 },{ "ILL", &NOP, &IMP, 4 },{ "ADC", &ADC, &ZPX, 4 },{ "ROR", &ROR, &ZPX, 6 },{ "ILL", &ILL, &IMP, 6 },{ "SEI", &SEI, &IMP, 2 },{ "ADC", &ADC, &ABY, 4 },{ "ILL", &NOP, &IMP, 2 },{ "ILL", &ILL, &IMP, 7 },{ "ILL", &NOP, &IMP, 4 },{ "ADC", &ADC, &ABX, 4 },{ "ROR", &ROR, &ABX, 7 },{ "ILL", &ILL, &IMP, 7 },
	{ "ILL", &NOP, &IMP, 2 },{ "STA", &STA, &IZX, 6 },{ "ILL", &NOP, &IMP, 2 },{ "ILL", &ILL, &IMP, 6 },{ "STY", &STY, &ZP0, 3 },{ "STA", &STA, &ZP0, 3 },{ "STX", &STX, &ZP0, 3 },{ "ILL", &ILL, &IMP, 3 },{ "DEY", &DEY, &IMP, 2 },{ "ILL", &NOP, &IMP, 2 },{ "TXA", &TXA, &IMP, 2 },{ "ILL", &ILL, &IMP, 2 },{ "STY", &STY, &ABS, 4 },{ "STA", &STA, &ABS, 4 },{ "STX", &STX, &ABS, 4 },{ "ILL", &ILL, &IMP, 4 },
	{ "BCC", &BCC, &REL, 2 },{ "STA", &STA, &IZY, 6 },{ "ILL", &ILL, &IMP, 2 },{ "ILL", &ILL, &IMP, 6 },{ "STY", &STY, &ZPX, 4 },{ "STA", &STA, &ZPX, 4 },{ "STX", &STX, &ZPY, 4 },{ "ILL", &ILL, &IMP, 4 },{ "TYA", &TYA, &IMP, 2 },{ "STA", &STA, &ABY, 5 },{ "TXS", &TXS, &IMP, 2 },{ "ILL", &ILL, &IMP, 5 },{ "ILL", &NOP, &IMP, 5 },{ "STA", &STA, &ABX, 5 },{ "ILL", &ILL, &IMP, 5 },{ "ILL", &ILL, &IMP, 5 },
	{ "LDY", &LDY, &IMM, 2 },{ "LDA", &LDA, &IZX, 6 },{ "LDX", &LDX, &IMM, 2 },{ "ILL", &ILL, &IMP, 6 },{ "LDY", &LDY, &ZP0, 3 },{ "LDA", &LDA, &ZP0, 3 },{ "LDX", &LDX, &ZP0, 3 },{ "ILL", &ILL, &IMP, 3 },{ "TAY", &TAY, &IMP, 2 },{ "LDA", &LDA, &IMM, 2 },{ "TAX", &TAX, &IMP, 2 },{ "ILL", &ILL, &IMP, 2 },{ "LDY", &LDY, &ABS, 4 },{ "LDA", &LDA, &ABS, 4 },{ "LDX", &LDX, &ABS, 4 },{ "ILL", &ILL, &IMP, 4 },
	{ "BCS", &BCS, &REL, 2 },{ "LDA", &LDA, &IZY, 5 },{ "ILL", &ILL, &IMP, 2 },{ "ILL", &ILL, &IMP, 5 },{ "LDY", &LDY, &ZPX, 4 },{ "LDA", &LDA, &ZPX, 4 },{ "LDX", &LDX, &ZPY, 4 },{ "ILL", &ILL, &IMP, 4 },{ "CLV", &CLV, &IMP, 2 },{ "LDA", &LDA, &ABY, 4 },{ "TSX", &TSX, &IMP, 2 },{ "ILL", &ILL, &IMP, 4 },{ "LDY", &LDY, &ABX, 4 },{ "LDA", &LDA, &ABX, 4 },{ "LDX", &LDX, &ABY, 4 },{ "ILL", &ILL, &IMP, 4 },
	{ "CPY", &CPY, &IMM, 2 },{ "CMP", &CMP, &IZX, 6 },{ "ILL", &NOP, &IMP, 2 },{ "ILL", &ILL, &IMP, 8 },{ "CPY", &CPY, &ZP0, 3 },{ "CMP", &CMP, &ZP0, 3 },{ "DEC", &DEC, &ZP0, 5 },{ "ILL", &ILL, &IMP, 5 },{ "INY", &INY, &IMP, 2 },{ "CMP", &CMP, &IMM, 2 },{ "DEX", &DEX, &IMP, 2 },{ "ILL", &ILL, &IMP, 2 },{ "CPY", &CPY, &ABS, 4 },{ "CMP", &CMP, &ABS, 4 },{ "DEC", &DEC, &ABS, 6 },{ "ILL", &ILL, &IMP, 6 },
	{ "BNE", &BNE, &REL, 2 },{ "CMP", &CMP, &IZY, 5 },{ "ILL", &ILL, &IMP, 2 },{ "ILL", &ILL, &IMP, 8 },{ "ILL", &NOP, &IMP, 4 },{ "CMP", &CMP, &ZPX, 4 },{ "DEC", &DEC, &ZPX, 6 },{ "ILL", &ILL, &IMP, 6 },{ "CLD", &CLD, &IMP, 2 },{ "CMP", &CMP, &ABY, 4 },{ "NOP", &NOP, &IMP, 2 },{ "ILL", &ILL, &IMP, 7 },{ "ILL", &NOP, &IMP, 4 },{ "CMP", &CMP, &ABX, 4 },{ "DEC", &DEC, &ABX, 7 },{ "ILL", &ILL, &IMP, 7 },
	{ "CPX", &CPX, &IMM, 2 },{ "SBC", &SBC, &IZX, 6 },{ "ILL", &NOP, &IMP, 2 },{ "ILL", &ILL, &IMP, 8 },{ "CPX", &CPX, &ZP0, 3 },{ "SBC", &SBC, &ZP0, 3 },{ "INC", &INC, &ZP0, 5 },{ "ILL", &ILL, &IMP, 5 },{ "INX", &INX, &IMP, 2 },{ "SBC", &SBC, &IMM, 2 },{ "NOP", &NOP, &IMP, 2 },{ "ILL", &SBC, &IMP, 2 },{ "CPX", &CPX, &ABS, 4 },{ "SBC", &SBC, &ABS, 4 },{ "INC", &INC, &ABS, 6 },{ "ILL", &ILL, &IMP, 6 },
	{ "BEQ", &BEQ, &REL, 2 },{ "SBC", &SBC, &IZY, 5 },{ "ILL", &ILL, &IMP, 2 },{ "ILL", &ILL, &IMP, 8 },{ "ILL", &NOP, &IMP, 4 },{ "SBC", &SBC, &ZPX, 4 },{ "INC", &INC, &ZPX, 6 },{ "ILL", &ILL, &IMP, 6 },{ "SED", &SED, &IMP, 2 },{ "SBC", &SBC, &ABY, 4 },{ "NOP", &NOP, &IMP, 2 },{ "ILL", &ILL, &IMP, 7 },{ "ILL", &NOP, &IMP, 4 },{ "SBC", &SBC, &ABX, 4 },{ "INC", &INC, &ABX, 7 },{ "ILL", &ILL, &IMP, 7 }
};

void clock();
void reset();
void interrupt_request();
void nmi();

uint8_t fetch();
uint8_t fetched = 0x00;

uint16_t address_relative = 0x0000, address_abs = 0x0000;
uint8_t opcode = 0x00, cycles = 0x00;

void clock() {
	if (cycles == 0) {
		opcode = read(PC);
		PC++;

		cycles = Instructions[opcode].cycles;
		uint8_t additionalCycleAddr = (*(Instructions[opcode].addrModePtr))(); //functions will return number of additional clock cycles they need if necessary
		uint8_t additionalCycleOp =   (*(Instructions[opcode].operationPtr))();

		cycles += additionalCycleAddr & additionalCycleOp;

	}
	cycles--;
}

//Addressing Mode Functions

uint8_t IMP() {
	fetched = a;
	return 0;
}

uint8_t IMM() {
	address_abs = PC++;
	return 0;
}

uint8_t ZP0() {
	address_abs = read(PC) & 0x00FF; //reads only from the zero page in memory (memory addresses 0x0000 to 0x00FF)
	PC++;
	return 0;
}

uint8_t ZPX() {
	address_abs = (read(PC) + x) & 0x00FF;
	PC++;
	return 0;
}

uint8_t ZPY() {
	address_abs = (read(PC) + y) & 0x00FF;
	PC++;
	return 0;
}

uint8_t ABS() {
	uint16_t LSB = read(PC); //little endian! LSB at lower address
	PC++;
	uint16_t MSB = read(PC);
	PC++;
	address_abs = (MSB << 8) | LSB;
	return 0;
}

uint8_t ABX() {
	uint16_t LSB = read(PC); //little endian! LSB at lower address
	PC++;
	uint16_t MSB = read(PC);
	PC++;
	address_abs = (MSB << 8) | LSB;
	address_abs += x;
	if ((address_abs & 0xFF00) != MSB << 8) {
		return 1; //change of page, add a cycle
	}
	else {
		return 0;
	}
}

uint8_t ABY() {
	uint16_t LSB = read(PC); //little endian! LSB at lower address
	PC++;
	uint16_t MSB = read(PC);
	PC++;
	address_abs = (MSB << 8) | LSB;
	address_abs += y;
	if ((address_abs & 0xFF00) != MSB << 8) {
		return 1; //change of page, add a cycle
	}
	else {
		return 0;
	}
}

uint8_t IND() {
	//read two bytes to form an address...
	uint16_t LSB = read(PC);
	PC++;
	uint16_t MSB = read(PC);
	PC++;

	//...read the two bytes at that address and the next one respectively
	
	uint16_t tempPtr = (MSB << 8) | LSB;
	LSB = read(tempPtr);
	if (LSB == 0x00FF) {
		//this if statement deals w the hardware page boundary bug on the 6502
		uint16_t MSB = read(tempPtr & 0xFF00);
	}
	else {
		tempPtr++;
		uint16_t MSB = read(tempPtr);
	}

	address_abs = (MSB << 8) | LSB;

	return 0;
}

uint8_t IZX() {
	uint16_t tempPtr = read(PC);
	PC++;
	uint16_t LSB = read((tempPtr + (uint16_t) x) & 0x00FF);
	tempPtr++;
	uint16_t MSB = read((tempPtr + (uint16_t) x) & 0x00FF);
	
	address_abs = (MSB << 8) | LSB;

	if ((address_abs & 0xFF00) != MSB << 8) {
		return 1; //change of page, add a cycle
	}
	else {
		return 0;
	}
}

uint8_t IZY() {
	uint16_t tempPtr = read(PC);
	PC++;
	uint16_t LSB = read((tempPtr) & 0x00FF);
	tempPtr++;
	uint16_t MSB = read((tempPtr) & 0x00FF);

	address_abs = ((MSB << 8) | LSB) + y;

	if ((address_abs & 0xFF00) != MSB << 8) {
		return 1; //change of page, add a cycle
	}
	else {
		return 0;
	}
}

uint8_t REL() {
	address_relative = read(PC);
	PC++;
	if (address_relative & 0x80) {
		address_relative |= 0xFF00; //if negative, sign extend so final relative offset is 8 bit signed number
	}
	return 0;
}

//Instruction Functions

uint8_t fetch() {
	if (Instructions[opcode].addrModePtr != IMP) {
		fetched = read(address_abs);
	}
	return fetched;
}

uint8_t AND() {
	a = a & fetch();
	if (a == 0x00) CC[Z] = 1;
	if (a & 0x80) CC[N] = 1;
	return 1;
}

uint8_t BIT() {
	fetched = fetch();
	CC_to_status();
	status &= 0x3F;
	status |= fetched;
	status_to_CC();
	CC[Z] = 0;
	if ((a & fetched) == 0x00) CC[Z] = 1;
	return 0;
}

uint8_t BRK() {
	PC++;
	CC[I] = 1;
	write(0x0100 + SP, (PC >> 8) & 0x00FF);
	SP--;
	write(0x0100 + SP, PC & 0x00FF);
	SP--;

	CC[B] = 1;
	CC_to_status();
	write(0x0100 + SP, status);
	SP--;
	CC[B] = 0;

	PC = (uint16_t)read(0xFFFE) | ((uint16_t)read(0xFFFF) << 8);
	return 0;
}

uint8_t BCS() {
	if (CC[C] == 1) {
		cycles++;
		address_abs = PC + address_relative;

		if ((address_abs & 0xFF00) != (PC & 0xFF00)) {
			cycles++; //change of page
		}
		PC = address_abs;
	}
	return 0;
}

uint8_t BCC() {
	if (CC[C] == 0) {
		cycles++;
		address_abs = PC + address_relative;

		if ((address_abs & 0xFF00) != (PC & 0xFF00)) {
			cycles++; //change of page
		}
		PC = address_abs;
	}
	return 0;
}

uint8_t BEQ() {
	if (CC[Z] == 1) {
		cycles++;
		address_abs = PC + address_relative;

		if ((address_abs & 0xFF00) != (PC & 0xFF00)) {
			cycles++; //change of page
		}
		PC = address_abs;
	}
	return 0;
}

uint8_t BMI() {
	if (CC[N] == 1) {
		cycles++;
		address_abs = PC + address_relative;

		if ((address_abs & 0xFF00) != (PC & 0xFF00)) {
			cycles++; //change of page
		}
		PC = address_abs;
	}
	return 0;
}

uint8_t BNE() {
	if (CC[Z] == 0) {
		cycles++;
		address_abs = PC + address_relative;

		if ((address_abs & 0xFF00) != (PC & 0xFF00)) {
			cycles++; //change of page
		}
		PC = address_abs;
	}
	return 0;
}

uint8_t BPL() {
	if (CC[N] == 0) {
		cycles++;
		address_abs = PC + address_relative;

		if ((address_abs & 0xFF00) != (PC & 0xFF00)) {
			cycles++; //change of page
		}
		PC = address_abs;
	}
	return 0;
}

uint8_t BVC() {
	if (CC[V] == 0) {
		cycles++;
		address_abs = PC + address_relative;

		if ((address_abs & 0xFF00) != (PC & 0xFF00)) {
			cycles++; //change of page
		}
		PC = address_abs;
	}
	return 0;
}

uint8_t BVS() {
	if (CC[V] == 1) {
		cycles++;
		address_abs = PC + address_relative;

		if ((address_abs & 0xFF00) != (PC & 0xFF00)) {
			cycles++; //change of page
		}
		PC = address_abs;
	}
	return 0;
}

uint8_t ASL() {
	if((fetch() << 1) & 0x80) CC[N] = 1;
	if(((fetch() << 1) & 0x00FF) == 0x00) CC[Z] = 1;
	if(((fetch() << 1) & 0xFF00) > 0) CC[C] = 1;
	if (Instructions[opcode].addrModePtr == &IMP) {
		a = (fetch() << 1) & 0x00FF;
	}
	else {
		write(address_abs, (fetch() << 1) & 0x00FF);
	}
	return 0;
}

uint8_t CLC() {
	CC[C] = 0;
	return 0;
}

uint8_t CLD() {
	CC[D] = 0;
	return 0;
}

uint8_t CLI() {
	CC[I] = 0;
	return 0;
}

uint8_t CLV() {
	CC[V] = 0;
	return 0;
}

uint8_t CMP() {
	if (a - fetch() > 0) {
		CC[N] = 0;
		CC[Z] = 0;
		CC[C] = 1;
	}
	if (a - fetch() < 0) {
		CC[N] = 1;
		CC[Z] = 0;
		if (a - fetch() > 0x00FF) {
			CC[C] = 1;
		}
		else {
			CC[C] = 0;
		}
	}
	if (a - fetch() == 0) {
		CC[N] = 0;
		CC[Z] = 1;
		CC[C] = 1;
	}
	return 1;
}

uint8_t CPX() {
	if (x - fetch() > 0) {
		CC[N] = 0;
		CC[Z] = 0;
		CC[C] = 1;
	}
	if (x - fetch() < 0) {
		CC[N] = 1;
		CC[Z] = 0;
		if (a - fetch() > 0x00FF) {
			CC[C] = 1;
		}
		else {
			CC[C] = 0;
		}
	}
	if (x - fetch() == 0) {
		CC[N] = 0;
		CC[Z] = 1;
		CC[C] = 1;
	}
	return 0;
}

uint8_t CPY() {
	if (y - fetch() > 0) {
		CC[N] = 0;
		CC[Z] = 0;
		CC[C] = 1;
	}
	if (y - fetch() < 0) {
		CC[N] = 1;
		CC[Z] = 0;
		if (a - fetch() > 0x00FF) {
			CC[C] = 1;
		}
		else {
			CC[C] = 0;
		}
	}
	if (y - fetch() == 0) {
		CC[N] = 0;
		CC[Z] = 1;
		CC[C] = 1;
	}
	return 0;
}

uint8_t DEC() {
	write(address_abs, (fetch() - 1) & 0x00FF);
	if ((fetch() - 1) == 0x0000) CC[Z] = 1;
	if ((fetch() - 1) & 0x0080) CC[N] = 1;
	return 0;
}

uint8_t DEX() {
	x--;
	if (x == 0x0000) CC[Z] = 1;
	if (x & 0x0080) CC[N] = 1;
	return 0;
}

uint8_t DEY() {
	y--;
	if (x == 0x0000) CC[Z] = 1;
	if (x & 0x0080) CC[N] = 1;
	return 0;
}

uint8_t EOR() {
	a = fetch() ^ a;
	if (a & 0x80) CC[N] = 1;
	if (a == 0x00) CC[Z] = 1;
	return 1;
}

uint8_t INC() {
	write(address_abs, (fetch() + 1) & 0x00FF);
	if ((fetch() + 1) == 0x0000) CC[Z] = 1;
	if ((fetch() + 1) & 0x0080) CC[N] = 1;
	return 0;
}

uint8_t INX() {
	x++;
	if (x == 0x0000) CC[Z] = 1;
	if (x & 0x0080) CC[N] = 1;
	return 0;
}

uint8_t INY() {
	y++;
	if (y == 0x0000) CC[Z] = 1;
	if (y & 0x0080) CC[N] = 1;
	return 0;
}

uint8_t JMP() {
	PC = address_abs; //calculation of new PC to jump to handled by addressing mode functions (ABS and REL)
	return 0;
}

uint8_t JSR() {
	//push current PC to stack (have to decrement bc PC incremented at beginning of instruction)
	PC--; 
	write(0x0100 + SP, (PC >> 8) & 0x00FF);
	SP--;
	write(0x0100 + SP, PC & 0x00FF);
	SP--; 

	PC = address_abs; //same as JMP
	return 0;
}

uint8_t LDA() {
	a = fetch();
	if (a & 0x80) CC[N] = 1;
	if (a == 0x00) CC[Z] = 1;
	return 1;
}

uint8_t LDX() {
	x = fetch();
	if (x & 0x80) CC[N] = 1;
	if (x == 0x00) CC[Z] = 1;
	return 1;
}

uint8_t LDY() {
	y = fetch();
	if (y & 0x80) CC[N] = 1;
	if (y == 0x00) CC[Z] = 1;
	return 1;
}

uint8_t LSR() {
	if ((fetch() >> 1) & 0x80) CC[N] = 1;
	if (((fetch() >> 1) & 0x00FF) == 0x00) CC[Z] = 1;
	if ((fetch() >> 1) & 0x0001) CC[C] = 1;
	if (Instructions[opcode].addrModePtr == &IMP) {
		a = (fetch() >> 1) & 0x00FF;
	}
	else {
		write(address_abs, (fetch() >> 1) & 0x00FF);
	}
	return 0;
}

uint8_t NOP() {
	return 0;
}

uint8_t ORA() {
	a = a | fetch();
	if (a == 0x00) CC[Z] = 1;
	if (a & 0x80) CC[N] = 1;
	return 1;
}



uint8_t ADC() {
	uint16_t temp = (uint16_t)a + (uint16_t)fetch() + (uint16_t)CC[C];
	if (temp > 0xFF) CC[C] = 1;
	if ((temp & 0x00FF) == 0) CC[Z] = 1;
	if (temp & 0x80) CC[N] = 1;
	if ((~((uint16_t)a ^ (uint16_t)fetch()) & ((uint16_t)a ^ (uint16_t)temp)) & 0x80) CC[V] = 1;
	a = temp & 0x00FF;
	return 1;
}

uint8_t SBC() {
	fetched = fetch();
	fetched = !fetched;
	fetched++; //invert data, then same as addition
	uint16_t temp = (uint16_t)a + (uint16_t)fetched + (uint16_t)CC[C];
	if (temp > 0xFF) CC[C] = 1;
	if ((temp & 0x00FF) == 0) CC[Z] = 1;
	if (temp & 0x80) CC[N] = 1;
	if ((~((uint16_t)a ^ (uint16_t)fetched) & ((uint16_t)a ^ (uint16_t)temp)) & 0x80) CC[V] = 1;
	a = temp & 0x00FF;
	return 1;
}

uint8_t PHA() {
	write(0x0100 + SP, a); //0x0100 is hardware assigned start of stack
	SP--;
	return 0;
}

uint8_t PHP() {
	CC[B] = 1;
	CC_to_status();
	write(0x0100 + SP, status); //0x0100 is hardware assigned start of stack
	CC[B] = 0;
	CC[U] = 0;
	SP--;
	return 0;
}

uint8_t PLA() {
	SP++;
	read(0x0100 + SP);
	if (a == 0x00) CC[Z] = 1;
	if (a & 0x80) CC[N] = 1;
	return 0;
}

uint8_t PLP() {
	SP++;
	status = read(0x0100 + SP);
	status_to_CC();
	CC[U] = 1;
	if (a == 0x00) CC[Z] = 1;
	if (a & 0x80) CC[N] = 1;
	return 0;
}

uint8_t ROL() {
	if (Instructions[opcode].addrModePtr == &IMM) {
		uint8_t temp = a & 0x80;
		a = a << 1;
		a = (a | (temp >> 7)) & 0x00FF;
		if (a == 0x00) CC[Z] = 1;
		if (a & 0x80) CC[N] = 1;
		if (temp != 0x00) CC[C] = 1;
	}
	else {
		uint8_t temp = fetch() & 0x80;
		fetched = fetch() << 1;
		fetched = fetched | (temp >> 7);
		write(address_abs, fetched & 0x00FF);
		if (fetched == 0x00) CC[Z] = 1;
		if (fetched & 0x80) CC[N] = 1;
		if (temp != 0x00) CC[C] = 1;
	}
	return 0;
}

uint8_t ROR() {
	if (Instructions[opcode].addrModePtr == &IMM) {
		uint8_t temp = a & 0x01;
		a = a >> 1;
		a = (a | (temp << 7)) & 0x00FF;
		if (a == 0x00) CC[Z] = 1;
		if (a & 0x80) CC[N] = 1;
		if (temp != 0x00) CC[C] = 1;
	}
	else {
		uint8_t temp = fetch() & 0x01;
		fetched = fetch() >> 1;
		fetched = fetched | (temp << 7);
		write(address_abs, fetched & 0x00FF);
		if (fetched == 0x00) CC[Z] = 1;
		if (fetched & 0x80) CC[N] = 1;
		if (temp != 0x00) CC[C] = 1;
	}
	return 0;
}

uint8_t SEC() {
	CC[C] = 1;
	return 0;
}

uint8_t SED() {
	CC[D] = 1;
	return 0;
}

uint8_t SEI() {
	CC[I] = 1;
	return 0;
}

uint8_t STA() {
	write(address_abs, a);
	return 0;
}

uint8_t STX() {
	write(address_abs, x);
	return 0;
}

uint8_t STY() {
	write(address_abs, y);
	return 0;
}

uint8_t TAX() {
	x = a;
	if (x == 0x00) CC[Z] = 1;
	if (x & 0x80) CC[N] = 1;
	return 0;
}

uint8_t TAY() {
	y = a;
	if (y == 0x00) CC[Z] = 1;
	if (y & 0x80) CC[N] = 1;
	return 0;
}

uint8_t TSX() {
	x = SP;
	if (x == 0x00) CC[Z] = 1;
	if (x & 0x80) CC[N] = 1;
	return 0;
}

uint8_t TXA() {
	a = x;
	if (a == 0x00) CC[Z] = 1;
	if (a & 0x80) CC[N] = 1;
	return 0;
}

uint8_t TYA() {
	a = y;
	if (a == 0x00) CC[Z] = 1;
	if (a & 0x80) CC[N] = 1;
	return 0;
}

uint8_t TXS() {
	SP = x;
	return 0;
}

uint8_t ILL() {
	return 0;
}

//special system functions

void reset() {
	a = 0x00;
	x = 0x00;
	y = 0x00;
	SP = 0xFD;
	status = 0x00 | (CC[U] << 5);

	address_abs = 0xFFFC;
	PC = read(address_abs + 1) << 8 | read(address_abs);

	address_abs = 0x0000;
	address_relative = 0x0000;
	fetched = 0x00;

	cycles = 8;
}

void irq() {
	if (!CC[I]) {
		write(0x0100 + SP, (PC >> 8) & 0x00FF);
		SP--;
		write(0x0100 + SP, (PC & 0x00FF));
		SP--;
		CC[B] = 0;
		CC[U] = 1;
		CC[I] = 1;
		CC_to_status();
		write(0x0100 + SP, status);
		SP--;

		address_abs = 0xFFFE;
		PC = ((read(address_abs + 1) << 8) | read(address_abs));

		cycles = 7;
	}
}

void nmi() {
	write(0x0100 + SP, (PC >> 8) & 0x00FF);
	SP--;
	write(0x0100 + SP, (PC & 0x00FF));
	SP--;
	CC[B] = 0;
	CC[U] = 1;
	CC[I] = 1;
	CC_to_status();
	write(0x0100 + SP, status);
	SP--;

	address_abs = 0xFFFE;
	PC = ((read(address_abs + 1) << 8) | read(address_abs));

	cycles = 7;
}

uint8_t RTI() {
	SP++;
	status = read(0x0100 + SP);
	status_to_CC();
	CC[B] = 0;
	CC[U] = 0;
	SP++;

	PC = (uint16_t)read(0x0100 + SP);
	SP++;
	PC |= (uint16_t)read(0x0100 + SP) << 8;
	return 0;
}

uint8_t RTS() {
	SP++;
	uint16_t LSB = read(0x0100 + SP);
	SP++;
	uint16_t MSB = read(0x0100 + SP);
	PC = (MSB << 8) | LSB;
	PC++;
	return 0;
}


