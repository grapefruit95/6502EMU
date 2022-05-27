#include <stdint.h>
#include "bus.h"

void clearRAM() {
	for (int i = 0; i < sizeof(RAM); i++) {
		RAM[i] = 0x00;
	}
}

void write(uint16_t address, uint8_t data) {
	if (address >= 0x0000 && address <= 0xFFFF) {
		RAM[address] = data;
	}
}

uint8_t read(uint16_t address) {
	if (address >= 0x0000 && address <= 0xFFFF) {
		return RAM[address];
	}
	else {
		return 0;
	}
}