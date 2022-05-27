#pragma once
#include <stdint.h>
#include "6502.h"

uint8_t RAM[64 * 1024];

void write(uint16_t address, uint8_t data);

uint8_t read(uint16_t address);