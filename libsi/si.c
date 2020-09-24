#include <stdint.h>

#define SISTATEBASE 0xD0001000
#define SISTATEADDRESS(sigrp,si) (SISTATEBASE + ((sigrp) << 7) + ((si) << 2))
#define SSM_ENTRY(isavailable, vlcwaddress, vlcwlength) (((isavailable) << 17) | (vlcwaddress) << 8 | (vlcwlength))
#define VLCW_START_ADDR 0xD0100000 // proFPGA

#define WORDS_IN_VLCW_LINE 32

#define si_words_per_vlcw 27

volatile unsigned int* si_state_mem;
static unsigned int si_vlcws[][si_words_per_vlcw] = {
	{0x4,0xFFEEFFFF,0xFFFF,0xFFFF8000,0xFFFFFFFF,0xFFFF,0xFFFF8000,0x0,0x0,0x0,0x0,0xFFEEFFFF,0xFFFF,0x0,0xFFFFFFFF,0xFFFF,0x0,0xFFFFF6FF,0x100000,0xFFFFF6FF,0x100000,0xFFFFF6FF,0x100000,0xFFFFF6FF,0x100000,0xFFFFFF20,0x302040},
	{0x4,0xFFFFFFFF,0xFFEE,0xFFFF8000,0xFFFFFFFF,0xFFFF,0xFFFF8000,0x0,0x0,0x0,0x0,0xFFFFFFFF,0xFFEE,0x0,0xFFFFFFFF,0xFFFF,0x0,0xFF6FFFFF,0x100000,0xFF6FFFFF,0x100000,0xFF6FFFFF,0x100000,0xFF6FFFFF,0x100000,0xFC8FFFFF,0x100000}
};

int si_num_steps = 2;

void si_state_memory_update(unsigned int si_grp, unsigned int si_id, unsigned char si_active, unsigned int si_vlcw_start, unsigned int si_vlcw_length) {
        si_state_mem = (unsigned int *)SISTATEADDRESS(si_grp, si_id);
        *(si_state_mem) = SSM_ENTRY(si_active, si_vlcw_start, si_vlcw_length);
}

void load_si_micro_program(unsigned int num_steps, unsigned int words_per_vlcw, unsigned int words_in_vlcw_line, unsigned int *micro_program) {
        int i, j;
        volatile unsigned int *vlcw_mem = (unsigned int *)VLCW_START_ADDR;
        for (i = 0; i < num_steps; ++i) {
                for (j = 0; j < words_per_vlcw; ++j) {
                        *vlcw_mem++ = micro_program[i*words_per_vlcw+j];
                }
                vlcw_mem += words_in_vlcw_line - words_per_vlcw;
        }
}

void init_si(uint32_t si_opcode) {
	load_si_micro_program(si_num_steps, si_words_per_vlcw, WORDS_IN_VLCW_LINE, si_vlcws[0]);
	si_state_memory_update(0, si_opcode, 1, 0, si_num_steps); //The number five was arbitrarily chosen
}
