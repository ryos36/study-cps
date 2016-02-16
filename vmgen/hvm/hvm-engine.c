#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include "hvm-engine.h"
#include <unistd.h> // for sleep/usleep

#define min(a, b) (((a)<(b))?(a):(b))

#define CHECK_REG_NO(__reg_no__) assert((0 <= __reg_no__) && ( __reg_no__ < MAX_REG_N))

Cell *inst;
Cell *stack;
extern struct name_to_label *the_name_to_label;

//----------------------------------------------------------------
#define PUSH_REG(__cell_reg_no__) { sp--; sp[0].i = (regs[(__cell_reg_no__).i]); }
#define PUSH_IMM32(__cell_imm32__) { sp--; sp[0].i = ((__cell_imm32__).i); }
#define PUSH_LABEL(__cell_label_pos__) { sp--; sp[0].target = (&ip[(__cell_label_pos__).i]); }

//----------------------------------------------------------------
int
engine(Cell *ip0, Cell *sp, char *fp)
{
    Cell *ip;
    Cell cfa;
    Cell spTOS;
    long regs[MAX_REG_N];
    int flag = 0;

    static struct name_to_label name_to_label[] = {
#include "hvm-labels.i"
        {0, 0}
    };

    for( int i = 0 ; i < MAX_REG_N ; i++ ) {
        regs[i] = i;
    }

    vm_out = stderr;

    if ( ip0 == 0 ) {
        the_name_to_label = name_to_label;
        return 0;
    }

    spTOS = sp[0];
    SET_IP(ip0);
    SUPER_END;

    NEXT;

#include "hvm-vm.i"
}
