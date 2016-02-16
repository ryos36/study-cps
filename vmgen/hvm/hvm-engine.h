#ifndef __HVM_ENGINE_H__
#define __HVM_ENGINE_H__

#include <stdio.h>
#include <stdint.h>

typedef void *Label;
typedef union Cell {
    unsigned long i;
    union Cell *target;
    Label inst;
    //char *a;
} Cell, Inst;

typedef union Operand {
    long i;
    union Cell *target;
    struct { // little endian
        unsigned char r2;
        unsigned char r1;
        unsigned char r0;
        unsigned char flag;
    };
} Operand;

enum {
    TYPE_REG   = 0,
    TYPE_IMM8  = 1,
    TYPE_IMM32 = 2
    //TYPE_ADDRESS = 3
};

enum {
    MAX_REG_N = 16,
    MAX_REG_N_MASK = 15,

    MAX_HEAP_OFFSET_N = 255,
    MAX_HEAP_OFFSET_N_MASK = 255,
};

typedef long Imm;

#define vm_Cell2operand(_cell, _o) ((_o).i=(_cell).i)
#define vm_operand2Cell(_o, _cell) ((_cell).i=(_o).i)
#define vm_Cell2imm(_cell, _imm) ((_imm)=(_cell).i)
#define vm_imm2Cell(_imm, _cell) ((_cell).i=(_imm))
#define vm_Cell2s(_cell, _s) ((_s).i=(_cell).i)
#define vm_s2Cell(_s, _cell) ((_cell).i=(_s).i)

#define IMM_ARG(access,value) access

extern FILE *vm_out;
extern int vm_debug;
extern Label *vm_prim;

#define IF_spTOS(x) x


struct name_to_label {
    const char *name;
    Label label;
};


#define NEXT ({DEF_CA NEXT_P1; NEXT_P2;})
#define IPTOS ((NEXT_INST))
#define INST_ADDR(name) { #name, (Label)&&I_##name }
#define LABEL(name) I_##name:
#define LABEL2(x)

#define INSTRUCTION(name) (Inst)(&&I_##name)

#ifdef VM_DEBUG
#define NAME(_x) if (vm_debug) {fprintf(vm_out, "[%d]: %-10s, ", (ip-1)-ip0, _x); fprintf(vm_out,"sp=%p", sp);}
#else
#define NAME(_x)
#endif

#define NEXT_P0
#define IP (ip)
#define SET_IP(p) ({ip=(p); NEXT_P0;})
#define NEXT_INST (*ip)
#define INC_IP(const_inc) ({ip+=(const_inc);})
#define DEF_CA
#ifdef VM_DEBUG
#define NEXT_P1 usleep(100000);
#define NEXT_P2 ({fprintf(vm_out, "cfa.inst:%p ip:%p\n", (ip)->inst, ip); cfa=*ip++; goto *(cfa.inst);})
#else
#define NEXT_P1
#define NEXT_P2 ({cfa=*ip++; goto *(cfa.inst);})
#endif /* VM_DEBUG */

#ifdef VM_PROFILING
#define SUPER_END  vm_count_block(IP)
#else
#define SUPER_END
#endif

int engine(Cell *ip0, Cell *sp, char *fp);

void printarg_operand(Operand operand);
void printarg_imm(Imm imm);

#endif /* __HVM_ENGINE_H__ */
