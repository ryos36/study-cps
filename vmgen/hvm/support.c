#include <stdio.h>
#include "hvm-engine.h"

extern FILE *vm_out;

void
printarg_operand(Operand operand)
{
    fprintf(vm_out, "%lx", operand.i);
}

void
printarg_imm(Imm imm)
{
    fprintf(vm_out, "%lx", imm);
}

void
printarg_s(Cell s)
{
    fprintf(vm_out, "%lx", s.i);
}
