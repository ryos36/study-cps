#include <stdio.h>
#include "hvm-engine.h"
#include "hvm-peephole.h"

void
gen_inst(Inst **vmcodepp, Label i)
{
    (**vmcodepp).inst = i;
    (*vmcodepp)++;
}

//----------------------------------------------------------------

void
genarg_operand(Inst **vmcodepp, Operand operand)
{
    vm_operand2Cell(operand, *((Cell*) *vmcodepp));
    (*vmcodepp)++;
}

//----------------------------------------------------------------

void
genarg_imm(Inst **vmcodepp, Imm imm)
{
    vm_imm2Cell(imm, *((Cell*) *vmcodepp));
    (*vmcodepp)++;
}

//----------------------------------------------------------------
void
genarg_uint32_t(Inst **vmcodepp, uint32_t v)
{
    ((Cell*) *vmcodepp)->i = v;
    (*vmcodepp)++;
}
