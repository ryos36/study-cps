#ifndef __HVM_PEEPHOLE_H__
#define __HVM_PEEPHOLE_H__

#include "hvm-engine.h"

void gen_inst(Inst **vmcodepp, Label i);
void genarg_operand(Inst **vmcodepp, Operand operand);
void genarg_imm(Inst **vmcodepp, Imm imm);
void genarg_uint32_t(Inst **vmcodepp, uint32_t v);

#endif /* __HVM_PEEPHOLE_H__ */
