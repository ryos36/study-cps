#include <stdio.h>
#include <stddef.h>
#include "hvm-engine.h"

int
main()
{
    Operand operand;

    operand.i = 0x12345678;
    printf("operand size:%d\n", (int)sizeof(operand));
    printf("operand offsetof\n");
    printf("	flag:%d\n", (int)offsetof(Operand, flag));
    printf("	r0:%d\n", (int)offsetof(Operand, r0));
    printf("	r1:%d\n", (int)offsetof(Operand, r1));
    printf("	r2:%d\n", (int)offsetof(Operand, r2));
    printf("operand <= 0x%08x\n", (int)operand.i);
    printf("    flag:0x%02x\n", operand.flag);
    printf("    r0:0x%02x\n", operand.r0);
    printf("    r1:0x%02x\n", operand.r1);
    printf("    r2:0x%02x\n", operand.r2);
}
