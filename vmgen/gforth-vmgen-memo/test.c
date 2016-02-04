#include <stdio.h>

typedef void *Label;
#define INST_ADDR(name) (Label)&&I_##name
#define LABEL(name) I_##name:


int
main()
{
    Label lp;

static Label labels[] = {
    INST_ADDR(label0),
    INST_ADDR(label1),
    INST_ADDR(label2),
    INST_ADDR(label3),
    (void *)&labels[1]
};

    printf("%x\n", labels[1]);
    lp = labels[1];
    goto *lp;

LABEL(label0);
    printf("label0:%x\n", labels[0]);

    return 0;

#if 1
LABEL(label1);
    printf("label1:%x\n", labels[1]);
    goto *labels[2];

LABEL(label2);
    printf("label2:%x\n", labels[2]);
    goto *labels[3];

LABEL(label3);
    printf("label3:%x\n", labels[3]);
    goto *labels[0];
#endif


}

