TARGET = hvm
OPTS= -O0 -ggdb -DVM_DEBUG
OPTS= -O3
CFLAGS = $(OPTS) -fomit-frame-pointer -Wall -std=c99
LDFLAGS = 
VMGEN = vmgen

MK_DIR=mk
include  $(MK_DIR)/c.mk

SOURCES_C = hvm.c hvm-engine.c hvm-peephole.c support.c
CLEAN_OBJECTS = hvm*.i

hvm: $(OBJECTS)

hvm.o:hvm-engine.h hvm-gen.i
hvm-engine.o:hvm-engine.h hvm-labels.i hvm-vm.i
hvm-peephole.o:hvm-peephole.h
support.o:hvm-engine.h

hvm-disasm.i hvm-gen.i hvm-labels.i hvm-peephole.i hvm-profile.i hvm-vm.i: hvm.vmg
	$(VMGEN) hvm.vmg
