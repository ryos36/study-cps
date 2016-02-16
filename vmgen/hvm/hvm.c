#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/fcntl.h>
#include <sys/mman.h>
#include <assert.h>
#include "hvm-engine.h"
#include "hvm-peephole.h"
#include "vm_code.h"
#include <alloca.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>

FILE *vm_out;
int vm_debug = 1;
extern Cell *inst;
extern Cell *stack;

void
Usage()
{
    fprintf(stderr, "Usage:hvm <vmb file>\n");
}

//----------------------------------------------------------------
#define INST_SIZE 4096
#define STACK_SIZE 4096
//----------------------------------------------------------------
struct name_to_label *the_name_to_label;

struct name_to_label *
search_label(char *name)
{
    struct name_to_label *n2lp = the_name_to_label;

#ifdef DEBUG
    fprintf(stderr, "name %s\n", name);
#endif

    while (n2lp->name) {
        if ( strcmp(n2lp->name, name) == 0 ) {
            return n2lp;
        }
        ++n2lp;
    }
    assert(0);
}

//----------------------------------------------------------------
int
build_32bit_vmb_structure(char *file)
{
    int fds;
    uint32_t *vm_code, *ui32p;
    int file_size;
    struct stat stat;
    fds = open(file, O_RDONLY);

    if ( fds < 0 ) {
        return errno;
    }

    if ( fstat(fds, &stat) < 0 ) {
        close(fds);
        return errno;
    }

    if (!S_ISREG(stat.st_mode)) {
        close(fds);
        return EINVAL;
    }

    file_size = stat.st_size;

    vm_code = (uint32_t *)mmap(0, file_size, PROT_READ, MAP_PRIVATE, fds, 0);
#ifdef DEBUG
    fprintf(stderr, "vm_code = %p\n", vm_code);
#endif
    struct vm_header *hp;
    hp = (struct vm_header *)vm_code;
    
    assert(hp->magic_id == MAGIC_ID);
#ifdef DEBUG
    for( int i = 0; i < 12 ; ++i ) {
        static const char *name[] = {
            "magic_id", "header_size",
            "codes_offset", "codes_size",
            "label_offset_pos_offset", "label_offset_pos_size",
            "address_pos_offset", "address_pos_size",
            "insn_pos_offset", "insn_pos_size",
            "insn_declare_offset", "insn_declare_size"
        };
        fprintf(stderr, "%s = 0x%08x\n", name[i], *(vm_code + i));
    }
#endif

    ui32p = &vm_code[hp->codes_offset / sizeof(uint32_t)];

    for( int i = 0 ; i < hp->codes_size / sizeof(uint32_t); ++i, ++ui32p) {
        inst[i].i = (unsigned long) *ui32p;
    }

    ui32p = &vm_code[hp->address_pos_offset / sizeof(uint32_t)];
    for( int i = 0 ; i < hp->address_pos_size / sizeof(uint32_t); ++i, ++ui32p) {
        inst[*ui32p].target = &inst[inst[*ui32p].i];
    }

    ui32p = &vm_code[hp->insn_declare_offset / sizeof(uint32_t)];

#ifdef DEBUG
    for( int i = 0 ; i < 16 ; i++ ) {
        fprintf(stderr, "%x\n", ui32p[i]);
    }
#endif

    int name_size = (int)(*ui32p);
    char **name = alloca(name_size * sizeof(char *));
#ifdef DEBUG
    fprintf(stderr, "size = %d\n", *ui32p);
#endif
    ui32p++;
    for( int i = 0 ; i < name_size ; ++i ) {
        struct np {
            uint16_t no;
            uint16_t size;

            char name[0];
        } __attribute__((packed)) *np = (struct np *)ui32p;
        uint16_t size4 = (np->size + 3) & ~3;

#ifdef DEBUG
        fprintf(stderr, "%d %d %s\n", (int)np->size, i, np->name);
#endif
        assert(np->no == i);
        name[i] = &np->name[0];

        ui32p = (uint32_t *)&(np->name[size4]);
    }

#ifdef DEBUG
    for( int i = 0 ; i < name_size ; ++i ) {
        fprintf(stderr, "name[%d]:%s\n", i, name[i]);
    }
#endif

    assert(the_name_to_label);
    ui32p = &vm_code[hp->insn_pos_offset / sizeof(uint32_t)];
    for( int i = 0 ; i < hp->insn_pos_size / sizeof(uint32_t); ++i, ++ui32p) {
        struct name_to_label *hitp;
        hitp = search_label(name[inst[*ui32p].i]);
        inst[*ui32p].inst = hitp->label;
#ifdef DEBUG
        fprintf(stderr, "code[%d] => %llx\n", *ui32p, inst[*ui32p].i);
#endif
    }

    close(fds);
    munmap(vm_code, file_size);
    return 0;
}


//----------------------------------------------------------------
int
main(int argc, char **argv)
{
    vm_out = stderr;
    int rv;

    if ( argc != 2 ) {
        Usage();
        return EINVAL;
    }

    inst = (Inst *)calloc(INST_SIZE, sizeof(Inst));
    stack = (Cell *)calloc(STACK_SIZE, sizeof(Cell));

    engine(0, 0, 0);

    rv = build_32bit_vmb_structure(argv[1]);
    if ( rv != 0 ) {
        Usage();
        return rv;
    }

    engine(inst, &stack[STACK_SIZE - 1], 0);

    return 0;
}

