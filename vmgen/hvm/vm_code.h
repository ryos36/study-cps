#ifndef __VM_CODE_H__
#define __VM_CODE_H__

#include <stdint.h>

// little endian
struct vm_header {
    uint32_t magic_id;
    uint32_t header_size;

    uint32_t codes_offset;
    uint32_t codes_size;

    uint32_t label_offset_pos_offset;
    uint32_t label_offset_pos_size;

    uint32_t address_pos_offset;
    uint32_t address_pos_size;

    uint32_t insn_pos_offset;
    uint32_t insn_pos_size;

    uint32_t insn_declare_offset;
    uint32_t insn_declare_size;
};

//#define MAGIC_ID 0x434d5600
#define MAGIC_ID 0x00564d43

#endif /* __VM_CODE_H__ */
