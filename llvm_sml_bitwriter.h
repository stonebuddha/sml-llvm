#ifndef LLVM_SML_BITWRITER_H
#define LLVM_SML_BITWRITER_H

#include "llvm_sml_types.h"

int llvm_write_bitcode_file(LLVMModuleRef, const char *);
LLVMBool llvm_write_bitcode_to_fd(LLVMBool, LLVMModuleRef, unsigned long);
LLVMMemoryBufferRef llvm_write_bitcode_to_memory_buffer(LLVMModuleRef);

#endif
