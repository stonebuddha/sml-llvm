#include "llvm-c/Core.h"
#include "llvm-c/BitWriter.h"

/* LlvmCore.llmodule * string -> bool */
extern "C"
int llvm_write_bitcode_file(LLVMModuleRef M, const char *Path) {
  int Result = LLVMWriteBitcodeToFile(M, Path);
  return (Result == 0);
}

/* LlvmCore.llmodule -> LlvmCore.llmemorybuffer */
extern "C"
LLVMMemoryBufferRef llvm_write_bitcode_to_memory_buffer(LLVMModuleRef M) {
  return LLVMWriteBitcodeToMemoryBuffer(M);
}
