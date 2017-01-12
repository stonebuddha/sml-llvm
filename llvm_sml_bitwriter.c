#include "llvm_sml_bitwriter.h"
#include <llvm-c/Core.h>
#include <llvm-c/BitWriter.h>

/* llmodule * string -> bool */
LLVMBool llvm_write_bitcode_file(LLVMModuleRef M, const char *Path) {
  int Result = LLVMWriteBitcodeToFile(M, Path);
  return (Result == 0);
}

/* llmodule -> llmemorybuffer */
LLVMMemoryBufferRef llvm_write_bitcode_to_memory_buffer(LLVMModuleRef M) {
  return LLVMWriteBitcodeToMemoryBuffer(M);
}
