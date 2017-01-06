#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "llvm-c/Core.h"
#include "llvm-c/Support.h"
#include "export.h"

LLVMContextRef llvm_create_context(void) {
  return LLVMContextCreate();
}

void llvm_dispose_context(LLVMContextRef C) {
  LLVMContextDispose(C);
}

LLVMContextRef llvm_global_context(void) {
  return LLVMGetGlobalContext();
}

Int32 llvm_mdkind_id(LLVMContextRef C, Pointer Name) {
  unsigned MDKindID = LLVMGetMDKindIDInContext(C, (char *) Name, strlen((char *) Name));
  return (Int32) MDKindID;
}

LLVMModuleRef llvm_create_module(LLVMContextRef C, Pointer ModuleID) {
  return LLVMModuleCreateWithNameInContext((char *) ModuleID, C);
}

void llvm_dispose_module(LLVMModuleRef M) {
  LLVMDisposeModule(M);
}

void llvm_dump_module(LLVMModuleRef M) {
  LLVMDumpModule(M);
}
