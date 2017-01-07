#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "llvm-c/Core.h"
#include "llvm-c/Support.h"

/*===-- Utils -------------------------------------------------------------===*/

static const char *copy_string(const char *src) {
  char *des = (char *) malloc(sizeof(char) * (strlen(src) + 1));
  strcpy(des, src);
  return des;
}

/*===-- Contexts ----------------------------------------------------------===*/

/* unit -> llcontext */
extern "C"
LLVMContextRef llvm_create_context(void) {
  return LLVMContextCreate();
}

/* llcontext -> unit */
extern "C"
void llvm_dispose_context(LLVMContextRef C) {
  LLVMContextDispose(C);
}

/* unit -> llcontext */
extern "C"
LLVMContextRef llvm_global_context(void) {
  return LLVMGetGlobalContext();
}

/* llcontext * string -> int */
extern "C"
int llvm_mdkind_id(LLVMContextRef C, const char *Name) {
  int MDKindID = LLVMGetMDKindIDInContext(C, Name, strlen(Name));
  return MDKindID;
}

/*===-- Modules -----------------------------------------------------------===*/

/* llcontext * string -> llmodule */
extern "C"
LLVMModuleRef llvm_create_module(LLVMContextRef C, const char *ModuleID) {
  return LLVMModuleCreateWithNameInContext(ModuleID, C);
}

/* llmodule -> unit */
extern "C"
void llvm_dispose_module(LLVMModuleRef M) {
  LLVMDisposeModule(M);
}

/* llmodule -> string */
extern "C"
const char *llvm_target_triple(LLVMModuleRef M) {
  return copy_string(LLVMGetTarget(M));
}

/* string * llmodule -> unit */
extern "C"
void llvm_set_target_triple(const char *Trip, LLVMModuleRef M) {
  LLVMSetTarget(M, Trip);
}

/* llmodule -> string */
extern "C"
const char *llvm_data_layout(LLVMModuleRef M) {
  return copy_string(LLVMGetDataLayout(M));
}

/* string * llmodule -> unit */
extern "C"
void llvm_set_data_layout(const char *Layout, LLVMModuleRef M) {
  LLVMSetDataLayout(M, Layout);
}

/* llmodule -> unit */
extern "C"
void llvm_dump_module(LLVMModuleRef M) {
  LLVMDumpModule(M);
}

/* string * llmodule -> unit */
extern "C"
void llvm_print_module(const char *Filename, LLVMModuleRef M) {
  char *Message;

  if (LLVMPrintModuleToFile(M, Filename, &Message)) {
    /* TODO: throw an exception */
  }
}

/* llmodule -> string */
extern "C"
const char *llvm_string_of_llmodule(LLVMModuleRef M) {
  const char *ModuleStr;
  char *ModuleCStr;

  ModuleCStr = LLVMPrintModuleToString(M);
  ModuleStr = copy_string(ModuleStr);
  LLVMDisposeMessage(ModuleCStr);

  return ModuleStr;
}

/* llmodule * string -> unit */
extern "C"
void llvm_set_module_inline_asm(LLVMModuleRef M, const char *Asm) {
  LLVMSetModuleInlineAsm(M, Asm);
}

/* llmodule -> llcontext */
extern "C"
LLVMContextRef llvm_get_module_context(LLVMModuleRef M) {
  return LLVMGetModuleContext(M);
}
