#include "llvm_sml_executionengine.h"
#include <assert.h>
#include <string.h>
#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>

/* unit -> bool */
LLVMBool llvm_ee_initialize(void) {
  LLVMLinkInMCJIT();

  return (!LLVMInitializeNativeTarget() && !LLVMInitializeNativeAsmParser() && !LLVMInitializeNativeAsmPrinter());
}

/* llcompileroptions option * llmodule -> llexecutionengine */
LLVMExecutionEngineRef llvm_ee_create(int *OptLevel, int *CodeModel, LLVMBool *NoFramePointerElim, LLVMBool *EnableFastISel, LLVMModuleRef M) {
  LLVMExecutionEngineRef MCJIT;
  char *Error;
  struct LLVMMCJITCompilerOptions Options;

  LLVMInitializeMCJITCompilerOptions(&Options, sizeof(Options));
  if (OptLevel != NULL) {
    Options.OptLevel = *OptLevel;
  }
  if (CodeModel != NULL) {
    Options.CodeModel = *CodeModel;
  }
  if (NoFramePointerElim != NULL) {
    Options.NoFramePointerElim = *NoFramePointerElim;
  }
  if (EnableFastISel != NULL) {
    Options.EnableFastISel = *EnableFastISel;
  }

  if (LLVMCreateMCJITCompilerForModule(&MCJIT, M, &Options, sizeof(Options), &Error)) {
    /* TODO: throw an exception */
    assert(0);
  }

  return MCJIT;
}

/* llexecutionengine -> unit */
void llvm_ee_dispose(LLVMExecutionEngineRef EE) {
  LLVMDisposeExecutionEngine(EE);
}

/* llmodule * llexecutionengine -> unit */
void llvm_ee_add_module(LLVMModuleRef M, LLVMExecutionEngineRef EE) {
  LLVMAddModule(EE, M);
}

/* llmodule * llexecutionengine -> llmodule */
LLVMModuleRef llvm_ee_remove_module(LLVMModuleRef M, LLVMExecutionEngineRef EE) {
  LLVMModuleRef RemovedModule;
  char *Error;
  if (LLVMRemoveModule(EE, M, &RemovedModule, &Error)) {
    /* TODO: throw an exception */
    assert(0);
  }
  return RemovedModule;
}

/* llexecutionengine -> unit */
void llvm_ee_run_static_ctors(LLVMExecutionEngineRef EE) {
  LLVMRunStaticConstructors(EE);
}

/* llexecutionengine -> unit */
void llvm_ee_run_static_dtors(LLVMExecutionEngineRef EE) {
  LLVMRunStaticDestructors(EE);
}

/* llexecutionengine -> DataLayout.t */
LLVMTargetDataRef llvm_ee_get_data_layout(LLVMExecutionEngineRef EE) {
  LLVMTargetDataRef OrigDataLayout;
  char *TargetDataCStr;

  OrigDataLayout = LLVMGetExecutionEngineTargetData(EE);
  TargetDataCStr = LLVMCopyStringRepOfTargetData(OrigDataLayout);
  LLVMDisposeMessage(TargetDataCStr);

  return OrigDataLayout;
}

/* llvalue * 'a C.ptr * llexecutionengine -> unit */
void llvm_ee_add_global_mapping(LLVMValueRef Global, void *Ptr, LLVMExecutionEngineRef EE) {
  LLVMAddGlobalMapping(EE, Global, Ptr);
}

/* string * 'a C.ptr C.T.Typ * llexecutionengine -> 'a C.ptr */
void *llvm_ee_get_global_value_address(const char *Name, LLVMExecutionEngineRef EE) {
  return (void *) LLVMGetGlobalValueAddress(EE, Name);
}

/* string * ('a -> 'b) C.fptr C.T.type * llexecutionengine -> 'a -> 'b */
void *llvm_ee_get_function_address(const char *Name, LLVMExecutionEngineRef EE) {
  return (void *) LLVMGetFunctionAddress(EE, Name);
}
