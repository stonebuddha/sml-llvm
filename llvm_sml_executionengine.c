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

/* lltype * Int64.int * bool -> llgenericvalue */
LLVMGenericValueRef llvm_create_generic_value_of_int(LLVMTypeRef Ty, unsigned long long N, LLVMBool IsSigned) {
  return LLVMCreateGenericValueOfInt(Ty, N, IsSigned);
}

/* C.voidptr -> llgenericvalue */
LLVMGenericValueRef llvm_create_generic_value_of_pointer(void *P) {
  return LLVMCreateGenericValueOfPointer(P);
}

/* lltype * real -> llgenericvalue */
LLVMGenericValueRef llvm_create_generic_value_of_float(LLVMTypeRef Ty, double N) {
  return LLVMCreateGenericValueOfFloat(Ty, N);
}

/* llgenericvalue -> int */
unsigned llvm_generic_value_int_width(LLVMGenericValueRef GenValRef) {
  return LLVMGenericValueIntWidth(GenValRef);
}

/* llgenericvalue * bool -> Int64.int */
unsigned long long llvm_generic_value_to_int(LLVMGenericValueRef GenVal, LLVMBool IsSigned) {
  return LLVMGenericValueToInt(GenVal, IsSigned);
}

/* llgenericvalue -> voidptr */
void *llvm_generic_value_to_pointer(LLVMGenericValueRef GenVal) {
  return LLVMGenericValueToPointer(GenVal);
}

/* lltype * llgenericvalue -> real */
double llvm_generic_value_to_float(LLVMTypeRef TyRef, LLVMGenericValueRef GenVal) {
  return LLVMGenericValueToFloat(TyRef, GenVal);
}

/* llgenericvalue -> void */
void llvm_dispose_generic_value(LLVMGenericValueRef GenVal) {
  LLVMDisposeGenericValue(GenVal);
}

/* llexecutionengine * llvalue * int * string array * string array -> int */
int llvm_run_function_as_main(LLVMExecutionEngineRef EE, LLVMValueRef F, unsigned ArgC, const char * const *ArgV, const char * const *EnvP) {
  return LLVMRunFunctionAsMain(EE, F, ArgC, ArgV, EnvP);
}

/* llexecutionengine * llvalue * int * llgenericvalue array -> llgenericvalue */
LLVMGenericValueRef llvm_run_function(LLVMExecutionEngineRef EE, LLVMValueRef F, unsigned ArgCount, LLVMGenericValueRef *Args) {
  return LLVMRunFunction(EE, F, ArgCount, Args);
}
