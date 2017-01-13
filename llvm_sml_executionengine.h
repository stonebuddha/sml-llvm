#ifndef LLVM_SML_EXECUTIONENGINE_H
#define LLVM_SML_EXECUTIONENGINE_H

#include "llvm_sml_types.h"
#include "llvm_sml_target.h"

typedef struct LLVMOpaqueExecutionEngine *LLVMExecutionEngineRef;

LLVMBool llvm_ee_initialize(void);
LLVMExecutionEngineRef llvm_ee_create(int *, int *, LLVMBool *, LLVMBool *, LLVMModuleRef);
void llvm_ee_dispose(LLVMExecutionEngineRef);
void llvm_ee_add_module(LLVMModuleRef, LLVMExecutionEngineRef);
LLVMModuleRef llvm_ee_remove_module(LLVMModuleRef, LLVMExecutionEngineRef);
void llvm_ee_run_static_ctors(LLVMExecutionEngineRef);
void llvm_ee_run_static_dtors(LLVMExecutionEngineRef);
LLVMTargetDataRef llvm_ee_get_data_layout(LLVMExecutionEngineRef);
void llvm_ee_add_global_mapping(LLVMValueRef, void *, LLVMExecutionEngineRef);
void *llvm_ee_get_global_value_address(const char *, LLVMExecutionEngineRef);
void *llvm_ee_get_function_address(const char *, LLVMExecutionEngineRef);

#endif
