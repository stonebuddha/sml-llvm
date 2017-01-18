#ifndef LLVM_SML_EXECUTIONENGINE_H
#define LLVM_SML_EXECUTIONENGINE_H

#include "llvm_sml_target.h"

typedef struct LLVMOpaqueExecutionEngine *LLVMExecutionEngineRef;
typedef struct LLVMOpaqueGenericValue *LLVMGenericValueRef;

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

LLVMGenericValueRef llvm_create_generic_value_of_int(LLVMTypeRef, unsigned long long, LLVMBool);
LLVMGenericValueRef llvm_create_generic_value_of_pointer(void *);
LLVMGenericValueRef llvm_create_generic_value_of_float(LLVMTypeRef, double);
unsigned llvm_generic_value_int_width(LLVMGenericValueRef);
unsigned long long llvm_generic_value_to_int(LLVMGenericValueRef, LLVMBool);
void *llvm_generic_value_to_pointer(LLVMGenericValueRef);
double llvm_generic_value_to_float(LLVMTypeRef, LLVMGenericValueRef);
void llvm_dispose_generic_value(LLVMGenericValueRef);

int llvm_run_function_as_main(LLVMExecutionEngineRef, LLVMValueRef, unsigned, const char * const *, const char * const *);
LLVMGenericValueRef llvm_run_function(LLVMExecutionEngineRef, LLVMValueRef, unsigned, LLVMGenericValueRef *);

#endif
