#ifndef LLVM_SML_TARGET_H
#define LLVM_SML_TARGET_H

#include "llvm_sml_core.h"

typedef struct LLVMOpaqueTargetData *LLVMTargetDataRef;
typedef struct LLVMTarget *LLVMTargetRef;
typedef struct LLVMOpaqueTargetMachine *LLVMTargetMachineRef;

LLVMTargetDataRef llvm_datalayout_of_string(const char *);
const char *llvm_datalayout_as_string(LLVMTargetDataRef);
int llvm_datalayout_byte_order(LLVMTargetDataRef);
unsigned llvm_datalayout_pointer_size(LLVMTargetDataRef);
LLVMTypeRef llvm_datalayout_intptr_type(LLVMContextRef, LLVMTargetDataRef);
unsigned llvm_datalayout_qualified_pointer_size(unsigned, LLVMTargetDataRef);
LLVMTypeRef llvm_datalayout_qualified_intptr_type(LLVMContextRef, unsigned, LLVMTargetDataRef);
unsigned long long llvm_datalayout_size_in_bits(LLVMTypeRef, LLVMTargetDataRef);
unsigned long long llvm_datalayout_store_size(LLVMTypeRef, LLVMTargetDataRef);
unsigned long long llvm_datalayout_abi_size(LLVMTypeRef, LLVMTargetDataRef);
unsigned llvm_datalayout_abi_align(LLVMTypeRef, LLVMTargetDataRef);
unsigned llvm_datalayout_stack_align(LLVMTypeRef, LLVMTargetDataRef);
unsigned llvm_datalayout_preferred_align(LLVMTypeRef, LLVMTargetDataRef);
unsigned llvm_datalayout_preferred_align_of_global(LLVMValueRef, LLVMTargetDataRef);
unsigned llvm_datalayout_element_at_offset(LLVMTypeRef, unsigned long long, LLVMTargetDataRef);
unsigned long long llvm_datalayout_offset_of_element(LLVMTypeRef, unsigned, LLVMTargetDataRef);

const char *llvm_target_default_triple(void);
LLVMTargetRef llvm_target_first(void);
LLVMTargetRef llvm_target_succ(LLVMTargetRef);
LLVMTargetRef llvm_target_by_name(const char *);
LLVMTargetRef llvm_target_by_triple(const char *);
const char *llvm_target_name(LLVMTargetRef);
const char *llvm_target_description(LLVMTargetRef);
LLVMBool llvm_target_has_jit(LLVMTargetRef);
LLVMBool llvm_target_has_target_machine(LLVMTargetRef);
LLVMBool llvm_target_has_asm_backend(LLVMTargetRef);

LLVMTargetMachineRef llvm_create_targetmachine(const char *, const char *, const char *, int, int, int, LLVMTargetRef);
LLVMTargetRef llvm_targetmachine_target(LLVMTargetMachineRef);
const char *llvm_targetmachine_triple(LLVMTargetMachineRef);
const char *llvm_targetmachine_cpu(LLVMTargetMachineRef);
const char *llvm_targetmachine_features(LLVMTargetMachineRef);
LLVMTargetDataRef llvm_targetmachine_data_layout(LLVMTargetMachineRef);
void llvm_targetmachine_set_verbose_asm(LLVMBool, LLVMTargetMachineRef);
void llvm_targetmachine_emit_to_file(LLVMModuleRef, int, const char *, LLVMTargetMachineRef);
LLVMMemoryBufferRef llvm_targetmachine_emit_to_memory_buffer(LLVMModuleRef, int, LLVMTargetMachineRef);
void llvm_targetmachine_add_analysis_passes(LLVMPassManagerRef, LLVMTargetMachineRef);

#endif
