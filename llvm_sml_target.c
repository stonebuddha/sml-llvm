#include "llvm_sml_target.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <llvm-c/Core.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>

/*===-- Utils -------------------------------------------------------------===*/

static const char *copy_string(const char *src) {
  unsigned Len;
  char *des;

  Len = strlen(src);
  des = (char *) malloc(sizeof(char) * (Len + 1));
  strcpy(des, src);
  des[Len] = '\0';

  return des;
}

/*===---- Data Layout -----------------------------------------------------===*/

/* string -> DataLayout.t */
LLVMTargetDataRef llvm_datalayout_of_string(const char *StringRep) {
  return LLVMCreateTargetData(StringRep);
}

/* DataLayout.t -> string */
const char *llvm_datalayout_as_string(LLVMTargetDataRef TD) {
  char *StringRep = LLVMCopyStringRepOfTargetData(TD);
  const char *Copy = copy_string(StringRep);
  LLVMDisposeMessage(StringRep);
  return Copy;
}

/* DataLayout.t -> Endian.t */
int llvm_datalayout_byte_order(LLVMTargetDataRef DL) {
  return LLVMByteOrder(DL);
}

/* DataLayout.t -> int */
unsigned llvm_datalayout_pointer_size(LLVMTargetDataRef DL) {
  return LLVMPointerSize(DL);
}

/* llcontext * DataLayout.t -> lltype */
LLVMTypeRef llvm_datalayout_intptr_type(LLVMContextRef C, LLVMTargetDataRef DL) {
  return LLVMIntPtrTypeInContext(C, DL);
}

/* int * DataLayout.t -> int */
unsigned llvm_datalayout_qualified_pointer_size(unsigned AS, LLVMTargetDataRef DL) {
  return LLVMPointerSizeForAS(DL, AS);
}

/* llcontext * int -> DataLayout.t */
LLVMTypeRef llvm_datalayout_qualified_intptr_type(LLVMContextRef C, unsigned AS, LLVMTargetDataRef DL) {
  return LLVMIntPtrTypeForASInContext(C, DL, AS);
}

/* lltype * DataLayout.t -> Int64.int */
unsigned long long llvm_datalayout_size_in_bits(LLVMTypeRef Ty, LLVMTargetDataRef DL) {
  return LLVMSizeOfTypeInBits(DL, Ty);
}

/* lltype * DataLayout.t -> Int64.int */
unsigned long long llvm_datalayout_store_size(LLVMTypeRef Ty, LLVMTargetDataRef DL) {
  return LLVMStoreSizeOfType(DL, Ty);
}

/* lltype * DataLayout.t -> Int64.int */
unsigned long long llvm_datalayout_abi_size(LLVMTypeRef Ty, LLVMTargetDataRef DL) {
  return LLVMABISizeOfType(DL, Ty);
}

/* lltype * DataLayout.t -> int */
unsigned llvm_datalayout_abi_align(LLVMTypeRef Ty, LLVMTargetDataRef DL) {
  return LLVMABIAlignmentOfType(DL, Ty);
}

/* lltype * DataLayout.t -> int */
unsigned llvm_datalayout_stack_align(LLVMTypeRef Ty, LLVMTargetDataRef DL) {
  return LLVMCallFrameAlignmentOfType(DL, Ty);
}

/* lltype * DataLayout.t -> int */
unsigned llvm_datalayout_preferred_align(LLVMTypeRef Ty, LLVMTargetDataRef DL) {
  return LLVMPreferredAlignmentOfType(DL, Ty);
}

/* llvalue * DataLayout.t -> int */
unsigned llvm_datalayout_preferred_align_of_global(LLVMValueRef GlobalVar, LLVMTargetDataRef DL) {
  return LLVMPreferredAlignmentOfGlobal(DL, GlobalVar);
}

/* lltype * Int64.int * DataLayout.t -> int */
unsigned llvm_datalayout_element_at_offset(LLVMTypeRef Ty, unsigned long long Offset, LLVMTargetDataRef DL) {
  return LLVMElementAtOffset(DL, Ty, Offset);
}

/* lltype * int * DataLayout.t -> Int64.int */
unsigned long long llvm_datalayout_offset_of_element(LLVMTypeRef Ty, unsigned Index, LLVMTargetDataRef DL) {
  return LLVMOffsetOfElement(DL, Ty, Index);
}

/*===---- Target ----------------------------------------------------------===*/

/* unit -> string */
const char *llvm_target_default_triple(void) {
  char *TripleCStr = LLVMGetDefaultTargetTriple();
  const char *TripleStr = copy_string(TripleCStr);
  LLVMDisposeMessage(TripleCStr);
  return TripleStr;
}

/* unit -> Target.t option */
LLVMTargetRef llvm_target_first(void) {
  return LLVMGetFirstTarget();
}

/* Target.t -> Target.t option */
LLVMTargetRef llvm_target_succ(LLVMTargetRef Target) {
  return LLVMGetNextTarget(Target);
}

/* string -> Target.t */
LLVMTargetRef llvm_target_by_name(const char *Name) {
  return LLVMGetTargetFromName(Name);
}

/* string -> Target.t */
LLVMTargetRef llvm_target_by_triple(const char *Triple) {
  LLVMTargetRef T;
  char *Error;

  if (LLVMGetTargetFromTriple(Triple, &T, &Error)) {
    /* TODO: throw an exception */
    assert(0);
  }

  return T;
}

/* Target.t -> string */
const char *llvm_target_name(LLVMTargetRef Target) {
  return copy_string(LLVMGetTargetName(Target));
}

/* Target.t -> string */
const char *llvm_target_description(LLVMTargetRef Target) {
  return copy_string(LLVMGetTargetDescription(Target));
}

/* Target.t -> bool */
LLVMBool llvm_target_has_jit(LLVMTargetRef Target) {
  return LLVMTargetHasJIT(Target);
}

/* Target.t -> bool */
LLVMBool llvm_target_has_target_machine(LLVMTargetRef Target) {
  return LLVMTargetHasTargetMachine(Target);
}

/* Target.t -> bool */
LLVMBool llvm_target_has_asm_backend(LLVMTargetRef Target) {
  return LLVMTargetHasAsmBackend(Target);
}

/*===---- Target Machine --------------------------------------------------===*/

/* string * string option * string option * CodeGenOptLevel.t option * RelocMode.t option * CodeModel.t option * Target.t -> TargetMachine.t */
LLVMTargetMachineRef llvm_create_targetmachine(const char *Triple, const char *CPU, const char *Features, int OptLevel, int RelocMode, int CodeModel, LLVMTargetRef Target) {
  LLVMTargetMachineRef Machine;
  const char *CPUStr = "", *FeaturesStr = "";
  LLVMCodeGenOptLevel OptLevelEnum = OptLevel;
  LLVMRelocMode RelocModeEnum = RelocMode;
  LLVMCodeModel CodeModelEnum = CodeModel;

  if (CPU != NULL) {
    CPUStr = CPU;
  }
  if (Features != NULL) {
    FeaturesStr = Features;
  }

  Machine = LLVMCreateTargetMachine(Target, Triple, CPUStr, FeaturesStr, OptLevelEnum, RelocModeEnum, CodeModelEnum);

  return Machine;
}

/* TargetMachine.t -> Target.t */
LLVMTargetRef llvm_targetmachine_target(LLVMTargetMachineRef Machine) {
  return LLVMGetTargetMachineTarget(Machine);
}

/* TargetMachine.t -> string */
const char *llvm_targetmachine_triple(LLVMTargetMachineRef Machine) {
  char *Msg = LLVMGetTargetMachineTriple(Machine);
  const char *Res = copy_string(Msg);
  LLVMDisposeMessage(Msg);
  return Res;
}

/* TargetMachine.t -> string */
const char *llvm_targetmachine_cpu(LLVMTargetMachineRef Machine) {
  char *Msg = LLVMGetTargetMachineCPU(Machine);
  const char *Res = copy_string(Msg);
  LLVMDisposeMessage(Msg);
  return Res;
}

/* TargetMachine.t -> string */
const char *llvm_targetmachine_features(LLVMTargetMachineRef Machine) {
  char *Msg = LLVMGetTargetMachineFeatureString(Machine);
  const char *Res = copy_string(Msg);
  LLVMDisposeMessage(Msg);
  return Res;
}

/* TargetMachine.t -> DataLayout.t */
LLVMTargetDataRef llvm_targetmachine_data_layout(LLVMTargetMachineRef Machine) {
  return LLVMCreateTargetDataLayout(Machine);
}

/* bool * TargetMachine.t -> unit */
void llvm_targetmachine_set_verbose_asm(LLVMBool Verb, LLVMTargetMachineRef Machine) {
  LLVMSetTargetMachineAsmVerbosity(Machine, Verb);
}

/* llmodule * CodeGenFileType.t * string * TargetMachine.t -> unit */
void llvm_targetmachine_emit_to_file(LLVMModuleRef Module, int FileType, const char *FileName, LLVMTargetMachineRef Machine) {
  char *ErrorMessage;
  unsigned Len = strlen(FileName);
  char *FN = malloc(sizeof(char) * (Len + 1));
  strcpy(FN, FileName);
  FN[Len] = '\0';

  if (LLVMTargetMachineEmitToFile(Machine, Module, FN, FileType, &ErrorMessage)) {
    /* TODO: throw an exception */
    assert(0);
  }

  free(FN);
}

/* llmodule * CodeGenFileType.t * TargetMachine.t -> llmemorybuffer */
LLVMMemoryBufferRef llvm_targetmachine_emit_to_memory_buffer(LLVMModuleRef Module, int FileType, LLVMTargetMachineRef Machine) {
  char *ErrorMessage;
  LLVMMemoryBufferRef Buffer;

  if (LLVMTargetMachineEmitToMemoryBuffer(Machine, Module, FileType, &ErrorMessage, &Buffer)) {
    /* TODO: throw an exception */
    assert(0);
  }

  return Buffer;
}

/* PassManager.t * TargetMachine.t -> unit */
void llvm_targetmachine_add_analysis_passes(LLVMPassManagerRef PM, LLVMTargetMachineRef Machine) {
  LLVMAddAnalysisPasses(Machine, PM);
}
