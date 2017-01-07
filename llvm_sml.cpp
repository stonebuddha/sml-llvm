#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "llvm-c/Core.h"
#include "llvm-c/Support.h"

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
  unsigned MDKindID = LLVMGetMDKindIDInContext(C, Name, strlen(Name));
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
void llvm_set_target_triple(const char *Triple, LLVMModuleRef M) {
  LLVMSetTarget(M, Triple);
}

/* llmodule -> string */
extern "C"
const char *llvm_data_layout(LLVMModuleRef M) {
  return copy_string(LLVMGetDataLayout(M));
}

/* string * llmodule -> unit */
extern "C"
void llvm_set_data_layout(const char *DataLayoutStr, LLVMModuleRef M) {
  LLVMSetDataLayout(M, DataLayoutStr);
}

/* llmodule -> unit */
extern "C"
void llvm_dump_module(LLVMModuleRef M) {
  LLVMDumpModule(M);
}

/* string * llmodule -> unit */
extern "C"
void llvm_print_module(const char *Filename, LLVMModuleRef M) {
  char *ErrorMessage;

  if (LLVMPrintModuleToFile(M, Filename, &ErrorMessage)) {
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
LLVMContextRef llvm_module_context(LLVMModuleRef M) {
  return LLVMGetModuleContext(M);
}

/*===-- Types -------------------------------------------------------------===*/

/* lltype -> int */
extern "C"
int llvm_classify_type(LLVMTypeRef Ty) {
  return LLVMGetTypeKind(Ty);
}

/* lltype -> llcontext */
extern "C"
LLVMContextRef llvm_type_context(LLVMTypeRef Ty) {
  return LLVMGetTypeContext(Ty);
}

/* lltype -> bool */
extern "C"
int llvm_type_is_sized(LLVMTypeRef Ty) {
  return LLVMTypeIsSized(Ty);
}

/* lltype -> unit */
extern "C"
void llvm_dump_type(LLVMTypeRef Ty) {
  LLVMDumpType(Ty);
}

/* lltype -> string */
extern "C"
const char *llvm_string_of_lltype(LLVMTypeRef Ty) {
  const char *TypeStr;
  char *TypeCStr;

  TypeCStr = LLVMPrintTypeToString(Ty);
  TypeStr = copy_string(TypeCStr);
  LLVMDisposeMessage(TypeCStr);

  return TypeStr;
}

/*--... Operations on integer types ........................................--*/

/* llcontext -> lltype */
extern "C"
LLVMTypeRef llvm_i1_type(LLVMContextRef C) {
  return LLVMInt1TypeInContext(C);
}

/* llcontext -> lltype */
extern "C"
LLVMTypeRef llvm_i8_type(LLVMContextRef C) {
  return LLVMInt8TypeInContext(C);
}

/* llcontext -> lltype */
extern "C"
LLVMTypeRef llvm_i16_type(LLVMContextRef C) {
  return LLVMInt16TypeInContext(C);
}

/* llcontext -> lltype */
extern "C"
LLVMTypeRef llvm_i32_type(LLVMContextRef C) {
  return LLVMInt32TypeInContext(C);
}

/* llcontext -> lltype */
extern "C"
LLVMTypeRef llvm_i64_type(LLVMContextRef C) {
  return LLVMInt64TypeInContext(C);
}

/* llcontext * int -> lltype */
extern "C"
LLVMTypeRef llvm_integer_type(LLVMContextRef C, int NumBits) {
  return LLVMIntTypeInContext(C, NumBits);
}

/* lltype -> int */
extern "C"
int llvm_integer_bitwidth(LLVMTypeRef IntegerTy) {
  return LLVMGetIntTypeWidth(IntegerTy);
}

/*--... Operations on real types ...........................................--*/

/* llcontext -> lltype */
extern "C"
LLVMTypeRef llvm_float_type(LLVMContextRef C) {
  return LLVMFloatTypeInContext(C);
}

/* llcontext -> lltype */
extern "C"
LLVMTypeRef llvm_double_type(LLVMContextRef C) {
  return LLVMDoubleTypeInContext(C);
}

/* llcontext -> lltype */
extern "C"
LLVMTypeRef llvm_x86fp80_type(LLVMContextRef C) {
  return LLVMX86FP80TypeInContext(C);
}

/* llcontext -> lltype */
extern "C"
LLVMTypeRef llvm_fp128_type(LLVMContextRef C) {
  return LLVMFP128TypeInContext(C);
}

/* llcontext -> lltype */
extern "C"
LLVMTypeRef llvm_ppc_fp128_type(LLVMContextRef C) {
  return LLVMPPCFP128TypeInContext(C);
}

/*--... Operations on function types .......................................--*/

/* lltype * lltype array -> lltype */
extern "C"
LLVMTypeRef llvm_function_type(LLVMTypeRef ReturnTy, LLVMTypeRef *ParamTys, int ParamCount) {
  return LLVMFunctionType(ReturnTy, ParamTys, ParamCount, 0);
}

/* lltype * lltype array -> lltype */
extern "C"
LLVMTypeRef llvm_var_arg_function_type(LLVMTypeRef ReturnTy, LLVMTypeRef *ParamTys, int ParamCount) {
  return LLVMFunctionType(ReturnTy, ParamTys, ParamCount, 1);
}

/* lltype -> bool */
extern "C"
int llvm_is_var_arg(LLVMTypeRef FunctionTy) {
  return LLVMIsFunctionVarArg(FunctionTy);
}

/* lltype -> lltype */
extern "C"
LLVMTypeRef llvm_return_type(LLVMTypeRef FunctionTy) {
  return LLVMGetReturnType(FunctionTy);
}

/* lltype -> lltype array */
extern "C"
LLVMTypeRef *llvm_param_types(LLVMTypeRef FunctionTy, int *ParamCount) {
  LLVMTypeRef *Tys = (LLVMTypeRef *) malloc(sizeof(LLVMTypeRef) * LLVMCountParamTypes(FunctionTy));
  LLVMGetParamTypes(FunctionTy, Tys);
  (*ParamCount) = LLVMCountParamTypes(FunctionTy);
  return Tys;
}

/*--... Operations on struct types .........................................--*/

/* llcontext * lltype array -> lltype */
extern "C"
LLVMTypeRef llvm_struct_type(LLVMContextRef C, LLVMTypeRef *ElemTys, int ElemCount) {
  return LLVMStructTypeInContext(C, ElemTys, ElemCount, 0);
}

/* llcontext * lltype array -> lltype */
extern "C"
LLVMTypeRef llvm_packed_struct_type(LLVMContextRef C, LLVMTypeRef *ElemTys, int ElemCount) {
  return LLVMStructTypeInContext(C, ElemTys, ElemCount, 1);
}

/* lltype -> string */
extern "C"
const char *llvm_struct_name(LLVMTypeRef StructTy) {
  const char *Name = LLVMGetStructName(StructTy);
  if (Name) {
    return copy_string(Name);
  } else {
    return NULL;
  }
}

/* llcontext * string -> lltype */
extern "C"
LLVMTypeRef llvm_named_struct_type(LLVMContextRef C, const char *Name) {
  return LLVMStructCreateNamed(C, Name);
}

/* lltype * lltype array * bool -> unit */
extern "C"
void llvm_struct_set_body(LLVMTypeRef StructTy, LLVMTypeRef *ElemTys, int ElemCount, int Packed) {
  LLVMStructSetBody(StructTy, ElemTys, ElemCount, Packed);
}

/* lltype -> lltype array */
extern "C"
LLVMTypeRef *llvm_struct_element_types(LLVMTypeRef StructTy, int *ElemCount) {
  LLVMTypeRef *Tys = (LLVMTypeRef *) malloc(sizeof(LLVMTypeRef) * LLVMCountStructElementTypes(StructTy));
  LLVMGetStructElementTypes(StructTy, Tys);
  (*ElemCount) = LLVMCountStructElementTypes(StructTy);
  return Tys;
}

/* lltype -> bool */
extern "C"
int llvm_is_packed(LLVMTypeRef StructTy) {
  return LLVMIsPackedStruct(StructTy);
}

/* lltype -> bool */
extern "C"
int llvm_is_opaque(LLVMTypeRef StructTy) {
  return LLVMIsOpaqueStruct(StructTy);
}
