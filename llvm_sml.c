#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "llvm-c/Core.h"
#include "llvm-c/Support.h"
#include "export.h"

/*===-- Utils -------------------------------------------------------------===*/

static char *copy_string(const char *src) {
  char *des = malloc(sizeof(char) * (strlen(src) + 1));
  strcpy(des, src);
  return des;
}

/*===-- Contexts ----------------------------------------------------------===*/

/* unit -> llcontext */
LLVMContextRef llvm_create_context(void) {
  return LLVMContextCreate();
}

/* llcontext -> unit */
void llvm_dispose_context(LLVMContextRef C) {
  LLVMContextDispose(C);
}

/* unit -> llcontext */
LLVMContextRef llvm_global_context(void) {
  return LLVMGetGlobalContext();
}

/* llcontext * string -> int */
Int32 llvm_mdkind_id(LLVMContextRef C, Pointer Name) {
  unsigned MDKindID = LLVMGetMDKindIDInContext(C, (char *) Name, strlen((char *) Name));
  return (Int32) MDKindID;
}

/*===-- Modules -----------------------------------------------------------===*/

/* llcontext * string -> llmodule */
LLVMModuleRef llvm_create_module(LLVMContextRef C, Pointer ModuleID) {
  return LLVMModuleCreateWithNameInContext((char *) ModuleID, C);
}

/* llmodule -> unit */
void llvm_dispose_module(LLVMModuleRef M) {
  LLVMDisposeModule(M);
}

/* llmodule -> string */
Pointer llvm_target_triple(LLVMModuleRef M) {
  return (Pointer) copy_string(LLVMGetTarget(M));
}

/* string * llmodule -> unit */
void llvm_set_target_triple(Pointer Trip, LLVMModuleRef M) {
  LLVMSetTarget(M, (char *) Trip);
}

/* llmodule -> string */
Pointer llvm_data_layout(LLVMModuleRef M) {
  return (Pointer) copy_string(LLVMGetDataLayout(M));
}

/* string * llmodule -> unit */
void llvm_set_data_layout(Pointer Layout, LLVMModuleRef M) {
  LLVMSetDataLayout(M, (const char *) Layout);
}

/* llmodule -> unit */
void llvm_dump_module(LLVMModuleRef M) {
  LLVMDumpModule(M);
}

/* string * llmodule -> unit */
void llvm_print_module(Pointer Filename, LLVMModuleRef M) {
  char *Message;

  if (LLVMPrintModuleToFile(M, (const char *) Filename, &Message)) {
    /* TODO: throw an exception */
  }
}

/* llmodule -> string */
Pointer llvm_string_of_llmodule(LLVMModuleRef M) {
  char *ModuleStr;
  char *ModuleCStr;

  ModuleCStr = LLVMPrintModuleToString(M);
  ModuleStr = copy_string(ModuleStr);
  LLVMDisposeMessage(ModuleCStr);

  return (Pointer) ModuleStr;
}

/* llmodule * string -> unit */
void llvm_set_module_inline_asm(LLVMModuleRef M, Pointer Asm) {
  LLVMSetModuleInlineAsm(M, (const char *) Asm);
}

/*===-- Types -------------------------------------------------------------===*/

/* lltype -> int */
Int32 llvm_classify_type(LLVMTypeRef Ty) {
  return LLVMGetTypeKind(Ty);
}

/* lltype -> llcontext */
LLVMContextRef llvm_type_context(LLVMTypeRef Ty) {
  return LLVMGetTypeContext(Ty);
}

/* lltype -> bool */
Bool llvm_type_is_sized(LLVMTypeRef Ty) {
  return LLVMTypeIsSized(Ty);
}

/* lltype -> unit */
void llvm_dump_type(LLVMTypeRef Ty) {
  LLVMDumpType(Ty);
}

/* lltype -> string */
Pointer llvm_string_of_lltype(LLVMTypeRef M) {
  char *TypeStr;
  char *TypeCStr;

  TypeCStr = LLVMPrintTypeToString(M);
  TypeStr = copy_string(TypeCStr);
  LLVMDisposeMessage(TypeCStr);

  return (Pointer) TypeStr;
}

/*--... Operations on integer types ........................................--*/

/* llcontext -> lltype */
LLVMTypeRef llvm_i1_type(LLVMContextRef C) {
  return LLVMInt1TypeInContext(C);
}

/* llcontext -> lltype */
LLVMTypeRef llvm_i8_type(LLVMContextRef C) {
  return LLVMInt8TypeInContext(C);
}

/* llcontext -> lltype */
LLVMTypeRef llvm_i16_type(LLVMContextRef C) {
  return LLVMInt16TypeInContext(C);
}

/* llcontext -> lltype */
LLVMTypeRef llvm_i32_type(LLVMContextRef C) {
  return LLVMInt32TypeInContext(C);
}

/* llcontext -> lltype */
LLVMTypeRef llvm_i64_type(LLVMContextRef C) {
  return LLVMInt64TypeInContext(C);
}

/* llcontext * int -> lltype */
LLVMTypeRef llvm_integer_type(LLVMContextRef C, Int32 Width) {
  return LLVMIntTypeInContext(C, Width);
}

/* lltype -> int */
Int32 llvm_integer_bitwidth(LLVMTypeRef Ty) {
  return LLVMGetIntTypeWidth(Ty);
}

/*--... Operations on real types ...........................................--*/

/* llcontext -> lltype */
LLVMTypeRef llvm_float_type(LLVMContextRef C) {
  return LLVMFloatTypeInContext(C);
}

/* llcontext -> lltype */
LLVMTypeRef llvm_double_type(LLVMContextRef C) {
  return LLVMDoubleTypeInContext(C);
}

/* llcontext -> lltype */
LLVMTypeRef llvm_x86fp80_type(LLVMContextRef C) {
  return LLVMX86FP80TypeInContext(C);
}

/* llcontext -> lltype */
LLVMTypeRef llvm_fp128_type(LLVMContextRef C) {
  return LLVMFP128TypeInContext(C);
}

/* llcontext -> lltype */
LLVMTypeRef llvm_ppc_fp128_type(LLVMContextRef C) {
  return LLVMPPCFP128TypeInContext(C);
}

/*--... Operations on function types .......................................--*/

/* lltype * lltype array * int -> lltype */
LLVMTypeRef llvm_function_type(LLVMTypeRef RetTy, Pointer ParamTys, Int32 NumParamTys) {
  return LLVMFunctionType(RetTy, (LLVMTypeRef *) ParamTys, NumParamTys, 0);
}

/* lltype * lltype array * int -> lltype */
LLVMTypeRef llvm_var_arg_function_type(LLVMTypeRef RetTy, Pointer ParamTys, Int32 NumParamTys) {
  return LLVMFunctionType(RetTy, (LLVMTypeRef *) ParamTys, NumParamTys, 1);
}

/* lltype -> bool */
Bool llvm_is_var_arg(LLVMTypeRef Ty) {
  return LLVMIsFunctionVarArg(Ty);
}

/* lltype -> lltype */
Pointer llvm_param_types(LLVMTypeRef Ty) {
  LLVMTypeRef *Tys = malloc(sizeof(LLVMTypeRef) * LLVMCountParamTypes(Ty));
  LLVMGetParamTypes(Ty, Tys);
  return (Pointer) Tys;
}

/*--... Operations on struct types .........................................--*/

/* llcontext * lltype array * int -> lltype */
LLVMTypeRef llvm_struct_type(LLVMContextRef C, Pointer ElemTys, Int32 NumElemTys) {
  return LLVMStructTypeInContext(C, (LLVMTypeRef *) ElemTys, NumElemTys, 0);
}

/* llcontext * lltype array * int -> lltype */
LLVMTypeRef llvm_packed_struct_type(LLVMContextRef C, Pointer ElemTys, Int32 NumElemTys) {
  return LLVMStructTypeInContext(C, (LLVMTypeRef *) ElemTys, NumElemTys, 1);
}

/* lltype * bool ref -> string */
Pointer llvm_struct_name(LLVMTypeRef Ty, Pointer is_null) {
  const char *C = LLVMGetStructName(Ty);
  if (C) {
    char *result;
    result = copy_string(C);
    return (Pointer) result;
  } else {
    (*((Bool *) is_null)) = 1;
    return NULL;
  }
}

/* llcontext * string -> lltype */
LLVMTypeRef llvm_named_struct_type(LLVMContextRef C, Pointer Name) {
  return LLVMStructCreateNamed(C, (const char *) Name);
}

/* lltype * lltype array * int * bool -> unit */
void llvm_struct_set_body(LLVMTypeRef Ty, Pointer ElemTys, Int32 NumElemTys, Bool Packed) {
  LLVMStructSetBody(Ty, (LLVMTypeRef *) ElemTys, NumElemTys, Packed);
}

/* lltype -> lltype array */
Pointer llvm_struct_element_types(LLVMTypeRef Ty) {
  LLVMTypeRef *Tys = malloc(sizeof(LLVMTypeRef) * LLVMCountStructElementTypes(Ty));
  LLVMGetStructElementTypes(Ty, Tys);
  return (Pointer) Tys;
}

/* lltype -> bool */
Bool llvm_is_packed(LLVMTypeRef Ty) {
  return LLVMIsPackedStruct(Ty);
}

/* lltype -> bool */
Bool llvm_is_opaque(LLVMTypeRef Ty) {
  return LLVMIsOpaqueStruct(Ty);
}

/*--... Operations on array, pointer, and vector types .....................--*/

/* lltype * int -> lltype */
LLVMTypeRef llvm_array_type(LLVMTypeRef ElemTy, Int32 Count) {
  return LLVMArrayType(ElemTy, Count);
}

/* lltype -> lltype */
LLVMTypeRef llvm_pointer_type(LLVMTypeRef ElemTy) {
  return LLVMPointerType(ElemTy, 0);
}

/* lltype * int -> lltype */
LLVMTypeRef llvm_qualified_pointer_type(LLVMTypeRef ElemTy, Int32 AddrSpace) {
  return LLVMPointerType(ElemTy, AddrSpace);
}

/* lltype * int -> lltype */
LLVMTypeRef llvm_vector_type(LLVMTypeRef ElemTy, Int32 Count) {
  return LLVMVectorType(ElemTy, Count);
}

/* lltype -> int */
Int32 llvm_array_length(LLVMTypeRef Ty) {
  return LLVMGetArrayLength(Ty);
}

/* lltype -> int */
Int32 llvm_address_space(LLVMTypeRef Ty) {
  return LLVMGetPointerAddressSpace(Ty);
}

/* lltype -> int */
Int32 llvm_vector_length(LLVMTypeRef Ty) {
  return LLVMGetVectorSize(Ty);
}

/*--... Operations on other types ..........................................--*/

/* llcontext -> lltype */
LLVMTypeRef llvm_void_type(LLVMContextRef C) {
  return LLVMVoidTypeInContext(C);
}

/* llcontext -> lltype */
LLVMTypeRef llvm_label_type(LLVMContextRef C) {
  return LLVMLabelTypeInContext(C);
}

/* llcontext -> lltype */
LLVMTypeRef llvm_x86_mmx_type(LLVMContextRef C) {
  return LLVMX86MMXTypeInContext(C);
}

/* llcontext * string * bool ref -> lltype */
LLVMTypeRef llvm_type_by_name(LLVMModuleRef M, Pointer Name, Pointer is_null) {
  LLVMTypeRef Ty = LLVMGetTypeByName(M, (const char *) Name);
  if (Ty) {
    return Ty;
  } else {
    (*((Bool *) is_null)) = 1;
    return NULL;
  }
}

/*===-- VALUES ------------------------------------------------------------===*/

/* keep in sync with ValueKind.t */
enum ValueKind {
  NullValue = 0,
  Argument,
  BasicBlock,
  InlineAsm,
  MDNode,
  MDString,
  BlockAddress,
  ConstantAggregateZero,
  ConstantArray,
  ConstantDataArray,
  ConstantDataVector,
  ConstantExpr,
  ConstantFP,
  ConstantInt,
  ConstantPointerNull,
  ConstantStruct,
  ConstantVector,
  Function,
  GlobalAlias,
  GlobalVariable,
  UndefValue,
  Instruction,
};

/* llvalue -> int */
Int32 llvm_classify_value(LLVMValueRef Val) {
  if (!Val) {
    return NullValue;
  } else if (LLVMIsConstant(Val)) {
    if (LLVMIsABlockAddress(Val)) return BlockAddress;
    if (LLVMIsAConstantAggregateZero(Val)) return ConstantAggregateZero;
    if (LLVMIsAConstantArray(Val)) return ConstantArray;
    if (LLVMIsAConstantDataArray(Val)) return ConstantDataArray;
    if (LLVMIsAConstantDataVector(Val)) return ConstantDataVector;
    if (LLVMIsAConstantExpr(Val)) return ConstantExpr;
    if (LLVMIsAConstantFP(Val)) return ConstantFP;
    if (LLVMIsAConstantPointerNull(Val)) return ConstantPointerNull;
    if (LLVMIsAConstantStruct(Val)) return ConstantStruct;
    if (LLVMIsAConstantVector(Val)) return ConstantVector;
    assert(0);
  } else if (LLVMIsAInstruction(Val)) {
    Int32 result;
    result = LLVMGetInstructionOpcode(Val);
    return -(result + 1);
  } else if (LLVMIsAGlobalValue(Val)) {
    if (LLVMIsAFunction(Val)) return Function;
    if (LLVMIsAGlobalAlias(Val)) return GlobalAlias;
    if (LLVMIsAGlobalVariable(Val)) return GlobalVariable;
    assert(0);
  } else {
    if (LLVMIsAArgument(Val)) return Argument;
    if (LLVMIsABasicBlock(Val)) return BasicBlock;
    if (LLVMIsAInlineAsm(Val)) return InlineAsm;
    if (LLVMIsAMDNode(Val)) return MDNode;
    if (LLVMIsAMDString(Val)) return MDString;
    if (LLVMIsAUndefValue(Val)) return UndefValue;
    assert(0);
  }
}

/* llvalue -> lltype */
LLVMTypeRef llvm_type_of(LLVMValueRef Val) {
  return LLVMTypeOf(Val);
}

/* llvalue -> string */
Pointer llvm_value_name(LLVMValueRef Val) {
  return (Pointer) copy_string(LLVMGetValueName(Val));
}

/* string * llvalue -> unit */
void llvm_set_value_name(Pointer Name, LLVMValueRef Val) {
  LLVMSetValueName(Val, (const char *) Name);
}

/* llvalue -> unit */
void llvm_dump_value(LLVMValueRef Val) {
  LLVMDumpValue(Val);
}

/* llvalue -> string */
Pointer llvm_string_of_llvalue(LLVMValueRef Val) {
  char *ValueStr;
  char *ValueCStr;

  ValueCStr = LLVMPrintValueToString(Val);
  ValueStr = copy_string(ValueCStr);
  LLVMDisposeMessage(ValueCStr);

  return (Pointer) ValueStr;
}

/* llvalue * llvalue -> unit */
void llvm_replace_all_uses_with(LLVMValueRef OldVal, LLVMValueRef NewVal) {
  LLVMReplaceAllUsesWith(OldVal, NewVal);
}

/*--... Operations on uses .................................................--*/

/* llvalue * bool ref -> lluse */
LLVMUseRef llvm_use_begin(LLVMValueRef Val, Pointer is_null) {
  LLVMUseRef First;
  if ((First = LLVMGetFirstUse(Val))) {
    return First;
  } else {
    (*((Bool *) is_null)) = 1;
    return NULL;
  }
}

/* llvalue * bool ref -> lluse */
LLVMUseRef llvm_use_succ(LLVMUseRef U, Pointer is_null) {
  LLVMUseRef Next;
  if ((Next = LLVMGetNextUse(U))) {
    return Next;
  } else {
    (*((Bool *) is_null)) = 1;
    return NULL;
  }
}

/* lluse -> llvalue */
LLVMValueRef llvm_user(LLVMUseRef U) {
  return LLVMGetUser(U);
}

/* lluse -> llvalue */
LLVMValueRef llvm_used_value(LLVMUseRef U) {
  return LLVMGetUsedValue(U);
}

/*--... Operations on users ................................................--*/

/* llvalue * int -> llvalue */
LLVMValueRef llvm_operand(LLVMValueRef Val, Int32 I) {
  return LLVMGetOperand(Val, I);
}

/* llvalue * int -> lluse */
LLVMUseRef llvm_operand_use(LLVMValueRef Val, Int32 I) {
  return LLVMGetOperandUse(Val, I);
}

/* llvalue * int * llvalue -> unit */
void llvm_set_operand(LLVMValueRef Val1, Int32 I, LLVMValueRef Val2) {
  LLVMSetOperand(Val1, I, Val2);
}

/* llvalue -> int */
Int32 llvm_num_operands(LLVMValueRef Val) {
  return LLVMGetNumOperands(Val);
}

/*--... Operations on constants of (mostly) any type .......................--*/

/* llvalue -> bool */
Bool llvm_is_constant(LLVMValueRef Val) {
  return LLVMIsConstant(Val);
}

/* llvalue -> bool */
Bool llvm_is_null(LLVMValueRef Val) {
  return LLVMIsNull(Val);
}

/* llvalue -> bool */
Bool llvm_is_undef(LLVMValueRef Val) {
  return LLVMIsUndef(Val);
}

/* llvalue -> int */
Int32 llvm_constexpr_get_opcode(LLVMValueRef Val) {
  return LLVMIsAConstantExpr(Val) ? LLVMGetConstOpcode(Val) : 0;
}

/*--... Operations on instructions .........................................--*/

/* llvalue -> bool */
Bool llvm_has_metadata(LLVMValueRef Val) {
  return LLVMHasMetadata(Val);
}

/* llvalue * llmdkind * bool ref -> llvalue */
LLVMValueRef llvm_metadata(LLVMValueRef Val, Int32 MDKindId, Pointer is_null) {
  LLVMValueRef MD;
  if ((MD = LLVMGetMetadata(Val, MDKindId))) {
    return MD;
  } else {
    (*((Bool *) is_null)) = 1;
    return NULL;
  }
}

/* llvalue * llmdkind * llvalue -> unit */
void llvm_set_metadata(LLVMValueRef Val, Int32 MDKindID, LLVMValueRef MD) {
  LLVMSetMetadata(Val, MDKindID, MD);
}

/* llvalue * llmdkind -> unit */
void llvm_clear_metadata(LLVMValueRef Val, Int32 MDKindID) {
  LLVMSetMetadata(Val, MDKindID, NULL);
}

/*--... Operations on metadata .............................................--*/

/* llcontext * string -> llvalue */
LLVMValueRef llvm_mdstring(LLVMContextRef C, Pointer S) {
  return LLVMMDStringInContext(C, (const char *) S, strlen((const char *) S));
}

/* llcontext * llvalue array * int -> llvalue */
LLVMValueRef llvm_mdnode(LLVMContextRef C, Pointer ElemVals, Int32 NumElemVals) {
  return LLVMMDNodeInContext(C, (LLVMValueRef *) ElemVals, NumElemVals);
}

/* llcontext -> llvalue */
LLVMValueRef llvm_mdnull(LLVMContextRef C) {
  return NULL;
}

/* llvalue * bool ref -> string */
Pointer llvm_get_mdstring(LLVMValueRef Val, Pointer is_null) {
  const char *S;
  unsigned Len;

  if ((S = LLVMGetMDString(Val, &Len))) {
    char *Str;

    Str = malloc(sizeof(char) * (Len + 1));
    memcpy(Str, S, sizeof(char) * Len);
    return (Pointer) Str;
  } else {
    (*((Bool *) is_null)) = 1;
    return NULL;
  }
}

/* llvalue -> llvalue array */
Pointer llvm_get_mdnode_operands(LLVMValueRef Val) {
  LLVMValueRef *Operands;
  unsigned int n;

  n = LLVMGetMDNodeNumOperands(Val);
  Operands = malloc(sizeof(LLVMValueRef) * n);
  LLVMGetMDNodeOperands(Val, Operands);
  return (Pointer) Operands;
}

/* llmodule * string -> llvalue array */
Pointer llvm_get_namedmd(LLVMModuleRef M, Pointer Name) {
  LLVMValueRef *Nodes;
  Nodes = malloc(sizeof(LLVMValueRef) * LLVMGetNamedMetadataNumOperands(M, (const char *) Name));
  LLVMGetNamedMetadataOperands(M, (const char *) Name, Nodes);
  return (Pointer) Nodes;
}

/* llmodule * string * llvalue -> unit */
void llvm_append_namedmd(LLVMModuleRef M, Pointer Name, LLVMValueRef Val) {
  LLVMAddNamedMetadataOperand(M, (const char *) Name, Val);
}
