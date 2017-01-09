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

/* llcontext * string -> llmdkind */
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
    assert(0);
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

/* lltype -> string option */
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

/*--... Operations on array, pointer, and vector types .....................--*/

/* lltype * int -> lltype */
extern "C"
LLVMTypeRef llvm_array_type(LLVMTypeRef ElemTy, int ElemCount) {
  return LLVMArrayType(ElemTy, ElemCount);
}

/* lltype -> lltype */
extern "C"
LLVMTypeRef llvm_pointer_type(LLVMTypeRef ElemTy) {
  return LLVMPointerType(ElemTy, 0);
}

/* lltype * int -> lltype */
extern "C"
LLVMTypeRef llvm_qualified_pointer_type(LLVMTypeRef ElemTy, int AddrSpace) {
  return LLVMPointerType(ElemTy, AddrSpace);
}

/* lltype * int -> lltype */
extern "C"
LLVMTypeRef llvm_vector_type(LLVMTypeRef ElemTy, int ElemCount) {
  return LLVMVectorType(ElemTy, ElemCount);
}

/* lltype -> lltype */
extern "C"
LLVMTypeRef llvm_element_type(LLVMTypeRef Ty) {
  return LLVMGetElementType(Ty);
}

/* lltype -> int */
extern "C"
int llvm_array_length(LLVMTypeRef ArrayTy) {
  return LLVMGetArrayLength(ArrayTy);
}

/* lltype -> int */
extern "C"
int llvm_address_space(LLVMTypeRef PtrTy) {
  return LLVMGetPointerAddressSpace(PtrTy);
}

/* lltype -> int */
extern "C"
int llvm_vector_size(LLVMTypeRef VectorTy) {
  return LLVMGetVectorSize(VectorTy);
}

/*--... Operations on other types ..........................................--*/

/* llcontext -> lltype */
extern "C"
LLVMTypeRef llvm_void_type(LLVMContextRef C) {
  return LLVMVoidTypeInContext(C);
}

/* llcontext -> lltype */
extern "C"
LLVMTypeRef llvm_label_type(LLVMContextRef C) {
  return LLVMLabelTypeInContext(C);
}

/* llcontext -> lltype */
extern "C"
LLVMTypeRef llvm_x86_mmx_type(LLVMContextRef C) {
  return LLVMX86MMXTypeInContext(C);
}

/* llcontext * string -> lltype option */
extern "C"
LLVMTypeRef llvm_type_by_name(LLVMModuleRef M, const char *Name) {
  LLVMTypeRef Ty = LLVMGetTypeByName(M, (const char *) Name);
  if (Ty) {
    return Ty;
  } else {
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
extern "C"
int llvm_classify_value(LLVMValueRef Val) {
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
  } else if (LLVMIsAInstruction(Val)) {
    int result;
    result = LLVMGetInstructionOpcode(Val);
    return -(result + 1);
  } else if (LLVMIsAGlobalValue(Val)) {
    if (LLVMIsAFunction(Val)) return Function;
    if (LLVMIsAGlobalAlias(Val)) return GlobalAlias;
    if (LLVMIsAGlobalVariable(Val)) return GlobalVariable;
  } else {
    if (LLVMIsAArgument(Val)) return Argument;
    if (LLVMIsABasicBlock(Val)) return BasicBlock;
    if (LLVMIsAInlineAsm(Val)) return InlineAsm;
    if (LLVMIsAMDNode(Val)) return MDNode;
    if (LLVMIsAMDString(Val)) return MDString;
    if (LLVMIsAUndefValue(Val)) return UndefValue;
  }
  assert(0);
  return 0; /* stop compiler from complaining */
}

/* llvalue -> lltype */
extern "C"
LLVMTypeRef llvm_type_of(LLVMValueRef Val) {
  return LLVMTypeOf(Val);
}

/* llvalue -> string */
extern "C"
const char *llvm_value_name(LLVMValueRef Val) {
  return copy_string(LLVMGetValueName(Val));
}

/* string * llvalue -> unit */
extern "C"
void llvm_set_value_name(const char *Name, LLVMValueRef Val) {
  LLVMSetValueName(Val, Name);
}

/* llvalue -> unit */
extern "C"
void llvm_dump_value(LLVMValueRef Val) {
  LLVMDumpValue(Val);
}

/* llvalue -> string */
extern "C"
const char *llvm_string_of_llvalue(LLVMValueRef Val) {
  const char *ValueStr;
  char *ValueCStr;

  ValueCStr = LLVMPrintValueToString(Val);
  ValueStr = copy_string(ValueCStr);
  LLVMDisposeMessage(ValueCStr);

  return ValueStr;
}

/* llvalue * llvalue -> unit */
extern "C"
void llvm_replace_all_uses_with(LLVMValueRef OldVal, LLVMValueRef NewVal) {
  LLVMReplaceAllUsesWith(OldVal, NewVal);
}

/*--... Operations on uses .................................................--*/

/* llvalue -> lluse option */
extern "C"
LLVMUseRef llvm_use_begin(LLVMValueRef Val) {
  LLVMUseRef First;
  if ((First = LLVMGetFirstUse(Val))) {
    return First;
  } else {
    return NULL;
  }
}

/* llvalue -> lluse option */
extern "C"
LLVMUseRef llvm_use_succ(LLVMUseRef U) {
  LLVMUseRef Next;
  if ((Next = LLVMGetNextUse(U))) {
    return Next;
  } else {
    return NULL;
  }
}

/* lluse -> llvalue */
extern "C"
LLVMValueRef llvm_user(LLVMUseRef U) {
  return LLVMGetUser(U);
}

/* lluse -> llvalue */
extern "C"
LLVMValueRef llvm_used_value(LLVMUseRef U) {
  return LLVMGetUsedValue(U);
}

/*--... Operations on users ................................................--*/

/* llvalue * int -> llvalue */
extern "C"
LLVMValueRef llvm_operand(LLVMValueRef Val, int I) {
  return LLVMGetOperand(Val, I);
}

/* llvalue * int -> lluse */
extern "C"
LLVMUseRef llvm_operand_use(LLVMValueRef Val, int I) {
  return LLVMGetOperandUse(Val, I);
}

/* llvalue * int * llvalue -> unit */
extern "C"
void llvm_set_operand(LLVMValueRef Val1, int I, LLVMValueRef Val2) {
  LLVMSetOperand(Val1, I, Val2);
}

/* llvalue -> int */
extern "C"
int llvm_num_operands(LLVMValueRef Val) {
  return LLVMGetNumOperands(Val);
}

/*--... Operations on constants of (mostly) any type .......................--*/

/* llvalue -> bool */
extern "C"
int llvm_is_constant(LLVMValueRef Val) {
  return LLVMIsConstant(Val);
}

/* lltype -> llvalue */
extern "C"
LLVMValueRef llvm_const_null(LLVMTypeRef Ty) {
  return LLVMConstNull(Ty);
}

/* lltype -> llvalue */
extern "C"
LLVMValueRef llvm_const_all_ones(LLVMTypeRef Ty) {
  return LLVMConstAllOnes(Ty);
}

/* lltype -> llvalue */
extern "C"
LLVMValueRef llvm_const_pointer_null(LLVMTypeRef Ty) {
  return LLVMConstPointerNull(Ty);
}

/* lltype -> llvalue */
extern "C"
LLVMValueRef llvm_undef(LLVMTypeRef Ty) {
  return LLVMGetUndef(Ty);
}

/* llvalue -> bool */
extern "C"
int llvm_is_null(LLVMValueRef Val) {
  return LLVMIsNull(Val);
}

/* llvalue -> bool */
extern "C"
int llvm_is_undef(LLVMValueRef Val) {
  return LLVMIsUndef(Val);
}

/* llvalue -> int */
extern "C"
int llvm_constexpr_get_opcode(LLVMValueRef Val) {
  return LLVMIsAConstantExpr(Val) ? LLVMGetConstOpcode(Val) : 0;
}

/*--... Operations on instructions .........................................--*/

/* llvalue -> bool */
extern "C"
int llvm_has_metadata(LLVMValueRef Val) {
  return LLVMHasMetadata(Val);
}

/* llvalue * llmdkind -> llvalue option */
extern "C"
LLVMValueRef llvm_metadata(LLVMValueRef Val, int MDKindId) {
  LLVMValueRef MD;
  if ((MD = LLVMGetMetadata(Val, MDKindId))) {
    return MD;
  } else {
    return NULL;
  }
}

/* llvalue * llmdkind * llvalue -> unit */
extern "C"
void llvm_set_metadata(LLVMValueRef Val, int MDKindID, LLVMValueRef MD) {
  LLVMSetMetadata(Val, MDKindID, MD);
}

/* llvalue * llmdkind -> unit */
extern "C"
void llvm_clear_metadata(LLVMValueRef Val, int MDKindID) {
  LLVMSetMetadata(Val, MDKindID, NULL);
}

/*--... Operations on metadata .............................................--*/

/* llcontext * string -> llvalue */
extern "C"
LLVMValueRef llvm_mdstring(LLVMContextRef C, const char *S) {
  return LLVMMDStringInContext(C, S, strlen(S));
}

/* llcontext * llvalue array -> llvalue */
extern "C"
LLVMValueRef llvm_mdnode(LLVMContextRef C, LLVMValueRef *ElemVals, int ElemCount) {
  return LLVMMDNodeInContext(C, ElemVals, ElemCount);
}

/* llcontext -> llvalue */
extern "C"
LLVMValueRef llvm_mdnull(LLVMContextRef C) {
  return NULL;
}

/* llvalue -> string option */
extern "C"
const char *llvm_get_mdstring(LLVMValueRef Val) {
  const char *S;
  unsigned Len;

  if ((S = LLVMGetMDString(Val, &Len))) {
    char *Str;

    Str = (char *) malloc(sizeof(char) * (Len + 1));
    memcpy(Str, S, sizeof(char) * Len);
    Str[Len] = '\0';
    return Str;
  } else {
    return NULL;
  }
}

/* llvalue -> llvalue array */
extern "C"
LLVMValueRef *llvm_get_mdnode_operands(LLVMValueRef Val, int *ElemCount) {
  LLVMValueRef *Operands;
  Operands = (LLVMValueRef *) malloc(sizeof(LLVMValueRef) * LLVMGetMDNodeNumOperands(Val));
  LLVMGetMDNodeOperands(Val, Operands);
  (*ElemCount) = LLVMGetMDNodeNumOperands(Val);
  return Operands;
}

/* llmodule * string -> llvalue array */
extern "C"
LLVMValueRef *llvm_get_namedmd(LLVMModuleRef M, const char *Name, int *ElemCount) {
  LLVMValueRef *Nodes;
  Nodes = (LLVMValueRef *) malloc(sizeof(LLVMValueRef) * LLVMGetNamedMetadataNumOperands(M, Name));
  LLVMGetNamedMetadataOperands(M, Name, Nodes);
  (*ElemCount) = LLVMGetNamedMetadataNumOperands(M, Name);
  return Nodes;
}

/* llmodule * string * llvalue -> unit */
extern "C"
void llvm_append_namedmd(LLVMModuleRef M, const char *Name, LLVMValueRef Val) {
  LLVMAddNamedMetadataOperand(M, Name, Val);
}

/*--... Operations on scalar constants .....................................--*/

/* lltype * int -> llvalue */
extern "C"
LLVMValueRef llvm_const_int(LLVMTypeRef IntTy, int N) {
  return LLVMConstInt(IntTy, (long long) N, 1);
}

/* lltype * Int64.int * bool -> llvalue */
extern "C"
LLVMValueRef llvm_const_of_int64(LLVMTypeRef IntTy, long long N, int SExt) {
  return LLVMConstInt(IntTy, N, SExt);
}

/* llvalue -> Int64.int option */
extern "C"
long long *llvm_int64_of_const(LLVMValueRef Val) {
  if (LLVMIsAConstantInt(Val) && LLVMGetIntTypeWidth(LLVMTypeOf(Val)) <= 64) {
    long long *Res = (long long *) malloc(sizeof(long long));
    (*Res) = LLVMConstIntGetSExtValue(Val);
    return Res;
  } else {
    return NULL;
  }
}

/* lltype * string * int -> llvalue */
extern "C"
LLVMValueRef llvm_const_int_of_string(LLVMTypeRef IntTy, const char *S, int Radix) {
  return LLVMConstIntOfStringAndSize(IntTy, S, strlen(S), Radix);
}

/* llvalue * real -> llvalue */
extern "C"
LLVMValueRef llvm_const_float(LLVMTypeRef RealTy, double N) {
  return LLVMConstReal(RealTy, N);
}

/* llvalue -> real option */
extern "C"
double *llvm_float_of_const(LLVMValueRef Val) {
  LLVMBool LosesInfo;
  double Result;

  if (LLVMIsAConstantFP(Val)) {
    Result = LLVMConstRealGetDouble(Val, &LosesInfo);
    if (LosesInfo) {
      return NULL;
    } else {
      double *Res = (double *) malloc(sizeof(double));
      (*Res) = Result;
      return Res;
    }
  } else {
    return NULL;
  }
}

/* lltype * string -> llvalue */
extern "C"
LLVMValueRef llvm_const_float_of_string(LLVMTypeRef RealTy, const char *S) {
  return LLVMConstRealOfStringAndSize(RealTy, S, strlen(S));
}

/*--... Operations on composite constants ..................................--*/

/* llcontext * string -> llvalue */
extern "C"
LLVMValueRef llvm_const_string(LLVMContextRef C, const char *S) {
  return LLVMConstStringInContext(C, S, strlen(S), 1);
}

/* llcontext * string -> llvalue */
extern "C"
LLVMValueRef llvm_const_stringz(LLVMContextRef C, const char *S) {
  return LLVMConstStringInContext(C, S, strlen(S), 0);
}

/* lltype * llvalue array -> llvalue */
extern "C"
LLVMValueRef llvm_const_array(LLVMTypeRef ElemTy, LLVMValueRef *ElemVals, int ElemCount) {
  return LLVMConstArray(ElemTy, ElemVals, ElemCount);
}

/* llcontext * llvalue array -> llvalue */
extern "C"
LLVMValueRef llvm_const_struct(LLVMContextRef C, LLVMValueRef *ElemVals, int ElemCount) {
  return LLVMConstStructInContext(C, ElemVals, ElemCount, 0);
}

/* lltype * llvalue array -> llvalue */
extern "C"
LLVMValueRef llvm_const_named_struct(LLVMTypeRef StructTy, LLVMValueRef *ElemVals, int ElemCount) {
  return LLVMConstNamedStruct(StructTy, ElemVals, ElemCount);
}

/* llcontext * llvalue array -> llvalue */
extern "C"
LLVMValueRef llvm_const_packed_struct(LLVMContextRef C, LLVMValueRef *ElemVals, int ElemCount) {
  return LLVMConstStructInContext(C, ElemVals, ElemCount, 1);
}

/* llvalue array -> llvalue */
extern "C"
LLVMValueRef llvm_const_vector(LLVMValueRef *ElemVals, int ElemCount) {
  return LLVMConstVector(ElemVals, ElemCount);
}

/* llvalue -> string option */
extern "C"
const char *llvm_string_of_const(LLVMValueRef Val) {
  const char *S;
  size_t Len;
  char *Str;

  if (LLVMIsAConstantDataSequential(Val) && LLVMIsConstantString(Val)) {
    S = LLVMGetAsString(Val, &Len);
    Str = (char *) malloc(sizeof(char) * (Len + 1));
    memcpy(Str, S, Len);
    Str[Len] = '\0';

    return Str;
  } else {
    return NULL;
  }
}

/* llvalue * int -> llvalue */
extern "C"
LLVMValueRef llvm_const_element(LLVMValueRef Val, int N) {
  return LLVMGetElementAsConstant(Val, N);
}

/*--... Constant expressions ...............................................--*/

/* lltype -> llvalue */
extern "C"
LLVMValueRef llvm_align_of(LLVMTypeRef Ty) {
  return LLVMAlignOf(Ty);
}

/* lltype -> llvalue */
extern "C"
LLVMValueRef llvm_size_of(LLVMTypeRef Ty) {
  return LLVMSizeOf(Ty);
}

/* llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_neg(LLVMValueRef Val) {
  return LLVMConstNeg(Val);
}

/* llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_nsw_neg(LLVMValueRef Val) {
  return LLVMConstNSWNeg(Val);
}

/* llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_nuw_neg(LLVMValueRef Val) {
  return LLVMConstNUWNeg(Val);
}

/* llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_fneg(LLVMValueRef Val) {
  return LLVMConstFNeg(Val);
}

/* llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_not(LLVMValueRef Val) {
  return LLVMConstNot(Val);
}

/* llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_add(LLVMValueRef LHS, LLVMValueRef RHS) {
  return LLVMConstAdd(LHS, RHS);
}

/* llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_nsw_add(LLVMValueRef LHS, LLVMValueRef RHS) {
  return LLVMConstNSWAdd(LHS, RHS);
}

/* llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_nuw_add(LLVMValueRef LHS, LLVMValueRef RHS) {
  return LLVMConstNUWAdd(LHS, RHS);
}

/* llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_fadd(LLVMValueRef LHS, LLVMValueRef RHS) {
  return LLVMConstFAdd(LHS, RHS);
}

/* llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_sub(LLVMValueRef LHS, LLVMValueRef RHS) {
  return LLVMConstSub(LHS, RHS);
}

/* llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_nsw_sub(LLVMValueRef LHS, LLVMValueRef RHS) {
  return LLVMConstNSWSub(LHS, RHS);
}

/* llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_nuw_sub(LLVMValueRef LHS, LLVMValueRef RHS) {
  return LLVMConstNUWSub(LHS, RHS);
}

/* llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_fsub(LLVMValueRef LHS, LLVMValueRef RHS) {
  return LLVMConstFSub(LHS, RHS);
}

/* llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_mul(LLVMValueRef LHS, LLVMValueRef RHS) {
  return LLVMConstMul(LHS, RHS);
}

/* llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_nsw_mul(LLVMValueRef LHS, LLVMValueRef RHS) {
  return LLVMConstNSWMul(LHS, RHS);
}

/* llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_nuw_mul(LLVMValueRef LHS, LLVMValueRef RHS) {
  return LLVMConstNUWMul(LHS, RHS);
}

/* llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_fmul(LLVMValueRef LHS, LLVMValueRef RHS) {
  return LLVMConstFMul(LHS, RHS);
}

/* llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_udiv(LLVMValueRef LHS, LLVMValueRef RHS) {
  return LLVMConstUDiv(LHS, RHS);
}

/* llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_sdiv(LLVMValueRef LHS, LLVMValueRef RHS) {
  return LLVMConstSDiv(LHS, RHS);
}

/* llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_exact_sdiv(LLVMValueRef LHS, LLVMValueRef RHS) {
  return LLVMConstExactSDiv(LHS, RHS);
}

/* llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_fdiv(LLVMValueRef LHS, LLVMValueRef RHS) {
  return LLVMConstFDiv(LHS, RHS);
}

/* llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_urem(LLVMValueRef LHS, LLVMValueRef RHS) {
  return LLVMConstURem(LHS, RHS);
}

/* llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_srem(LLVMValueRef LHS, LLVMValueRef RHS) {
  return LLVMConstSRem(LHS, RHS);
}

/* llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_frem(LLVMValueRef LHS, LLVMValueRef RHS) {
  return LLVMConstFRem(LHS, RHS);
}

/* llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_and(LLVMValueRef LHS, LLVMValueRef RHS) {
  return LLVMConstAnd(LHS, RHS);
}

/* llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_or(LLVMValueRef LHS, LLVMValueRef RHS) {
  return LLVMConstOr(LHS, RHS);
}

/* llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_xor(LLVMValueRef LHS, LLVMValueRef RHS) {
  return LLVMConstXor(LHS, RHS);
}

/* Icmp.t * llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_icmp(int Pred, LLVMValueRef LHSConstant, LLVMValueRef RHSConstant) {
  return LLVMConstICmp((LLVMIntPredicate) (Pred + LLVMIntEQ), LHSConstant, RHSConstant);
}

/* Fcmp.t * llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_fcmp(int Pred, LLVMValueRef LHSConstant, LLVMValueRef RHSConstant) {
  return LLVMConstFCmp((LLVMRealPredicate) Pred, LHSConstant, RHSConstant);
}

/* llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_shl(LLVMValueRef LHS, LLVMValueRef RHS) {
  return LLVMConstShl(LHS, RHS);
}

/* llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_ashr(LLVMValueRef LHS, LLVMValueRef RHS) {
  return LLVMConstAShr(LHS, RHS);
}

/* llvalue * llvalue array -> llvalue */
extern "C"
LLVMValueRef llvm_const_gep(LLVMValueRef ConstantVal, LLVMValueRef *Indices, int IndexCount) {
  return LLVMConstGEP(ConstantVal, Indices, IndexCount);
}

/* llvalue * llvalue array -> llvalue */
extern "C"
LLVMValueRef llvm_const_in_bounds_gep(LLVMValueRef ConstantVal, LLVMValueRef *Indices, int IndexCount) {
  return LLVMConstInBoundsGEP(ConstantVal, Indices, IndexCount);
}

/* llvalue * lltype -> llvalue */
extern "C"
LLVMValueRef llvm_const_trunc(LLVMValueRef Val, LLVMTypeRef Ty) {
  return LLVMConstTrunc(Val, Ty);
}

/* llvalue * lltype -> llvalue */
extern "C"
LLVMValueRef llvm_const_sext(LLVMValueRef Val, LLVMTypeRef Ty) {
  return LLVMConstSExt(Val, Ty);
}

/* llvalue * lltype -> llvalue */
extern "C"
LLVMValueRef llvm_const_zext(LLVMValueRef Val, LLVMTypeRef Ty) {
  return LLVMConstZExt(Val, Ty);
}

/* llvalue * lltype -> llvalue */
extern "C"
LLVMValueRef llvm_const_fptrunc(LLVMValueRef Val, LLVMTypeRef Ty) {
  return LLVMConstFPTrunc(Val, Ty);
}

/* llvalue * lltype -> llvalue */
extern "C"
LLVMValueRef llvm_const_fpext(LLVMValueRef Val, LLVMTypeRef Ty) {
  return LLVMConstFPExt(Val, Ty);
}

/* llvalue * lltype -> llvalue */
extern "C"
LLVMValueRef llvm_const_uitofp(LLVMValueRef Val, LLVMTypeRef Ty) {
  return LLVMConstUIToFP(Val, Ty);
}

/* llvalue * lltype -> llvalue */
extern "C"
LLVMValueRef llvm_const_sitofp(LLVMValueRef Val, LLVMTypeRef Ty) {
  return LLVMConstSIToFP(Val, Ty);
}

/* llvalue * lltype -> llvalue */
extern "C"
LLVMValueRef llvm_const_fptoui(LLVMValueRef Val, LLVMTypeRef Ty) {
  return LLVMConstFPToUI(Val, Ty);
}

/* llvalue * lltype -> llvalue */
extern "C"
LLVMValueRef llvm_const_fptosi(LLVMValueRef Val, LLVMTypeRef Ty) {
  return LLVMConstFPToSI(Val, Ty);
}

/* llvalue * lltype -> llvalue */
extern "C"
LLVMValueRef llvm_const_ptrtoint(LLVMValueRef Val, LLVMTypeRef Ty) {
  return LLVMConstPtrToInt(Val, Ty);
}

/* llvalue * lltype -> llvalue */
extern "C"
LLVMValueRef llvm_const_inttoptr(LLVMValueRef Val, LLVMTypeRef Ty) {
  return LLVMConstIntToPtr(Val, Ty);
}

/* llvalue * lltype -> llvalue */
extern "C"
LLVMValueRef llvm_const_bitcast(LLVMValueRef Val, LLVMTypeRef Ty) {
  return LLVMConstBitCast(Val, Ty);
}

/* llvalue * lltype -> llvalue */
extern "C"
LLVMValueRef llvm_const_zext_or_bitcast(LLVMValueRef Val, LLVMTypeRef Ty) {
  return LLVMConstZExtOrBitCast(Val, Ty);
}

/* llvalue * lltype -> llvalue */
extern "C"
LLVMValueRef llvm_const_sext_or_bitcast(LLVMValueRef Val, LLVMTypeRef Ty) {
  return LLVMConstSExtOrBitCast(Val, Ty);
}

/* llvalue * lltype -> llvalue */
extern "C"
LLVMValueRef llvm_const_trunc_or_bitcast(LLVMValueRef Val, LLVMTypeRef Ty) {
  return LLVMConstTruncOrBitCast(Val, Ty);
}

/* llvalue * lltype -> llvalue */
extern "C"
LLVMValueRef llvm_const_pointercast(LLVMValueRef Val, LLVMTypeRef Ty) {
  return LLVMConstPointerCast(Val, Ty);
}

/* llvalue * lltype -> llvalue */
extern "C"
LLVMValueRef llvm_const_intcast(LLVMValueRef Val, LLVMTypeRef Ty, int IsSigned) {
  return LLVMConstIntCast(Val, Ty, IsSigned);
}

/* llvalue * lltype -> llvalue */
extern "C"
LLVMValueRef llvm_const_fpcast(LLVMValueRef Val, LLVMTypeRef Ty) {
  return LLVMConstFPCast(Val, Ty);
}

/* llvalue * llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_select(LLVMValueRef If, LLVMValueRef Then, LLVMValueRef Else) {
  return LLVMConstSelect(If, Then, Else);
}

/* llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_extractelement(LLVMValueRef Val1, LLVMValueRef Val2) {
  return LLVMConstExtractElement(Val1, Val2);
}

/* llvalue * llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_insertelement(LLVMValueRef Val1, LLVMValueRef Val2, LLVMValueRef Val3) {
  return LLVMConstInsertElement(Val1, Val2, Val3);
}

/* llvalue * llvalue * llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_const_shufflevector(LLVMValueRef Val1, LLVMValueRef Val2, LLVMValueRef Val3) {
  return LLVMConstShuffleVector(Val1, Val2, Val3);
}

/* llvalue * int array -> llvalue */
extern "C"
LLVMValueRef llvm_const_extractvalue(LLVMValueRef Aggregate, int *Indices, int IndexCount) {
  unsigned *Idxs = (unsigned *) malloc(sizeof(unsigned) * IndexCount);
  int I;
  LLVMValueRef Res;

  for (I = 0; I < IndexCount; I += 1) {
    Idxs[I] = Indices[I];
  }

  Res = LLVMConstExtractValue(Aggregate, Idxs, IndexCount);
  free(Idxs);
  return Res;
}

/* llvalue * llvalue * int array -> llvalue */
extern "C"
LLVMValueRef llvm_const_insertvalue(LLVMValueRef Aggregate, LLVMValueRef Val, int *Indices, int IndexCount) {
  unsigned *Idxs = (unsigned *) malloc(sizeof(unsigned) * IndexCount);
  int I;
  LLVMValueRef Res;

  for (I = 0; I < IndexCount; I += 1) {
    Idxs[I] = Indices[I];
  }

  Res = LLVMConstInsertValue(Aggregate, Val, Idxs, IndexCount);
  free(Idxs);
  return Res;
}

/* lltype * string * string * bool * bool -> llvalue */
extern "C"
LLVMValueRef llvm_const_inline_asm(LLVMTypeRef Ty, const char *Asm, const char *Constraints, int HasSideEffect, int IsAlignStack) {
  return LLVMConstInlineAsm(Ty, Asm, Constraints, HasSideEffect, IsAlignStack);
}

/* llvalue * llbasicblock -> llvalue */
extern "C"
LLVMValueRef llvm_block_address(LLVMValueRef Val, LLVMBasicBlockRef BB) {
  return LLVMBlockAddress(Val, BB);
}

/*--... Operations on global variables, functions, and aliases (globals) ...--*/

/* llvalue -> llmodule */
extern "C"
LLVMModuleRef llvm_global_parent(LLVMValueRef Global) {
  return LLVMGetGlobalParent(Global);
}

/* llvalue -> bool */
extern "C"
int llvm_is_declaration(LLVMValueRef Global) {
  return LLVMIsDeclaration(Global);
}

/* llvalue -> Linkage.t */
extern "C"
int llvm_linkage(LLVMValueRef Global) {
  return LLVMGetLinkage(Global);
}

/* Linkage.t * llvalue -> unit */
extern "C"
void llvm_set_linkage(int Linkage, LLVMValueRef Global) {
  LLVMSetLinkage(Global, (LLVMLinkage) Linkage);
}

/* llvalue -> bool */
extern "C"
int llvm_unnamed_addr(LLVMValueRef Global) {
  return LLVMHasUnnamedAddr(Global);
}

/* bool * llvalue -> unit */
extern "C"
void llvm_set_unnamed_addr(int UseUnnamedAddr, LLVMValueRef Global) {
  LLVMSetUnnamedAddr(Global, UseUnnamedAddr);
}

/* llvalue -> string */
extern "C"
const char *llvm_section(LLVMValueRef Global) {
  return LLVMGetSection(Global);
}

/* string * llvalue -> unit */
extern "C"
void llvm_set_section(const char *Section, LLVMValueRef Global) {
  LLVMSetSection(Global, Section);
}

/* llvalue -> Visibility.t */
extern "C"
int llvm_visibility(LLVMValueRef Global) {
  return LLVMGetVisibility(Global);
}

/* Visibility.t * llvalue -> unit */
extern "C"
void llvm_set_visibility(int Viz, LLVMValueRef Global) {
  LLVMSetVisibility(Global, (LLVMVisibility) Viz);
}

/* llvalue -> DLLStorageClass.t */
extern "C"
int llvm_dll_storage_class(LLVMValueRef Global) {
  return LLVMGetDLLStorageClass(Global);
}

/* DLLStorageClass.t * llvalue -> unit */
extern "C"
void llvm_set_dll_storage_class(int Viz, LLVMValueRef Global) {
  LLVMSetDLLStorageClass(Global, (LLVMDLLStorageClass) Viz);
}

/* llvalue -> int */
extern "C"
int llvm_alignment(LLVMValueRef Global) {
  return LLVMGetAlignment(Global);
}

/* int * llvalue -> unit */
extern "C"
void llvm_set_alignment(int Bytes, LLVMValueRef Global) {
  LLVMSetAlignment(Global, Bytes);
}

/* llvalue -> bool */
extern "C"
int llvm_is_global_constant(LLVMValueRef Val) {
  return LLVMIsGlobalConstant(Val);
}

/* bool * llvalue -> unit */
extern "C"
void llvm_set_global_constant(int IsConstant, LLVMValueRef Val) {
  LLVMSetGlobalConstant(Val, IsConstant);
}

/*--... Operations on global variables .....................................--*/

/* lltype * string * llmodule -> llvalue */
extern "C"
LLVMValueRef llvm_declare_global(LLVMTypeRef Ty, const char *Name, LLVMModuleRef M) {
  LLVMValueRef GlobalVar;
  if ((GlobalVar = LLVMGetNamedGlobal(M, Name))) {
    if (LLVMGetElementType(LLVMTypeOf(GlobalVar)) != Ty) {
      return LLVMConstBitCast(GlobalVar, LLVMPointerType(Ty, 0));
    } else {
      return GlobalVar;
    }
  } else {
    return LLVMAddGlobal(M, Ty, Name);
  }
}

/* lltype * string * int * llmodule -> llvalue */
extern "C"
LLVMValueRef llvm_declare_qualified_global(LLVMTypeRef Ty, const char *Name, int AddrSpace, LLVMModuleRef M) {
  LLVMValueRef GlobalVar;
  if ((GlobalVar = LLVMGetNamedGlobal(M, Name))) {
    if (LLVMGetElementType(LLVMTypeOf(GlobalVar)) != Ty) {
      return LLVMConstBitCast(GlobalVar, LLVMPointerType(Ty, AddrSpace));
    } else {
      return GlobalVar;
    }
  } else {
    return LLVMAddGlobalInAddressSpace(M, Ty, Name, AddrSpace);
  }
}

/* string * llvalue * llmodule -> llvalue */
extern "C"
LLVMValueRef llvm_define_global(const char *Name, LLVMValueRef Initializer, LLVMModuleRef M) {
  LLVMValueRef GlobalVar = LLVMAddGlobal(M, LLVMTypeOf(Initializer), Name);
  LLVMSetInitializer(GlobalVar, Initializer);
  return GlobalVar;
}

/* string * llvalue * int * llmodule -> llvalue */
extern "C"
LLVMValueRef llvm_define_qualified_global(const char *Name, LLVMValueRef Initializer, int AddrSpace, LLVMModuleRef M) {
  LLVMValueRef GlobalVar = LLVMAddGlobalInAddressSpace(M, LLVMTypeOf(Initializer), Name, AddrSpace);
  LLVMSetInitializer(GlobalVar, Initializer);
  return GlobalVar;
}

/* string * llmodule -> llvalue option */
extern "C"
LLVMValueRef llvm_lookup_global(const char *Name, LLVMModuleRef M) {
  LLVMValueRef GlobalVar;
  if ((GlobalVar = LLVMGetNamedGlobal(M, Name))) {
    return GlobalVar;
  } else {
    return NULL;
  }
}

/* llvalue -> unit */
extern "C"
void llvm_delete_global(LLVMValueRef GlobalVar) {
  LLVMDeleteGlobal(GlobalVar);
}

/* llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_global_initializer(LLVMValueRef GlobalVar) {
  return LLVMGetInitializer(GlobalVar);
}

/* llvalue * llvalue -> unit */
extern "C"
void llvm_set_initializer(LLVMValueRef ConstantVal, LLVMValueRef GlobalVar) {
  LLVMSetInitializer(GlobalVar, ConstantVal);
}

/* llvalue -> unit */
extern "C"
void llvm_remove_initializer(LLVMValueRef GlobalVar) {
  LLVMSetInitializer(GlobalVar, NULL);
}

/* llvalue -> bool */
extern "C"
int llvm_is_thread_local(LLVMValueRef GlobalVar) {
  return LLVMIsThreadLocal(GlobalVar);
}

/* bool * llvalue -> unit */
extern "C"
void llvm_set_thread_local(int IsThreadLocal, LLVMValueRef GlobalVar) {
  LLVMSetThreadLocal(GlobalVar, IsThreadLocal);
}

/* llvalue -> ThreadLocalMode.t */
extern "C"
int llvm_thread_local_mode(LLVMValueRef GlobalVar) {
  return LLVMGetThreadLocalMode(GlobalVar);
}

/* ThreadLocalMode.t * llvalue -> unit */
extern "C"
void llvm_set_thread_local_mode(int ThreadLocalMode, LLVMValueRef GlobalVar) {
  LLVMSetThreadLocalMode(GlobalVar, (LLVMThreadLocalMode) ThreadLocalMode);
}

/* llvalue -> bool */
extern "C"
int llvm_is_externally_initialized(LLVMValueRef GlobalVar) {
  return LLVMIsExternallyInitialized(GlobalVar);
}

/* bool * llvalue -> unit */
extern "C"
void llvm_set_externally_initialized(int IsExternallyInitialized, LLVMValueRef GlobalVar) {
  LLVMSetExternallyInitialized(GlobalVar, IsExternallyInitialized);
}

/* llmodule -> (llmodule, llvalue) llpos */
extern "C"
void *llvm_global_begin(LLVMModuleRef M, int *Tag) {
  LLVMValueRef First = LLVMGetFirstGlobal(M);
  if (First) {
    (*Tag) = 1;
    return First;
  } else {
    (*Tag) = 0;
    return M;
  }
}

/* llvalue -> (llmodule, llvalue) llpos */
extern "C"
void *llvm_global_succ(LLVMValueRef Val, int *Tag) {
  LLVMValueRef Next = LLVMGetNextGlobal(Val);
  if (Next) {
    (*Tag) = 1;
    return Next;
  } else {
    (*Tag) = 0;
    return LLVMGetGlobalParent(Val);
  }
}

/* llmodule -> (llmodule, llvalue) llrev_pos */
extern "C"
void *llvm_global_end(LLVMModuleRef M, int *Tag) {
  LLVMValueRef Last = LLVMGetLastGlobal(M);
  if (Last) {
    (*Tag) = 1;
    return Last;
  } else {
    (*Tag) = 0;
    return M;
  }
}

/* llvalue -> (llmodule, llvalue) llrev_pos */
extern "C"
void *llvm_global_pred(LLVMValueRef Val, int *Tag) {
  LLVMValueRef Prev = LLVMGetPreviousGlobal(Val);
  if (Prev) {
    (*Tag) = 1;
    return Prev;
  } else {
    (*Tag) = 0;
    return LLVMGetGlobalParent(Val);
  }
}

/*--... Operations on aliases ..............................................--*/

/* llmodule * lltype * llvalue * string -> llvalue */
extern "C"
LLVMValueRef llvm_add_alias(LLVMModuleRef M, LLVMTypeRef Ty, LLVMValueRef Aliasee, const char *Name) {
  return LLVMAddAlias(M, Ty, Aliasee, Name);
}

/*--... Operations on functions ............................................--*/

/* string * lltype * llmodule -> llvalue */
extern "C"
LLVMValueRef llvm_declare_function(const char *Name, LLVMTypeRef Ty, LLVMModuleRef M) {
  LLVMValueRef Fn;
  if ((Fn = LLVMGetNamedFunction(M, Name))) {
    if (LLVMGetElementType(LLVMTypeOf(Fn)) != Ty) {
      return LLVMConstBitCast(Fn, LLVMPointerType(Ty, 0));
    } else {
      return Fn;
    }
  } else {
    return LLVMAddFunction(M, Name, Ty);
  }
}

/* string * lltype * llmodule -> llvalue */
extern "C"
LLVMValueRef llvm_define_function(const char *Name, LLVMTypeRef Ty, LLVMModuleRef M) {
  LLVMValueRef Fn = LLVMAddFunction(M, Name, Ty);
  LLVMAppendBasicBlockInContext(LLVMGetTypeContext(Ty), Fn, "entry");
  return Fn;
}

/* string * llmodule -> llvalue option */
extern "C"
LLVMValueRef llvm_lookup_function(const char *Name, LLVMModuleRef M) {
  LLVMValueRef Fn;
  if ((Fn = LLVMGetNamedFunction(M, Name))) {
    return Fn;
  } else {
    return NULL;
  }
}

/* llvalue -> unit */
extern "C"
void llvm_delete_function(LLVMValueRef Fn) {
  LLVMDeleteFunction(Fn);
}

/* llvalue -> bool */
extern "C"
int llvm_is_intrinsic(LLVMValueRef Fn) {
  return (LLVMGetIntrinsicID(Fn) > 0 ? 1 : 0);
}

/* llvalue ->int */
extern "C"
int llvm_function_call_conv(LLVMValueRef Fn) {
  return LLVMGetFunctionCallConv(Fn);
}

/* int * llvalue -> unit */
extern "C"
void llvm_set_function_call_conv(int Id, LLVMValueRef Fn) {
  LLVMSetFunctionCallConv(Fn, Id);
}

/* llvalue -> string option */
extern "C"
const char *llvm_gc(LLVMValueRef Fn) {
  const char *GC;
  const char *Name;

  if ((GC = LLVMGetGC(Fn))) {
    Name = copy_string(GC);
    return Name;
  } else {
    return NULL;
  }
}

/* string option * llvalue -> unit */
extern "C"
void llvm_set_gc(const char *GC, LLVMValueRef Fn) {
  LLVMSetGC(Fn, GC == NULL ? NULL : GC);
}

/* llmodule -> (llmodule, llvalue) llpos */
extern "C"
void *llvm_function_begin(LLVMModuleRef M, int *Tag) {
  LLVMValueRef First = LLVMGetFirstFunction(M);
  if (First) {
    (*Tag) = 1;
    return First;
  } else {
    (*Tag) = 0;
    return M;
  }
}

/* llvalue -> (llmodule, llvalue) llpos */
extern "C"
void *llvm_function_succ(LLVMValueRef Val, int *Tag) {
  LLVMValueRef Next = LLVMGetNextFunction(Val);
  if (Next) {
    (*Tag) = 1;
    return Next;
  } else {
    (*Tag) = 0;
    return LLVMGetGlobalParent(Val);
  }
}

/* llmodule -> (llmodule, llvalue) llrev_pos */
extern "C"
void *llvm_function_end(LLVMModuleRef M, int *Tag) {
  LLVMValueRef Last = LLVMGetLastFunction(M);
  if (Last) {
    (*Tag) = 1;
    return Last;
  } else {
    (*Tag) = 0;
    return M;
  }
}

/* llvalue -> (llmodule, llvalue) llrev_pos */
extern "C"
void *llvm_function_pred(LLVMValueRef Val, int *Tag) {
  LLVMValueRef Prev = LLVMGetPreviousFunction(Val);
  if (Prev) {
    (*Tag) = 1;
    return Prev;
  } else {
    (*Tag) = 0;
    return LLVMGetGlobalParent(Val);
  }
}

/* llvalue * Int32.int -> unit */
extern "C"
void llvm_add_function_attr(LLVMValueRef Arg, int PA) {
  LLVMAddFunctionAttr(Arg, (LLVMAttribute) PA);
}

/* llvalue * Int32.int -> unit */
extern "C"
void llvm_remove_function_attr(LLVMValueRef Arg, int PA) {
  LLVMRemoveFunctionAttr(Arg, (LLVMAttribute) PA);
}

/* llvalue -> Int32.int */
extern "C"
int llvm_function_attr(LLVMValueRef Fn) {
  return LLVMGetFunctionAttr(Fn);
}

/* llvalue * string * string -> unit */
extern "C"
void llvm_add_target_dependent_function_attr(LLVMValueRef Arg, const char *A, const char *V) {
  LLVMAddTargetDependentFunctionAttr(Arg, A, V);
}

/*--... Operations on parameters ...........................................--*/

/* llvalue -> llvalue array */
extern "C"
LLVMValueRef *llvm_params(LLVMValueRef Fn, int *Len) {
  LLVMValueRef *Params = (LLVMValueRef *) malloc(sizeof(LLVMValueRef) * LLVMCountParams(Fn));
  LLVMGetParams(Fn, Params);
  (*Len) = LLVMCountParams(Fn);
  return Params;
}

/* llvalue * int -> llvalue */
extern "C"
LLVMValueRef llvm_param(LLVMValueRef Fn, int Index) {
  return LLVMGetParam(Fn, Index);
}

/* llvalue -> Int32.int */
extern "C"
int llvm_param_attr(LLVMValueRef Param) {
  return LLVMGetAttribute(Param);
}

/* llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_param_parent(LLVMValueRef Param) {
  return LLVMGetParamParent(Param);
}

/* llvalue -> (llvalue, llvalue) llpos */
extern "C"
LLVMValueRef llvm_param_begin(LLVMValueRef Fn, int *Tag) {
  LLVMValueRef First = LLVMGetFirstParam(Fn);
  if (First) {
    (*Tag) = 1;
    return First;
  } else {
    (*Tag) = 0;
    return Fn;
  }
}

/* llvalue -> (llvalue, llvalue) llpos */
extern "C"
LLVMValueRef llvm_param_succ(LLVMValueRef Val, int *Tag) {
  LLVMValueRef Next = LLVMGetNextParam(Val);
  if (Next) {
    (*Tag) = 1;
    return Next;
  } else {
    (*Tag) = 0;
    return LLVMGetParamParent(Val);
  }
}

/* llvalue -> (llvalue, llvalue) llrev_pos */
extern "C"
LLVMValueRef llvm_param_end(LLVMValueRef Fn, int *Tag) {
  LLVMValueRef Last = LLVMGetLastParam(Fn);
  if (Last) {
    (*Tag) = 1;
    return Last;
  } else {
    (*Tag) = 0;
    return Fn;
  }
}

/* llvalue -> (llvalue, llvalue) llrev_pos */
extern "C"
LLVMValueRef llvm_param_pred(LLVMValueRef Val, int *Tag) {
  LLVMValueRef Prev = LLVMGetPreviousParam(Val);
  if (Prev) {
    (*Tag) = 1;
    return Prev;
  } else {
    (*Tag) = 0;
    return LLVMGetParamParent(Val);
  }
}

/* llvalue * Int32.int -> unit */
extern "C"
void llvm_add_param_attr(LLVMValueRef Arg, int PA) {
  LLVMAddAttribute(Arg, (LLVMAttribute) PA);
}

/* llvalue * Int32.int -> unit */
extern "C"
void llvm_remove_param_attr(LLVMValueRef Arg, int PA) {
  LLVMRemoveAttribute(Arg, (LLVMAttribute) PA);
}

/* llvalue * int -> unit */
extern "C"
void llvm_set_param_alignment(LLVMValueRef Arg, int Align) {
  LLVMSetParamAlignment(Arg, Align);
}

/*--... Operations on basic blocks .........................................--*/

/* llbasicblock -> llvalue */
extern "C"
LLVMValueRef llvm_value_of_block(LLVMBasicBlockRef BB) {
  return LLVMBasicBlockAsValue(BB);
}

/* llvalue -> bool */
extern "C"
int llvm_value_is_block(LLVMValueRef Val) {
  return LLVMValueIsBasicBlock(Val);
}

/* llvalue -> llbasicblock */
extern "C"
LLVMBasicBlockRef llvm_block_of_value(LLVMValueRef Val) {
  return LLVMValueAsBasicBlock(Val);
}

/* llbasicblock -> llvalue */
extern "C"
LLVMValueRef llvm_block_parent(LLVMBasicBlockRef BB) {
  return LLVMGetBasicBlockParent(BB);
}

/* llvalue -> llbasicblock array */
extern "C"
LLVMBasicBlockRef *llvm_basic_blocks(LLVMValueRef Fn, int *Len) {
  LLVMBasicBlockRef *MLArray = (LLVMBasicBlockRef *) malloc(sizeof(LLVMBasicBlockRef) * LLVMCountBasicBlocks(Fn));
  LLVMGetBasicBlocks(Fn, MLArray);
  (*Len) = LLVMCountBasicBlocks(Fn);
  return MLArray;
}

/* llvalue -> llbasicblock */
extern "C"
LLVMBasicBlockRef llvm_entry_block(LLVMValueRef Val) {
  return LLVMGetEntryBasicBlock(Val);
}

/* llbasicblock -> unit */
extern "C"
void llvm_delete_block(LLVMBasicBlockRef BB) {
  LLVMDeleteBasicBlock(BB);
}

/* llbasicblock -> unit */
extern "C"
void llvm_remove_block(LLVMBasicBlockRef BB) {
  LLVMRemoveBasicBlockFromParent(BB);
}

/* llbasicblock * llbasicblock -> unit */
extern "C"
void llvm_move_block_before(LLVMBasicBlockRef Pos, LLVMBasicBlockRef BB) {
  LLVMMoveBasicBlockBefore(BB, Pos);
}

/* llbasicblock * llbasicblock -> unit */
extern "C"
void llvm_move_block_after(LLVMBasicBlockRef Pos, LLVMBasicBlockRef BB) {
  LLVMMoveBasicBlockAfter(BB, Pos);
}

/* llcontext * string * llvalue -> llbasicblock */
extern "C"
LLVMBasicBlockRef llvm_append_block(LLVMContextRef C, const char *Name, LLVMValueRef Fn) {
  return LLVMAppendBasicBlockInContext(C, Fn, Name);
}

/* llcontext * string * llbasicblock -> llbasicblock */
extern "C"
LLVMBasicBlockRef llvm_insert_block(LLVMContextRef C, const char *Name, LLVMBasicBlockRef BB) {
  return LLVMInsertBasicBlockInContext(C, BB, Name);
}

/* llvalue -> (llvalue, llbasicblock) llpos */
extern "C"
void *llvm_block_begin(LLVMValueRef Fn, int *Tag) {
  LLVMBasicBlockRef First = LLVMGetFirstBasicBlock(Fn);
  if (First) {
    (*Tag) = 1;
    return First;
  } else {
    (*Tag) = 0;
    return Fn;
  }
}

/* llbasicblock -> (llvalue, llbasicblock) llpos */
extern "C"
void *llvm_block_succ(LLVMBasicBlockRef BB, int *Tag) {
  LLVMBasicBlockRef Next = LLVMGetNextBasicBlock(BB);
  if (Next) {
    (*Tag) = 1;
    return Next;
  } else {
    (*Tag) = 0;
    return LLVMGetBasicBlockParent(BB);
  }
}

/* llvalue -> (llvalue, llbasicblock) llrev_pos */
extern "C"
void *llvm_block_end(LLVMValueRef Fn, int *Tag) {
  LLVMBasicBlockRef Last = LLVMGetLastBasicBlock(Fn);
  if (Last) {
    (*Tag) = 1;
    return Last;
  } else {
    (*Tag) = 0;
    return Fn;
  }
}

/* llbasicblock -> (llvalue, llbasicblock) llrev_pos */
extern "C"
void *llvm_block_pred(LLVMBasicBlockRef BB, int *Tag) {
  LLVMBasicBlockRef Prev = LLVMGetPreviousBasicBlock(BB);
  if (Prev) {
    (*Tag) = 1;
    return Prev;
  } else {
    (*Tag) = 0;
    return LLVMGetBasicBlockParent(BB);
  }
}

/* llbasicblock -> llvalue option */
extern "C"
void *llvm_block_terminator(LLVMBasicBlockRef BB) {
  LLVMValueRef Term = LLVMGetBasicBlockTerminator(BB);
  if (Term) {
    return Term;
  } else {
    return NULL;
  }
}

/*--... Operations on instructions .........................................--*/

/* llvalue -> llbasicblock */
extern "C"
void *llvm_instr_parent(LLVMValueRef Inst) {
  return LLVMGetInstructionParent(Inst);
}

/* llbasicblock -> (llbasicblock, llvalue) llpos */
extern "C"
void *llvm_instr_begin(LLVMBasicBlockRef BB, int *Tag) {
  LLVMValueRef First = LLVMGetFirstInstruction(BB);
  if (First) {
    (*Tag) = 1;
    return First;
  } else {
    (*Tag) = 0;
    return BB;
  }
}

/* llvalue -> (llbasicblock, llvalue) llpos */
extern "C"
void *llvm_instr_succ(LLVMValueRef Val, int *Tag) {
  LLVMValueRef Next = LLVMGetNextInstruction(Val);
  if (Next) {
    (*Tag) = 1;
    return Next;
  } else {
    (*Tag) = 0;
    return LLVMGetInstructionParent(Val);
  }
}

/* llbasicblock -> (llbasicblock, llvalue) llrev_pos */
extern "C"
void *llvm_instr_end(LLVMBasicBlockRef BB, int *Tag) {
  LLVMValueRef Last = LLVMGetLastInstruction(BB);
  if (Last) {
    (*Tag) = 1;
    return Last;
  } else {
    (*Tag) = 0;
    return BB;
  }
}

/* llvalue -> (llbasicblock, llvalue) llrev_pos */
extern "C"
void *llvm_instr_pred(LLVMValueRef Val, int *Tag) {
  LLVMValueRef Prev = LLVMGetPreviousInstruction(Val);
  if (Prev) {
    (*Tag) = 1;
    return Prev;
  } else {
    (*Tag) = 0;
    return LLVMGetInstructionParent(Val);
  }
}

/* llvalue -> Opcode.t */
extern "C"
int llvm_instr_opcode(LLVMValueRef Inst) {
  LLVMOpcode O;
  if (!LLVMIsAInstruction(Inst)) {
    /* TODO: throw an exception */
    assert(0);
  }
  O = LLVMGetInstructionOpcode(Inst);
  assert(O <= LLVMLandingPad);
  return O;
}

/* llvalue -> Icmp.t option */
extern "C"
int *llvm_icmp_predicate(LLVMValueRef Inst) {
  int X = LLVMGetICmpPredicate(Inst);
  if (X) {
    int *Res = (int *) malloc(sizeof(int));
    (*Res) = X - LLVMIntEQ;
    return Res;
  } else {
    return NULL;
  }
}

/* llvalue -> Fcmp.t option */
extern "C"
int *llvm_fcmp_predicate(LLVMValueRef Inst) {
  int X = LLVMGetFCmpPredicate(Inst);
  if (X) {
    int *Res = (int *) malloc(sizeof(int));
    (*Res) = X - LLVMRealPredicateFalse;
    return Res;
  } else {
    return NULL;
  }
}

/* llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_instr_clone(LLVMValueRef Inst) {
  if (!LLVMIsAInstruction(Inst)) {
    /* TODO: throw an exception */
    assert(0);
  }
  return LLVMInstructionClone(Inst);
}

/*--... Operations on call sites ...........................................--*/

/* llvalue -> int */
extern "C"
int llvm_instruction_call_conv(LLVMValueRef Inst) {
  return LLVMGetInstructionCallConv(Inst);
}

/* int * llvalue -> unit */
extern "C"
void llvm_set_instruction_call_conv(int CC, LLVMValueRef Inst) {
  LLVMSetInstructionCallConv(Inst, CC);
}

/* llvalue * int * Int32.int -> unit */
extern "C"
void llvm_add_instruction_param_attr(LLVMValueRef Inst, int Index, int PA) {
  LLVMAddInstrAttribute(Inst, Index, (LLVMAttribute) PA);
}

/* llvalue * int * Int32.int -> unit */
extern "C"
void llvm_remove_instruction_param_attr(LLVMValueRef Inst, int Index, int PA) {
  LLVMRemoveInstrAttribute(Inst, Index, (LLVMAttribute) PA);
}

/*--... Operations on call instructions (only) .............................--*/

/* llvalue -> bool */
extern "C"
int llvm_is_tail_call(LLVMValueRef CallInst) {
  return LLVMIsTailCall(CallInst);
}

/* bool * llvalue -> unit */
extern "C"
void llvm_set_tail_call(int IsTailCall, LLVMValueRef CallInst) {
  LLVMSetTailCall(CallInst, IsTailCall);
}

/*--... Operations on load/store instructions (only)........................--*/

/* llvalue -> bool */
extern "C"
int llvm_is_volatile(LLVMValueRef MemoryInst) {
  return LLVMGetVolatile(MemoryInst);
}

/* bool * llvalue -> unit */
extern "C"
void llvm_set_volatile(int IsVolatile, LLVMValueRef MemoryInst) {
  LLVMSetVolatile(MemoryInst, IsVolatile);
}

/*--.. Operations on terminators ...........................................--*/

/* llvalue * int -> llbasicblock */
extern "C"
LLVMBasicBlockRef llvm_successor(LLVMValueRef Val, int I) {
  return LLVMGetSuccessor(Val, I);
}

/* llvalue * int * llbasicblock -> unit */
extern "C"
void llvm_set_successor(LLVMValueRef Val, int I, LLVMBasicBlockRef BB) {
  LLVMSetSuccessor(Val, I, BB);
}

/* llvalue -> int */
extern "C"
int llvm_num_successors(LLVMValueRef Val) {
  return LLVMGetNumSuccessors(Val);
}

/*--.. Operations on branch ................................................--*/

/* llvalue -> llvalue */
extern "C"
LLVMValueRef llvm_condition(LLVMValueRef Val) {
  return LLVMGetCondition(Val);
}

/* llvalue * llvalue -> unit */
extern "C"
void llvm_set_condition(LLVMValueRef B, LLVMValueRef C) {
  LLVMSetCondition(B, C);
}

/* llvalue -> bool */
extern "C"
int llvm_is_conditional(LLVMValueRef Val) {
  return LLVMIsConditional(Val);
}

/*--... Operations on phi nodes ............................................--*/

/* (llvalue * llbasicblock) * llvalue -> unit */
extern "C"
void llvm_add_incoming(LLVMValueRef Val, LLVMBasicBlockRef BB, LLVMValueRef PhiNode) {
  LLVMAddIncoming(PhiNode, &Val, &BB, 1);
}

/* llvalue -> (llvalue * llbasicblock) list */
extern "C"
void **llvm_incoming(LLVMValueRef PhiNode, int *Len) {
  int Size = LLVMCountIncoming(PhiNode) * 2;
  int I, J;
  void **Res = (void **) malloc(sizeof(void *) * Size);
  for (I = 0, J = 0; I < Size; I += 2, J += 1) {
    Res[I] = LLVMGetIncomingValue(PhiNode, J);
    Res[I + 1] = LLVMGetIncomingBlock(PhiNode, J);
  }
  (*Len) = Size;
  return Res;
}

/* llvalue -> unit */
extern "C"
void llvm_delete_instruction(LLVMValueRef Inst) {
  LLVMInstructionEraseFromParent(Inst);
}

/*===-- Instruction builders ----------------------------------------------===*/

/* llcontext -> llbuilder */
extern "C"
LLVMBuilderRef llvm_builder(LLVMContextRef C) {
  return LLVMCreateBuilderInContext(C);
}

/* (llbasicblock, llvalue) llpos * llbuilder -> unit */
extern "C"
void llvm_position_builder(void *Pos, int Tag, LLVMBuilderRef B) {
  if (Tag == 0) {
    LLVMBasicBlockRef BB = (LLVMBasicBlockRef) Pos;
    LLVMPositionBuilderAtEnd(B, BB);
  } else {
    LLVMValueRef I = (LLVMValueRef) Pos;
    LLVMPositionBuilderBefore(B, I);
  }
}

/* llbuilder -> llbasicblock */
extern "C"
LLVMBasicBlockRef llvm_insertion_block(LLVMBuilderRef B) {
  LLVMBasicBlockRef InsertBlock = LLVMGetInsertBlock(B);
  if (!InsertBlock) {
    /* TODO: throw an exception */
    assert(0);
  }
  return InsertBlock;
}

/* llvalue * string * llbuilder -> unit */
extern "C"
void llvm_insert_into_builder(LLVMValueRef Inst, const char *Name, LLVMBuilderRef B) {
  LLVMInsertIntoBuilderWithName(B, Inst, Name);
}

/*--... Metadata ...........................................................--*/

/* llbuilder * llvalue -> unit */
extern "C"
void llvm_set_current_debug_location(LLVMBuilderRef B, LLVMValueRef Val) {
  LLVMSetCurrentDebugLocation(B, Val);
}

/* llbuilder -> unit */
extern "C"
void llvm_clear_current_debug_location(LLVMBuilderRef B) {
  LLVMSetCurrentDebugLocation(B, NULL);
}

/* llbuilder -> llvalue option */
extern "C"
LLVMValueRef llvm_current_debug_location(LLVMBuilderRef B) {
  LLVMValueRef L;
  if ((L = LLVMGetCurrentDebugLocation(B))) {
    return L;
  } else {
    return NULL;
  }
}

/* llbuilder * llvalue -> unit */
extern "C"
void llvm_set_inst_debug_location(LLVMBuilderRef B, LLVMValueRef Val) {
  LLVMSetInstDebugLocation(B, Val);
}

/*--... Terminators ........................................................--*/

/* llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_ret_void(LLVMBuilderRef B) {
  return LLVMBuildRetVoid(B);
}

/* llvalue * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_ret(LLVMValueRef Val, LLVMBuilderRef B) {
  return LLVMBuildRet(B, Val);
}

/* llvalue array * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_aggregate_ret(LLVMValueRef *RetVals, int RetCount, LLVMBuilderRef B) {
  return LLVMBuildAggregateRet(B, RetVals, RetCount);
}

/* llbasicblock * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_br(LLVMBasicBlockRef BB, LLVMBuilderRef B) {
  return LLVMBuildBr(B, BB);
}

/* llvalue * llbasicblock * llbasicblock * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_cond_br(LLVMValueRef If, LLVMBasicBlockRef Then, LLVMBasicBlockRef Else, LLVMBuilderRef B) {
  return LLVMBuildCondBr(B, If, Then, Else);
}

/* llvalue * llbasicblock * int * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_switch(LLVMValueRef Of, LLVMBasicBlockRef Else, int EstimatedCount, LLVMBuilderRef B) {
  return LLVMBuildSwitch(B, Of, Else, EstimatedCount);
}

/* lltype * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_malloc(LLVMTypeRef Ty, const char *Name, LLVMBuilderRef B) {
  return LLVMBuildMalloc(B, Ty, Name);
}

/* lltype * llvalue * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_array_malloc(LLVMTypeRef Ty, LLVMValueRef Val, const char *Name, LLVMBuilderRef B) {
  return LLVMBuildArrayMalloc(B, Ty, Val, Name);
}

/* llvalue * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_free(LLVMValueRef P, LLVMBuilderRef B) {
  return LLVMBuildFree(B, P);
}

/* llvalue * llvalue * llbasicblock -> unit */
extern "C"
void llvm_add_case(LLVMValueRef Switch, LLVMValueRef OnVal, LLVMBasicBlockRef Dest) {
  LLVMAddCase(Switch, OnVal, Dest);
}

/* llvalue -> llbasicblock */
extern "C"
LLVMBasicBlockRef llvm_switch_default_dest(LLVMValueRef Val) {
  return LLVMGetSwitchDefaultDest(Val);
}

/* llvalue * int * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_indirect_br(LLVMValueRef Addr, int EstimatedDests, LLVMBuilderRef B) {
  return LLVMBuildIndirectBr(B, Addr, EstimatedDests);
}

/* llvalue * llbasicblock -> unit */
extern "C"
void llvm_add_destination(LLVMValueRef IndirectBr, LLVMBasicBlockRef Dest) {
  LLVMAddDestination(IndirectBr, Dest);
}

/* llvalue * llvalue array * llbasicblock * llbasicblock * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_invoke(LLVMValueRef Fn, LLVMValueRef *Args, int ArgCount, LLVMBasicBlockRef Then, LLVMBasicBlockRef Catch, const char *Name, LLVMBuilderRef B) {
  return LLVMBuildInvoke(B, Fn, Args, ArgCount, Then, Catch, Name);
}

/* lltype * llvalue * int * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_landingpad(LLVMTypeRef Ty, LLVMValueRef PersFn, int NumClauses, const char *Name, LLVMBuilderRef B) {
  return LLVMBuildLandingPad(B, Ty, PersFn, NumClauses, Name);
}

/* llvalue * bool -> unit */
extern "C"
void llvm_set_cleanup(LLVMValueRef LandingPadInst, int Flag) {
  LLVMSetCleanup(LandingPadInst, Flag);
}

/* llvalue * llvalue -> unit */
extern "C"
void llvm_add_clause(LLVMValueRef LandingPadInst, LLVMValueRef ClauseVal) {
  LLVMAddClause(LandingPadInst, ClauseVal);
}

/* llvalue * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_resume(LLVMValueRef Exn, LLVMBuilderRef B) {
  return LLVMBuildResume(B, Exn);
}

/* llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_unreachable(LLVMBuilderRef B) {
  return LLVMBuildUnreachable(B);
}

/*--... Arithmetic .........................................................--*/

// #define DEFINE_BINOP(mlname, cname) \
//   /* llvalue * llvalue * string * llbuilder -> llvalue */ \
//   extern "C" \
//   LLVMValueRef llvm_build_##mlname(LLVMValueRef LHS, LLVMValueRef RHS, const char *Name, LLVMBuilderRef B) { \
//     return LLVMBuild##cname(B, LHS, RHS, Name); \
//   }
// #define DEFINE_UNOP(mlname, cname) \
//   /* llvalue * string * llbuilder -> llvalue */ \
//   extern "C" \\
//   LLVMValueRef llvm_build_##mlname(LLVMValueRef X, const char *Name, LLVMBuilderRef B) { \
//     return LLVMBuild##cname(B, X, Name); \
//   }

// DEFINE_BINOP(add, Add)
// DEFINE_BINOP(nsw_add, NSWAdd)
// DEFINE_BINOP(nuw_add, NUWAdd)
// DEFINE_BINOP(fadd, FAdd)
// DEFINE_BINOP(sub, Sub)
// DEFINE_BINOP(nsw_sub, NSWSub)
// DEFINE_BINOP(nuw_sub, NUWSub)
// DEFINE_BINOP(fsub, FSub)
// DEFINE_BINOP(mul, Mul)
// DEFINE_BINOP(nsw_mul, NSWMul)
// DEFINE_BINOP(nuw_mul, NUWMul)
// DEFINE_BINOP(fmul, FMul)
// DEFINE_BINOP(udiv, UDiv)
// DEFINE_BINOP(sdiv, SDiv)
// DEFINE_BINOP(exact_sdiv, ExactSDiv)
// DEFINE_BINOP(fdiv, FDiv)
// DEFINE_BINOP(urem, URem)
// DEFINE_BINOP(srem, SRem)
// DEFINE_BINOP(frem, FRem)
// DEFINE_BINOP(shl, Shl)
// DEFINE_BINOP(lshr, LShr)
// DEFINE_BINOP(ashr, AShr)
// DEFINE_BINOP(and, And)
// DEFINE_BINOP(or, Or)
// DEFINE_BINOP(xor, Xor)
// DEFINE_UNOP(neg, Neg)
// DEFINE_UNOP(nsw_neg, NSWNeg)
// DEFINE_UNOP(nuw_neg, NUWNeg)
// DEFINE_UNOP(fneg, FNeg)
// DEFINE_UNOP(not, Not)

/*--... Memory .............................................................--*/

/* lltype * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_alloca(LLVMTypeRef Ty, const char *Name, LLVMBuilderRef B) {
  return LLVMBuildAlloca(B, Ty, Name);
}

/* lltype * llvalue * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_array_alloca(LLVMTypeRef Ty, LLVMValueRef Size, const char *Name, LLVMBuilderRef B) {
  return LLVMBuildArrayAlloca(B, Ty, Size, Name);
}

/* llvalue * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_load(LLVMValueRef Ptr, const char *Name, LLVMBuilderRef B) {
  return LLVMBuildLoad(B, Ptr, Name);
}

/* llvalue * llvalue * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_store(LLVMValueRef Val, LLVMValueRef Ptr, LLVMBuilderRef B) {
  return LLVMBuildStore(B, Val, Ptr);
}

/* AtomicRMWBinOp.t * llvalue * llvalue * AtomicOrdering.t * bool * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_atomicrmw(int BinOp, LLVMValueRef Ptr, LLVMValueRef Val, int Ord, int ST, const char *Name, LLVMBuilderRef B) {
  LLVMValueRef Inst;
  Inst = LLVMBuildAtomicRMW(B, (LLVMAtomicRMWBinOp) BinOp, Ptr, Val, (LLVMAtomicOrdering) Ord, ST);
  LLVMSetValueName(Inst, Name);
  return Inst;
}

/* llvalue * llvalue array * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_gep(LLVMValueRef Ptr, LLVMValueRef *Indices, int IndexCount, const char *Name, LLVMBuilderRef B) {
  return LLVMBuildGEP(B, Ptr, Indices, IndexCount, Name);
}

/* llvalue * llvalue array * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_in_bounds_gep(LLVMValueRef Ptr, LLVMValueRef *Indices, int IndexCount, const char *Name, LLVMBuilderRef B) {
  return LLVMBuildInBoundsGEP(B, Ptr, Indices, IndexCount, Name);
}

/* llvalue * int * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_struct_gep(LLVMValueRef Ptr, int Index, const char *Name, LLVMBuilderRef B) {
  return LLVMBuildStructGEP(B, Ptr, Index, Name);
}

/* string * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_global_string(const char *Str, const char *Name, LLVMBuilderRef B) {
  return LLVMBuildGlobalString(B, Str, Name);
}

/* string * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_global_stringptr(const char *Str, const char *Name, LLVMBuilderRef B) {
  return LLVMBuildGlobalStringPtr(B, Str, Name);
}

/*--... Casts ..............................................................--*/

// #define DEFINE_CAST(mlname, cname) \
//   /* llvalue * lltype * string * llbuilder -> llvalue */ \
//   extern "C" \
//   LLVMValueRef llvm_build_##mlname(LLVMValueRef X, LLVMTypeRef Ty, const char *Name, LLVMBuilderRef B) { \
//     return LLVMBuild##cname(B, X, Ty, Name); \
//   }

// DEFINE_CAST(trunc, Trunc)
// DEFINE_CAST(zext, ZExt)
// DEFINE_CAST(sext, SExt)
// DEFINE_CAST(fptoui, FPToUI)
// DEFINE_CAST(fptosi, FPToSI)
// DEFINE_CAST(uitofp, UIToFP)
// DEFINE_CAST(sitofp, SIToFP)
// DEFINE_CAST(fptrunc, FPTrunc)
// DEFINE_CAST(fpext, FPExt)
// DEFINE_CAST(ptrtoint, PtrToInt)
// DEFINE_CAST(inttoptr, IntToPtr)
// DEFINE_CAST(bitcast, BitCast)
// DEFINE_CAST(zext_or_bitcast, ZExtOrBitCast)
// DEFINE_CAST(sext_or_bitcast, SExtOrBitCast)
// DEFINE_CAST(trunc_or_bitcast, TruncOrBitCast)
// DEFINE_CAST(pointercast, PointerCast)
// DEFINE_CAST(intcast, IntCast)
// DEFINE_CAST(fpcast, FPCast)

/*--... Comparisons ........................................................--*/

/* Icmp.t * llvalue * llvalue * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_icmp(int Pred, LLVMValueRef LHS, LLVMValueRef RHS, const char *Name, LLVMBuilderRef B) {
  return LLVMBuildICmp(B, (LLVMIntPredicate) (Pred + LLVMIntEQ), LHS, RHS, Name);
}

/* Fcmp.t * llvalue * llvalue * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_fcmp(int Pred, LLVMValueRef LHS, LLVMValueRef RHS, const char *Name, LLVMBuilderRef B) {
  return LLVMBuildFCmp(B, (LLVMRealPredicate) Pred, LHS, RHS, Name);
}

/*--... Miscellaneous instructions .........................................--*/

/* (llvalue * llbasicblock) list * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_phi(LLVMValueRef *Vals, LLVMBasicBlockRef *BBs, int Count, const char *Name, LLVMBuilderRef B) {
  LLVMValueRef PhiNode;
  int I;

  assert(Count > 0);

  PhiNode = LLVMBuildPhi(B, LLVMTypeOf(Vals[0]), Name);
  for (I = 0; I < Count; I += 1) {
    LLVMAddIncoming(PhiNode, &Vals[I], &BBs[I], 1);
  }

  return PhiNode;
}

/* lltype * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_empty_phi(LLVMTypeRef Ty, const char *Name, LLVMBuilderRef B) {
  return LLVMBuildPhi(B, Ty, Name);
}

/* llvalue * llvalue * llvalue * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_select(LLVMValueRef If, LLVMValueRef Then, LLVMValueRef Else, const char *Name, LLVMBuilderRef B) {
  return LLVMBuildSelect(B, If, Then, Else, Name);
}

/* llvalue * lltype * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_va_arg(LLVMValueRef List, LLVMTypeRef Ty, const char *Name, LLVMBuilderRef B) {
  return LLVMBuildVAArg(B, List, Ty, Name);
}

/* llvalue * llvalue * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_extractelement(LLVMValueRef Vec, LLVMValueRef Idx, const char *Name, LLVMBuilderRef B) {
  return LLVMBuildExtractElement(B, Vec, Idx, Name);
}

/* llvalue * llvalue * llvalue * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_insertelement(LLVMValueRef Vec, LLVMValueRef Elem, LLVMValueRef Idx, const char *Name, LLVMBuilderRef B) {
  return LLVMBuildInsertElement(B, Vec, Elem, Idx, Name);
}

/* llvalue * llvalue * llvalue * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_shufflevector(LLVMValueRef V1, LLVMValueRef V2, LLVMValueRef Mask, const char *Name, LLVMBuilderRef B) {
  return LLVMBuildShuffleVector(B, V1, V2, Mask, Name);
}

/* llvalue * int * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_extractvalue(LLVMValueRef Aggregate, int Idx, const char *Name, LLVMBuilderRef B) {
  return LLVMBuildExtractValue(B, Aggregate, Idx, Name);
}

/* llvalue * llvalue * int * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_insertvalue(LLVMValueRef Aggregate, LLVMValueRef Val, int Idx, const char *Name, LLVMBuilderRef B) {
  return LLVMBuildInsertValue(B, Aggregate, Val, Idx, Name);
}

/* llvalue * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_is_null(LLVMValueRef Val, const char *Name, LLVMBuilderRef B) {
  return LLVMBuildIsNull(B, Val, Name);
}

/* llvalue * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_is_not_null(LLVMValueRef Val, const char *Name, LLVMBuilderRef B) {
  return LLVMBuildIsNotNull(B, Val, Name);
}

/* llvalue * llvalue * string * llbuilder -> llvalue */
extern "C"
LLVMValueRef llvm_build_ptrdiff(LLVMValueRef LHS, LLVMValueRef RHS, const char *Name, LLVMBuilderRef B) {
  return LLVMBuildPtrDiff(B, LHS, RHS, Name);
}

/*===-- Memory buffers ----------------------------------------------------===*/

/* string * llmemorybuffer */
extern "C"
LLVMMemoryBufferRef llvm_memorybuffer_of_file(const char *Path) {
  char *Message;
  LLVMMemoryBufferRef MemBuf;

  if (LLVMCreateMemoryBufferWithContentsOfFile(Path, &MemBuf, &Message)) {
    /* TODO: throw an exception */
    assert(0);
  }

  return MemBuf;
}

/* unit -> llmemorybuffer */
extern "C"
LLVMMemoryBufferRef llvm_memorybuffer_of_stdin(void) {
  char *Message;
  LLVMMemoryBufferRef MemBuf;

  if (LLVMCreateMemoryBufferWithSTDIN(&MemBuf, &Message)) {
    /* TODO: throw an exception */
    assert(0);
  }

  return MemBuf;
}

/* string option * string -> llmemorybuffer */
extern "C"
LLVMMemoryBufferRef llvm_memorybuffer_of_string(const char *Name, const char *String) {
  LLVMMemoryBufferRef MemBuf;
  const char *NameCStr;

  if (Name == NULL) {
    NameCStr = "";
  } else {
    NameCStr = Name;
  }

  MemBuf = LLVMCreateMemoryBufferWithMemoryRangeCopy(String, strlen(String), NameCStr);

  return MemBuf;
}

/* llmemorybuffer -> string */
extern "C"
const char *llvm_memorybuffer_as_string(LLVMMemoryBufferRef MemBuf) {
  char *String = (char *) malloc(sizeof(char) * (LLVMGetBufferSize(MemBuf) + 1));
  memcpy(String, LLVMGetBufferStart(MemBuf), LLVMGetBufferSize(MemBuf));
  String[LLVMGetBufferSize(MemBuf)] = '\0';
  return String;
}

/* llmemorybuffer -> unit */
extern "C"
void llvm_memorybuffer_dispose(LLVMMemoryBufferRef MemBuf) {
  LLVMDisposeMemoryBuffer(MemBuf);
}

/*===-- Pass Managers -----------------------------------------------------===*/

/* unit -> PassManager.Module PassManager.t */
extern "C"
LLVMPassManagerRef llvm_passmanager_create(void) {
  return LLVMCreatePassManager();
}

/* llmodule -> PassManager.Function PassManager.t */
extern "C"
LLVMPassManagerRef llvm_passmanager_create_function(LLVMModuleRef M) {
  return LLVMCreateFunctionPassManager((LLVMModuleProviderRef) M);
}

/* llmodule * PassManager.Function PassManager.t -> bool */
extern "C"
int llvm_passmanager_run_module(LLVMModuleRef M, LLVMPassManagerRef PM) {
  return LLVMRunPassManager(PM, M);
}

/* PassManager.Function PassManager.t -> bool */
extern "C"
int llvm_passmanager_initialize(LLVMPassManagerRef FPM) {
  return LLVMInitializeFunctionPassManager(FPM);
}

/* llvalue * PassManager.Function PassManager.t -> bool */
extern "C"
int llvm_passmanager_run_function(LLVMValueRef Fn, LLVMPassManagerRef FPM) {
  return LLVMRunFunctionPassManager(FPM, Fn);
}

/* PassManager.Function PassManager.t -> bool */
extern "C"
int llvm_passmanager_finalize(LLVMPassManagerRef FPM) {
  return LLVMFinalizeFunctionPassManager(FPM);
}

/* PassManager.any PassManager.t -> unit */
extern "C"
void llvm_passmanager_dispose(LLVMPassManagerRef PM) {
  LLVMDisposePassManager(PM);
}
