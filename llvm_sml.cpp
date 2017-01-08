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

/* Icmp.t * llvalue * llvalue -> llvaue */
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
