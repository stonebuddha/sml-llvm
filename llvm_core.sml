structure LlvmCore (*:> LLVM_CORE*) =
struct

infixr 0 $
fun f $ x = f x

type llcontext = C.voidptr
type llmodule = C.voidptr
type lltype = C.voidptr
type llvalue = C.voidptr
type lluse = C.voidptr
type llbasicblock = C.voidptr
type llbuilder = C.voidptr
type llmemorybuffer = C.voidptr
type llmdkind = int

structure TypeKind =
struct
datatype t =
         Void
         | Half
         | Float
         | Double
         | X86fp80
         | Fp128
         | Ppc_fp128
         | Label
         | Integer
         | Function
         | Struct
         | Array
         | Pointer
         | Vector
         | Metadata
         | X86_mmx

fun fromInt 0 = Void
  | fromInt 1 = Half
  | fromInt 2 = Float
  | fromInt 3 = Double
  | fromInt 4 = X86fp80
  | fromInt 5 = Fp128
  | fromInt 6 = Ppc_fp128
  | fromInt 7 = Label
  | fromInt 8 = Integer
  | fromInt 9 = Function
  | fromInt 10 = Struct
  | fromInt 11 = Array
  | fromInt 12 = Pointer
  | fromInt 13 = Vector
  | fromInt 14 = Metadata
  | fromInt 15 = X86_mmx
  | fromInt _ = raise (Fail "TypeKind.fromInt")
end

structure Linkage =
struct
datatype t =
         External
         | Available_externally
         | Link_once
         | Link_once_odr
         | Link_once_odr_auto_hide
         | Weak
         | Weak_odr
         | Appending
         | Internal
         | Private
         | Dllimport
         | Dllexport
         | External_weak
         | Ghost
         | Common
         | Linker_private
         | Linker_private_weak
end

structure Visibility =
struct
datatype t =
         Default
         | Hidden
         | Protected
end

structure DLLStorageClass =
struct
datatype t =
         Default
         | DLLImport
         | DLLExport
end

structure CallConv =
struct
val c = 0
val fast = 8
val cold = 9
val x86_stdcall = 64
val x86_fastcall = 65
end

structure Attribute =
struct
datatype t =
         Zext
         | Sext
         | Noreturn
         | Inreg
         | Structret
         | Nounwind
         | Noalias
         | Byval
         | Nest
         | Readonce
         | Readonly
         | Noinline
         | Alwaysinline
         | Optsize
         | Ssp
         | Sspreq
         | Alignment of int
         | Nocapture
         | Noredzone
         | Noimplicitfloat
         | Naked
         | Inlinehint
         | Stackalignment of int
         | ReturnsTwice
         | UWTable
         | NonLazyBind
end

structure Icmp =
struct
datatype t =
         Eq
         | Ne
         | Ugt
         | Uge
         | Ult
         | Ule
         | Sgt
         | Sge
         | Slt
         | Sle

fun toInt Eq = 0
  | toInt Ne = 1
  | toInt Ugt = 2
  | toInt Uge = 3
  | toInt Ult = 4
  | toInt Ule = 5
  | toInt Sgt = 6
  | toInt Sge = 7
  | toInt Slt = 8
  | toInt Sle = 9
end

structure Fcmp =
struct
datatype t =
         False
         | Oeq
         | Ogt
         | Oge
         | Olt
         | Ole
         | One
         | Ord
         | Uno
         | Ueq
         | Ugt
         | Uge
         | Ult
         | Ule
         | Une
         | True

fun toInt False = 0
  | toInt Oeq = 1
  | toInt Ogt = 2
  | toInt Oge = 3
  | toInt Olt = 4
  | toInt Ole = 5
  | toInt One = 6
  | toInt Ord = 7
  | toInt Uno = 8
  | toInt Ueq = 9
  | toInt Ugt = 10
  | toInt Uge = 11
  | toInt Ult = 12
  | toInt Ule = 13
  | toInt Une = 14
  | toInt True = 15
end

structure Opcode =
struct
datatype t =
         Invalid (* not an instruction *)
         (* Terminator Instructions *)
         | Ret
         | Br
         | Switch
         | IndirectBr
         | Invoke
         | Invalid2
         | Unreachable
         (* Standard Binary Operators *)
         | Add
         | FAdd
         | Sub
         | FSub
         | Mul
         | FMul
         | UDiv
         | SDiv
         | FDiv
         | URem
         | SRem
         | FRem
         (* Logical Operators *)
         | Shl
         | LShr
         | AShr
         | And
         | Or
         | Xor
         (* Memory Operators *)
         | Alloca
         | Load
         | Store
         | GetElementPtr
         (* Case Operators *)
         | Trunc
         | ZExt
         | SExt
         | FPToUI
         | FPToSI
         | UIToFP
         | SIToFP
         | FPTrunc
         | FPExt
         | PtrToInt
         | IntToPtr
         | BitCast
         (* Other Operators *)
         | ICmp
         | FCmp
         | PHI
         | Call
         | Select
         | UserOp1
         | UserOp2
         | VAArg
         | ExtractElement
         | InsertElement
         | ShuffleVector
         | ExtractValue
         | InsertValue
         | Fence
         | AtomicCmpXchg
         | AtomicRMW
         | Resume
         | LandingPad

fun fromInt 0 = Invalid
  | fromInt 1 = Ret
  | fromInt 2 = Br
  | fromInt 3 = Switch
  | fromInt 4 = IndirectBr
  | fromInt 5 = Invoke
  | fromInt 6 = Invalid2
  | fromInt 7 = Unreachable
  | fromInt 8 = Add
  | fromInt 9 = FAdd
  | fromInt 10 = Sub
  | fromInt 11 = FSub
  | fromInt 12 = Mul
  | fromInt 13 = FMul
  | fromInt 14 = UDiv
  | fromInt 15 = SDiv
  | fromInt 16 = FDiv
  | fromInt 17 = URem
  | fromInt 18 = SRem
  | fromInt 19 = FRem
  | fromInt 20 = Shl
  | fromInt 21 = LShr
  | fromInt 22 = AShr
  | fromInt 23 = And
  | fromInt 24 = Or
  | fromInt 25 = Xor
  | fromInt 26 = Alloca
  | fromInt 27 = Load
  | fromInt 28 = Store
  | fromInt 29 = GetElementPtr
  | fromInt 30 = Trunc
  | fromInt 31 = ZExt
  | fromInt 32 = SExt
  | fromInt 33 = FPToUI
  | fromInt 34 = FPToSI
  | fromInt 35 = UIToFP
  | fromInt 36 = SIToFP
  | fromInt 37 = FPTrunc
  | fromInt 38 = FPExt
  | fromInt 39 = PtrToInt
  | fromInt 40 = IntToPtr
  | fromInt 41 = BitCast
  | fromInt 42 = ICmp
  | fromInt 43 = FCmp
  | fromInt 44 = PHI
  | fromInt 45 = Call
  | fromInt 46 = Select
  | fromInt 47 = UserOp1
  | fromInt 48 = UserOp2
  | fromInt 49 = VAArg
  | fromInt 50 = ExtractElement
  | fromInt 51 = InsertElement
  | fromInt 52 = ShuffleVector
  | fromInt 53 = ExtractValue
  | fromInt 54 = InsertValue
  | fromInt 55 = Fence
  | fromInt 56 = AtomicCmpXchg
  | fromInt 57 = AtomicRMW
  | fromInt 58 = Resume
  | fromInt 59 = LandingPad
  | fromInt _ = raise (Fail "Opcode.fromInt")
end

structure LandingPadClauseTy =
struct
datatype t =
         Catch
         | Filter
end

structure ThreadLocalMode =
struct
datatype t =
         None
         | GeneralDynamic
         | LocalDynamic
         | InitialExec
         | LocalExec
end

structure AtomicOrdering =
struct
datatype t =
         NotAtomic
         | Unordered
         | Monotonic
         | Invalid
         | Acquire
         | Release
         | AcqiureRelease
         | SequentiallyConsistent
end

structure AtomicRMWBinOp =
struct
datatype t =
         Xchg
         | Add
         | Sub
         | And
         | Nand
         | Or
         | Xor
         | Max
         | Min
         | UMax
         | UMin
end

structure ValueKind =
struct
datatype t =
         NullValue
         | Argument
         | BasicBlock
         | InlineAsm
         | MDNode
         | MDString
         | BlockAddress
         | ConstantAggregateZero
         | ConstantArray
         | ConstantDataArray
         | ConstantDataVector
         | ConstantExpr
         | ConstantFP
         | ConstantInt
         | ConstantPointerNull
         | ConstantStruct
         | ConstantVector
         | Function
         | GlobalAlias
         | GlobalVariable
         | UndefValue
         | Instruction of Opcode.t

fun fromInt n =
  if n < 0 then Instruction (Opcode.fromInt (~n - 1))
  else
      case n of
          0 => NullValue
        | 1 => Argument
        | 2 => BasicBlock
        | 3 => InlineAsm
        | 4 => MDNode
        | 5 => MDString
        | 6 => BlockAddress
        | 7 => ConstantAggregateZero
        | 8 => ConstantArray
        | 9 => ConstantDataArray
        | 10 => ConstantDataVector
        | 11 => ConstantExpr
        | 12 => ConstantFP
        | 13 => ConstantInt
        | 14 => ConstantPointerNull
        | 15 => ConstantStruct
        | 16 => ConstantVector
        | 17 => Function
        | 18 => GlobalAlias
        | 19 => GlobalVariable
        | 20 => UndefValue
        | _ => raise (Fail "ValueKind.fromInt")
end

structure DiagnosticSeverity =
struct
datatype t =
         Error
         | Warning
         | Remark
         | Note
end

(*===-- Contexts ----------------------------------------------------------===*)
fun create_context () : llcontext = F_llvm_create_context.f ()
fun dispose_context (C : llcontext) : unit = F_llvm_dispose_context.f C
fun global_context () : llcontext = F_llvm_global_context.f ()
fun mdkind_id (C : llcontext) (S : string) : llmdkind =
  let
      val S' = ZString.dupML S
  in
      (Int32.toInt $ F_llvm_mdkind_id.f (C, S'))
      before
      C.free S'
  end

(*===-- Modules -----------------------------------------------------------===*)
fun create_module (C : llcontext) (S : string) : llmodule =
  let
      val S' = ZString.dupML S
  in
      F_llvm_create_module.f (C, S')
      before
      C.free S'
  end
fun dispose_module (M : llmodule) : unit = F_llvm_dispose_module.f M
fun target_triple (M : llmodule) : string =
  let
      val S = F_llvm_target_triple.f M
  in
      ZString.toML S
      before
      C.free S
  end
fun set_target_triple (S : string) (M : llmodule) : unit =
  let
      val S' = ZString.dupML S
  in
      F_llvm_set_target_triple.f (S', M)
      before
      C.free S'
  end
fun data_layout (M : llmodule) : string =
  let
      val S = F_llvm_data_layout.f M
  in
      ZString.toML S
      before
      C.free S
  end
fun set_data_layout (S : string) (M : llmodule) : unit =
  let
      val S' = ZString.dupML S
  in
      F_llvm_set_data_layout.f (S', M)
      before
      C.free S'
  end
fun dump_module (M : llmodule) : unit = F_llvm_dump_module.f M
fun print_module (S : string) (M : llmodule) : unit =
  let
      val S' = ZString.dupML S
  in
      F_llvm_print_module.f (S', M)
      before
      C.free S'
  end
fun string_of_llmodule (M : llmodule) : string =
  let
      val S = F_llvm_string_of_llmodule.f M
  in
      ZString.toML S
      before
      C.free S
  end
fun set_module_inline_asm (M : llmodule) (S : string) =
  let
      val S' = ZString.dupML S
  in
      F_llvm_set_module_inline_asm.f (M, S')
      before
      C.free S'
  end
fun module_context (M : llmodule) : llcontext = F_llvm_module_context.f M

(*===-- Types -------------------------------------------------------------===*)
fun classify_type (Ty : lltype) : TypeKind.t = TypeKind.fromInt $ Int32.toInt $ F_llvm_classify_type.f Ty
fun type_context (Ty : lltype) : llcontext = F_llvm_type_context.f Ty
fun type_is_sized (Ty : lltype) : bool =
  case F_llvm_type_is_sized.f Ty of
      0 => false
    | 1 => true
    | _ => raise (Fail "type_is_sized")
fun dump_type (Ty : lltype) : unit = F_llvm_dump_type.f Ty
fun string_of_lltype (Ty : lltype) : string =
  let
      val S = F_llvm_string_of_lltype.f Ty
  in
      ZString.toML S
      before
      C.free S
  end

(*--... Operations on integer types ........................................--*)
fun i1_type (C : llcontext) : lltype = F_llvm_i1_type.f C
fun i8_type (C : llcontext) : lltype = F_llvm_i8_type.f C
fun i16_type (C : llcontext) : lltype = F_llvm_i16_type.f C
fun i32_type (C : llcontext) : lltype = F_llvm_i32_type.f C
fun i64_type (C : llcontext) : lltype = F_llvm_i64_type.f C

fun integer_type (C : llcontext) (NumBits : int) : lltype = F_llvm_integer_type.f (C, Int32.fromInt NumBits)
fun integer_bitwidth (IntegerTy : lltype) : int = Int32.toInt $ F_llvm_integer_bitwidth.f IntegerTy

(*--... Operations on real types ...........................................--*)
fun float_type (C : llcontext) : lltype = F_llvm_float_type.f C
fun double_type (C : llcontext) : lltype = F_llvm_double_type.f C
fun x86fp80_type (C : llcontext) : lltype = F_llvm_x86fp80_type.f C
fun fp128_type (C : llcontext) : lltype = F_llvm_fp128_type.f C
fun ppc_fp128_type (C : llcontext) : lltype = F_llvm_ppc_fp128_type.f C

(*--... Operations on function types .......................................--*)
fun dupVPtrArr (arr : C.voidptr array) : C.rw C.voidptr_obj C.ptr =
  let
      val buf = C.alloc C.T.voidptr (Word.fromInt $ Array.length arr)
      val () = Array.appi (fn (i, vptr) =>
                              let
                                  val loc = C.Ptr.|+| (buf, i)
                              in
                                  C.Set.voidptr (C.Ptr.|*| loc, vptr)
                              end) arr
  in
      buf
  end
fun toVPtrArr (buf : C.rw C.voidptr_obj C.ptr) (len : C.rw C.sint_obj) : C.voidptr array =
  Array.tabulate (Int32.toInt $ C.Get.sint len, fn i =>
                                                   let
                                                       val loc = C.Ptr.|+| (buf, i)
                                                   in
                                                       C.Get.voidptr $ C.Ptr.|*| loc
                                                   end)
fun dupIntArr (arr : int array) : C.rw C.sint_obj C.ptr =
  let
      val buf = C.alloc C.T.sint (Word.fromInt $ Array.length arr)
      val () = Array.appi (fn (i, num) =>
                               let
                                   val loc = C.Ptr.|+| (buf, i)
                               in
                                   C.Set.sint (C.Ptr.|*| loc, Int32.fromInt num)
                               end) arr
  in
      buf
  end
fun toIntArr (buf : C.rw C.sint_obj C.ptr) (len : C.rw C.sint_obj) : int array =
  Array.tabulate (Int32.toInt $ C.Get.sint len, fn i =>
                                                   let
                                                       val loc = C.Ptr.|+| (buf, i)
                                                   in
                                                       Int32.toInt $ C.Get.sint $ C.Ptr.|*| loc
                                                   end)

fun function_type (ReturnTy : lltype) (ParamTys : lltype array) : lltype =
  let
      val ParamTys' = dupVPtrArr ParamTys
  in
      F_llvm_function_type.f (ReturnTy, ParamTys', Int32.fromInt $ Array.length ParamTys)
      before
      C.free ParamTys'
  end
fun var_arg_function_type (ReturnTy : lltype) (ParamTys : lltype array) : lltype =
  let
      val ParamTys' = dupVPtrArr ParamTys
  in
      F_llvm_var_arg_function_type.f (ReturnTy, ParamTys', Int32.fromInt $ Array.length ParamTys)
      before
      C.free ParamTys'
  end

fun is_var_arg (FunctionTy : lltype) : bool =
  case F_llvm_is_var_arg.f FunctionTy of
      0 => false
    | 1 => true
    | _ => raise (Fail "is_var_arg")
fun return_type (FunctionTy : lltype) : lltype = F_llvm_return_type.f FunctionTy
fun param_types (FunctionTy : lltype) : lltype array =
  let
      val Len = C.new C.T.sint
      val Buf = F_llvm_param_types.f (FunctionTy, C.Ptr.|&| Len)
  in
      toVPtrArr Buf Len
      before
      (C.free Buf; C.free (C.Ptr.|&| Len))
  end

(*--... Operations on struct types .........................................--*)
fun struct_type (C : llcontext) (ElemTys : lltype array) : lltype =
  let
      val ElemTys' = dupVPtrArr ElemTys
  in
      F_llvm_struct_type.f (C, ElemTys', Int32.fromInt $ Array.length ElemTys)
      before
      C.free ElemTys'
  end
fun packed_struct_type (C : llcontext) (ElemTys : lltype array) : lltype =
  let
      val ElemTys' = dupVPtrArr ElemTys
  in
      F_llvm_packed_struct_type.f (C, ElemTys', Int32.fromInt $ Array.length ElemTys)
      before
      C.free ElemTys'
  end
fun struct_name (StructTy : lltype) : string option =
  let
      val S = F_llvm_struct_name.f StructTy
  in
      if C.Ptr.isNull S then NONE
      else
          SOME (ZString.toML S)
          before
          C.free S
  end
fun named_struct_type (C : llcontext) (Name : string) : lltype =
  let
      val Name' = ZString.dupML Name
  in
      F_llvm_named_struct_type.f (C, Name')
      before
      C.free Name'
  end
fun struct_set_body (StructTy : lltype) (ElemTys : lltype array) (Packed : bool) : unit =
  let
      val ElemTys' = dupVPtrArr ElemTys
  in
      F_llvm_struct_set_body.f (StructTy, ElemTys', Int32.fromInt $ Array.length ElemTys, if Packed then 1 else 0)
      before
      C.free ElemTys'
  end
fun struct_element_types (StructTy : lltype) : lltype array =
  let
      val Len = C.new C.T.sint
      val Buf = F_llvm_struct_element_types.f (StructTy, C.Ptr.|&| Len)
  in
      toVPtrArr Buf Len
      before
      (C.free Buf; C.free (C.Ptr.|&| Len))
  end
fun is_packed (StructTy : lltype) : bool =
  case F_llvm_is_packed.f StructTy of
      0 => false
    | 1 => true
    | _ => raise (Fail "is_packed")
fun is_opaque (StructTy : lltype) : bool =
  case F_llvm_is_opaque.f StructTy of
      0 => false
    | 1 => true
    | _ => raise (Fail "is_opaque")

(*--... Operations on pointer, vector, and array types .....................--*)
fun array_type (ElemTy : lltype) (ElemCount : int) : lltype = F_llvm_array_type.f (ElemTy, Int32.fromInt ElemCount)
fun pointer_type (ElemTy : lltype) : lltype = F_llvm_pointer_type.f ElemTy
fun qualified_pointer_type (ElemTy : lltype) (AddrSpace : int) : lltype = F_llvm_qualified_pointer_type.f (ElemTy, Int32.fromInt AddrSpace)
fun vector_type (ElemTy : lltype) (ElemCount : int) : lltype = F_llvm_vector_type.f (ElemTy, Int32.fromInt ElemCount)

fun element_type (Ty : lltype) : lltype = F_llvm_element_type.f Ty
fun array_length (ArrayTy : lltype) : int = Int32.toInt $ F_llvm_array_length.f ArrayTy
fun address_space (PtrTy : lltype) : int = Int32.toInt $ F_llvm_address_space.f PtrTy
fun vector_size (VectorTy : lltype) : int = Int32.toInt $ F_llvm_vector_size.f VectorTy

(*--... Operations on other types ..........................................--*)
fun void_type (C : llcontext) : lltype = F_llvm_void_type.f C
fun label_type (C : llcontext) : lltype = F_llvm_label_type.f C
fun x86_mmx_type (C : llcontext) : lltype = F_llvm_x86_mmx_type.f C
fun type_by_name (M : llmodule) (Name : string) : lltype option =
  let
      val Name' = ZString.dupML Name
      val Res = F_llvm_type_by_name.f (M, Name') before C.free Name'
  in
      if C.Ptr.isNull' Res then NONE
      else SOME Res
  end

(*===-- Values ------------------------------------------------------------===*)
fun classify_value (Val : llvalue) : ValueKind.t = ValueKind.fromInt $ Int32.toInt $ F_llvm_classify_value.f Val
fun type_of (Val : llvalue) : lltype = F_llvm_type_of.f Val
fun value_name (Val : llvalue) : string =
  let
      val Name = F_llvm_value_name.f Val
  in
      ZString.toML Name
      before
      C.free Name
  end
fun set_value_name (Name : string) (Val : llvalue) : unit =
  let
      val Name' = ZString.dupML Name
  in
      F_llvm_set_value_name.f (Name', Val)
      before
      C.free Name'
  end
fun dump_value (Val : llvalue) : unit = F_llvm_dump_value.f Val
fun string_of_llvalue (Val : llvalue) : string =
  let
      val S = F_llvm_string_of_llvalue.f Val
  in
      ZString.toML S
      before
      C.free S
  end
fun replace_all_uses_with (OldVal : llvalue) (NewVal : llvalue) : unit = F_llvm_replace_all_uses_with.f (OldVal, NewVal)

(*--... Operations on uses .................................................--*)
fun use_begin (Val : llvalue) : lluse option =
  let
      val Res = F_llvm_use_begin.f Val
  in
      if C.Ptr.isNull' Res then NONE
      else SOME Res
  end
fun use_succ (U : lluse) : lluse option =
  let
      val Res = F_llvm_use_succ.f U
  in
      if C.Ptr.isNull' Res then NONE
      else SOME Res
  end
fun user (U : lluse) : llvalue = F_llvm_user.f U
fun used_value (U : lluse) : llvalue = F_llvm_used_value.f U

fun iter_uses f v =
  let
      fun aux NONE = ()
        | aux (SOME u) = (f u; aux (use_succ u))
  in
      aux (use_begin v)
  end

fun fold_left_uses f init v =
  let
      fun aux init NONE = init
        | aux init (SOME u) = aux (f init u) (use_succ u)
  in
      aux init (use_begin v)
  end

fun fold_right_uses f v init =
  let
      fun aux NONE init = init
        | aux (SOME u) init = f u (aux (use_succ u) init)
  in
      aux (use_begin v) init
  end

(*--... Operations on users ................................................--*)
fun operand (Val : llvalue) (I : int) : llvalue = F_llvm_operand.f (Val, Int32.fromInt I)
fun operand_use (Val : llvalue) (I : int) : lluse = F_llvm_operand_use.f (Val, Int32.fromInt I)
fun set_operand (Val1 : llvalue) (I : int) (Val2 : llvalue) : unit = F_llvm_set_operand.f (Val1, Int32.fromInt I, Val2)
fun num_operands (Val : llvalue) : int = Int32.toInt $ F_llvm_num_operands.f Val

(*--... Operations on constants of (mostly) any type .......................--*)
fun is_constant (Val : llvalue) : bool =
  case F_llvm_is_constant.f Val of
      0 => false
    | 1 => true
    | _ => raise (Fail "is_constant")
fun const_null (Ty : lltype) : llvalue = F_llvm_const_null.f Ty
fun const_all_ones (Ty : lltype) : llvalue = F_llvm_const_all_ones.f Ty
fun const_pointer_null (Ty : lltype) : llvalue = F_llvm_const_pointer_null.f Ty
fun undef (Ty : lltype) : llvalue = F_llvm_undef.f Ty
fun is_null (Val : llvalue) : bool =
  case F_llvm_is_null.f Val of
      0 => false
    | 1 => true
    | _ => raise (Fail "is_null")
fun is_undef (Val : llvalue) : bool =
  case F_llvm_is_undef.f Val of
      0 => false
    | 1 => true
    | _ => raise (Fail "is_undef")
fun constexpr_opcode (Val : llvalue) : Opcode.t = Opcode.fromInt $ Int32.toInt $ F_llvm_constexpr_get_opcode.f Val

(*--... Operations on instructions .........................................--*)
fun has_metadata (Val : llvalue) : bool =
  case F_llvm_has_metadata.f Val of
      0 => false
    | 1 => true
    | _ => raise (Fail "has_metadata")
fun metadata (Val : llvalue) (MDKindId : int) : llvalue option =
  let
      val Res = F_llvm_metadata.f (Val, Int32.fromInt MDKindId)
  in
      if C.Ptr.isNull' Res then NONE
      else SOME Res
  end
fun set_metadata (Val : llvalue) (MDKindId : int) (MD : llvalue) : unit = F_llvm_set_metadata.f (Val, Int32.fromInt MDKindId, MD)
fun clear_metadata (Val : llvalue) (MDKindId : int) : unit = F_llvm_clear_metadata.f (Val, Int32.fromInt MDKindId)

(* (*--... Operations on metadata .......,.....................................--*) *)
fun mdstring (C : llcontext) (S : string) : llvalue =
  let
      val S' = ZString.dupML S
  in
      F_llvm_mdstring.f (C, S')
      before
      C.free S'
  end
fun mdnode (C : llcontext) (ElemVals : llvalue array) : llvalue =
  let
      val ElemVals' = dupVPtrArr ElemVals
  in
      F_llvm_mdnode.f (C, ElemVals', Int32.fromInt $ Array.length ElemVals)
      before
      C.free ElemVals'
  end
fun mdnull (C : llcontext) : llvalue = F_llvm_mdnull.f C
fun get_mdstring (Val : llvalue) : string option =
  let
      val S = F_llvm_get_mdstring.f Val
  in
      if C.Ptr.isNull S then NONE
      else
          SOME (ZString.toML S)
          before
          C.free S
  end
fun get_mdnode_operands (Val : llvalue) : llvalue array =
  let
      val Len = C.new C.T.sint
      val Buf = F_llvm_get_mdnode_operands.f (Val, C.Ptr.|&| Len)
  in
      toVPtrArr Buf Len
      before
      (C.free Buf; C.free (C.Ptr.|&| Len))
  end
fun get_named_metadata (Val : llvalue) (Name : string) : llvalue array =
  let
      val Name' = ZString.dupML Name
      val Len = C.new C.T.sint
      val Buf = F_llvm_get_namedmd.f (Val, Name', C.Ptr.|&| Len)
  in
      toVPtrArr Buf Len
      before
      (C.free Name'; C.free Buf; C.free (C.Ptr.|&| Len))
  end
fun add_named_metadata_operand (M : llmodule) (Name : string) (Val : llvalue) : unit =
  let
      val Name' = ZString.dupML Name
  in
      F_llvm_append_namedmd.f (M, Name', Val)
      before
      C.free Name'
  end

(*--... Operations on scalar constants .....................................--*)
fun const_int (IntTy : lltype) (N : int) : llvalue = F_llvm_const_int.f (IntTy, Int32.fromInt N)
(* FIXME: this can not compile under SML/NJ *)
(* fun const_of_int64 (IntTy : lltype) (N : Int64.int) (SExt : bool) : llvalue = F_llvm_const_of_int64.f (IntTy, N, if SExt then 1 else 0) *)
fun int64_of_const (Val : llvalue) : Int64.int option =
  let
      val Res = F_llvm_int64_of_const.f Val
  in
      if C.Ptr.isNull Res then NONE
      else
          SOME (C.Get.slonglong $ C.Ptr.|*| Res)
          before
          C.free Res
  end
fun const_int_of_string (IntTy : lltype) (S : string) (Radix : int) : llvalue =
  let
      val S' = ZString.dupML S
  in
      F_llvm_const_int_of_string.f (IntTy, S', Int32.fromInt Radix)
      before
      C.free S'
  end
fun const_float (RealTy : lltype) (N : real) : llvalue = F_llvm_const_float.f (RealTy, N)
fun float_of_const (Val : llvalue) : real option =
  let
      val Res = F_llvm_float_of_const.f Val
  in
      if C.Ptr.isNull Res then NONE
      else
          SOME (C.Get.double $ C.Ptr.|*| Res)
          before
          C.free Res
  end
fun const_float_of_string (RealTy : lltype) (S : string) : llvalue =
  let
      val S' = ZString.dupML S
  in
      F_llvm_const_float_of_string.f (RealTy, S')
      before
      C.free S'
  end

(*--... Operations on composite constants ..................................--*)
fun const_string (C : llcontext) (S : string) : llvalue =
  let
      val S' = ZString.dupML S
  in
      F_llvm_const_string.f (C, S')
      before
      C.free S'
  end
fun const_stringz (C : llcontext) (S : string) : llvalue =
  let
      val S' = ZString.dupML S
  in
      F_llvm_const_stringz.f (C, S')
  end
fun const_array (ElemTy : lltype) (ElemVals : llvalue array) : llvalue =
  let
      val ElemVals' = dupVPtrArr ElemVals
  in
      F_llvm_const_array.f (ElemTy, ElemVals', Int32.fromInt $ Array.length ElemVals)
      before
      C.free ElemVals'
  end
fun const_struct (C : llcontext) (ElemVals : llvalue array) : llvalue =
  let
      val ElemVals' = dupVPtrArr ElemVals
  in
      F_llvm_const_struct.f (C, ElemVals', Int32.fromInt $ Array.length ElemVals)
      before
      C.free ElemVals'
  end
fun const_named_struct (StructTy : lltype) (ElemVals : llvalue array) : llvalue =
  let
      val ElemVals' = dupVPtrArr ElemVals
  in
      F_llvm_const_named_struct.f (StructTy, ElemVals', Int32.fromInt $ Array.length ElemVals)
      before
      C.free ElemVals'
  end
fun const_packed_struct (C : llcontext) (ElemVals : llvalue array) : llvalue =
  let
      val ElemVals' = dupVPtrArr ElemVals
  in
      F_llvm_const_packed_struct.f (C, ElemVals', Int32.fromInt $ Array.length ElemVals)
      before
      C.free ElemVals'
  end
fun const_vector (ElemVals : llvalue array) : llvalue =
  let
      val ElemVals' = dupVPtrArr ElemVals
  in
      F_llvm_const_vector.f (ElemVals', Int32.fromInt $ Array.length ElemVals)
      before
      C.free ElemVals'
  end
fun string_of_const (Val : llvalue) : string option =
  let
      val S = F_llvm_string_of_const.f Val
  in
      if C.Ptr.isNull S then NONE
      else
          SOME (ZString.toML S)
          before
          C.free S
  end
fun const_element (Val : llvalue) (N : int) : llvalue = F_llvm_const_element.f (Val, Int32.fromInt N)

(*--... Constant expressions ...............................................--*)
fun align_of (Ty : lltype) : llvalue = F_llvm_align_of.f Ty
fun size_of (Ty : lltype) : llvalue = F_llvm_size_of.f Ty
fun const_neg (Val : llvalue) : llvalue = F_llvm_const_neg.f Val
fun const_nsw_neg (Val : llvalue) : llvalue = F_llvm_const_nsw_neg.f Val
fun const_nuw_neg (Val : llvalue) : llvalue = F_llvm_const_nuw_neg.f Val
fun const_fneg (Val : llvalue) : llvalue = F_llvm_const_fneg.f Val
fun const_not (Val : llvalue) : llvalue = F_llvm_const_not.f Val
fun const_add (Val1 : llvalue) (Val2 : llvalue) : llvalue = F_llvm_const_add.f (Val1, Val2)
fun const_nsw_add (Val1 : llvalue) (Val2 : llvalue) : llvalue = F_llvm_const_nsw_add.f (Val1, Val2)
fun const_nuw_add (Val1 : llvalue) (Val2 : llvalue) : llvalue = F_llvm_const_nuw_add.f (Val1, Val2)
fun const_fadd (Val1 : llvalue) (Val2 : llvalue) : llvalue = F_llvm_const_fadd.f (Val1, Val2)
fun const_sub (Val1 : llvalue) (Val2 : llvalue) : llvalue = F_llvm_const_sub.f (Val1, Val2)
fun const_nsw_sub (Val1 : llvalue) (Val2 : llvalue) : llvalue = F_llvm_const_nsw_sub.f (Val1, Val2)
fun const_nuw_sub (Val1 : llvalue) (Val2 : llvalue) : llvalue = F_llvm_const_nuw_sub.f (Val1, Val2)
fun const_fsub (Val1 : llvalue) (Val2 : llvalue) : llvalue = F_llvm_const_fsub.f (Val1, Val2)
fun const_mul (Val1 : llvalue) (Val2 : llvalue) : llvalue = F_llvm_const_mul.f (Val1, Val2)
fun const_nsw_mul (Val1 : llvalue) (Val2 : llvalue) : llvalue = F_llvm_const_nsw_mul.f (Val1, Val2)
fun const_nuw_mul (Val1 : llvalue) (Val2 : llvalue) : llvalue = F_llvm_const_nuw_mul.f (Val1, Val2)
fun const_fmul (Val1 : llvalue) (Val2 : llvalue) : llvalue = F_llvm_const_fmul.f (Val1, Val2)
fun const_udiv (Val1 : llvalue) (Val2 : llvalue) : llvalue = F_llvm_const_udiv.f (Val1, Val2)
fun const_sdiv (Val1 : llvalue) (Val2 : llvalue) : llvalue = F_llvm_const_sdiv.f (Val1, Val2)
fun const_exact_sdiv (Val1 : llvalue) (Val2 : llvalue) : llvalue = F_llvm_const_exact_sdiv.f (Val1, Val2)
fun const_fdiv (Val1 : llvalue) (Val2 : llvalue) : llvalue = F_llvm_const_fdiv.f (Val1, Val2)
fun const_urem (Val1 : llvalue) (Val2 : llvalue) : llvalue = F_llvm_const_urem.f (Val1, Val2)
fun const_srem (Val1 : llvalue) (Val2 : llvalue) : llvalue = F_llvm_const_srem.f (Val1, Val2)
fun const_frem (Val1 : llvalue) (Val2 : llvalue) : llvalue = F_llvm_const_frem.f (Val1, Val2)
fun const_and (Val1 : llvalue) (Val2 : llvalue) : llvalue = F_llvm_const_and.f (Val1, Val2)
fun const_or (Val1 : llvalue) (Val2 : llvalue) : llvalue = F_llvm_const_or.f (Val1, Val2)
fun const_xor (Val1 : llvalue) (Val2 : llvalue) : llvalue = F_llvm_const_xor.f (Val1, Val2)
fun const_icmp (Pred : Icmp.t) (LHSConstant : llvalue) (RHSConstant : llvalue) : llvalue = F_llvm_const_icmp.f (Int32.fromInt $ Icmp.toInt Pred, LHSConstant, RHSConstant)
fun const_fcmp (Pred : Fcmp.t) (LHSConstant : llvalue) (RHSConstant : llvalue) : llvalue = F_llvm_const_fcmp.f (Int32.fromInt $ Fcmp.toInt Pred, LHSConstant, RHSConstant)
fun const_shl (Val1 : llvalue) (Val2 : llvalue) : llvalue = F_llvm_const_shl.f (Val1, Val2)
fun const_lshr (Val1 : llvalue) (Val2 : llvalue) : llvalue = F_llvm_const_lshr.f (Val1, Val2)
fun const_ashr (Val1 : llvalue) (Val2 : llvalue) : llvalue = F_llvm_const_ashr.f (Val1, Val2)
fun const_gep (ConstantVal : llvalue) (Indices : llvalue array) : llvalue =
  let
      val Indices' = dupVPtrArr Indices
  in
      F_llvm_const_gep.f (ConstantVal, Indices', Int32.fromInt $ Array.length Indices)
      before
      C.free Indices'
  end
fun const_in_bounds_gep (ConstantVal : llvalue) (Indices : llvalue array) : llvalue =
  let
      val Indices' = dupVPtrArr Indices
  in
      F_llvm_const_in_bounds_gep.f (ConstantVal, Indices', Int32.fromInt $ Array.length Indices)
      before
      C.free Indices'
  end
fun const_trunc (Val : llvalue) (Ty : lltype) : llvalue = F_llvm_const_trunc.f (Val, Ty)
fun const_sext (Val : llvalue) (Ty : lltype) : llvalue = F_llvm_const_sext.f (Val, Ty)
fun const_zext (Val : llvalue) (Ty : lltype) : llvalue = F_llvm_const_zext.f (Val, Ty)
fun const_fptrunc (Val : llvalue) (Ty : lltype) : llvalue = F_llvm_const_fptrunc.f (Val, Ty)
fun const_fpext (Val : llvalue) (Ty : lltype) : llvalue = F_llvm_const_fpext.f (Val, Ty)
fun const_uitofp (Val : llvalue) (Ty : lltype) : llvalue = F_llvm_const_uitofp.f (Val, Ty)
fun const_sitofp (Val : llvalue) (Ty : lltype) : llvalue = F_llvm_const_sitofp.f (Val, Ty)
fun const_fptoui (Val : llvalue) (Ty : lltype) : llvalue = F_llvm_const_fptoui.f (Val, Ty)
fun const_fptosi (Val : llvalue) (Ty : lltype) : llvalue = F_llvm_const_fptosi.f (Val, Ty)
fun const_ptrtoint (Val : llvalue) (Ty : lltype) : llvalue = F_llvm_const_ptrtoint.f (Val, Ty)
fun const_inttoptr (Val : llvalue) (Ty : lltype) : llvalue = F_llvm_const_inttoptr.f (Val, Ty)
fun const_bitcast (Val : llvalue) (Ty : lltype) : llvalue = F_llvm_const_bitcast.f (Val, Ty)
fun const_zext_or_bitcast (Val : llvalue) (Ty : lltype) : llvalue = F_llvm_const_zext_or_bitcast.f (Val, Ty)
fun const_sext_or_bitcast (Val : llvalue) (Ty : lltype) : llvalue = F_llvm_const_sext_or_bitcast.f (Val, Ty)
fun const_trunc_or_bitcast (Val : llvalue) (Ty : lltype) : llvalue = F_llvm_const_trunc_or_bitcast.f (Val, Ty)
fun const_pointercast (Val : llvalue) (Ty : lltype) : llvalue = F_llvm_const_pointercast.f (Val, Ty)
fun const_intcast (CV : llvalue) (T : lltype) (IsSigned : bool) : llvalue = F_llvm_const_intcast.f (CV, T, if IsSigned then 1 else 0)
fun const_fpcast (Val : llvalue) (Ty : lltype) : llvalue = F_llvm_const_fpcast.f (Val, Ty)
fun const_select (Val1 : llvalue) (Val2 : llvalue) (Val3 : llvalue) : llvalue = F_llvm_const_select.f (Val1, Val2, Val3)
fun const_extractelement (Val1 : llvalue) (Val2 : llvalue) : llvalue = F_llvm_const_extractelement.f (Val1, Val2)
fun const_insertelement (Val1 : llvalue) (Val2 : llvalue) (Val3 : llvalue) : llvalue = F_llvm_const_insertelement.f (Val1, Val2, Val3)
fun const_shufflevector (Val1 : llvalue) (Val2 : llvalue) (Val3 : llvalue) : llvalue = F_llvm_const_shufflevector.f (Val1, Val2, Val3)
fun const_extractvalue (Aggregate : llvalue) (Indices : int array) : llvalue =
  let
      val Indices' = dupIntArr Indices
  in
      F_llvm_const_extractvalue.f (Aggregate, Indices', Int32.fromInt $ Array.length Indices)
      before
      C.free Indices'
  end
fun const_insertvalue (Aggregate : llvalue) (Val : llvalue) (Indices : int array) : llvalue =
  let
      val Indices' = dupIntArr Indices
  in
      F_llvm_const_insertvalue.f (Aggregate, Val, Indices', Int32.fromInt $ Array.length Indices)
      before
      C.free Indices'
  end
fun const_inline_asm (Ty : lltype) (Asm : string) (Constraints : string) (HasSideEffects : bool) (IsAlignStack : bool) : llvalue =
  let
      val Asm' = ZString.dupML Asm
      val Constraints' = ZString.dupML Constraints
  in
      F_llvm_const_inline_asm.f (Ty, Asm', Constraints', if HasSideEffects then 1 else 0, if IsAlignStack then 1 else 0)
      before
      (C.free Asm'; C.free Constraints')
  end
fun block_address (Val : llvalue) (BB : llbasicblock) : llvalue = F_llvm_block_address.f (Val, BB)

(*--... Operations on global variables, functions, and aliases (globals) ...--*)

(*--... Operations on global variables .....................................--*)

(*--... Operations on aliases ..............................................--*)

(*--... Operations on functions ............................................--*)

(*--... Operations on par AMS ...............................................--*)

(*--... Operations on basic blocks .........................................--*)

(*--... Operations on instructions .........................................--*)

(*--... Operations on call sites ...........................................--*)

(*--... Operations on call instructions (only) .............................--*)

(*--... Operations on load/store instructions (only) .......................--*)

(*--... Operations on terminators ..........................................--*)

(*--... Operations on branches .............................................--*)

(*--... Operations on phi nodes ............................................--*)

(*===-- Instruction builders ----------------------------------------------===*)

(*--... Metadata ...........................................................--*)

(*--... Terminators ........................................................--*)

(*--... Arithmetic .........................................................--*)

(*--... Memory .............................................................--*)

(*--... Casts ..............................................................--*)

(*--... Comparisons ........................................................--*)

(*--... Miscellaneous instructions .........................................--*)

(*===-- Memory buffers ----------------------------------------------------===*)

structure MemoryBuffer =
struct
end

(*===-- Pass Manager ------------------------------------------------------===*)

structure PassManager =
struct
end

end
