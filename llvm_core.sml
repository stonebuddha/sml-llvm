structure LlvmCore :> LLVM_CORE =
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

fun fromInt 0 = External
  | fromInt 1 = Available_externally
  | fromInt 2 = Link_once
  | fromInt 3 = Link_once_odr
  | fromInt 4 = Link_once_odr_auto_hide
  | fromInt 5 = Weak
  | fromInt 6 = Weak_odr
  | fromInt 7 = Appending
  | fromInt 8 = Internal
  | fromInt 9 = Private
  | fromInt 10 = Dllimport
  | fromInt 11 = Dllexport
  | fromInt 12 = External_weak
  | fromInt 13 = Ghost
  | fromInt 14 = Common
  | fromInt 15 = Linker_private
  | fromInt 16 = Linker_private_weak
  | fromInt _ = raise (Fail "Linkage.fromInt")

fun toInt External = 0
  | toInt Available_externally = 1
  | toInt Link_once = 2
  | toInt Link_once_odr = 3
  | toInt Link_once_odr_auto_hide = 4
  | toInt Weak = 5
  | toInt Weak_odr = 6
  | toInt Appending = 7
  | toInt Internal = 8
  | toInt Private = 9
  | toInt Dllimport = 10
  | toInt Dllexport = 11
  | toInt External_weak = 12
  | toInt Ghost = 13
  | toInt Common = 14
  | toInt Linker_private = 15
  | toInt Linker_private_weak = 16
end

structure Visibility =
struct
datatype t =
         Default
         | Hidden
         | Protected

fun fromInt 0 = Default
  | fromInt 1 = Hidden
  | fromInt 2 = Protected
  | fromInt _ = raise (Fail "Visibility.fromInt")

fun toInt Default = 0
  | toInt Hidden = 1
  | toInt Protected = 2
end

structure DLLStorageClass =
struct
datatype t =
         Default
         | DLLImport
         | DLLExport

fun fromInt 0 = Default
  | fromInt 1 = DLLImport
  | fromInt 2 = DLLExport
  | fromInt _ = raise (Fail "DLLStorageClass.fromInt")

fun toInt Default = 0
  | toInt DLLImport = 1
  | toInt DLLExport = 2
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
         | Readnone
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

fun fromInt 0 = Eq
  | fromInt 1 = Ne
  | fromInt 2 = Ugt
  | fromInt 3 = Uge
  | fromInt 4 = Ult
  | fromInt 5 = Ule
  | fromInt 6 = Sgt
  | fromInt 7 = Sge
  | fromInt 8 = Slt
  | fromInt 9 = Sle
  | fromInt _ = raise (Fail "Icmp.fromInt")
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

fun fromInt 0 = False
  | fromInt 1 = Oeq
  | fromInt 2 = Ogt
  | fromInt 3 = Oge
  | fromInt 4 = Olt
  | fromInt 5 = Ole
  | fromInt 6 = One
  | fromInt 7 = Ord
  | fromInt 8 = Uno
  | fromInt 9 = Ueq
  | fromInt 10 = Ugt
  | fromInt 11 = Uge
  | fromInt 12 = Ult
  | fromInt 13 = Ule
  | fromInt 14 = Une
  | fromInt 15 = True
  | fromInt _ = raise (Fail "Fcmp.fromInt")
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

fun fromInt 0 = None
  | fromInt 1 = GeneralDynamic
  | fromInt 2 = LocalDynamic
  | fromInt 3 = InitialExec
  | fromInt 4 = LocalExec
  | fromInt _ = raise (Fail "ThreadLocalMode.fromInt")

fun toInt None = 0
  | toInt GeneralDynamic = 1
  | toInt LocalDynamic = 2
  | toInt InitialExec = 3
  | toInt LocalExec = 4
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
         | AcquireRelease
         | SequentiallyConsistent

fun toInt NotAtomic = 0
  | toInt Unordered = 1
  | toInt Monotonic = 2
  | toInt Invalid = 3
  | toInt Acquire = 4
  | toInt Release = 5
  | toInt AcquireRelease = 6
  | toInt SequentiallyConsistent = 7
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

fun toInt Xchg = 0
  | toInt Add = 1
  | toInt Sub = 2
  | toInt And = 3
  | toInt Nand = 4
  | toInt Or = 5
  | toInt Xor = 6
  | toInt Max = 7
  | toInt Min = 8
  | toInt UMax = 9
  | toInt UMin = 10
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

datatype ('a, 'b) llpos =
         At_end of 'a
         | Before of 'b

datatype ('a, 'b) llrev_pos =
         At_start of 'a
         | After of 'b

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
fun global_parent (Global : llvalue) : llmodule = F_llvm_global_parent.f Global
fun is_declaration (Global : llvalue) : bool =
  case F_llvm_is_declaration.f Global of
      0 => false
    | 1 => true
    | _ => raise (Fail "is_declaration")
fun linkage (Global : llvalue) : Linkage.t = Linkage.fromInt $ Int32.toInt $ F_llvm_linkage.f Global
fun set_linkage (Linkage : Linkage.t) (Global : llvalue) : unit = F_llvm_set_linkage.f (Int32.fromInt $ Linkage.toInt Linkage, Global)
fun unnamed_addr (Global : llvalue) : bool =
  case F_llvm_unnamed_addr.f Global of
      0 => false
    | 1 => true
    | _ => raise (Fail "unnamed_addr")
fun set_unnamed_addr (UseUnnamedAddr : bool) (Global : llvalue) : unit = F_llvm_set_unnamed_addr.f (if UseUnnamedAddr then 1 else 0, Global)
fun section (Global : llvalue) : string =
  let
      val S = F_llvm_section.f Global
  in
      ZString.toML S
      before
      C.free S
  end
fun set_section (Section : string) (Global : llvalue) : unit =
  let
      val Section' = ZString.dupML Section
  in
      F_llvm_set_section.f (Section', Global)
      before
      C.free Section'
  end
fun visibility (Global : llvalue) : Visibility.t = Visibility.fromInt $ Int32.toInt $ F_llvm_visibility.f Global
fun set_visibility (Viz : Visibility.t) (Global : llvalue) : unit = F_llvm_set_visibility.f (Int32.fromInt $ Visibility.toInt Viz, Global)
fun dll_storage_class (Global : llvalue) : DLLStorageClass.t = DLLStorageClass.fromInt $ Int32.toInt $ F_llvm_dll_storage_class.f Global
fun set_dll_storage_class (Viz : DLLStorageClass.t) (Global : llvalue) : unit = F_llvm_set_dll_storage_class.f (Int32.fromInt $ DLLStorageClass.toInt Viz, Global)
fun alignment (Global : llvalue) : int = Int32.toInt $ F_llvm_alignment.f Global
fun set_alignment (Bytes : int) (Global : llvalue) : unit = F_llvm_set_alignment.f (Int32.fromInt Bytes, Global)
fun is_global_constant (Val : llvalue) : bool =
  case F_llvm_is_global_constant.f Val of
      0 => false
    | 1 => true
    | _ => raise (Fail "is_global_constant")
fun set_global_constant (IsConstant : bool) (Val : llvalue) : unit = F_llvm_set_global_constant.f (if IsConstant then 1 else 0, Val)

(*--... Operations on global variables .....................................--*)
fun declare_global (Ty : lltype) (Name : string) (M : llmodule) : llvalue =
  let
      val Name' = ZString.dupML Name
  in
      F_llvm_declare_global.f (Ty, Name', M)
      before
      C.free Name'
  end
fun declare_qualified_global (Ty : lltype) (Name : string) (AddrSpace : int) (M : llmodule) : llvalue =
  let
      val Name' = ZString.dupML Name
  in
      F_llvm_declare_qualified_global.f (Ty, Name', Int32.fromInt AddrSpace, M)
      before
      C.free Name'
  end
fun define_global (Name : string) (Initializer : llvalue) (M : llmodule) : llvalue =
  let
      val Name' = ZString.dupML Name
  in
      F_llvm_define_global.f (Name', Initializer, M)
      before
      C.free Name'
  end
fun define_qualified_global (Name : string) (Initializer : llvalue) (AddrSpace : int) (M : llmodule) : llvalue =
  let
      val Name' = ZString.dupML Name
  in
      F_llvm_define_qualified_global.f (Name', Initializer, Int32.fromInt AddrSpace, M)
      before
      C.free Name'
  end
fun lookup_global (Name : string) (M : llmodule) : llvalue option =
  let
      val Name' = ZString.dupML Name
      val Res = F_llvm_lookup_global.f (Name', M)
  in
      (if C.Ptr.isNull' Res then NONE else SOME Res)
      before
      C.free Name'
  end
fun delete_global (GlobalVar : llvalue) : unit = F_llvm_delete_global.f GlobalVar
fun global_initializer (GlobalVar : llvalue) : llvalue = F_llvm_global_initializer.f GlobalVar
fun set_initializer (ConstantVal : llvalue) (GlobalVar : llvalue) : unit = F_llvm_set_initializer.f (ConstantVal, GlobalVar)
fun remove_initializer (GlobalVar : llvalue) : unit = F_llvm_remove_initializer.f GlobalVar
fun is_thread_local (GlobalVar : llvalue) : bool =
  case F_llvm_is_thread_local.f GlobalVar of
      0 => false
    | 1 => true
    | _ => raise (Fail "is_thread_local")
fun set_thread_local (IsThreadLocal : bool) (GlobalVar : llvalue) : unit = F_llvm_set_thread_local.f (if IsThreadLocal then 1 else 0, GlobalVar)
fun thread_local_mode (GlobalVar : llvalue) : ThreadLocalMode.t = ThreadLocalMode.fromInt $ Int32.toInt $ F_llvm_thread_local_mode.f GlobalVar
fun set_thread_local_mode (ThreadLocalMode : ThreadLocalMode.t) (GlobalVar : llvalue) : unit = F_llvm_set_thread_local_mode.f (Int32.fromInt $ ThreadLocalMode.toInt ThreadLocalMode, GlobalVar)
fun is_externally_initialized (GlobalVar : llvalue) : bool =
  case F_llvm_is_externally_initialized.f GlobalVar of
      0 => false
    | 1 => true
    | _ => raise (Fail "is_externally_initialized")
fun set_externally_initialized (IsExternallyInitialized : bool) (GlobalVar : llvalue) : unit = F_llvm_set_externally_initialized.f (if IsExternallyInitialized then 1 else 0, GlobalVar)
fun global_begin (M : llmodule) : (llmodule, llvalue) llpos =
  let
      val Tag = C.new C.T.sint
      val Res = F_llvm_global_begin.f (M, C.Ptr.|&| Tag)
  in
      (case C.Get.sint Tag of
           0 => At_end Res
         | 1 => Before Res
         | _ => raise (Fail "global_begin"))
      before
      C.free (C.Ptr.|&| Tag)
  end
fun global_succ (Val : llvalue) : (llmodule, llvalue) llpos =
  let
      val Tag = C.new C.T.sint
      val Res = F_llvm_global_succ.f (Val, C.Ptr.|&| Tag)
  in
      (case C.Get.sint Tag of
           0 => At_end Res
         | 1 => Before Res
         | _ => raise (Fail "global_succ"))
      before
      C.free (C.Ptr.|&| Tag)
  end
fun global_end (M : llmodule) : (llmodule, llvalue) llrev_pos =
  let
      val Tag = C.new C.T.sint
      val Res = F_llvm_global_end.f (M, C.Ptr.|&| Tag)
  in
      (case C.Get.sint Tag of
           0 => At_start Res
         | 1 => After Res
         | _ => raise (Fail "global_end"))
      before
      C.free (C.Ptr.|&| Tag)
  end
fun global_pred (Val : llvalue) : (llmodule, llvalue) llrev_pos =
  let
      val Tag = C.new C.T.sint
      val Res = F_llvm_global_pred.f (Val, C.Ptr.|&| Tag)
  in
      (case C.Get.sint Tag of
           0 => At_start Res
         | 1 => After Res
         | _ => raise (Fail "global_pred"))
      before
      C.free (C.Ptr.|&| Tag)
  end

fun iter_global_range f i e =
  if i = e then () else
  case i of
      At_end _ => raise (Fail "Invalid global variable range.")
    | Before bb => (f bb; iter_global_range f (global_succ bb) e)

fun iter_globals f m = iter_global_range f (global_begin m) (At_end m)

fun fold_left_global_range f init i e =
  if i = e then init else
  case i of
      At_end _ => raise (Fail "Invalid global variable range.")
    | Before bb => fold_left_global_range f (f init bb) (global_succ bb) e

fun fold_left_globals f init m = fold_left_global_range f init (global_begin m) (At_end m)

fun rev_iter_global_range f i e =
  if i = e then () else
  case i of
      At_start _ => raise (Fail "Invalid global variable range.")
    | After bb => (f bb; rev_iter_global_range f (global_pred bb) e)

fun rev_iter_globals f m = rev_iter_global_range f (global_end m) (At_start m)

fun fold_right_global_range f i e init =
  if i = e then init else
  case i of
      At_start _ => raise (Fail "Invalid global variable range.")
    | After bb => fold_right_global_range f (global_pred bb) e (f bb init)

fun fold_right_globals f m init = fold_right_global_range f (global_end m) (At_start m) init

(*--... Operations on aliases ..............................................--*)
fun add_alias (M : llmodule) (Ty : lltype) (Aliasee : llvalue) (Name : string) : llvalue =
  let
      val Name' = ZString.dupML Name
  in
      F_llvm_add_alias.f (M, Ty, Aliasee, Name')
      before
      C.free Name'
  end

(*--... Operations on functions ............................................--*)
fun declare_function (Name : string) (Ty : lltype) (M : llmodule) : llvalue =
  let
      val Name' = ZString.dupML Name
  in
      F_llvm_declare_function.f (Name', Ty, M)
      before
      C.free Name'
  end
fun define_function (Name : string) (Ty : lltype) (M : llmodule) : llvalue =
  let
      val Name' = ZString.dupML Name
  in
      F_llvm_define_function.f (Name', Ty, M)
      before
      C.free Name'
  end
fun lookup_function (Name : string) (M : llmodule) : llvalue option =
  let
      val Name' = ZString.dupML Name
      val Res = F_llvm_lookup_function.f (Name', M)
  in
      (if C.Ptr.isNull' Res then NONE else SOME Res)
      before
      C.free Name'
  end
fun delete_function (Fn : llvalue) : unit = F_llvm_delete_function.f Fn
fun is_intrinsic (Fn : llvalue) : bool =
  case F_llvm_is_intrinsic.f Fn of
      0 => false
    | 1 => true
    | _ => raise (Fail "is_intrinsic")
fun function_call_conv (Fn : llvalue) : int = Int32.toInt $ F_llvm_function_call_conv.f Fn
fun set_function_call_conv (Id : int) (Fn : llvalue) : unit = F_llvm_set_function_call_conv.f (Int32.fromInt Id, Fn)
fun gc (Fn : llvalue) : string option =
  let
      val S = F_llvm_gc.f Fn
  in
      if C.Ptr.isNull S then NONE
      else
          SOME (ZString.toML S)
          before
          C.free S
  end
fun set_gc (GC : string option) (Fn : llvalue) : unit =
  case GC of
      NONE => F_llvm_set_gc.f (C.Ptr.null $ C.T.ro $ C.T.pointer C.T.uchar, Fn)
    | SOME S =>
      let
          val S' = ZString.dupML S
      in
          F_llvm_set_gc.f (S', Fn)
          before
          C.free S'
      end
fun function_begin (M : llmodule) : (llmodule, llvalue) llpos =
  let
      val Tag = C.new C.T.sint
      val Res = F_llvm_function_begin.f (M, C.Ptr.|&| Tag)
  in
      (case C.Get.sint Tag of
           0 => At_end Res
         | 1 => Before Res
         | _ => raise (Fail "function_begin"))
      before
      C.free (C.Ptr.|&| Tag)
  end
fun function_succ (Val : llvalue) : (llmodule, llvalue) llpos =
  let
      val Tag = C.new C.T.sint
      val Res = F_llvm_function_succ.f (Val, C.Ptr.|&| Tag)
  in
      (case C.Get.sint Tag of
           0 => At_end Res
         | 1 => Before Res
         | _ => raise (Fail "function_succ"))
      before
      C.free (C.Ptr.|&| Tag)
  end
fun function_end (M : llmodule) : (llmodule, llvalue) llrev_pos =
  let
      val Tag = C.new C.T.sint
      val Res = F_llvm_function_end.f (M, C.Ptr.|&| Tag)
  in
      (case C.Get.sint Tag of
           0 => At_start Res
         | 1 => After Res
         | _ => raise (Fail "function_end"))
      before
      C.free (C.Ptr.|&| Tag)
  end
fun function_pred (Val : llvalue) : (llmodule, llvalue) llrev_pos =
  let
      val Tag = C.new C.T.sint
      val Res = F_llvm_function_pred.f (Val, C.Ptr.|&| Tag)
  in
      (case C.Get.sint Tag of
           0 => At_start Res
         | 1 => After Res
         | _ => raise (Fail "function_pred"))
      before
      C.free (C.Ptr.|&| Tag)
  end

fun iter_function_range f i e =
  if i = e then () else
  case i of
      At_end _ => raise (Fail "Invalid function range.")
    | Before func => (f func; iter_function_range f (function_succ func) e)

fun iter_functions f m = iter_function_range f (function_begin m) (At_end m)

fun fold_left_function_range f init i e =
  if i = e then init else
  case i of
      At_end _ => raise (Fail "Invalid function range.")
    | Before func => fold_left_function_range f (f init func) (function_succ func) e

fun fold_left_functions f init m = fold_left_function_range f init (function_begin m) (At_end m)

fun rev_iter_function_range f i e =
  if i = e then () else
  case i of
      At_start _ => raise (Fail "Invalid function range.")
    | After func => (f func; rev_iter_function_range f (function_pred func) e)

fun rev_iter_functions f m = rev_iter_function_range f (function_end m) (At_start m)

fun fold_right_function_range f i e init =
  if i = e then init else
  case i of
      At_start _ => raise (Fail "Invalid function range.")
    | After func => fold_right_function_range f (function_pred func) e (f func init)

fun fold_right_functions f m init = fold_right_function_range f (function_end m) (At_start m) init

fun llvm_add_function_attr (Arg : llvalue) (PA : Int32.int) : unit = F_llvm_add_function_attr.f (Arg, PA)
fun llvm_remove_function_attr (Arg : llvalue) (PA : Int32.int) : unit = F_llvm_remove_function_attr.f (Arg, PA)
fun llvm_function_attr (Fn : llvalue) : Int32.int = F_llvm_function_attr.f Fn

fun pack_attr (attr : Attribute.t) : Int32.int =
  case attr of
      Attribute.Zext => Int32.fromLarge $ IntInf.<< (1, Word.fromInt 0)
    | Attribute.Sext => Int32.fromLarge $ IntInf.<< (1, Word.fromInt 1)
    | Attribute.Noreturn => Int32.fromLarge $ IntInf.<< (1, Word.fromInt 2)
    | Attribute.Inreg => Int32.fromLarge $ IntInf.<< (1, Word.fromInt 3)
    | Attribute.Structret => Int32.fromLarge $ IntInf.<< (1, Word.fromInt 4)
    | Attribute.Nounwind => Int32.fromLarge $ IntInf.<< (1, Word.fromInt 5)
    | Attribute.Noalias => Int32.fromLarge $ IntInf.<< (1, Word.fromInt 6)
    | Attribute.Byval => Int32.fromLarge $ IntInf.<< (1, Word.fromInt 7)
    | Attribute.Nest => Int32.fromLarge $ IntInf.<< (1, Word.fromInt 8)
    | Attribute.Readnone => Int32.fromLarge $ IntInf.<< (1, Word.fromInt 9)
    | Attribute.Readonly => Int32.fromLarge $ IntInf.<< (1, Word.fromInt 10)
    | Attribute.Noinline => Int32.fromLarge $ IntInf.<< (1, Word.fromInt 11)
    | Attribute.Alwaysinline => Int32.fromLarge $ IntInf.<< (1, Word.fromInt 12)
    | Attribute.Optsize => Int32.fromLarge $ IntInf.<< (1, Word.fromInt 13)
    | Attribute.Ssp => Int32.fromLarge $ IntInf.<< (1, Word.fromInt 14)
    | Attribute.Sspreq => Int32.fromLarge $ IntInf.<< (1, Word.fromInt 15)
    | Attribute.Alignment n => Int32.fromLarge $ IntInf.<< (IntInf.fromInt n, Word.fromInt 16)
    | Attribute.Nocapture => Int32.fromLarge $ IntInf.<< (1, Word.fromInt 21)
    | Attribute.Noredzone => Int32.fromLarge $ IntInf.<< (1, Word.fromInt 22)
    | Attribute.Noimplicitfloat => Int32.fromLarge $ IntInf.<< (1, Word.fromInt 23)
    | Attribute.Naked => Int32.fromLarge $ IntInf.<< (1, Word.fromInt 24)
    | Attribute.Inlinehint => Int32.fromLarge $ IntInf.<< (1, Word.fromInt 25)
    | Attribute.Stackalignment n => Int32.fromLarge $ IntInf.<< (IntInf.fromInt n, Word.fromInt 26)
    | Attribute.ReturnsTwice => Int32.fromLarge $ IntInf.<< (1, Word.fromInt 29)
    | Attribute.UWTable => Int32.fromLarge $ IntInf.<< (1, Word.fromInt 30)
    | Attribute.NonLazyBind => Int32.fromLarge $ IntInf.<< (1, Word.fromInt 31)

fun unpack_attr (a : Int32.int) : Attribute.t list =
  let
      val l = ref []
      fun check attr = IntInf.andb (Int32.toLarge (pack_attr attr), Int32.toLarge a)
      fun checkattr attr = if (check attr) <> 0 then l := attr :: !l else ()
      val () = checkattr Attribute.Zext
      val () = checkattr Attribute.Sext
      val () = checkattr Attribute.Noreturn
      val () = checkattr Attribute.Inreg
      val () = checkattr Attribute.Structret
      val () = checkattr Attribute.Nounwind
      val () = checkattr Attribute.Noalias
      val () = checkattr Attribute.Byval
      val () = checkattr Attribute.Nest
      val () = checkattr Attribute.Readnone
      val () = checkattr Attribute.Readonly
      val () = checkattr Attribute.Noinline
      val () = checkattr Attribute.Alwaysinline
      val () = checkattr Attribute.Optsize
      val () = checkattr Attribute.Ssp
      val () = checkattr Attribute.Sspreq
      val align = IntInf.andb (IntInf.~>> (Int32.toLarge a, Word.fromInt 16), 31)
      val () = if align <> 0 then l := Attribute.Alignment (IntInf.toInt align) :: !l else ()
      val () = checkattr Attribute.Nocapture
      val () = checkattr Attribute.Noredzone
      val () = checkattr Attribute.Noimplicitfloat
      val () = checkattr Attribute.Naked
      val () = checkattr Attribute.Inlinehint
      val stackalign = IntInf.andb (IntInf.~>> (Int32.toLarge a, Word.fromInt 26), 7)
      val () = if stackalign <> 0 then l := Attribute.Stackalignment (IntInf.toInt stackalign) :: !l else ()
      val () = checkattr Attribute.ReturnsTwice
      val () = checkattr Attribute.UWTable
      val () = checkattr Attribute.NonLazyBind
  in
      !l
  end

fun add_function_attr (Val : llvalue) (PA : Attribute.t) : unit = llvm_add_function_attr Val (pack_attr PA)
fun remove_function_attr (Val : llvalue) (PA : Attribute.t) : unit = llvm_remove_function_attr Val (pack_attr PA)
fun function_attr (Fn : llvalue) : Attribute.t list = unpack_attr $ llvm_function_attr Fn

fun add_target_dependent_function_attr (Arg : llvalue) (A : string) (V : string) : unit =
  let
      val A' = ZString.dupML A
      val V' = ZString.dupML V
  in
      F_llvm_add_target_dependent_function_attr.f (Arg, A', V')
      before
      (C.free A'; C.free V')
  end

(*--... Operations on params ...............................................--*)
fun params (Fn : llvalue) : llvalue array =
  let
      val Len = C.new C.T.sint
      val Buf = F_llvm_params.f (Fn, C.Ptr.|&| Len)
  in
      toVPtrArr Buf Len
      before
      (C.free Buf; C.free (C.Ptr.|&| Len))
  end
fun param (Fn : llvalue) (Index : int) : llvalue = F_llvm_param.f (Fn, Int32.fromInt Index)
fun llvm_param_attr (Param : llvalue) : Int32.int = F_llvm_param_attr.f Param
fun param_attr (Param : llvalue) : Attribute.t list = unpack_attr $ llvm_param_attr Param
fun param_parent (Param : llvalue) : llvalue = F_llvm_param_parent.f Param
fun param_begin (Fn : llvalue) : (llvalue, llvalue) llpos =
  let
      val Tag = C.new C.T.sint
      val Res = F_llvm_param_begin.f (Fn, C.Ptr.|&| Tag)
  in
      (case C.Get.sint Tag of
           0 => At_end Res
         | 1 => Before Res
         | _ => raise (Fail "param_begin"))
      before
      C.free (C.Ptr.|&| Tag)
  end
fun param_succ (Param : llvalue) : (llvalue, llvalue) llpos =
  let
      val Tag = C.new C.T.sint
      val Res = F_llvm_param_succ.f (Param, C.Ptr.|&| Tag)
  in
      (case C.Get.sint Tag of
           0 => At_end Res
         | 1 => Before Res
         | _ => raise (Fail "param_succ"))
      before
      C.free (C.Ptr.|&| Tag)
  end
fun param_end (Fn : llvalue) : (llvalue, llvalue) llrev_pos =
  let
      val Tag = C.new C.T.sint
      val Res = F_llvm_param_end.f (Fn, C.Ptr.|&| Tag)
  in
      (case C.Get.sint Tag of
           0 => At_start Res
         | 1 => After Res
         | _ => raise (Fail "param_end"))
      before
      C.free (C.Ptr.|&| Tag)
  end
fun param_pred (Param : llvalue)  : (llvalue, llvalue) llrev_pos =
  let
      val Tag = C.new C.T.sint
      val Res = F_llvm_param_pred.f (Param, C.Ptr.|&| Tag)
  in
      (case C.Get.sint Tag of
           0 => At_start Res
         | 1 => After Res
         | _ => raise (Fail "param_pred"))
      before
      C.free (C.Ptr.|&| Tag)
  end

fun iter_param_range f i e =
  if i = e then () else
  case i of
      At_end _ => raise (Fail "Invalid parameter range.")
    | Before p => (f p; iter_param_range f (param_succ p) e)

fun iter_params f func = iter_param_range f (param_begin func) (At_end func)

fun fold_left_param_range f init i e =
  if i = e then init else
  case i of
      At_end _ => (raise (Fail "Invalid parameter range."))
    | Before p => fold_left_param_range f (f init p) (param_succ p) e

fun fold_left_params f init func = fold_left_param_range f init (param_begin func) (At_end func)

fun rev_iter_param_range f i e =
  if i = e then () else
  case i of
      At_start _ => raise (Fail "Invalid parameter range.")
    | After p => (f p; rev_iter_function_range f (param_pred p) e)

fun rev_iter_params f func = rev_iter_param_range f (param_end func) (At_start func)

fun fold_right_param_range f init i e =
  if i = e then init else
  case i of
      At_start _ => raise (Fail "Invalid parameter range.")
    | After p => fold_right_param_range f (f p init) (param_pred p) e

fun fold_right_params f func init = fold_right_param_range f init (param_end func) (At_start func)

fun llvm_add_param_attr (Arg : llvalue) (PA : Int32.int) : unit = F_llvm_add_param_attr.f (Arg, PA)
fun llvm_remove_param_attr (Arg : llvalue) (PA : Int32.int) : unit = F_llvm_remove_param_attr.f (Arg, PA)

fun add_param_attr (Arg : llvalue) (PA : Attribute.t) : unit = llvm_add_param_attr Arg (pack_attr PA)
fun remove_param_attr (Arg : llvalue) (PA : Attribute.t) : unit = llvm_remove_param_attr Arg (pack_attr PA)

fun set_param_alignment (Arg : llvalue) (Align : int) : unit = F_llvm_set_param_alignment.f (Arg, Int32.fromInt Align)

(*--... Operations on basic blocks .........................................--*)
fun value_of_block (BB : llbasicblock) : llvalue = F_llvm_value_of_block.f BB
fun value_is_block (Val : llvalue) : bool =
  case F_llvm_value_is_block.f Val of
      0 => false
    | 1 => true
    | _ => raise (Fail "value_is_block")
fun block_of_value (Val : llvalue) : llbasicblock = F_llvm_block_of_value.f Val
fun block_parent (BB : llbasicblock) : llvalue = F_llvm_block_parent.f BB
fun basic_blocks (Fn : llvalue) : llbasicblock array =
  let
      val Len = C.new C.T.sint
      val Buf = F_llvm_basic_blocks.f (Fn, C.Ptr.|&| Len)
  in
      toVPtrArr Buf Len
      before
      (C.free Buf; C.free (C.Ptr.|&| Len))
  end
fun entry_block (Val : llvalue) : llbasicblock = F_llvm_entry_block.f Val
fun delete_block (BB : llbasicblock) : unit = F_llvm_delete_block.f BB
fun remove_block (BB : llbasicblock) : unit = F_llvm_remove_block.f BB
fun move_block_before (Pos : llbasicblock) (BB : llbasicblock) : unit = F_llvm_move_block_before.f (Pos, BB)
fun move_block_after (Pos : llbasicblock) (BB : llbasicblock) : unit = F_llvm_move_block_after.f (Pos, BB)
fun append_block (C : llcontext) (Name : string) (Fn : llvalue) : llbasicblock =
  let
      val Name' = ZString.dupML Name
  in
      F_llvm_append_block.f (C, Name', Fn)
      before
      C.free Name'
  end
fun insert_block (C : llcontext) (Name : string) (BB : llbasicblock) : llbasicblock =
  let
      val Name' = ZString.dupML Name
  in
      F_llvm_insert_block.f (C, Name', BB)
      before
      C.free Name'
  end
fun block_begin (Fn : llvalue) : (llvalue, llbasicblock) llpos =
  let
      val Tag = C.new C.T.sint
      val Res = F_llvm_block_begin.f (Fn, C.Ptr.|&| Tag)
  in
      (case C.Get.sint Tag of
           0 => At_end Res
         | 1 => Before Res
         | _ => raise (Fail "block_begin"))
      before
      C.free (C.Ptr.|&| Tag)
  end
fun block_succ (BB : llbasicblock) : (llvalue, llbasicblock) llpos =
  let
      val Tag = C.new C.T.sint
      val Res = F_llvm_block_succ.f (BB, C.Ptr.|&| Tag)
  in
      (case C.Get.sint Tag of
           0 => At_end Res
         | 1 => Before Res
         | _ => raise (Fail "block_succ"))
      before
      C.free (C.Ptr.|&| Tag)
  end
fun block_end (Fn : llvalue) : (llvalue, llbasicblock) llrev_pos =
  let
      val Tag = C.new C.T.sint
      val Res = F_llvm_block_end.f (Fn, C.Ptr.|&| Tag)
  in
      (case C.Get.sint Tag of
           0 => At_start Res
         | 1 => After Res
         | _ => raise (Fail "block_end"))
      before
      C.free (C.Ptr.|&| Tag)
  end
fun block_pred (BB : llbasicblock) : (llvalue, llbasicblock) llrev_pos =
  let
      val Tag = C.new C.T.sint
      val Res = F_llvm_block_pred.f (BB, C.Ptr.|&| Tag)
  in
      (case C.Get.sint Tag of
           0 => At_start Res
         | 1 => After Res
         | _ => raise (Fail "block_pred"))
      before
      C.free (C.Ptr.|&| Tag)
  end
fun block_terminator (BB : llbasicblock) : llvalue option =
  let
      val Res = F_llvm_block_terminator.f BB
  in
      if C.Ptr.isNull' Res then NONE
      else SOME Res
  end

fun iter_block_range f i e =
  if i = e then () else
  case i of
      At_end _ => raise (Fail "Invalid block range.")
    | Before bb => (f bb; iter_block_range f (block_succ bb) e)

fun iter_blocks f func = iter_block_range f (block_begin func) (At_end func)

fun fold_left_block_range f init i e =
  if i = e then init else
  case i of
      At_end _ => (raise (Fail "Invalid block range."))
    | Before bb => fold_left_block_range f (f init bb) (block_succ bb) e

fun fold_left_blocks f init func = fold_left_block_range f init (block_begin func) (At_end func)

fun rev_iter_block_range f i e =
  if i = e then () else
  case i of
      At_start _ => raise (Fail "Invalid block range.")
    | After bb => (f bb; rev_iter_function_range f (block_pred bb) e)

fun rev_iter_blocks f func = rev_iter_block_range f (block_end func) (At_start func)

fun fold_right_block_range f init i e =
  if i = e then init else
  case i of
      At_start _ => raise (Fail "Invalid block range.")
    | After bb => fold_right_block_range f (f bb init) (block_pred bb) e

fun fold_right_blocks f func init = fold_right_block_range f init (block_end func) (At_start func)

(*--... Operations on instructions .........................................--*)
fun instr_parent (Inst : llvalue) : llbasicblock = F_llvm_instr_parent.f Inst
fun instr_begin (BB : llbasicblock) : (llbasicblock, llvalue) llpos =
  let
      val Tag = C.new C.T.sint
      val Res = F_llvm_instr_begin.f (BB, C.Ptr.|&| Tag)
  in
      (case C.Get.sint Tag of
           0 => At_end Res
         | 1 => Before Res
         | _ => raise (Fail "instr_begin"))
      before
      C.free (C.Ptr.|&| Tag)
  end
fun instr_succ (Inst : llvalue) : (llbasicblock, llvalue) llpos =
  let
      val Tag = C.new C.T.sint
      val Res = F_llvm_instr_succ.f (Inst, C.Ptr.|&| Tag)
  in
      (case C.Get.sint Tag of
           0 => At_end Res
         | 1 => Before Res
         | _ => raise (Fail "instr_succ"))
      before
      C.free (C.Ptr.|&| Tag)
  end
fun instr_end (BB : llbasicblock) : (llbasicblock, llvalue) llrev_pos =
  let
      val Tag = C.new C.T.sint
      val Res = F_llvm_instr_end.f (BB, C.Ptr.|&| Tag)
  in
      (case C.Get.sint Tag of
           0 => At_start Res
         | 1 => After Res
         | _ => raise (Fail "instr_end"))
      before
      C.free (C.Ptr.|&| Tag)
  end
fun instr_pred (Inst : llvalue) : (llbasicblock, llvalue) llrev_pos =
  let
      val Tag = C.new C.T.sint
      val Res = F_llvm_instr_pred.f (Inst, C.Ptr.|&| Tag)
  in
      (case C.Get.sint Tag of
           0 => At_start Res
         | 1 => After Res
         | _ => raise (Fail "instr_pred"))
      before
      C.free (C.Ptr.|&| Tag)
  end
fun instr_opcode (Inst : llvalue) : Opcode.t = Opcode.fromInt $ Int32.toInt $ F_llvm_instr_opcode.f Inst
fun icmp_predicate (Inst : llvalue) : Icmp.t option =
  let
      val Res = F_llvm_icmp_predicate.f Inst
  in
      if C.Ptr.isNull Res then NONE
      else
          SOME (Icmp.fromInt $ Int32.toInt $ C.Get.sint $ C.Ptr.|*| Res)
          before
          C.free Res
  end
fun fcmp_predicate (Inst : llvalue) : Fcmp.t option =
  let
      val Res = F_llvm_fcmp_predicate.f Inst
  in
      if C.Ptr.isNull Res then NONE
      else
          SOME (Fcmp.fromInt $ Int32.toInt $ C.Get.sint $ C.Ptr.|*| Res)
          before
          C.free Res
  end
fun instr_clone (Inst : llvalue) : llvalue = F_llvm_instr_clone.f Inst

fun iter_instr_range f i e =
  if i = e then () else
  case i of
      At_end _ => raise (Fail "Invalid instruction range.")
    | Before i => (f i; iter_instr_range f (instr_succ i) e)

fun iter_instrs f bb = iter_instr_range f (instr_begin bb) (At_end bb)

fun fold_left_instr_range f init i e =
  if i = e then init else
  case i of
      At_end _ => (raise (Fail "Invalid instruction range."))
    | Before i => fold_left_instr_range f (f init i) (instr_succ i) e

fun fold_left_instrs f init bb = fold_left_instr_range f init (instr_begin bb) (At_end bb)

fun rev_iter_instr_range f i e =
  if i = e then () else
  case i of
      At_start _ => raise (Fail "Invalid instruction range.")
    | After i => (f i; rev_iter_function_range f (instr_pred i) e)

fun rev_iter_instrs f bb = rev_iter_instr_range f (instr_end bb) (At_start bb)

fun fold_right_instr_range f init i e =
  if i = e then init else
  case i of
      At_start _ => raise (Fail "Invalid instruction range.")
    | After i => fold_right_instr_range f (f i init) (instr_pred i) e

fun fold_right_instrs f bb init = fold_right_instr_range f init (instr_end bb) (At_start bb)

(*--... Operations on call sites ...........................................--*)
fun instruction_call_conv (Inst : llvalue) : int = Int32.toInt $ F_llvm_instruction_call_conv.f Inst
fun set_instruction_call_conv (CC : int) (Inst : llvalue) : unit = F_llvm_set_instruction_call_conv.f (Int32.fromInt CC, Inst)
fun llvm_add_instruction_param_attr (Inst : llvalue) (Index : int) (PA : Int32.int) : unit = F_llvm_add_instruction_param_attr.f (Inst, Int32.fromInt Index, PA)
fun llvm_remove_instrution_param_attr (Inst : llvalue) (Index : int) (PA : Int32.int) : unit = F_llvm_remove_instruction_param_attr.f (Inst, Int32.fromInt Index, PA)

fun add_instruction_param_attr (Inst : llvalue) (Index : int) (PA : Attribute.t) : unit = llvm_add_instruction_param_attr Inst Index (pack_attr PA)
fun remove_instruction_param_attr (Inst : llvalue) (Index : int) (PA : Attribute.t) : unit = llvm_remove_instrution_param_attr Inst Index (pack_attr PA)

(*--... Operations on call instructions (only) .............................--*)
fun is_tail_call (CallInst : llvalue) : bool =
  case F_llvm_is_tail_call.f CallInst of
      0 => false
    | 1 => true
    | _ => raise (Fail "is_tail_call")
fun set_tail_call (IsTailCall : bool) (CallInst : llvalue) : unit = F_llvm_set_tail_call.f (if IsTailCall then 1 else 0, CallInst)

(*--... Operations on load/store instructions (only) .......................--*)
fun is_volatile (MemoryInst : llvalue) : bool =
  case F_llvm_is_volatile.f MemoryInst of
      0 => false
    | 1 => true
    | _ => raise (Fail "is_volatile")
fun set_volatile (IsVolatile : bool) (MemoryInst : llvalue) : unit = F_llvm_set_volatile.f (if IsVolatile then 1 else 0, MemoryInst)

(*--... Operations on terminators ..........................................--*)
fun is_terminator llv =
  case classify_value llv of
      ValueKind.Instruction Opcode.Br => true
    | ValueKind.Instruction Opcode.IndirectBr => true
    | ValueKind.Instruction Opcode.Invoke => true
    | ValueKind.Instruction Opcode.Resume => true
    | ValueKind.Instruction Opcode.Ret => true
    | ValueKind.Instruction Opcode.Switch => true
    | ValueKind.Instruction Opcode.Unreachable => true
    | _ => false

fun successor (Val : llvalue) (I : int) : llbasicblock = F_llvm_successor.f (Val, Int32.fromInt I)
fun set_successor (Val : llvalue) (I : int) (BB : llbasicblock) : unit = F_llvm_set_successor.f (Val, Int32.fromInt I, BB)
fun num_successors (Val : llvalue) : int = Int32.toInt $ F_llvm_num_successors.f Val

fun successors llv =
  if not (is_terminator llv) then
      raise (Fail "successors can only be used on terminators")
  else
      Array.tabulate (num_successors llv, successor llv)

fun iter_successors f llv =
  if not (is_terminator llv) then
      raise (Fail "iter_successors can only be used on terminators")
  else
      let
          val num = num_successors llv
          fun loop idx =
            if idx = num then () else
            (f (successor llv idx); loop (idx + 1))
      in
          loop 0
      end

fun fold_successors f llv z =
  if not (is_terminator llv) then
      raise (Fail "fold_successors can only be used on terminators")
  else
      let
          val num = num_successors llv
          fun loop idx acc =
            if idx = num then acc else
            loop (idx + 1) (f (successor llv idx) acc)
      in
          loop 0 z
      end

(*--... Operations on branches .............................................--*)
fun condition (Val : llvalue) : llvalue = F_llvm_condition.f Val
fun set_condition (B : llvalue) (C : llvalue) : unit = F_llvm_set_condition.f (B, C)
fun is_conditional (Val : llvalue) : bool =
  case F_llvm_is_conditional.f Val of
      0 => false
    | 1 => true
    | _ => raise (Fail "is_conditional")

structure Branch =
struct
datatype t =
         Conditional of llvalue * llbasicblock * llbasicblock
         | Unconditional of llbasicblock
end

fun get_branch llv =
  if classify_value llv <> ValueKind.Instruction Opcode.Br then
      NONE
  else
      if is_conditional llv then
          SOME (Branch.Conditional (condition llv, successor llv 0, successor llv 1))
      else
          SOME (Branch.Unconditional (successor llv 0))

(*--... Operations on phi nodes ............................................--*)
fun add_incoming (Val : llvalue, BB : llbasicblock) (PhiNode : llvalue) : unit = F_llvm_add_incoming.f (Val, BB, PhiNode)
fun incoming (PhiNode : llvalue) : (llvalue * llbasicblock) list =
  let
      val Len = C.new C.T.sint
      val Buf = F_llvm_incoming.f (PhiNode, C.Ptr.|&| Len)
      val Num = Int32.toInt $ C.Get.sint Len
      val Arr = toVPtrArr Buf Len before (C.free Buf; C.free (C.Ptr.|&| Len))
      fun loop Idx Acc =
        if Idx = 0 then Acc else
        loop (Idx - 2) ((Array.sub (Arr, Idx - 2), Array.sub (Arr, Idx - 1)) :: Acc)
  in
      loop Num []
  end

fun delete_instruction (Inst : llvalue) : unit = F_llvm_delete_instruction.f Inst

(*===-- Instruction builders ----------------------------------------------===*)
fun builder (C : llcontext) : llbuilder = F_llvm_builder.f C
fun position_builder (Pos : (llbasicblock, llvalue) llpos) (B : llbuilder) =
  case Pos of
      At_end BB => F_llvm_position_builder.f (BB, 0, B)
    | Before Inst => F_llvm_position_builder.f (Inst, 1, B)
fun insertion_block (B : llbuilder) : llbasicblock = F_llvm_insertion_block.f B
fun insert_into_builder (Inst : llvalue) (Name : string) (B : llbuilder) : unit =
  let
      val Name' = ZString.dupML Name
  in
      F_llvm_insert_into_builder.f (Inst, Name', B)
      before
      C.free Name'
  end

fun builder_at context ip =
  let
      val b = builder context
      val () = position_builder ip context
  in
      b
  end

fun builder_before context i = builder_at context (Before i)
fun builder_at_end context bb = builder_at context (At_end bb)

fun position_before i = position_builder (Before i)
fun position_at_end bb = position_builder (At_end bb)

(*--... Metadata ...........................................................--*)
fun set_current_debug_location (B : llbuilder) (Val : llvalue) : unit = F_llvm_set_current_debug_location.f (B, Val)
fun clear_current_debug_location (B : llbuilder) : unit = F_llvm_clear_current_debug_location.f B
fun current_debug_location (B : llbuilder) : llvalue option =
  let
      val Res = F_llvm_current_debug_location.f B
  in
      if C.Ptr.isNull' Res then NONE
      else SOME Res
  end
fun set_inst_debug_location (B : llbuilder) (Val : llvalue) : unit = F_llvm_set_inst_debug_location.f (B, Val)

(*--... Terminators ........................................................--*)
val build_ret_void : llbuilder -> llvalue = fn B => F_llvm_build_ret_void.f B
val build_ret : llvalue -> llbuilder -> llvalue = fn Val => fn B => F_llvm_build_ret.f (Val, B)
val build_aggregate_ret : llvalue array -> llbuilder -> llvalue =
 fn RetVals => fn B =>
    let
        val RetVals' = dupVPtrArr RetVals
    in
        F_llvm_build_aggregate_ret.f (RetVals', Int32.fromInt $ Array.length RetVals, B)
        before
        C.free RetVals'
    end
val build_br : llbasicblock -> llbuilder -> llvalue = fn BB => fn B => F_llvm_build_br.f (BB, B)
val build_cond_br : llvalue -> llbasicblock -> llbasicblock -> llbuilder -> llvalue = fn If => fn Then => fn Else => fn B => F_llvm_build_cond_br.f (If, Then, Else, B)
val build_switch : llvalue -> llbasicblock -> int -> llbuilder -> llvalue = fn Of => fn Else => fn EstimatedCount => fn B => F_llvm_build_switch.f (Of, Else, Int32.fromInt EstimatedCount, B)
val build_malloc : lltype -> string -> llbuilder -> llvalue =
 fn Ty => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_malloc.f (Ty, Name', B)
        before
        C.free Name'
    end
val build_array_malloc : lltype -> llvalue -> string -> llbuilder -> llvalue =
 fn Ty => fn Val => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_array_malloc.f (Ty, Val, Name', B)
        before
        C.free Name'
    end
val build_free : llvalue -> llbuilder -> llvalue = fn P => fn B => F_llvm_build_free.f (P, B)
val add_case : llvalue -> llvalue -> llbasicblock -> unit = fn Switch => fn OnVal => fn Dest => F_llvm_add_case.f (Switch, OnVal, Dest)
val switch_default_dest : llvalue -> llbasicblock = fn Inst => F_llvm_switch_default_dest.f Inst
val build_indirect_br : llvalue -> int -> llbuilder -> llvalue = fn Addr => fn EstimatedDests => fn B => F_llvm_build_indirect_br.f (Addr, Int32.fromInt EstimatedDests, B)
val add_destination : llvalue -> llbasicblock -> unit = fn IndirectBr => fn Dest => F_llvm_add_destination.f (IndirectBr, Dest)
val build_invoke : llvalue -> llvalue array -> llbasicblock -> llbasicblock -> string -> llbuilder -> llvalue =
 fn Fn => fn Args => fn Then => fn Catch => fn Name => fn B =>
    let
        val Args' = dupVPtrArr Args
        val Name' = ZString.dupML Name
    in
        F_llvm_build_invoke.f (Fn, Args', Int32.fromInt $ Array.length Args, Then, Catch, Name', B)
        before
        (C.free Args'; C.free Name')
    end
val build_landingpad : lltype -> llvalue -> int -> string -> llbuilder -> llvalue =
 fn Ty => fn PersFn => fn NumClauses => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_landingpad.f (Ty, PersFn, Int32.fromInt NumClauses, Name', B)
        before
        C.free Name'
    end
val set_cleanup : llvalue -> bool -> unit = fn LandingPadInst => fn Flag => F_llvm_set_cleanup.f (LandingPadInst, if Flag then 1 else 0)
val add_clause : llvalue -> llvalue -> unit = fn LandingPadInst => fn ClauseVal => F_llvm_add_clause.f (LandingPadInst, ClauseVal)
val build_resume : llvalue -> llbuilder -> llvalue = fn Exn => fn B => F_llvm_build_resume.f (Exn, B)
val build_unreachable : llbuilder -> llvalue = fn B => F_llvm_build_unreachable.f B

(*--... Arithmetic .........................................................--*)
val build_add : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_add.f (LHS, RHS, Name', B)
        before
        C.free Name'
    end
val build_nsw_add : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_nsw_add.f (LHS, RHS, Name', B)
        before
        C.free Name'
    end
val build_nuw_add : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_nuw_add.f (LHS, RHS, Name', B)
        before
        C.free Name'
    end
val build_fadd : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_fadd.f (LHS, RHS, Name', B)
        before
        C.free Name'
    end
val build_sub : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_sub.f (LHS, RHS, Name', B)
        before
        C.free Name'
    end
val build_nsw_sub : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_nsw_sub.f (LHS, RHS, Name', B)
        before
        C.free Name'
    end
val build_nuw_sub : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_nuw_sub.f (LHS, RHS, Name', B)
        before
        C.free Name'
    end
val build_fsub : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_fsub.f (LHS, RHS, Name', B)
        before
        C.free Name'
    end
val build_mul : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_mul.f (LHS, RHS, Name', B)
        before
        C.free Name'
    end
val build_nsw_mul : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_nsw_mul.f (LHS, RHS, Name', B)
        before
        C.free Name'
    end
val build_nuw_mul : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_nuw_mul.f (LHS, RHS, Name', B)
        before
        C.free Name'
    end
val build_fmul : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_fmul.f (LHS, RHS, Name', B)
        before
        C.free Name'
    end
val build_udiv : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_udiv.f (LHS, RHS, Name', B)
        before
        C.free Name'
    end
val build_sdiv : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_sdiv.f (LHS, RHS, Name', B)
        before
        C.free Name'
    end
val build_exact_sdiv : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_exact_sdiv.f (LHS, RHS, Name', B)
        before
        C.free Name'
    end
val build_fdiv : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_fdiv.f (LHS, RHS, Name', B)
        before
        C.free Name'
    end
val build_urem : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_urem.f (LHS, RHS, Name', B)
        before
        C.free Name'
    end
val build_srem : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_srem.f (LHS, RHS, Name', B)
        before
        C.free Name'
    end
val build_frem : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_frem.f (LHS, RHS, Name', B)
        before
        C.free Name'
    end
val build_shl : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_shl.f (LHS, RHS, Name', B)
        before
        C.free Name'
    end
val build_lshr : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_lshr.f (LHS, RHS, Name', B)
        before
        C.free Name'
    end
val build_ashr : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_ashr.f (LHS, RHS, Name', B)
        before
        C.free Name'
    end
val build_and : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_and.f (LHS, RHS, Name', B)
        before
        C.free Name'
    end
val build_or : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_or.f (LHS, RHS, Name', B)
        before
        C.free Name'
    end
val build_xor : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_xor.f (LHS, RHS, Name', B)
        before
        C.free Name'
    end
val build_neg : llvalue -> string -> llbuilder -> llvalue =
 fn X => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_neg.f (X, Name', B)
        before
        C.free Name'
    end
val build_nsw_neg : llvalue -> string -> llbuilder -> llvalue =
 fn X => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_nsw_neg.f (X, Name', B)
        before
        C.free Name'
    end
val build_nuw_neg : llvalue -> string -> llbuilder -> llvalue =
 fn X => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_nuw_neg.f (X, Name', B)
        before
        C.free Name'
    end
val build_fneg : llvalue -> string -> llbuilder -> llvalue =
 fn X => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_fneg.f (X, Name', B)
        before
        C.free Name'
    end
val build_not : llvalue -> string -> llbuilder -> llvalue =
 fn X => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_not.f (X, Name', B)
        before
        C.free Name'
    end

(*--... Memory .............................................................--*)
val build_alloca : lltype -> string -> llbuilder -> llvalue =
 fn Ty => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_alloca.f (Ty, Name', B)
        before
        C.free Name'
    end
val build_array_alloca : lltype -> llvalue -> string -> llbuilder -> llvalue =
 fn Ty => fn Sz => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_array_alloca.f (Ty, Sz, Name', B)
        before
        C.free Name'
    end
val build_load : llvalue -> string -> llbuilder -> llvalue =
 fn Ptr => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_load.f (Ptr, Name', B)
        before
        C.free Name'
    end
val build_store : llvalue -> llvalue -> llbuilder -> llvalue = fn Val => fn Ptr => fn B => F_llvm_build_store.f (Val, Ptr, B)
val build_atomicrmw : AtomicRMWBinOp.t -> llvalue -> llvalue -> AtomicOrdering.t -> bool -> string -> llbuilder -> llvalue =
 fn BinOp => fn Ptr => fn Val => fn Ord => fn ST => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_atomicrmw.f (Int32.fromInt $ AtomicRMWBinOp.toInt BinOp, Ptr, Val, Int32.fromInt $ AtomicOrdering.toInt Ord, if ST then 1 else 0, Name', B)
        before
        C.free Name'
    end
val build_gep : llvalue -> llvalue array -> string -> llbuilder -> llvalue =
 fn Ptr => fn Indices => fn Name => fn B =>
    let
        val Indices' = dupVPtrArr Indices
        val Name' = ZString.dupML Name
    in
        F_llvm_build_gep.f (Ptr, Indices', Int32.fromInt $ Array.length Indices, Name', B)
        before
        (C.free Indices'; C.free Name')
    end
val build_in_bounds_gep : llvalue -> llvalue array -> string -> llbuilder -> llvalue =
 fn Ptr => fn Indices => fn Name => fn B =>
    let
        val Indices' = dupVPtrArr Indices
        val Name' = ZString.dupML Name
    in
        F_llvm_build_in_bounds_gep.f (Ptr, Indices', Int32.fromInt $ Array.length Indices, Name', B)
        before
        (C.free Indices'; C.free Name')
    end
val build_struct_gep : llvalue -> int -> string -> llbuilder -> llvalue =
 fn Ptr => fn Index => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_struct_gep.f (Ptr, Int32.fromInt Index, Name', B)
        before
        C.free Name'
    end

val build_global_string : string -> string -> llbuilder -> llvalue =
 fn Str => fn Name => fn B =>
    let
        val Str' = ZString.dupML Str
        val Name' = ZString.dupML Name
    in
        F_llvm_build_global_string.f (Str', Name', B)
        before
        (C.free Str'; C.free Name')
    end
val build_global_stringptr : string -> string -> llbuilder -> llvalue =
 fn Str => fn Name => fn B =>
    let
        val Str' = ZString.dupML Str
        val Name' = ZString.dupML Name
    in
        F_llvm_build_global_stringptr.f (Str', Name', B)
        before
        (C.free Str'; C.free Name')
    end

(*--... Casts ..............................................................--*)
val build_trunc : llvalue -> lltype -> string -> llbuilder -> llvalue =
 fn X => fn Ty => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_trunc.f (X, Ty, Name', B)
        before
        C.free Name'
    end
val build_zext : llvalue -> lltype -> string -> llbuilder -> llvalue =
 fn X => fn Ty => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_zext.f (X, Ty, Name', B)
        before
        C.free Name'
    end
val build_sext : llvalue -> lltype -> string -> llbuilder -> llvalue =
 fn X => fn Ty => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_sext.f (X, Ty, Name', B)
        before
        C.free Name'
    end
val build_fptoui : llvalue -> lltype -> string -> llbuilder -> llvalue =
 fn X => fn Ty => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_fptoui.f (X, Ty, Name', B)
        before
        C.free Name'
    end
val build_fptosi : llvalue -> lltype -> string -> llbuilder -> llvalue =
 fn X => fn Ty => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_fptosi.f (X, Ty, Name', B)
        before
        C.free Name'
    end
val build_uitofp : llvalue -> lltype -> string -> llbuilder -> llvalue =
 fn X => fn Ty => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_uitofp.f (X, Ty, Name', B)
        before
        C.free Name'
    end
val build_sitofp : llvalue -> lltype -> string -> llbuilder -> llvalue =
 fn X => fn Ty => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_sitofp.f (X, Ty, Name', B)
        before
        C.free Name'
    end
val build_fptrunc : llvalue -> lltype -> string -> llbuilder -> llvalue =
 fn X => fn Ty => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_fptrunc.f (X, Ty, Name', B)
        before
        C.free Name'
    end
val build_fpext : llvalue -> lltype -> string -> llbuilder -> llvalue =
 fn X => fn Ty => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_fpext.f (X, Ty, Name', B)
        before
        C.free Name'
    end
val build_ptrtoint : llvalue -> lltype -> string -> llbuilder -> llvalue =
 fn X => fn Ty => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_ptrtoint.f (X, Ty, Name', B)
        before
        C.free Name'
    end
val build_inttoptr : llvalue -> lltype -> string -> llbuilder -> llvalue =
 fn X => fn Ty => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_inttoptr.f (X, Ty, Name', B)
        before
        C.free Name'
    end
val build_bitcast : llvalue -> lltype -> string -> llbuilder -> llvalue =
 fn X => fn Ty => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_bitcast.f (X, Ty, Name', B)
        before
        C.free Name'
    end
val build_zext_or_bitcast : llvalue -> lltype -> string -> llbuilder -> llvalue =
 fn X => fn Ty => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_zext_or_bitcast.f (X, Ty, Name', B)
        before
        C.free Name'
    end
val build_sext_or_bitcast : llvalue -> lltype -> string -> llbuilder -> llvalue =
 fn X => fn Ty => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_sext_or_bitcast.f (X, Ty, Name', B)
        before
        C.free Name'
    end
val build_trunc_or_bitcast : llvalue -> lltype -> string -> llbuilder -> llvalue =
 fn X => fn Ty => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_trunc_or_bitcast.f (X, Ty, Name', B)
        before
        C.free Name'
    end
val build_pointercast : llvalue -> lltype -> string -> llbuilder -> llvalue =
 fn X => fn Ty => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_pointercast.f (X, Ty, Name', B)
        before
        C.free Name'
    end
val build_intcast : llvalue -> lltype -> string -> llbuilder -> llvalue =
 fn X => fn Ty => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_intcast.f (X, Ty, Name', B)
        before
        C.free Name'
    end
val build_fpcast : llvalue -> lltype -> string -> llbuilder -> llvalue =
 fn X => fn Ty => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_fpcast.f (X, Ty, Name', B)
        before
        C.free Name'
    end

(*--... Comparisons ........................................................--*)
val build_icmp : Icmp.t -> llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn Pred => fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_icmp.f (Int32.fromInt $ Icmp.toInt Pred, LHS, RHS, Name', B)
        before
        C.free Name'
    end
val build_fcmp : Fcmp.t -> llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn Pred => fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_fcmp.f (Int32.fromInt $ Fcmp.toInt Pred, LHS, RHS, Name', B)
        before
        C.free Name'
    end

(*--... Miscellaneous instructions .........................................--*)
val build_phi : (llvalue * llbasicblock) list -> string -> llbuilder -> llvalue =
 fn Incoming => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
        val (Vals, BBs) = ListPair.unzip Incoming
        val Vals' = dupVPtrArr $ Array.fromList Vals
        val BBs' = dupVPtrArr $ Array.fromList BBs
    in
        F_llvm_build_phi.f (Vals', BBs', Int32.fromInt $ List.length Incoming, Name', B)
        before
        (C.free Name'; C.free Vals'; C.free BBs')
    end
val build_empty_phi : lltype -> string -> llbuilder -> llvalue =
 fn Ty => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_empty_phi.f (Ty, Name', B)
        before
        C.free Name'
    end
val build_call : llvalue -> llvalue array -> string -> llbuilder -> llvalue =
 fn Fn => fn Params => fn Name => fn B =>
    let
        val Params' = dupVPtrArr Params
        val Name' = ZString.dupML Name
    in
        F_llvm_build_call.f (Fn, Params', Int32.fromInt $ Array.length Params, Name', B)
        before
        (C.free Params'; C.free Name')
    end
val build_select : llvalue -> llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn If => fn Then => fn Else => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_select.f (If, Then, Else, Name', B)
        before
        C.free Name'
    end
val build_va_arg : llvalue -> lltype -> string -> llbuilder -> llvalue =
 fn List => fn Ty => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_va_arg.f (List, Ty, Name', B)
        before
        C.free Name'
    end
val build_extractelement : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn Vec => fn Idx => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_extractelement.f (Vec, Idx, Name', B)
        before
        C.free Name'
    end
val build_insertelement : llvalue -> llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn Vec => fn Elem => fn Idx => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_insertelement.f (Vec, Elem, Idx, Name', B)
        before
        C.free Name'
    end
val build_shufflevector : llvalue -> llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn V1 => fn V2 => fn Mask => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_shufflevector.f (V1, V2, Mask, Name', B)
        before
        C.free Name'
    end
val build_extractvalue : llvalue -> int -> string -> llbuilder -> llvalue =
 fn Aggregate => fn Idx => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_extractvalue.f (Aggregate, Int32.fromInt Idx, Name', B)
        before
        C.free Name'
    end
val build_insertvalue : llvalue -> llvalue -> int -> string -> llbuilder -> llvalue =
 fn Aggregate => fn Val => fn Idx => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_insertvalue.f (Aggregate, Val, Int32.fromInt Idx, Name', B)
        before
        C.free Name'
    end
val build_is_null : llvalue -> string -> llbuilder -> llvalue =
 fn Val => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_is_null.f (Val, Name', B)
        before
        C.free Name'
    end
val build_is_not_null : llvalue -> string -> llbuilder -> llvalue =
 fn Val => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_is_not_null.f (Val, Name', B)
        before
        C.free Name'
    end
val build_ptrdiff : llvalue -> llvalue -> string -> llbuilder -> llvalue =
 fn LHS => fn RHS => fn Name => fn B =>
    let
        val Name' = ZString.dupML Name
    in
        F_llvm_build_ptrdiff.f (LHS, RHS, Name', B)
        before
        C.free Name'
    end

(*===-- Memory buffers ----------------------------------------------------===*)

structure MemoryBuffer =
struct
fun of_file (Path : string) : llmemorybuffer =
  let
      val Path' = ZString.dupML Path
  in
      F_llvm_memorybuffer_of_file.f (Path')
      before
      C.free Path'
  end
fun of_stdin () : llmemorybuffer = F_llvm_memorybuffer_of_stdin.f ()
fun of_string (Name : string option) (Str : string) : llmemorybuffer =
  let
      val Str' = ZString.dupML Str
  in
      (case Name of
           NONE => F_llvm_memorybuffer_of_string.f (C.Ptr.null $ C.T.ro $ C.T.pointer C.T.uchar, Str')
         | SOME Name =>
           let
               val Name' = ZString.dupML Name
           in
               F_llvm_memorybuffer_of_string.f (Name', Str') before C.free Name'
           end)
      before
      C.free Str'
  end
fun as_string (MemBuf : llmemorybuffer) : string =
  let
      val S = F_llvm_memorybuffer_as_string.f MemBuf
  in
      ZString.toML S
      before
      C.free S
  end
fun dispose (MemBuf : llmemorybuffer) : unit = F_llvm_memorybuffer_dispose.f MemBuf
end

(*===-- Pass Manager ------------------------------------------------------===*)

structure PassManager =
struct
type Module = unit
type Function = unit
type 'a t = C.voidptr
fun create () : Module t = F_llvm_passmanager_create.f ()
fun create_function (M : llmodule) : Function t = F_llvm_passmanager_create_function.f M
fun run_module (M : llmodule) (PM : Module t) : bool =
  case F_llvm_passmanager_run_module.f (M, PM) of
      0 => false
    | 1 => true
    | _ => raise (Fail "PassManager.run_module")
fun initialize (FPM : Function t) : bool =
  case F_llvm_passmanager_initialize.f FPM of
      0 => false
    | 1 => true
    | _ => raise (Fail "PassManager.initialize")
fun run_function (F : llvalue) (FPM : Function t) : bool =
  case F_llvm_passmanager_run_function.f (F, FPM) of
      0 => false
    | 1 => true
    | _ => raise (Fail "PassManager.run_function")
fun finalize (FPM : Function t) : bool =
  case F_llvm_passmanager_finalize.f FPM of
      0 => false
    | 1 => true
    | _ => raise (Fail "PassManager.finalize")
fun dispose (PM : 'a t) : unit = F_llvm_passmanager_dispose.f PM
end

end
