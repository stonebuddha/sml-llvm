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
      val len = C.new C.T.sint
      val buf = F_llvm_param_types.f (FunctionTy, C.Ptr.|&| len)
  in
      toVPtrArr buf len
      before
      (C.free buf; C.free (C.Ptr.|&| len))
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
      val len = C.new C.T.sint
      val buf = F_llvm_struct_element_types.f (StructTy, C.Ptr.|&| len)
  in
      toVPtrArr buf len
      before
      (C.free buf; C.free (C.Ptr.|&| len))
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

(* (*--... Operations on pointer, vector, and array types .....................--*) *)
(* val array_type = _import "llvm_array_type" : lltype * int -> lltype; *)
(* val pointer_type = _import "llvm_pointer_type" : lltype -> lltype; *)
(* val qualified_pointer_type = _import "llvm_qualified_pointer_type" : lltype * int -> lltype; *)
(* val vector_type = _import "llvm_vector_type" : lltype * int -> lltype; *)

(* val element_type = _import "LLVMGetElementType" : lltype -> lltype; *)
(* val array_length = _import "llvm_array_length" : lltype -> int; *)
(* val address_space = _import "llvm_address_space" : lltype -> int; *)
(* val vector_size = _import "llvm_vector_size" : lltype -> int; *)

(* (*--... Operations on other types ..........................................--*) *)
(* val void_type = _import "llvm_void_type" : llcontext -> lltype; *)
(* val label_type = _import "llvm_label_type" : llcontext -> lltype; *)
(* val x86_mmx_type = _import "llvm_x86_mmx_type" : llcontext -> lltype; *)
(* val type_by_name = *)
(*     let *)
(*         val f = _import "llvm_type_by_name" : llmodule * string * bool ref -> lltype; *)
(*     in *)
(*         fn M => fn Name => *)
(*            let *)
(*                val is_null = ref false *)
(*                val res = f (M, Name, is_null) *)
(*            in *)
(*                if !is_null then NONE else SOME res *)
(*            end *)
(*     end *)

(* (*===-- Values ------------------------------------------------------------===*) *)
(* val classify_value = *)
(*     let *)
(*         val f = _import "llvm_classify_value" : llvalue -> int; *)
(*     in *)
(*         fn Val => ValueKind.fromInt (f Val) *)
(*     end *)
(* val type_of = _import "llvm_type_of" : llvalue -> lltype; *)
(* val value_name = _import "llvm_value_name" : llvalue -> string; *)
(* val set_value_name = _import "llvm_set_value_name" : string * llvalue -> unit; *)
(* val dump_value = _import "llvm_dump_value" : llvalue -> unit; *)
(* val string_of_llvalue = _import "llvm_string_of_llvalue" : llvalue -> string; *)
(* val replace_all_uses_with = _import "llvm_replace_all_uses_with" : llvalue * llvalue -> unit; *)

(* (*--... Operations on uses .................................................--*) *)
(* val use_begin = *)
(*     let *)
(*         val f = _import "llvm_use_begin" : llvalue * bool ref -> lluse; *)
(*     in *)
(*         fn Val => *)
(*            let *)
(*                val is_null = ref false *)
(*                val res = f (Val, is_null) *)
(*            in *)
(*                if !is_null then NONE else SOME res *)
(*            end *)
(*     end *)
(* val use_succ = *)
(*     let *)
(*         val f = _import "llvm_use_succ" : lluse * bool ref -> lluse; *)
(*     in *)
(*         fn U => *)
(*            let *)
(*                val is_null = ref false *)
(*                val res = f (U, is_null) *)
(*            in *)
(*                if !is_null then NONE else SOME res *)
(*            end *)
(*     end *)
(* val user = _import "llvm_user" : lluse -> llvalue; *)
(* val used_value = _import "llvm_used_value" : lluse -> llvalue; *)

(* fun iter_uses f v = *)
(*   let *)
(*       fun aux NONE = () *)
(*         | aux (SOME u) = (f u; aux (use_succ u)) *)
(*   in *)
(*       aux (use_begin v) *)
(*   end *)

(* fun fold_left_uses f init v = *)
(*   let *)
(*       fun aux init NONE = init *)
(*         | aux init (SOME u) = aux (f init u) (use_succ u) *)
(*   in *)
(*       aux init (use_begin v) *)
(*   end *)

(* fun fold_right_uses f v init = *)
(*   let *)
(*       fun aux NONE init = init *)
(*         | aux (SOME u) init = f u (aux (use_succ u) init) *)
(*   in *)
(*       aux (use_begin v) init *)
(*   end *)

(* (*--... Operations on users ................................................--*) *)
(* val operand = _import "llvm_operand" : llvalue * int -> llvalue; *)
(* val operand_use = _import "llvm_operand_use" : llvalue * int -> lluse; *)
(* val set_operand = _import "llvm_set_operand" : llvalue * int * llvalue -> unit; *)
(* val num_operands = _import "llvm_num_operands" : llvalue -> int; *)

(* (*--... Operations on constants of (mostly) any type .......................--*) *)
(* val is_constant = _import "llvm_is_constant" : llvalue -> bool; *)
(* val const_null = _import "LLVMConstNull" : lltype -> llvalue; *)
(* val const_all_ones = _import "LLVMConstAllOnes" : lltype -> llvalue; *)
(* val const_pointer_null = _import "LLVMConstPointerNull" : lltype -> llvalue; *)
(* val undef = _import "LLVMGetUndef" : lltype -> llvalue; *)
(* val is_null = _import "llvm_is_null" : llvalue -> bool; *)
(* val is_undef = _import "llvm_is_undef" : llvalue -> bool; *)
(* val constexpr_opcode = *)
(*     let *)
(*         val f = _import "llvm_constexpr_get_opcode" : llvalue -> int; *)
(*     in *)
(*         fn Val => Opcode.fromInt (f Val) *)
(*     end *)

(* (*--... Operations on instructions .........................................--*) *)
(* val has_metadata = _import "llvm_has_metadata" : llvalue -> bool; *)
(* val metadata = *)
(*     let *)
(*         val f = _import "llvm_metadata" : llvalue * llmdkind * bool ref -> llvalue; *)
(*     in *)
(*         fn Val => fn MDKindId => *)
(*            let *)
(*                val is_null = ref false *)
(*                val res = f (Val, MDKindId, is_null) *)
(*            in *)
(*                if !is_null then NONE else SOME res *)
(*            end *)
(*     end *)
(* val set_metadata = _import "llvm_set_metadata" : llvalue * llmdkind * llvalue -> unit; *)
(* val clear_metadata = _import "llvm_clear_metadata" : llvalue * llmdkind -> unit; *)

(* (*--... Operations on metadata .......,.....................................--*) *)
(* val mdstring = _import "llvm_mdstring" : llcontext * string -> llvalue; *)
(* val mdnode = *)
(*     let *)
(*         val f = _import "llvm_mdnode" : llcontext * llvalue array * int -> llvalue; *)
(*     in *)
(*         fn C => fn ElemVals => f (C, ElemVals, Array.length ElemVals) *)
(*     end *)
(* val mdnull = _import "llvm_mdnull" : llcontext -> llvalue; *)
(* val get_mdstring = *)
(*     let *)
(*         val f = _import "llvm_get_mdstring" : llvalue * bool ref -> string; *)
(*     in *)
(*         fn Val => *)
(*            let *)
(*                val is_null = ref false *)
(*                val res = f (Val, is_null) *)
(*            in *)
(*                if !is_null then NONE else SOME res *)
(*            end *)
(*     end *)
(* val get_mdnode_operands = _import "llvm_get_mdnode_operands" : llvalue -> llvalue array; *)
(* val get_named_metadata = _import "llvm_get_namedmd" : llmodule * string -> llvalue array; *)
(* val add_named_metadata_operand = _import "llvm_append_namedmd" : llmodule * string * llvalue -> unit; *)

(* (*--... Operations on scalar constants .....................................--*) *)
(* val const_int = _import "llvm_const_int" : lltype * int -> llvalue; *)
(* val const_of_int64 = _import "llvm_const_of_int64" : lltype * Int64.int * bool -> llvalue; *)
(* val int64_of_const = *)
(*     let *)
(*         val f = _import "llvm_int64_of_const" : llvalue * bool ref -> Int64.int; *)
(*     in *)
(*         fn Val => *)
(*            let *)
(*                val is_null = ref false *)
(*                val res = f (Val, is_null) *)
(*            in *)
(*                if !is_null then NONE else SOME res *)
(*            end *)
(*     end *)
(* val const_int_of_string = _import "llvm_const_int_of_string" : lltype * string * int -> llvalue; *)
(* val const_float = _import "llvm_const_float" : lltype * real -> llvalue; *)
(* val float_of_const = *)
(*     let *)
(*         val f = _import "llvm_float_of_const" : llvalue * bool ref -> real; *)
(*     in *)
(*         fn Val => *)
(*            let *)
(*                val is_null = ref false *)
(*                val res = f (Val, is_null) *)
(*            in *)
(*                if !is_null then NONE else SOME res *)
(*            end *)
(*     end *)
(* val const_float_of_string = _import "llvm_const_float_of_string" : lltype * string -> llvalue; *)

(* (*--... Operations on composite constants ..................................--*) *)
(* val const_string = _import "llvm_const_string" : llcontext * string -> llvalue; *)
(* val const_stringz = _import "llvm_const_stringz" : llcontext * string -> llvalue; *)
(* val const_array = *)
(*     let *)
(*         val f = _import "llvm_const_array" : lltype * llvalue array * int -> llvalue; *)
(*     in *)
(*         fn C => fn ElemVals => f (C, ElemVals, Array.length ElemVals) *)
(*     end *)
(* val const_struct = *)
(*     let *)
(*         val f = _import "llvm_const_struct" : llcontext * llvalue array * int -> llvalue; *)
(*     in *)
(*         fn C => fn ElemVals => f (C, ElemVals, Array.length ElemVals) *)
(*     end *)
(* val const_named_struct = *)
(*     let *)
(*         val f = _import "llvm_const_named_struct" : lltype * llvalue array * int -> llvalue; *)
(*     in *)
(*         fn Ty => fn ElemVals => f (Ty, ElemVals, Array.length ElemVals) *)
(*     end *)
(* val const_packed_struct = *)
(*     let *)
(*         val f = _import "llvm_packed_struct" : llcontext * llvalue array * int -> llvalue; *)
(*     in *)
(*         fn C => fn ElemVals => f (C, ElemVals, Array.length ElemVals) *)
(*     end *)
(* val const_vector = *)
(*     let *)
(*         val f = _import "llvm_const_vector" : llvalue array * int -> llvalue; *)
(*     in *)
(*         fn ElemVals => f (ElemVals, Array.length ElemVals) *)
(*     end *)
(* val string_of_const = *)
(*     let *)
(*         val f = _import "llvm_string_of_const" : llvalue * bool ref -> string; *)
(*     in *)
(*         fn Val => *)
(*            let *)
(*                val is_null = ref false *)
(*                val res = f (Val, is_null) *)
(*            in *)
(*                if !is_null then NONE else SOME res *)
(*            end *)
(*     end *)
(* val const_element = _import "llvm_const_element" : llvalue * int -> llvalue; *)

(* (*--... Constant expressions ...............................................--*) *)

(* (*--... Operations on global variables, functions, and aliases (globals) ...--*) *)

(* (*--... Operations on global variables .....................................--*) *)

(* (*--... Operations on aliases ..............................................--*) *)

(* (*--... Operations on functions ............................................--*) *)

(* (*--... Operations on params ...............................................--*) *)

(* (*--... Operations on basic blocks .........................................--*) *)

(* (*--... Operations on instructions .........................................--*) *)

(* (*--... Operations on call sites ...........................................--*) *)

(* (*--... Operations on call instructions (only) .............................--*) *)

(* (*--... Operations on load/store instructions (only) .......................--*) *)

(* (*--... Operations on terminators ..........................................--*) *)

(* (*--... Operations on branches .............................................--*) *)

(* (*--... Operations on phi nodes ............................................--*) *)

(* (*===-- Instruction builders ----------------------------------------------===*) *)

(* (*--... Metadata ...........................................................--*) *)

(* (*--... Terminators ........................................................--*) *)

(* (*--... Arithmetic .........................................................--*) *)

(* (*--... Memory .............................................................--*) *)

(* (*--... Casts ..............................................................--*) *)

(* (*--... Comparisons ........................................................--*) *)

(* (*--... Miscellaneous instructions .........................................--*) *)

(* (*===-- Memory buffers ----------------------------------------------------===*) *)

(* structure MemoryBuffer = *)
(* struct *)
(* end *)

(* (*===-- Pass Manager ------------------------------------------------------===*) *)

(* structure PassManager = *)
(* struct *)
(* end *)

end
