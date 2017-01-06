structure LlvmCore :> LLVM_CORE =
struct

type llcontext = MLton.Pointer.t
type llmodule = MLton.Pointer.t
type lltype = MLton.Pointer.t
type llvalue = MLton.Pointer.t
type lluse = MLton.Pointer.t
type llbasicblock = MLton.Pointer.t
type llbuilder = MLton.Pointer.t
type llmemorybuffer = MLton.Pointer.t
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
val create_context = _import "llvm_create_context" : unit -> llcontext;
val dispose_context = _import "llvm_dispose_context" : llcontext -> unit;
val global_context = _import "llvm_global_context" : unit -> llcontext;
val mdkind_id = _import "llvm_mdkind_id" : llcontext * string -> llmdkind;

(*===-- Modules -----------------------------------------------------------===*)
val create_module = _import "llvm_create_module" : llcontext * string -> llmodule;
val dispose_module = _import "llvm_dispose_module" : llmodule -> unit;
val target_triple = _import "llvm_target_triple" : llmodule -> string;
val set_target_triple = _import "llvm_set_target_triple" : string * llmodule -> unit;
val data_layout = _import "llvm_data_layout" : llmodule -> string;
val set_data_layout = _import "llvm_set_data_layout" : string * llmodule -> unit;
val dump_module = _import "llvm_dump_module" : llmodule -> unit;
val print_module = _import "llvm_print_module" : string * llmodule -> unit;
val string_of_llmodule = _import "llvm_string_of_llmodule" : llmodule -> string;
val set_module_inline_asm = _import "llvm_set_module_inline_asm" : llmodule * string -> unit;
val module_context = _import "LLVMGetModuleContext" : llmodule -> llcontext;

(*===-- Types -------------------------------------------------------------===*)
val classify_type =
    let
        val f = _import "llvm_classify_type" : lltype -> int;
    in
        fn Ty => TypeKind.fromInt (f Ty)
    end
val type_context = _import "llvm_type_context" : lltype -> llcontext;
val type_is_sized = _import "llvm_type_is_sized" : lltype -> bool;
val dump_type = _import "llvm_dump_type" : lltype -> unit;
val string_of_lltype = _import "llvm_string_of_lltype" : lltype -> string;

(*--... Operations on integer types ........................................--*)
val i1_type = _import "llvm_i1_type" : llcontext -> lltype;
val i8_type = _import "llvm_i8_type" : llcontext -> lltype;
val i16_type = _import "llvm_i16_type" : llcontext -> lltype;
val i32_type = _import "llvm_i32_type" : llcontext -> lltype;
val i64_type = _import "llvm_i64_type" : llcontext -> lltype;

val integer_type = _import "llvm_integer_type" : llcontext * int -> lltype;
val integer_bitwidth = _import "llvm_integer_bitwidth" : lltype -> int;

(*--... Operations on real types ...........................................--*)
val float_type = _import "llvm_float_type" : llcontext -> lltype;
val double_type = _import "llvm_double_type" : llcontext -> lltype;
val x86fp80_type = _import "llvm_x86fp80_type" : llcontext -> lltype;
val fp128_type = _import "llvm_fp128_type" : llcontext -> lltype;
val ppc_fp128_type = _import "llvm_ppc_fp128_type" : llcontext -> lltype;

(*--... Operations on function types .......................................--*)
val function_type =
    let
        val f = _import "llvm_function_type" : lltype * lltype array * int -> lltype;
    in
        fn RetTy => fn ParamTys => f (RetTy, ParamTys, Array.length ParamTys)
    end
val var_arg_function_type =
    let
        val f = _import "llvm_var_arg_function_type" : lltype * lltype array * int -> lltype;
    in
        fn RetTy => fn ParamTys => f (RetTy, ParamTys, Array.length ParamTys)
    end

val is_var_arg = _import "llvm_is_var_arg" : lltype -> bool;
val return_type = _import "LLVMGetReturnType" : lltype -> lltype;
val param_types = _import "llvm_param_types" : lltype -> lltype array;

(*--... Operations on struct types .........................................--*)
val struct_type =
    let
        val f = _import "llvm_struct_type" : llcontext * lltype array * int -> lltype;
    in
        fn C => fn ElemTys => f (C, ElemTys, Array.length ElemTys)
    end
val packed_struct_type =
    let
        val f = _import "llvm_packed_struct_type" : llcontext * lltype array * int -> lltype;
    in
        fn C => fn ElemTys => f (C, ElemTys, Array.length ElemTys)
    end
val struct_name =
    let
        val f = _import "llvm_struct_name" : lltype * bool ref -> string;
    in
        fn Ty =>
           let
               val is_null = ref false
               val res = f (Ty, is_null)
           in
               if !is_null then NONE else SOME res
           end
    end
val named_struct_type = _import "llvm_named_struct_type" : llcontext * string -> lltype;
val struct_set_body =
    let
        val f = _import "llvm_struct_set_body" : lltype * lltype array * int * bool -> unit;
    in
        fn Ty => fn ElemTys => fn Packed => f (Ty, ElemTys, Array.length ElemTys, Packed)
    end
val struct_element_types = _import "llvm_struct_element_types" : lltype -> lltype array;
val is_packed = _import "llvm_is_packed" : lltype -> bool;
val is_opaque = _import "llvm_is_opaque" : lltype -> bool;

(*--... Operations on pointer, vector, and array types .....................--*)
val array_type = _import "llvm_array_type" : lltype * int -> lltype;
val pointer_type = _import "llvm_pointer_type" : lltype -> lltype;
val qualified_pointer_type = _import "llvm_qualified_pointer_type" : lltype * int -> lltype;
val vector_type = _import "llvm_vector_type" : lltype * int -> lltype;

val element_type = _import "LLVMGetElementType" : lltype -> lltype;
val array_length = _import "llvm_array_length" : lltype -> int;
val address_space = _import "llvm_address_space" : lltype -> int;
val vector_size = _import "llvm_vector_size" : lltype -> int;

(*--... Operations on other types ..........................................--*)
val void_type = _import "llvm_void_type" : llcontext -> lltype;
val label_type = _import "llvm_label_type" : llcontext -> lltype;
val x86_mmx_type = _import "llvm_x86_mmx_type" : llcontext -> lltype;
val type_by_name =
    let
        val f = _import "llvm_type_by_name" : llmodule * string * bool ref -> lltype;
    in
        fn M => fn Name =>
           let
               val is_null = ref false
               val res = f (M, Name, is_null)
           in
               if !is_null then NONE else SOME res
           end
    end

(*===-- Values ------------------------------------------------------------===*)
val classify_value =
    let
        val f = _import "llvm_classify_value" : llvalue -> int;
    in
        fn Val => ValueKind.fromInt (f Val)
    end
val type_of = _import "llvm_type_of" : llvalue -> lltype;
val value_name = _import "llvm_value_name" : llvalue -> string;
val set_value_name = _import "llvm_set_value_name" : string * llvalue -> unit;
val dump_value = _import "llvm_dump_value" : llvalue -> unit;
val string_of_llvalue = _import "llvm_string_of_llvalue" : llvalue -> string;
val replace_all_uses_with = _import "llvm_replace_all_uses_with" : llvalue * llvalue -> unit;

(*--... Operations on uses .................................................--*)
val use_begin =
    let
        val f = _import "llvm_use_begin" : llvalue * bool ref -> lluse;
    in
        fn Val =>
           let
               val is_null = ref false
               val res = f (Val, is_null)
           in
               if !is_null then NONE else SOME res
           end
    end
val use_succ =
    let
        val f = _import "llvm_use_succ" : lluse * bool ref -> lluse;
    in
        fn U =>
           let
               val is_null = ref false
               val res = f (U, is_null)
           in
               if !is_null then NONE else SOME res
           end
    end
val user = _import "llvm_user" : lluse -> llvalue;
val used_value = _import "llvm_used_value" : lluse -> llvalue;

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
val operand = _import "llvm_operand" : llvalue * int -> llvalue;
val operand_use = _import "llvm_operand_use" : llvalue * int -> lluse;
val set_operand = _import "llvm_set_operand" : llvalue * int * llvalue -> unit;
val num_operands = _import "llvm_num_operands" : llvalue -> int;

(*--... Operations on constants of (mostly) any type .......................--*)
val is_constant = _import "llvm_is_constant" : llvalue -> bool;
val const_null = _import "LLVMConstNull" : lltype -> llvalue;
val const_all_ones = _import "LLVMConstAllOnes" : lltype -> llvalue;
val const_pointer_null = _import "LLVMConstPointerNull" : lltype -> llvalue;
val undef = _import "LLVMGetUndef" : lltype -> llvalue;
val is_null = _import "llvm_is_null" : llvalue -> bool;
val is_undef = _import "llvm_is_undef" : llvalue -> bool;
val constexpr_opcode =
    let
        val f = _import "llvm_constexpr_get_opcode" : llvalue -> int;
    in
        fn Val => Opcode.fromInt (f Val)
    end

(*--... Operations on instructions .........................................--*)
val has_metadata = _import "llvm_has_metadata" : llvalue -> bool;
val metadata =
    let
        val f = _import "llvm_metadata" : llvalue * llmdkind * bool ref -> llvalue;
    in
        fn Val => fn MDKindId =>
           let
               val is_null = ref false
               val res = f (Val, MDKindId, is_null)
           in
               if !is_null then NONE else SOME res
           end
    end
val set_metadata = _import "llvm_set_metadata" : llvalue * llmdkind * llvalue -> unit;
val clear_metadata = _import "llvm_clear_metadata" : llvalue * llmdkind -> unit;

(*--... Operations on metadata .......,.....................................--*)
val mdstring = _import "llvm_mdstring" : llcontext * string -> llvalue;
val mdnode =
    let
        val f = _import "llvm_mdnode" : llcontext * llvalue array * int -> llvalue;
    in
        fn C => fn ElemVals => f (C, ElemVals, Array.length ElemVals)
    end
val mdnull = _import "llvm_mdnull" : llcontext -> llvalue;
val get_mdstring =
    let
        val f = _import "llvm_get_mdstring" : llvalue * bool ref -> string;
    in
        fn Val =>
           let
               val is_null = ref false
               val res = f (Val, is_null)
           in
               if !is_null then NONE else SOME res
           end
    end
val get_mdnode_operands = _import "llvm_get_mdnode_operands" : llvalue -> llvalue array;
val get_named_metadata = _import "llvm_get_namedmd" : llmodule * string -> llvalue array;
val add_named_metadata_operand = _import "llvm_append_namedmd" : llmodule * string * llvalue -> unit;

val () =
    let
        val C = global_context ()
        val M = create_module (C, "timl")
        val () = dump_module M
        val () = dispose_module M
    in
        ()
    end

end
