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
type llmdkind = MLton.Pointer.t

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
end

structure DiagnosticSeverity =
struct
datatype t =
         Error
         | Warning
         | Remark
         | Note
end

exception IoError of string

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
