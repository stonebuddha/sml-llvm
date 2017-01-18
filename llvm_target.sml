structure LlvmTarget =
struct

infixr 0 $
fun f $ x = f x

open LlvmCore

structure Endian =
struct
datatype t =
         Big
         | Little

fun fromInt 0 = Big
  | fromInt 1 = Little
  | fromInt _ = raise (Fail "Endian.fromInt")
end

structure CodeGenOptLevel =
struct
datatype t =
         None
         | Less
         | Default
         | Aggressive

fun toInt None = 0
  | toInt Less = 1
  | toInt Default = 2
  | toInt Aggressive = 3
end

structure RelocMode =
struct
datatype t =
         Default
         | Static
         | PIC
         | DynamicNoPIC

fun toInt Default = 0
  | toInt Static = 1
  | toInt PIC = 2
  | toInt DynamicNoPIC = 3
end

structure CodeModel =
struct
datatype t =
         Default
         | JITDefault
         | Small
         | Kernel
         | Medium
         | Large

fun toInt Default = 0
  | toInt JITDefault = 1
  | toInt Small = 2
  | toInt Kernel = 3
  | toInt Medium = 4
  | toInt Large = 5
end

structure CodeGenFileType =
struct
datatype t =
         AssemblyFile
         | ObjectFile

fun toInt AssemblyFile = 0
  | toInt ObjectFile = 1
end

structure DataLayout =
struct
type t = (ST_LLVMOpaqueTargetData.tag, C.rw) C.su_obj C.ptr'

val of_string : string -> t =
 fn StringRep =>
    let
        val StringRep' = ZString.dupML' StringRep
    in
        F_llvm_datalayout_of_string.f' StringRep'
        before
        C.free' StringRep'
    end
val as_string : t -> string =
 fn TD =>
    let
        val S = F_llvm_datalayout_as_string.f' TD
    in
        ZString.toML' S
        before
        C.free' S
    end
val byte_order : t -> Endian.t = fn DL => Endian.fromInt $ Int32.toInt $ F_llvm_datalayout_byte_order.f' DL
val pointer_size : t -> int = fn DL => Word32.toInt $ F_llvm_datalayout_pointer_size.f' DL
val intptr_type : llcontext -> t -> lltype = fn C => fn DL => F_llvm_datalayout_intptr_type.f' (C, DL)
val qualified_pointer_size : int -> t -> int = fn AS => fn DL => Word32.toInt $ F_llvm_datalayout_qualified_pointer_size.f' (Word32.fromInt AS, DL)
val qualified_intptr_type : llcontext -> int -> t -> lltype = fn C => fn AS => fn DL => F_llvm_datalayout_qualified_intptr_type.f' (C, Word32.fromInt AS, DL)
(* FIXME: this can not compile under SML/NJ *)
(* val size_in_bits : lltype -> t -> Int64.int = fn Ty => fn DL => Int64.fromLarge $ Word64.toLargeInt $ F_llvm_datalayout_size_in_bits.f' (Ty, DL) *)
(* FIXME: this can not compile under SML/NJ *)
(* val store_size : lltype -> t -> Int64.int = fn Ty => fn DL => Int64.fromLarge $ Word64.toLargeInt $ F_llvm_datalayout_store_size.f' (Ty, DL) *)
(* FIXME: this can not compile under SML/NJ *)
(* val abi_size : lltype -> t -> Int64.int = fn Ty => fn DL => Int64.fromLarge $ Word64.toLargeInt $ F_llvm_datalayout_abi_size.f' (Ty, DL) *)
val abi_align : lltype -> t -> int = fn Ty => fn DL => Word32.toInt $ F_llvm_datalayout_abi_align.f' (Ty, DL)
val stack_align : lltype -> t -> int = fn Ty => fn DL => Word32.toInt $ F_llvm_datalayout_stack_align.f' (Ty, DL)
val preferred_align : lltype -> t -> int = fn Ty => fn DL => Word32.toInt $ F_llvm_datalayout_preferred_align.f' (Ty, DL)
val preferred_align_of_global : llvalue -> t -> int = fn GlobalVar => fn DL => Word32.toInt $ F_llvm_datalayout_preferred_align_of_global.f' (GlobalVar, DL)
(* FIXME: this can not compile under SML/NJ *)
(* val element_at_offset : lltype -> Int64.int -> t -> int = fn Ty => fn Offset => fn DL => Word32.toInt $ F_llvm_datalayout_element_at_offset.f' (Ty, Word64.fromLargeInt $ Int64.toLarge Offset, DL) *)
(* FIXME: this can not compile under SML/NJ *)
(* val offset_of_element : lltype -> int -> t -> Int64.int = fn Ty => fn Index => fn DL => Int64.fromLarge $ Word64.toLargeInt $ F_llvm_datalayout_offset_of_element.f' (Ty, Word32.fromInt Index, DL) *)
end

structure Target =
struct
type t = (ST_LLVMTarget.tag, C.rw) C.su_obj C.ptr'

val default_triple : unit -> string =
 fn () =>
    let
        val S = F_llvm_target_default_triple.f' ()
    in
        ZString.toML' S
        before
        C.free' S
    end
val first : unit -> t option =
 fn () =>
    let
        val Res = F_llvm_target_first.f' ()
    in
        if C.Ptr.isNull' Res then NONE
        else SOME Res
    end
val succ : t -> t option =
 fn Target =>
    let
        val Res = F_llvm_target_succ.f' Target
    in
        if C.Ptr.isNull' Res then NONE
        else SOME Res
    end
val by_name : string -> t option =
 fn Name =>
    let
        val Name' = ZString.dupML' Name
        val Res = F_llvm_target_by_name.f' Name'
    in
        (if C.Ptr.isNull' Res then NONE else SOME Res)
        before
        C.free' Name'
    end
val by_triple : string -> t =
 fn Triple =>
    let
        val Triple' = ZString.dupML' Triple
    in
        F_llvm_target_by_triple.f' Triple'
        before
        C.free' Triple'
    end
val name : t -> string =
 fn Target =>
    let
        val Name = F_llvm_target_name.f' Target
    in
        ZString.toML' Name
        before
        C.free' Name
    end
val description : t -> string =
 fn Target =>
    let
        val Desc = F_llvm_target_description.f' Target
    in
        ZString.toML' Desc
        before
        C.free' Desc
    end
val has_jit : t -> bool =
 fn Target =>
    case F_llvm_target_has_jit.f' Target of
        0 => false
      | 1 => true
      | _ => raise (Fail "Target.has_jit")
val has_target_machine : t -> bool =
 fn Target =>
    case F_llvm_target_has_target_machine.f' Target of
        0 => false
      | 1 => true
      | _ => raise (Fail "Target.has_target_machine")
val has_asm_backend : t -> bool =
 fn Target =>
    case F_llvm_target_has_asm_backend.f' Target of
        0 => false
      | 1 => true
      | _ => raise (Fail "Target.has_asm_backend")

fun all () =
  let
      fun step elem lst =
        case elem of
            SOME target => step (succ target) (target :: lst)
          | NONE => lst
  in
      step (first ()) []
  end
end

structure TargetMachine =
struct
type t = (ST_LLVMOpaqueTargetMachine.tag, C.rw) C.su_obj C.ptr'

val create : string -> string option -> string option -> CodeGenOptLevel.t option -> RelocMode.t option -> CodeModel.t option -> Target.t -> t =
 fn Triple => fn CPU => fn Features => fn Level => fn RelocMode => fn CodeModel => fn Target =>
    let
        val Triple' = ZString.dupML' Triple
        val CPU' = Option.getOpt (Option.map ZString.dupML' CPU, C.Ptr.null')
        val Features' = Option.getOpt (Option.map ZString.dupML' Features, C.Ptr.null')
        val Level' = Int32.fromInt $ CodeGenOptLevel.toInt $ Option.getOpt (Level, CodeGenOptLevel.Default)
        val RelocMode' = Int32.fromInt $ RelocMode.toInt $ Option.getOpt (RelocMode, RelocMode.Default)
        val CodeModel' = Int32.fromInt $ CodeModel.toInt $ Option.getOpt (CodeModel, CodeModel.Default)
    in
        F_llvm_create_targetmachine.f' (Triple', CPU', Features', Level', RelocMode', CodeModel', Target)
        before
        (C.free' Triple';
         if C.Ptr.isNull' CPU' then () else C.free' CPU';
         if C.Ptr.isNull' Features' then () else C.free' Features')
    end
val target : t -> Target.t = fn Machine => F_llvm_targetmachine_target.f' Machine
val triple : t -> string =
 fn Machine =>
    let
        val Triple = F_llvm_targetmachine_triple.f' Machine
    in
        ZString.toML' Triple
        before
        C.free' Triple
    end
val cpu : t -> string =
 fn Machine =>
    let
        val CPU = F_llvm_targetmachine_cpu.f' Machine
    in
        ZString.toML' CPU
        before
        C.free' CPU
    end
val features : t -> string =
 fn Machine =>
    let
        val Features = F_llvm_targetmachine_features.f' Machine
    in
        ZString.toML' Features
        before
        C.free' Features
    end
val data_layout : t -> DataLayout.t = fn Machine => F_llvm_targetmachine_data_layout.f' Machine
val add_analysis_passes : 'a PassManager.t -> t -> unit = fn PM => fn Machine => F_llvm_targetmachine_add_analysis_passes.f' (PM, Machine)
val set_verbose_asm : bool -> t -> unit = fn Verb => fn Machine => F_llvm_targetmachine_set_verbose_asm.f' (if Verb then 1 else 0, Machine)
val emit_to_file : llmodule -> CodeGenFileType.t -> string -> t -> unit =
 fn Module => fn FileType => fn FileName => fn Machine =>
    let
        val FileName' = ZString.dupML' FileName
    in
        F_llvm_targetmachine_emit_to_file.f' (Module, Int32.fromInt $ CodeGenFileType.toInt FileType, FileName', Machine)
        before
        C.free' FileName'
    end
val emit_to_memory_buffer : llmodule -> CodeGenFileType.t -> t -> llmemorybuffer = fn Module => fn FileType => fn Machine => F_llvm_targetmachine_emit_to_memory_buffer.f' (Module, Int32.fromInt $ CodeGenFileType.toInt FileType, Machine)
end

end
