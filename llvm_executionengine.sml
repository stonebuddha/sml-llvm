structure LlvmExecutionengine =
struct

infixr 0 $
fun f $ x = f x

open LlvmTarget

fun dupPtrArr (arr : ('a C.ptr') array) : ('a C.ptr, C.rw) C.obj C.ptr' =
  let
      val buf = C.alloc' C.S.ptr (Word.fromInt $ Array.length arr)
      fun iter (i, ptr) =
        let
            val loc = C.Ptr.|+! C.S.ptr (buf, i)
        in
            C.Set.ptr' (C.Ptr.|*! loc, ptr)
        end
      val () = Array.appi iter arr
  in
      buf
  end

val initialize : unit -> bool =
 fn () =>
    case F_llvm_ee_initialize.f' () of
        0 => false
      | 1 => true
      | _ => raise (Fail "initialize")

type llexecutionengine = (ST_LLVMOpaqueExecutionEngine.tag, C.rw) C.su_obj C.ptr'
type llgenericvalue = (ST_LLVMOpaqueGenericValue.tag, C.rw) C.su_obj C.ptr'

type llcompileroptions = {
    opt_level : int,
    code_model : CodeModel.t,
    no_framepointer_elim : bool,
    enable_fast_isel : bool
}

val default_compiler_options = {
    opt_level = 0,
    code_model = CodeModel.JITDefault,
    no_framepointer_elim = false,
    enable_fast_isel = false
}

val create : llcompileroptions option -> llmodule -> llexecutionengine =
 fn Options => fn M =>
    let
        val (OptLevel, CodeModel, NoFramePointerElim, EnableFastISel) =
            Option.getOpt
                (Option.map
                     (fn content =>
                         let
                             val OptLevel = C.new' C.S.sint
                             val () = C.Set.sint' (OptLevel, Int32.fromInt $ #opt_level content)
                             val CodeModel = C.new' C.S.sint
                             val () = C.Set.sint' (CodeModel, Int32.fromInt $ CodeModel.toInt $ #code_model content)
                             val NoFramePointerElim = C.new' C.S.sint
                             val () = C.Set.sint' (NoFramePointerElim, if #no_framepointer_elim content then 1 else 0)
                             val EnableFastISel = C.new' C.S.sint
                             val () = C.Set.sint' (EnableFastISel, if #enable_fast_isel content then 1 else 0)
                         in
                             (C.Ptr.|&! OptLevel, C.Ptr.|&! CodeModel, C.Ptr.|&! NoFramePointerElim, C.Ptr.|&! EnableFastISel)
                         end) Options, (C.Ptr.null', C.Ptr.null', C.Ptr.null', C.Ptr.null'))
    in
        F_llvm_ee_create.f' (OptLevel, CodeModel, NoFramePointerElim, EnableFastISel, M)
        before
        (if C.Ptr.isNull' OptLevel then () else C.free' OptLevel;
         if C.Ptr.isNull' CodeModel then () else C.free' CodeModel;
         if C.Ptr.isNull' NoFramePointerElim then () else C.free' NoFramePointerElim;
         if C.Ptr.isNull' EnableFastISel then () else C.free' EnableFastISel)
    end
val dispose : llexecutionengine -> unit = fn EE => F_llvm_ee_dispose.f' EE
val add_module : llmodule -> llexecutionengine -> unit = fn M => fn EE => F_llvm_ee_add_module.f' (M, EE)
val remove_module : llmodule -> llexecutionengine -> llmodule = fn M => fn EE => F_llvm_ee_remove_module.f' (M, EE)
val run_static_ctors : llexecutionengine -> unit = fn EE => F_llvm_ee_run_static_ctors.f' EE
val run_static_dtors : llexecutionengine -> unit = fn EE => F_llvm_ee_run_static_dtors.f' EE
val data_layout : llexecutionengine -> DataLayout.t = fn EE => F_llvm_ee_get_data_layout.f' EE
val add_global_mapping : llvalue -> 'a C.ptr -> llexecutionengine -> unit =
 fn Global => fn Ptr => fn EE =>
    F_llvm_ee_add_global_mapping.f' (Global, C.Ptr.inject Ptr, EE)
fun get_global_value_address (Name : string) (Typ : 'a C.ptr C.T.typ) (EE : llexecutionengine) : 'a C.ptr =
  let
      val Name' = ZString.dupML' Name
      val Res = F_llvm_ee_get_global_value_address.f' (Name', EE) before C.free' Name'
  in
      C.Get.ptr $ C.Ptr.|*| $ C.Ptr.cast (C.T.pointer Typ) Res
  end
fun get_function_address (Name : string) (Typ : 'a C.fptr C.T.typ) (EE : llexecutionengine) : 'a C.fptr =
  let
      val Name' = ZString.dupML' Name
      val Res = F_llvm_ee_get_function_address.f' (Name', EE) before C.free' Name'
  in
      C.Get.fptr $ C.Ptr.|*| $ C.Ptr.cast (C.T.pointer Typ) Res
  end
(* FIXME: this can not compile under SML/NJ *)
(* val create_generic_value_of_int : lltype -> Int64.int -> bool -> llgenericvalue = *)
(*  fn Ty => fn N => fn IsSigned => F_llvm_create_generic_value_of_int.f' (Ty, Word64.fromLargeInt $ Int64.toLarge N, if IsSigned then 1 else 0) *)
val create_generic_value_of_pointer : C.voidptr -> llgenericvalue =
 fn P => F_llvm_create_generic_value_of_pointer.f' P
val create_generic_value_of_float : lltype -> real -> llgenericvalue =
 fn Ty => fn N => F_llvm_create_generic_value_of_float.f' (Ty, N)
val generic_value_int_width : llgenericvalue -> int =
 fn GenVal => Word32.toInt $ F_llvm_generic_value_int_width.f' GenVal
(* FIXME: this can not compile under SML/NJ *)
(* val generic_value_to_int : llgenericvalue -> bool -> Int64.int = *)
(*  fn GenVal => fn IsSigned => Int64.fromLarge $ Word64.toLargeInt $ F_llvm_generic_value_to_int.f' (GenVal, if IsSigned then 1 else 0) *)
val generic_value_to_pointer : llgenericvalue -> C.voidptr =
 fn GenVal => F_llvm_generic_value_to_pointer.f' GenVal
val generic_value_to_float : lltype -> llgenericvalue -> real =
 fn Ty => fn GenVal => F_llvm_generic_value_to_float.f' (Ty, GenVal)
val dispose_generic_value : llgenericvalue -> unit =
 fn GenVal => F_llvm_dispose_generic_value.f' GenVal
val run_function_as_main : llexecutionengine -> llvalue -> int -> string list -> string list -> int =
 fn EE => fn F => fn ArgC => fn ArgV => fn EnvP =>
    let
        val ArgVC = Array.fromList $ List.map ZString.dupML' ArgV
        val ArgV' = dupPtrArr ArgVC
        val EnvPC = Array.fromList $ List.map ZString.dupML' EnvP
        val EnvP' = dupPtrArr EnvPC
    in
        (Int32.toInt $ F_llvm_run_function_as_main.f' (EE, F, Word32.fromInt ArgC, C.Ptr.ro' ArgV', C.Ptr.ro' EnvP'))
        before
        (Array.app C.free' ArgVC; Array.app C.free' EnvPC; C.free' ArgV'; C.free' EnvP')
    end
val run_function : llexecutionengine -> llvalue -> int -> llgenericvalue array -> llgenericvalue =
 fn EE => fn F => fn ArgCount => fn Args =>
    let
        val Args' = dupPtrArr Args
    in
        F_llvm_run_function.f' (EE, F, Word32.fromInt ArgCount, Args')
        before
        C.free' Args'
    end

end
