structure LlvmExecutionengineInt =
struct

infixr 0 $
fun f $ x = f x

structure Core = LlvmCoreInt
structure Target = LlvmTargetInt

val initialize : unit -> bool =
 fn () =>
    case F_llvm_ee_initialize.f' () of
        0 => false
      | 1 => true
      | _ => raise (Fail "initialize")

type llexecutionengine = (ST_LLVMOpaqueExecutionEngine.tag, C.rw) C.su_obj C.ptr'

type llcompileroptions = {
    opt_level : int,
    code_model : Target.CodeModel.t,
    no_framepointer_elim : bool,
    enable_fast_isel : bool
}

val default_compiler_options = {
    opt_level = 0,
    code_model = Target.CodeModel.JITDefault,
    no_framepointer_elim = false,
    enable_fast_isel = false
}

val create : llcompileroptions option -> Core.llmodule -> llexecutionengine =
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
                             val () = C.Set.sint' (CodeModel, Int32.fromInt $ Target.CodeModel.toInt $ #code_model content)
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
val add_module : Core.llmodule -> llexecutionengine -> unit = fn M => fn EE => F_llvm_ee_add_module.f' (M, EE)
val remove_module : Core.llmodule -> llexecutionengine -> Core.llmodule = fn M => fn EE => F_llvm_ee_remove_module.f' (M, EE)
val run_static_ctors : llexecutionengine -> unit = fn EE => F_llvm_ee_run_static_ctors.f' EE
val run_static_dtors : llexecutionengine -> unit = fn EE => F_llvm_ee_run_static_dtors.f' EE
val data_layout : llexecutionengine -> Target.DataLayout.t = fn EE => F_llvm_ee_get_data_layout.f' EE
val add_global_mapping : Core.llvalue -> 'a C.ptr -> llexecutionengine -> unit =
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

end

structure LlvmExecutionengine : LLVM_EXECUTIONENGINE = LlvmExecutionengineInt
