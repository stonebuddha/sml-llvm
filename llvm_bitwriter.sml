structure LlvmBitwriter :> LLVM_BITWRITER =
struct

structure Core = LlvmCore

(* fun write_bitcode_file (M : Core.llmodule) (Path : string) : bool = *)
  (* let *)
      (* val Path' = ZString.dupML Path *)
  (* in *)
      (* (case F_llvm_write_bitcode_file.f (M, Path') of *)
           (* 0 => false *)
         (* | 1 => true *)
         (* | _ => raise (Fail "write_bitcode")) *)
      (* before *)
      (* C.free Path' *)
  (* end *)

(* fun wrtie_bitcode_to_memory_buffer (M : Core.llmodule) : Core.llmemorybuffer = *)
  (* F_llvm_write_bitcode_to_memory_buffer.f M *)

end
