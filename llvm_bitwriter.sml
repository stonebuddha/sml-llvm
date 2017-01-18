structure LlvmBitwriter :> LLVM_BITWRITER =
struct

infixr 0 $
fun f $ x = f x

structure Core = LlvmCore

fun write_bitcode_file (M : Core.llmodule) (Path : string) : bool =
  let
      val Path' = ZString.dupML' Path
  in
      (case F_llvm_write_bitcode_file.f' (M, Path') of
           0 => false
         | 1 => true
         | _ => raise (Fail "write_bitcode_file"))
      before
      C.free' Path'
  end

fun write_bitcode_to_fd (U : bool option) (M : Core.llmodule) (FD : Posix.FileSys.file_desc) : bool =
  case F_llvm_write_bitcode_to_fd.f' (if (Option.getOpt (U, false)) then 1 else 0, M, Posix.FileSys.fdToWord FD) of
      0 => false
    | 1 => true
    | _ => raise (Fail "write_bitcode_to_fd")

fun write_bitcode_to_memory_buffer (M : Core.llmodule) : Core.llmemorybuffer =
  F_llvm_write_bitcode_to_memory_buffer.f' M

fun output_bitcode (U : bool option) (S : BinIO.outstream) (M : Core.llmodule) : bool =
  case #1 (BinIO.StreamIO.getWriter $ BinIO.getOutstream S) of
      BinPrimIO.WR wr => write_bitcode_to_fd U M (Option.valOf $ Posix.FileSys.iodToFD $ Option.valOf $ #ioDesc wr)

end
