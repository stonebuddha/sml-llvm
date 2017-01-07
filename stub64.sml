structure Stub =
struct

local
    val lib = DynLinkage.open_lib { name = "./llvm64.dylib", global = true, lazy = true }
in

fun libh s =
  let
      val sh = DynLinkage.lib_symbol (lib, s)
  in
      fn () => DynLinkage.addr sh
  end

end

end
