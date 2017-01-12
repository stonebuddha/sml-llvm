.PHONY: default all stub32 stub64 shared32 shared64 clean

default:

all: stub32 stub64 shared32 shared64

stub32: FFI32/llvm.cm

stub64: FFI64/llvm.mlb

shared32: libllvm32.so

shared64: libllvm64.so

clean:
	rm -rf .cm FFI32 FFI64 *.so *.dSYM

FFI32/llvm.cm: llvm_sml_core.h llvm_sml_bitwriter.h
	rm -rf FFI32
	mkdir FFI32
	echo "structure Stub =\n\
struct\n\
local\n\
    val lib = DynLinkage.open_lib { name = \"./libllvm32.so\", global = true, lazy = true }\n\
in\n\
fun libh s =\n\
  let\n\
      val sh = DynLinkage.lib_symbol (lib, s)\n\
  in\n\
       fn () => DynLinkage.addr sh\n\
  end\n\
end\n\
end" > FFI32/stub.sml
	ml-nlffigen -include stub.sml -libhandle Stub.libh -dir FFI32 -cmfile llvm.cm $^

FFI64/llvm.mlb: llvm_sml_core.h llvm_sml_bitwriter.h
	rm -rf FFI64
	mlnlffigen -linkage shared -dir FFI64 -mlbfile llvm.mlb $^

libllvm32.so: llvm_sml_core.c llvm_sml_bitwriter.c
	gcc -m32 -dynamiclib -lstdc++ `$(LLVMBIN32)/llvm-config --cflags --ldflags --system-libs --libs all` -o $@ $^

libllvm64.so: llvm_sml_core.c llvm_sml_bitwriter.c
	gcc -shared -lstdc++ `$(LLVMBIN64)/llvm-config --cflags --ldflags --system-libs --libs all` -o $@ $^
