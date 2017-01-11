.PHONY: default all stub32 stub64 dynamic32 shared64 clean

default:

all: stub32 stub64 dynamic32 shared64

stub32: FFI32/llvm.cm

stub64: FFI64/llvm.mlb

dynamic32: llvm.dylib

shared64: llvm_core.o llvm_bitwriter.o

clean:
	rm -rf .cm FFI32 FFI64 *.o *.dylib

FFI32/llvm.cm: llvm_core.h llvm_bitwriter.h
	rm -rf FFI32
	mkdir FFI32
	echo "structure Stub =\n\
struct\n\
local\n\
    val lib = DynLinkage.open_lib { name = \"./llvm.dylib\", global = true, lazy = true }\n\
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

FFI64/llvm.mlb: llvm_core.h llvm_bitwriter.h
	rm -rf FFI64
	mlnlffigen -linkage shared -dir FFI64 -mlbfile llvm.mlb $^

llvm.dylib: llvm_core.cpp llvm_bitwriter.cpp
	g++ -m32 -dynamiclib `$(LLVMBIN32)/llvm-config --cxxflags --ldflags --system-libs --libs all` -o $@ $^

llvm_core.o: llvm_core.cpp
	g++ -c `$(LLVMBIN64)/llvm-config --cxxflags` -o $@ $^

llvm_bitwriter.o: llvm_bitwriter.cpp
	g++ -c `$(LLVMBIN64)/llvm-config --cxxflags` -o $@ $^
