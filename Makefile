.PHONY: default clean

default:

llvm32.dylib: llvm_sml.cpp
	g++ -m32 -dynamiclib `~/Experiments/llvm/build32/bin/llvm-config --cxxflags --ldflags --system-libs --libs core` -o $@ $^

llvm64.dylib: llvm_sml.cpp
	g++ -dynamiclib `llvm-config --cxxflags --ldflags --system-libs --libs core` -o $@ $^

clean:
	rm -rf .cm FFI32 FFI64 *.dylib *.dSYM
