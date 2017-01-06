.PHONY: default

FILES = \
llvm_core.sig \
llvm_core.sml

default: llvm.exe

export.h llvm.0.o llvm.1.o: llvm.mlb $(FILES)
	mlton -stop o -default-ann 'allowFFI true' -export-header export.h llvm.mlb

llvm_sml.o: llvm_sml.c export.h
	gcc -I/usr/local/include -fPIC -Wall -W -Wno-unused-parameter -Wwrite-strings -Wmissing-field-initializers -pedantic -Wno-long-long -Wcovered-switch-default -Wdelete-non-virtual-dtor -Werror=date-time -g -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -c llvm_sml.c

llvm.exe: llvm.0.o llvm.1.o llvm_sml.o
	g++ -o llvm.exe llvm.0.o llvm.1.o llvm_sml.o -L/usr/local/lib/mlton/targets/self -lmlton-pic -lgdtoa-pic -lm -lgmp -m64 -L/usr/local/lib -Wl,-search_paths_first -Wl,-headerpad_max_install_names -lLLVMX86Disassembler -lLLVMX86AsmParser -lLLVMX86CodeGen -lLLVMSelectionDAG -lLLVMAsmPrinter -lLLVMDebugInfoCodeView -lLLVMCodeGen -lLLVMScalarOpts -lLLVMInstCombine -lLLVMInstrumentation -lLLVMTransformUtils -lLLVMBitWriter -lLLVMX86Desc -lLLVMMCDisassembler -lLLVMX86Info -lLLVMX86AsmPrinter -lLLVMX86Utils -lLLVMMCJIT -lLLVMExecutionEngine -lLLVMTarget -lLLVMAnalysis -lLLVMProfileData -lLLVMRuntimeDyld -lLLVMObject -lLLVMMCParser -lLLVMBitReader -lLLVMMC -lLLVMCore -lLLVMSupport -lcurses -lz -lm
