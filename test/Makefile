.PHONY: default smlnj mlton clean

FILES = \
test.sml \
mlton-main.sml

default: smlnj

smlnj:
	cp ../libllvm32.so .
	ml-build -Ccompiler-mc.error-non-exhaustive-match=true -Ccompiler-mc.error-non-exhaustive-bind=true test.cm Test.main test-image

mlton: test

test: test.mlb $(FILES) ../libllvm64.a
	mlton -link-opt "-lstdc++ -L.. -lllvm64 `llvm-config --ldflags --system-libs --libs all`" test.mlb

clean:
	rm -rf .cm test test-image* *.so *.bc
