SRC=$(wildcard *.hs)

default:
	ghc compiler.hs -o compiler

run:
	./compiler	
	cat prolog.ll > prog.ll
	cat test/helloworld.ll >> prog.ll
	cat epilog.ll >> prog.ll
	cat prog.ll
	llvm-as prog.ll -f
	opt -mem2reg -instcombine -stats prog.bc > optimized_prog.bc
	mv optimized_prog.bc prog.bc
	llc prog.bc -f
	gcc prog.s -o prog
	./prog
clean:
	\rm *.hi
	\rm *.o
	
