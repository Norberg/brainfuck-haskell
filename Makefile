SRC=$(wildcard *.hs)

default:
	ghc compiler.hs -o compiler
prog = "test/helloworld.bf"
run:
	./compiler $(prog)
	cat prolog.ll > prog.ll
	cat test/helloworld.ll >> prog.ll
	cat epilog.ll >> prog.ll
	#cat prog.ll
	llvm-as prog.ll -f
	opt -mem2reg -instcombine -stats prog.bc > optimized_prog.bc
	mv optimized_prog.bc prog.bc
	llc prog.bc -f
	wc -l prog.s
	gcc prog.s -o prog
	time ./prog
clean:
	\rm *.hi
	\rm *.o
	
