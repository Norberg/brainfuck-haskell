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
clean:
	\rm *.hi
	\rm *.o
	
