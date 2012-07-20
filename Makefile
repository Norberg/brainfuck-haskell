SRC=$(wildcard *.hs)

default:
	ghc compiler.hs -o compiler
	ghc state.hs -o state

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
	
