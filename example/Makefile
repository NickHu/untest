CC=clang
all: examples
examples: hello factorial
hello:
	$(CC) -o hello hello.c
factorial:
	ghc -threaded factorial.hs
clean:
	rm -f hello factorial factorial.o factorial.hi
