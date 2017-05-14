all: interpreter

interpreter:
	wget -O stack.tar.gz https://www.stackage.org/stack/linux-x86_64
	tar xvvf stack.tar.gz
	ln -s stack-*-linux-x86_64/stack stack
	./stack setup
	./stack install megaparsec
	./stack ghc -- interpreter.hs -isrc

clean:
	rm interpreter.{hi,o} src/Interpreter/*.{hi,o}
