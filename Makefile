all: interpreter

interpreter:
	wget -O stack.tar.gz https://www.stackage.org/stack/linux-x86_64
	tar xvvf stack.tar.gz
	./stack-*-linux-x86_64/stack setup

clean:
	rm interpreter.{hi,o} src/Interpreter/*.{hi,o}
