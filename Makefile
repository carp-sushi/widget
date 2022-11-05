.PHONY: all format build test clean run cmd

all: format build test

format:
	purs-tidy format-in-place src/**/*.purs test/**/*.purs

build:
	spago build

test:
	spago test

clean:
	rm -rf index.js index.min.js index.wasm output .spago output-es .psci_modules

run:
	spago run

cmd:
	spago -x prod.dhall build && purs-backend-es bundle-module --no-build
	sed -i s/export\ \{/Cmd=\{execute\}\;\\/\*/g index.js && echo "*/" >> index.js
	minify -o index.min.js index.js
	js2wasm index.min.js
	wasm-opt -ol 3 -s 0 --strip -o wasm/cmd.wasm index.wasm
