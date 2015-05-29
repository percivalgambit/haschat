all: .setup-complete

.PHONY: haschat run build test clean

haschat: run

run: setup
ifndef CHAT_SERVER_PORT
	export CHAT_SERVER_PORT=22311; cabal run
else
	cabal run
endif

build: setup
	cabal build

test: setup
	cabal test

clean:
	cabal clean

setup: .setup-complete

.setup-complete:
	cabal update
	cabal sandbox init
	cabal install --only-dependencies --enable-tests
	touch $@
