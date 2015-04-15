IVORY_REPO ?= ../../../ivory

default:
	cabal build

create-sandbox:
	cabal sandbox init
	cabal install --enable-tests --dependencies-only

test:
	cabal run -- --src-dir=codegen-out
