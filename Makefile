
IVORY_REPO ?= ../ivory

default:
	cabal build

create-sandbox:
	cabal sandbox init
	cabal sandbox add-source $(IVORY_REPO)/ivory-artifact
	cabal install --dependencies-only

test: haskell-backend-test
test: ivory-backend-test

haskell-backend-test:
	cabal run gidl -- -b haskell \
		-i tests/example.idl \
		-o tests/gidl-haskell-backend-test \
		-p gidl-haskell-backend-test \
		-n Gidl.Haskell.Test
	make -C tests/gidl-haskell-backend-test create-sandbox
	make -C tests/gidl-haskell-backend-test
	make -C tests/gidl-haskell-backend-test test

haskell-backend-test-clean:
	-rm -rf tests/gidl-haskell-backend-test

ivory-backend-test:
	cabal run gidl -- -b ivory \
		-i tests/example.idl \
		-o tests/gidl-ivory-backend-test \
		-p gidl-ivory-backend-test \
		-n Gidl.Ivory.Test
	make -C tests/gidl-ivory-backend-test create-sandbox
	make -C tests/gidl-ivory-backend-test
	make -C tests/gidl-ivory-backend-test test

ivory-backend-test-clean:
	-rm -rf tests/gidl-ivory-backend-test


clean: ivory-backend-test-clean
clean: haskell-backend-test-clean

distclean: clean
	-rm -rf dist

clean-sandbox: distclean
	-rm -rf .cabal-sandbox
	-rm cabal.sandbox.config

