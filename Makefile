
IVORY_REPO ?= ../smaccmpilot-build/ivory

default:
	cabal build

clean-sandbox:
	-rm -rf .cabal-sandbox
	-rm cabal.sandbox.config
	-rm -rf dist

create-sandbox:
	cabal sandbox init
	cabal sandbox add-source $(IVORY_REPO)/ivory-artifact
	cabal install --dependencies-only

