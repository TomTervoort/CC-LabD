CABAL-CONFIGURE-FLAGS := --user
CABAL-BUILD-FLAGS     :=

all : haskell

src/CCO/HM/AG.hs : src/CCO/HM/AG.ag src/CCO/HM/AG/Base.ag
	uuagc -Hdcfws --self -P src/CCO/HM src/CCO/HM/AG.ag

src/CCO/SystemF/AG.hs : src/CCO/SystemF/AG.ag src/CCO/SystemF/AG/Base.ag \
		src/CCO/SystemF/AG/Printing.ag
	uuagc -Hdcfws --self -P src/CCO/SystemF src/CCO/SystemF/AG.ag

haskell : src/CCO/HM/AG.hs src/CCO/SystemF/AG.hs
	runhaskell Setup.lhs configure $(CABAL-CONFIGURE-FLAGS)
	runhaskell Setup.lhs build $(CABAL-BUILD-FLAGS)

.PHONY : haskell