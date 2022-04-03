GHC        = ghc

# List of goals not corresponding to file names.

.PHONY : all clean

# Default goal.

all : Latte

Latte : AbsLatte.hs LexLatte.hs ParLatte.hs PrintLatte.hs TestLatte.hs
	${GHC} ${GHC_OPTS} $@

# Rules for cleaning generated files.

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi

# EOF
