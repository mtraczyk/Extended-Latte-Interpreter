GHC        = ghc

# List of goals not corresponding to file names.

.PHONY : all clean

# Default goal.

all : Main

Main : 
	${GHC} Main.hs --make -o Latte

# Rules for cleaning generated files.

clean :
	find . -name "*.hi" -type f -delete
	find . -name "*.o" -type f -delete
	rm Latte

# EOF
