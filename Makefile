GHC        = ghc

# List of goals not corresponding to file names.

.PHONY : all clean all_clean

# Default goal.

all : Main

Main : 
	${GHC} -isrc src/Main.hs --make -o interpreter

# Rules for cleaning generated files.

clean :
	find . -name "*.hi" -type f -delete
	find . -name "*.o" -type f -delete

all_clean :
	make clean
	rm interpreter

# EOF

