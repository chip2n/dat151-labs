all:
	happy -gca ParGrammar.y
	alex -g LexGrammar.x
	latex DocGrammar.tex; dvips DocGrammar.dvi -o DocGrammar.ps
	ghc --make TestGrammar.hs -o TestGrammar
	ghc --make -o lab4 lab4.hs
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocGrammar.ps
distclean: clean
	-rm -f DocGrammar.* LexGrammar.* ParGrammar.* LayoutGrammar.* SkelGrammar.* PrintGrammar.* TestGrammar.* AbsGrammar.* TestGrammar ErrM.* SharedString.* Grammar.dtd XMLGrammar.* Makefile*

