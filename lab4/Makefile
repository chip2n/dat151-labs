all:
	bnfc Grammar.cf
	happy -gca ParGrammar.y
	alex -g LexGrammar.x
	ghc --make lab4.hs -o lab4

clean:
	-rm -f lab4
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocGrammar.ps

distclean: clean
	-rm -f DocGrammar.* LexGrammar.* ParGrammar.* LayoutGrammar.* SkelGrammar.* PrintGrammar.* TestGrammar.* AbsGrammar.* TestGrammar ErrM.* SharedString.* Grammar.dtd XMLGrammar.*

