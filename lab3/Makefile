all:
	happy -gca ParCPP.y
	alex -g LexCPP.x
	latex DocCPP.tex; dvips DocCPP.dvi -o DocCPP.ps
	ghc --make TestCPP.hs -o TestCPP
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocCPP.ps
distclean: clean
	-rm -f DocCPP.* LexCPP.* ParCPP.* LayoutCPP.* SkelCPP.* PrintCPP.* TestCPP.* AbsCPP.* TestCPP ErrM.* SharedString.* CPP.dtd XMLCPP.* Makefile*

