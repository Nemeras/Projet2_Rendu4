all :
	ocamlbuild -yaccflag -v -lib unix -Is Code/DPLL,Code/General,Code/Lexers_Parsers,Code/Theories,Code/Heuristics Code/main.native
	ln -fs main.native resol

byte :
	ocamlbuild -yaccflag -v -lib unix -Is Code/DPLL,Code/General,Code/Lexers_Parsers,Code/Theories,Code/Heuristics Code/main.byte
	ln -fs main.byte resol

clean:
	ocamlbuild -clean
	rm resol

prof:
	ocamlbuild -yaccflag -v -lib unix -Is Code/DPLL,Code/General,Code/Lexers_Parsers,Code/Theories,Code/Heuristics Code/main.p.native
	mv main.p.native resol.p

exemples:
	wget http://perso.ens-lyon.fr/clement.sartori/Tests.tar.gz
	tar -xvf Tests.tar.gz
	rm Tests.tar.gz
