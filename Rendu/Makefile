all :
	ocamlbuild -yaccflag -v -lib unix -lib nums -Is Code/DPLL,Code/General,Code/Lexers_Parsers,Code/Theories,Code/Heuristics Code/main.native
	ln -fs main.native resol

byte :
	ocamlbuild -yaccflag -v -lib unix -lib nums -Is Code/DPLL,Code/General,Code/Lexers_Parsers,Code/Theories,Code/Heuristics Code/main.byte
	ln -fs main.byte resol

clean:
	ocamlbuild -clean
	rm resol

prof:
	ocamlbuild -yaccflag -v -lib unix -lib nums -Is Code/DPLL,Code/General,Code/Lexers_Parsers,Code/Theories,Code/Heuristics Code/main.p.native
	mv main.p.native resol.p

exemples:
	wget http://perso.ens-lyon.fr/clement.sartori/Tests.tar.gz
	tar -xvf Tests.tar.gz
	rm Tests.tar.gz

tests:
	ocamlbuild -lib unix Moulinettes/tests3d.native
	ln -fs tests3d.native 3d
	ocamlbuild -lib unix Moulinettes/tests_versions.native
	ln -fs tests_versions.native versions
	ocamlbuild -lib unix Moulinettes/tests_heuristics.native
	ln -fs tests_heuristics.native heuristics
