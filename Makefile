all :
	ocamlbuild -yaccflag -v  -lib str  main.native
	mv main.native exec

clean :
	ocamlbuild -clean
