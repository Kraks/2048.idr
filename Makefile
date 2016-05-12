all: 2048 2048-dep

2048: 2048.idr
	idris -p effects 2048.idr -o 2048

2048-dep: 2048-dependent.idr
	idris -p effects 2048-dependent.idr -o 2048-dep

clean:
	rm -rf 2048
	rm -rf 2048-dep
	rm -rf *.ibc

test:
	idris -p effects 2048-dependent.idr
