# This might produce a link-time error if you forget the "-threaded"!
repeated-4-lvar:
	ghc -O2 repeated-4-lvar.hs -rtsopts -threaded
	while true; do ./repeated-4-lvar +RTS -N2; done

repeated-write-lvar-wrong:
	ghc -O2 repeated-write-lvar-wrong.hs -rtsopts -threaded
	./repeated-write-lvar-wrong +RTS -N2

parallel-and:
	ghc -O2 parallel-and.hs -rtsopts -threaded
	while true; do ./parallel-and +RTS -N2; done

set-lvar-waitsize:
	ghc -O2 set-lvar-waitsize.hs -rtsopts -threaded
	while true; do ./set-lvar-waitsize +RTS -N2; done

set-lvar-waitelem:
	ghc -O2 set-lvar-waitelem.hs -rtsopts -threaded
	while true; do ./set-lvar-waitelem +RTS -N2; done

set-lvar-freezeafter:
	ghc -O2 set-lvar-freezeafter.hs -rtsopts -threaded
	while true; do ./set-lvar-freezeafter +RTS -N2; done

set-lvar-quasi:
	ghc -O2 set-lvar-quasi.hs -rtsopts -threaded
	while true; do ./set-lvar-quasi +RTS -N2; done

map-lvar-waitsize:
	ghc -O2 map-lvar-waitsize.hs -rtsopts -threaded
	while true; do ./map-lvar-waitsize +RTS -N2; done

map-lvar-getkey:
	ghc -O2 map-lvar-getkey.hs -rtsopts -threaded
	while true; do ./map-lvar-getkey +RTS -N2; done

map-lvar-getkey-lib:
	ghc -O2 map-lvar-getkey-lib.hs -rtsopts -threaded
	while true; do ./map-lvar-getkey-lib +RTS -N2; done

map-lvar-freezeafter:
	ghc -O2 map-lvar-freezeafter.hs -rtsopts -threaded
	while true; do ./map-lvar-freezeafter +RTS -N2; done

map-lvar-quasi:
	ghc -O2 map-lvar-quasi.hs -rtsopts -threaded
	while true; do ./map-lvar-quasi +RTS -N2; done

graph-traversal-explicit-freeze:
	ghc -O2 graph-traversal-explicit-freeze.hs -rtsopts -threaded
	while true; do ./graph-traversal-explicit-freeze +RTS -N2; done

graph-traversal-implicit-freeze:
	ghc -O2 graph-traversal-implicit-freeze.hs -rtsopts -threaded
	while true; do ./graph-traversal-implicit-freeze +RTS -N2; done

clean:
	rm -f *.hi *.o repeated-4-lvar repeated-write-lvar-wrong parallel-and set-lvar-waitsize set-lvar-waitelem set-lvar-freezeafter set-lvar-quasi map-lvar-waitsize map-lvar-getkey map-lvar-getkey-lib map-lvar-freezeafter map-lvar-quasi graph-traversal-explicit-freeze graph-traversal-implicit-freeze
