data-race-example:
	ghc -O2 data-race-example.hs -rtsopts -threaded
	while true; do ./data-race-example +RTS -N2; done

ivar-example:
	ghc -O2 ivar-example.hs -rtsopts -threaded
	./ivar-example +RTS -N2

repeated-4-ivar:
	ghc -O2 repeated-4-ivar.hs -rtsopts -threaded
	./repeated-4-ivar +RTS -N2

# This might produce a link-time error if you forget the "-threaded"!
repeated-4-lvar:
	ghc -O2 repeated-4-lvar.hs -rtsopts -threaded
	while true; do ./repeated-4-lvar +RTS -N2; done

repeated-write-lvar-wrong:
	ghc -O2 repeated-write-lvar-wrong.hs -rtsopts -threaded
	./repeated-write-lvar-wrong +RTS -N2

repeated-write-lvar-max-counter:
	ghc -O2 repeated-write-lvar-max-counter.hs -rtsopts -threaded
	while true; do ./repeated-write-lvar-max-counter +RTS -N2; done

pair-lvar:
	ghc -O2 pair-lvar-example.hs -rtsopts -threaded
	while true; do ./pair-lvar +RTS -N2; done

parallel-and:
	ghc -O2 parallel-and.hs -rtsopts -threaded
	while true; do ./parallel-and +RTS -N2; done

set-ioref-data-race:
	ghc -O2 set-ioref-data-race.hs -rtsopts -threaded
	while true; do ./set-ioref-data-race +RTS -N2; done

set-synchronized:
	ghc -O2 set-synchronized.hs -rtsopts -threaded
	while true; do ./set-synchronized +RTS -N2; done

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

map-ioref-data-race:
	ghc -O2 map-ioref-data-race.hs -rtsopts -threaded
	while true; do ./map-ioref-data-race +RTS -N2; done

map-synchronized:
	ghc -O2 map-synchronized.hs -rtsopts -threaded
	while true; do ./map-synchronized +RTS -N2; done

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

clean:
	rm -f *.hi *.o data-race-example ivar-example repeated-4-ivar repeated-4-lvar repeated-write-lvar-wrong repeated-write-lvar-max-counter pair-lvar-example parallel-and set-ioref-data-race set-synchronized set-lvar-waitsize set-lvar-freezeafter set-lvar-quasi map-ioref-data-race map-synchronized map-lvar-waitsize map-lvar-getkey map-lvar-getkey-lib map-lvar-freezeafter map-lvar-quasi
