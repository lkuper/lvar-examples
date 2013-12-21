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

pair-lvar-example:
	ghc -O2 pair-lvar-example.hs -rtsopts -threaded
	while true; do ./pair-lvar-example +RTS -N2; done

parallel-and:
	ghc -O2 parallel-and.hs -rtsopts -threaded
	while true; do ./parallel-and +RTS -N2; done

clean:
	rm -f *.hi *.o data-race-example ivar-example repeated-4-ivar repeated-4-lvar repeated-write-lvar-wrong repeated-write-lvar-max-counter pair-lvar-example parallel-and
