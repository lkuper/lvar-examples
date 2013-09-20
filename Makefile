data-race-example:
	ghc -O2 data-race-example.hs -rtsopts -threaded
	while true; do ./data-race-example +RTS -N2; done

ivar-example:
	ghc -O2 ivar-example.hs -rtsopts -threaded
	./ivar-example +RTS -N2

repeated-write-ivar:
	ghc -O2 repeated-write-ivar.hs -rtsopts -threaded
	./repeated-write-ivar +RTS -N2

# This might produce a link-time error if you forget the "-threaded"!
repeated-write-lvar:
	ghc -O2 repeated-write-lvar.hs -rtsopts -threaded
	while true; do ./repeated-write-lvar +RTS -N2; done

repeated-write-lvar-wrong:
	ghc -O2 repeated-write-lvar-wrong.hs -rtsopts -threaded
	./repeated-write-lvar-wrong +RTS -N2

clean:
	rm *.hi *.o data-race-example ivar-example repeated-write-ivar repeated-write-lvar repeated-write-lvar-wrong
