data-race-example:
	ghc -O2 data-race-example.hs -rtsopts -threaded
	while true; do ./data-race-example +RTS -N2; done

ivar-example:
	ghc -O2 ivar-example.hs -rtsopts -threaded
	while true; do ./ivar-example +RTS -N2; done

repeated-write-ivar:
	ghc -O2 repeated-write-ivar.hs -rtsopts -threaded
	while true; do ./repeated-write-ivar +RTS -N2; done

# This might produce a link-time error if you forget the "-threaded"!
repeated-write-lvar:
	ghc -O2 repeated-write-lvar.hs -rtsopts -threaded
	while true; do ./repeated-write-lvar +RTS -N2; done

clean:
	rm *.hi *.o data-race-example ivar-example repeated-write-ivar repeated-write-lvar
