default:
	stack ghc -- -j8 +RTS -H128m -K64m -N --RTS -O2 --make -Wall -fno-warn-orphans -rtsopts -with-rtsopts=-N -optl-pthread -threaded Main.hs -o Zillow
hood: 
	curl -v -o hood "http://static.quandl.com/zillow/hood_codes.csv"
.PHONY: hood
