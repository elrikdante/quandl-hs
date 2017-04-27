default:
	stack ghc --resolver lts-6.2 -- -dcore-lint -ddump-simpl -j8 +RTS -H500m -K100m -N --RTS -O2 --make -Wall -fno-warn-orphans -rtsopts -with-rtsopts=-N -optl-pthread -threaded Main.hs -o Zillow
hood: 
	curl -v -o hood "http://static.quandl.com/zillow/hood_codes.csv"
.PHONY: hood
