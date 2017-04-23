default:
	stack ghc -- +RTS -N --RTS -O2 -threaded Main.hs -o Zillow
hood: 
	curl -v -o hood "http://static.quandl.com/zillow/hood_codes.csv"
.PHONY: hood
