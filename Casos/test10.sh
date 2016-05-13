cd "$(dirname "$0")"
# clean
rm awki-output.ltxt
rm awk-output.ltxt
rm awki
rm *.o
rm *.hi


# build
ghc -o awki Awki.hs

# run tests
		
		awk "$(cat 10.awk)" 10E.txt > sE10.ltxt
		./awki "$(cat 10.awk)" < 10E.txt  >  smE10.txt
