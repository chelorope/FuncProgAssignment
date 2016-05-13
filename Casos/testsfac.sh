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

for i in {1..10}
	do
		./awki "$(cat $i.awk)" < $i.txt  >  sm$i.txt
		diff -b -E  sm$i.txt s$i.txt
	done
