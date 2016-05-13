# clean
rm awki-tests2.ltxt
rm awk-tests2.ltxt
rm awki
rm *.o
rm *.hi

# build
ghc -o awki Awki.hs

# run tests

progs[0]='{ i=3 }; {for( i=0; i < 7; i++) print "hola"}; { print i}'

IFS="" # to avoid spaces messing the array...

for prog in ${progs[@]}
do
  echo "running $prog ..."
  echo "------------------------------------------------------------------------------------------------------------------------------------------------------------------">> awk-tests2.ltxt
  echo "running $prog ..." >> awk-tests2.ltxt
  awk "$prog" sample.ltxt >> awk-tests2.ltxt
done

# cleanup
#rm awki-tests2.ltxt
#rm awk-tests2.ltxt
#rm awki
rm *.o
rm *.hi