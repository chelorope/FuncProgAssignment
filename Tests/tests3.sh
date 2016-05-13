# clean
rm awki-output3.ltxt
rm awk-output3.ltxt
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
  echo "------------------------------------------------------------------------------------------------------------------------------------------------------------------">> awki-output3.ltxt
  echo "running $prog ..." >> awki-output3.ltxt
  cat sample1.ltxt | ./awki "$prog" >> awki-output3.ltxt
done

# cleanup
#rm awki-output3.ltxt
#rm awk-output3.ltxt
#rm awki
rm *.o
rm *.hi