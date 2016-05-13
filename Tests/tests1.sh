# clean
rm awki-output.ltxt
rm awk-output.ltxt
rm awki
rm *.o
rm *.hi

# build
ghc -o awki Awki.hs

# run tests

progs[0]='{ print }'
progs[1]='BEGIN { print "NR: ", NR, "NF: ", NF, "$0: ", $0, "$1: ", $1 }; END { print "NR: ", NR, "NF: ", NF, "$0: ", $0, "$1: ", $1 }'
progs[2]='{}'
progs[3]='1 > 0 { print }'
progs[4]='{ if (NF > max) max = NF }; END { print max }'
progs[5]='NF > 0'
progs[6]='{ if (NF > 0) print }'
progs[7]='{ nlines++ }; END { print nlines }'
progs[8]='{ nfields += NF }; END { print nfields }'
progs[9]='{ if (NR > 1) exit; print }; END { print "The end" }'
progs[10]='{ print NR, $0 }'
progs[11]='!(NR >= 5) { for (f=1; f <= 3; ++f) if ($f != "") print $f }'
progs[12]='{ line = $0 }; { print line }'
progs[13]='{ one = 1 }; { lines += one }; END { print lines }'
progs[14]='{ while (i < NF) i++ }; END { print i }'
progs[15]='{ i = 0; while (i < NF) i++ }; { f += i }; END { print f }'
progs[16]='{ print }; END { print "END" }; BEGIN { print "BEGIN" }'
progs[17]='END { print "end 1" }; BEGIN { print "begin 1" }; END { print "end 2" }; BEGIN { print "begin 2" }'

IFS="" # to avoid spaces messing the array...

for prog in ${progs[@]}
do
  echo "running $prog"
  echo "------------------------------------------------------------------------------------------------------------------------------------------------------------------">> awk-output.ltxt
  echo "------------------------------------------------------------------------------------------------------------------------------------------------------------------">> awki-output.ltxt
  echo "running $prog" >> awk-output.ltxt
  echo "running $prog" >> awki-output.ltxt
  awk "$prog" sample.ltxt >> awk-output.ltxt
  cat sample.ltxt | ./awki "$prog" >> awki-output.ltxt
  diff awk-output.ltxt awki-output.ltxt

done

# cleanup
#rm awki-output.ltxt
#rm awk-output.ltxt
#rm awki
rm *.o
rm *.hi