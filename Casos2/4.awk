BEGIN { 
    print "maximo y minimo de todos los campos"
};
NR == 1 {
    min = max = $1 + 0};
{ for (i=1; i<=NF; i++) 
      if ($i < min) { 
	  min = $i 
      } else if ($i > max) 
	  max = $i
};

END { 
    print "El minimo es: ", min; print "El maximo es: " , max
}
