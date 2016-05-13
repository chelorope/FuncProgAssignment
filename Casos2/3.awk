BEGIN { print "seleccion del ultimo campo" };
NF > 0 {print $NF}
