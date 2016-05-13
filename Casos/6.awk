BEGIN {
    print "invierte el orden de todos los campos";
};

NF {linea = $NF ;
    for (i=NF-1; i>0; i--) 
	linea = linea . " " . $i ;
    print linea
}
