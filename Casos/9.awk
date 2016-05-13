BEGIN {
    print "Suma o multiplica todos los campos de una fila"
};
$1 == "+" || $1 == "*" {
    por =  $1 == "*";
    result = 0;
    if (por) result = 1;
    while (NF>1) { 
	if (por) { 
	    result *= $NF
	} else 
	    result += $NF;
	--NF
    };
    print result
}
