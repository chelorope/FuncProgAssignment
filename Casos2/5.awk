BEGIN {
    print "suma de todos los campos"
};

NF  { do {
	suma += $NF;
	NF--
    } while (NF)
};

END {print "Suma : ", suma}
