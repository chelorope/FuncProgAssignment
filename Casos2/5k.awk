BEGIN {
    print "suma de todos los campos"
};

NF  { do {
	suma += $NF;
    } while (NF--)
};

END {print "Suma : ", suma}
