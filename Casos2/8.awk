BEGIN {
    print "probando las variables automáticas"
};

{ print NR, NF, $1, $2, $3, $4, $5 }
