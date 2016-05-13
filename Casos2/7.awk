BEGIN {
    print "probando el exit"
};

$2 >= 3 ;
$2 < 3 { exit };
END { print "Se termina";
    exit;
    a = 2;
    b <= 4;
}
