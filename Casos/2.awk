BEGIN { print a + 0, a . "algo";
      a = 1 ; print a ; a = a + 3 ; print a;
      a = 15; print a; print a++; print ++a;
      a = 12;  print a; a += 10; print a ; print a -= 10;
      a = 55;  print a++ , ++a , --a, a--, a *= 11;
      a = 55; print a + b, a + (b = 10), b + (a += 5), a, b 
}
