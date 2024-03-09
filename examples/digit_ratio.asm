## EINGABE ##

# $0 ist die Eingabezahl
load int $0 18734
# $1 ist die gesuchte Ziffer
load int $1 4

## PROGRAMM ##

# $2 ist die Anzahl der gefunden Ziffern
load int $2 0
# $3 ist die Anzahl von Ziffern insgesamt
load int $3 0
load int $4 10

loop:
  modi $0 $4 $5
  eqi $5 $1 $5
  jnz $5 found
  jmp finally

found:
  load int $5 1
  addi $2 $5 $2

finally:
  load int $5 1
  addi $3 $5 $3
  dbg $0
  divi $0 $4 $0
  dbg $0
  # load int $5 0
  # eqi $0 $5 $5
  jez $0 done
  dbg $0
  jmp loop

done:
  convitof $2 $2
  convitof $3 $3
  divf $2 $3 $4
  dbg $4
