jmp start

function:
  load int $0 1
  addi $e $0 $e
  jmpdy $f

start:
  load int $f :return
  load int $e 20
  jmp function
return:
  dbg $e
