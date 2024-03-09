load int $0 0
load int $1 1
load int $2 20

loop:
  addi $0 $1 $0
  dbg $0
  lti $0 $2 $3
  jnz $3 loop
