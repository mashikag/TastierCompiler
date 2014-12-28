.names 4
.proc Main
.var 1 i
Main: Enter 1
Const 5
Sto 0 0
Const 5
Load 0 0
Equ
L$1: FJmp
Const 5
Write
L$1: Nop
L$0: Nop
Const 5
Write
Leave
Ret
