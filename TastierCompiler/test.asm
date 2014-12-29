.names 4
.proc Main
.var 1 i
Main: Enter 1
Const 5
Sto 0 0
Const 5
Load 0 0
Equ
FJmp L$1
Const 5
Write
Jmp L$0
L$1: Nop
Const 6
Load 0 0
Equ
FJmp L$2
Const 6
Write
L$2: Nop
Const 7
Write
L$0: Nop
Leave
Ret
