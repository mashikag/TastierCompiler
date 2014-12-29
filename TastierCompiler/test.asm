.names 4
.proc Main
.var 1 i
Main: Enter 1
Const 0
FJmp L$0
Const 1
Jmp L$1
L$0: Nop
Sto 0 0
Const 2
L$1: Nop
Sto 0 0
Load 0 0
Write
Const 5
Load 0 0
Equ
FJmp L$3
Const 5
Write
Jmp L$2
L$3: Nop
Const 7
Write
L$2: Nop
Const 5
Write
Leave
Ret
