.names 9
.proc Main
Main: Enter 7
Const 5
Sto 0 1
Const 0
Sto 0 2
Const 0
Sto 0 3
Const 0
Sto 0 4
Const 0
Sto 0 5
Const 0
Sto 0 6
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
L$2: Nop
Const 5
Write
Const 5
Const 80
WriteChar
Const 114
WriteChar
Const 111
WriteChar
Const 98
WriteChar
Const 97
WriteChar
Const 5
Sto 0 4
Load 0 4
Write
Leave
Ret
