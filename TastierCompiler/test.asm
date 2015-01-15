.names 9
.proc Main
Main: Enter 27
Const 5
Sto 0 1
Const 5
Sto 0 1
Sto 0 2
Sto 0 3
Sto 0 4
Sto 0 5
Sto 0 6
Sto 0 7
Sto 0 8
Sto 0 9
Sto 0 10
Sto 0 11
Sto 0 12
Sto 0 13
Sto 0 14
Sto 0 15
Sto 0 16
Sto 0 17
Sto 0 18
Sto 0 19
Sto 0 20
Sto 0 21
Sto 0 22
Sto 0 23
Sto 0 24
Sto 0 25
Sto 0 26
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
Const 2
Load 0 0
Equ
FJmp L$3
Const 2
Write
Jmp L$2
L$3: Nop
Const 5
Load 0 0
Equ
FJmp L$4
Const 5
Write
Jmp L$2
L$4: Nop
Const 7
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
Sto 0 8
Load 0 8
Write
Leave
Ret
