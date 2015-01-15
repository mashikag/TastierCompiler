.names 38
.proc Main
.var 1 x[4][4]
.var 1 x[4][3]
.var 1 x[4][2]
.var 1 x[4][1]
.var 1 x[4][0]
.var 1 x[3][4]
.var 1 x[3][3]
.var 1 x[3][2]
.var 1 x[3][1]
.var 1 x[3][0]
.var 1 x[2][4]
.var 1 x[2][3]
.var 1 x[2][2]
.var 1 x[2][1]
.var 1 x[2][0]
.var 1 x[1][4]
.var 1 x[1][3]
.var 1 x[1][2]
.var 1 x[1][1]
.var 1 x[1][0]
.var 1 x[0][4]
.var 1 x[0][3]
.var 1 x[0][2]
.var 1 x[0][1]
.var 1 x[0][0]
.var 1 x
.var 1 Rectangle.height
.var 1 Rectangle.width
Const 5
StoG 5
Const 5
StoG 5
StoG 6
StoG 7
StoG 8
StoG 9
StoG 10
StoG 11
StoG 12
StoG 13
StoG 14
StoG 15
StoG 16
StoG 17
StoG 18
StoG 19
StoG 20
StoG 21
StoG 22
StoG 23
StoG 24
StoG 25
StoG 26
StoG 27
StoG 28
StoG 29
StoG 30
Main: Enter 29
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
Const 999
Sto 0 28
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
LoadG 5
Sto 0 1
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
Load 0 28
Write
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
