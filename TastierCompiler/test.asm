.names 5
.proc Main
.proc SumUp
.var 1 i
SumUp: Enter 1
Const 0
Sto 0 0
L$0: Nop
LoadG 3
Const 0
Gtr
FJmp L$1
Load 0 0
LoadG 3
Add
Sto 0 0
LoadG 3
Const 1
Sub
StoG 3
Jmp L$0
L$1: Nop
Load 0 0
Write
Leave
Ret
Main: Enter 1
Const 0
FJmp L$2
Const 1
Jmp L$3
L$2: Nop
Sto 0 0
Const 2
L$3: Nop
Sto 0 0
Load 0 0
Write
Const 5
Load 0 0
Equ
FJmp L$5
Const 5
Write
Jmp L$4
L$5: Nop
Const 7
Write
L$4: Nop
Const 10
Sto 0 0
L$6: Nop
Load 0 0
Const 1
Sub
Load 0 0
Const 0
Gtr
FJmp L$7
Load 0 0
Write
Sto 0 0
Jmp L$6
L$7: Nop
Read
StoG 3
L$8: Nop
LoadG 3
Const 0
Gtr
FJmp L$9
Call 1 SumUp
Read
StoG 3
Jmp L$8
L$9: Nop
Const 5
Write
Leave
Ret
