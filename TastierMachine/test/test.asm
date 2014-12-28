Main: Enter 0
;; sum up the numbers from one to ten
Const 1
StoG 4
;; address 0 contains the increment
Const 0
StoG 5
;; address 1 contains the sum
LOOPSTART: LoadG 4
           Dup
           LoadG 5
           Add
           StoG 5
           ;; top of stack is the number we added
           Const 1
           Add
           Dup
           StoG 4
           ;; store the new increment, top of stack is the increment
           Const 11
           Equ
           FJmp LOOPSTART
;; if the increment has not reached 10, jump back to LOOPSTART
;; finally, write out the total
LoadG 5
Write
Halt
