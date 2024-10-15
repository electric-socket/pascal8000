         TITLE 'PASCAL MAIN PROGRAM'
PASCAL   CSECT
         USING *,R15
         B     *+12
         DC    AL1(7),CL7'PASCAL'
         STM   R14,R12,12(R13)
         LR    R14,R13
         LR    R12,R15
         DROP  R15
         USING PASCAL,R12
         LA    R13,PASCSAVE
         ST    R13,8(,R14)
         ST    R14,4(,R13)
         LA    R11,PASDATA
         SPACE 2
*        INITIALISE SYSTEM
         L     R15,=V(PASINIT)         OPEN SYSPRINT
         BALR  R14,R15                 INITIALISE TTIMER
*        GETS MEMORY REQD AND INITIALISES IT WITH PAT
*        INITIALISES STACK WITH PROC TABLE AND VALUE STMNTS
         LTR   R0,R0                   LOAD OK?
         BC    7,PA10                  FINISH UP IF NOT
         SPACE 2
         L     R15,=V(PASRUN)          ENTER USER PGM
         BALR  R14,R15
         SPACE
PA10     DS    0H
         L     R15,=V(PASTERM)         EXIT VIA TERMINATION
         BALR  R14,R15
         SPACE 3
         LTORG
         EJECT
PASCSAVE DC    18A(0)                  SAVE AREA
         ENTRY PASDATA,SAVEAREA,PASFLINK,OUTBLK
         ENTRY SPIEOPSW
PASDATA  DC    0D'0'                   START OF COMMON AREA
         PASLDATA
         SPACE 2
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
RSTACK   EQU   8
         END
