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
*              SET REGISTERS   R8-->  NEXT POSN IN PARM FIELD
*                              R9-->  END OF PARM FIELD
*                              R10--> LINK TO NEXT SCB
         SPACE
         SR    R0,R0                   INDICATE NO ERRORS YET
PA1      DS    0H
         L     R15,=V(PASGET)          CLEAN UP LAST STEP,
         BALR  R14,R15                 INITIALISE FOR NEXT
*        GETS NEXT STEPNAME IF ANY TO INDDNAME AND INDCB
*        GETS MEMORY REQD AND INITIALISES IT WITH PAT
*        UPDATES PARM FIELD PTR REGS
*        INITIALISES R6 TO HEAD AND RNP(=7) TO TAIL
*        RETURNS R0  # 0 TO FINISH WITH ERROR IND
*                R15 = 0 TO FINISH W/OUT ERROR
*                R15 # 0 TO CONTINUE WITH NEXT STEP
         LTR   R0,R0                   ERROR INDICATED?
         BC    7,PA10                  FINISH IF SO
         LTR   R15,R15                 FINISH UP REQD?
         BC    8,PA10                  BR IF SO
         SPACE 2
*        CONTINUE WITH NEXT STEP
         L     R15,=V(PASLOAD)         LOADS USER CODE TO AREA STARTING
         BALR  R14,R15                 AT HEAD, R6 UPDATED TO --> STACK
         LTR   R0,R0                   LOAD OK?
         BC    7,PA10                  FINISH UP IF NOT
         SPACE 2
         L     R15,=V(PASRUN)          ENTER USER PGM
         BALR  R14,R15
         SPACE
         B     PA1                     FINISH STEP AND EXIT IF ERROR
*                                      OTHERWISE PREPARE NEXT STEP
         SPACE 3
PA10     DS    0H
         L     R15,=V(PASTERM)         EXIT VIA TERMINATION
         BALR  R14,R15
         SPACE 3
         LTORG
         EJECT
PASCSAVE DC    18A(0)                  SAVE AREA
PASDATA  DC    0D'0'                   START OF COMMON AREA
         ENTRY PASDATA
         ENTRY INDDNAM,OUTDCB,OUTBLK
         ENTRY PASFLINK                LINK PTR FOR DCB CHAIN
         ENTRY SPIEOPSW
         PASDATA
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
         END
