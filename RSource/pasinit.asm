         TITLE 'PASCAL - INITIALISE PASCAL SYSTEM'
PASINIT  CSECT
*        ON ENTRY, R11 --> COMMON DATA AREA CALLED PASDATA
*        THIS ROUTINE OPENS SYSPRINT, DOES STIMER TO INITIALISE TTIMER
*        AND SETS UP R8, R9 AND R10 TO
*        R8    CURRENT POSN IN PARM FIELD
*        R9    END OF PARM FIELD
*        R10   PTR TO (NULL) STEP CONTROL BLOCK
         SPACE 2
         USING *,R15
         B     12(,R15)
         DC    AL1(7),CL7'PASINIT'
         STM   R14,R12,12(R13)         SAVE
         LR    R14,R13
         LA    R13,PASISAVE
         ST    R14,4(,R13)
         ST    R13,8(,R14)
         LR    R12,R15
         USING PASINIT,R12
         USING PASDATA,R11
         DROP  R15
         LA    R10,OUTDCB
         USING DFCB,R10
         LA    R0,INITEXL
         ST    R0,DCBEXLST
         OPEN  ((R10),OUTPUT)          OPEN SYSPRINT
         TM    DCBOFLGS,DCBOFOPN
         BC    1,PASI1                 BR IF OK
         WTO   'UNABLE TO OPEN SYSPRINT',ROUTCDE=11,DESC=7
         L     R13,4(,R13)             RETURN
         L     R13,4(,R13)              RIGHT
         LM    R14,R12,12(R13)           BACK
         LA    R15,8                      TO
         BR    R14                         SYSTEM WITH CC 8
         SPACE
PASI1    DS    0H
         OI    BLKSTAT-BLOCK+OUTBLK,BSWRITE+BSSTD SET WRITE/STD FILE
         EJECT
*        INITIALISE THE TIMER
         SPACE
PASI5    DS    0H
*        FIRST FIND OUT HOW MUCH TIME LEFT
         L     R1,16                   CVT
         L     R1,0(,R1)
         L     R1,4(,R1)               TCB
         NOP   0
         NOP   0
         NOP   0
         L     R1,132(,R1)             INIT TCB
*        VS1 SYSTEMS HAVE A ZERO INITIATOR TCB FIELD
         LTR   R1,R1                   TEST IF VS1-TYPE
         BC    8,PASIST1               ABANDON TIMER SETUP IF SO
         L     R1,120(,R1)             TQE
         CLI   0(R1),X'14'             IS IT OK
         BC    7,PASIST1               BR IF NOT
         L     R1,20(,R1)              TIME LEFT IN TU
         S     R1,=A(10*1000000/2604*100) SUBTR ABOUT 10 SECS
         ST    R1,INTERVAL
PASIST1  L     R0,=V(TIMEX)
         STIMER TASK,(R0),TUINTVL=INTERVAL
         SPACE
*        INITIALISE PARM FIELD REGISTERS
         DROP  R10
         LA    R10,PASDLINK            INITIALISE R10
         SR    R8,R8
         SR    R9,R9
         L     R1,4(,R13)              PREV SAVE
         L     R1,24(,R1)              INITIAL R1
         LTR   R1,R1
         BC    8,PASI6                 BR IF NO PARM PTR
         L     R1,0(,R1)               POINT TO Y(LENGTH),C'PARMS'
         LH    R9,0(,R1)               GET LENGTH
         LTR   R9,R9                   BR IF NONE
         BC    8,PASI6
         LA    R8,2(,R1)               START OF PARM FIELD
         AR    R9,R8                   END OF PARM FIELD
         BAL   R14,NUM
         ST    R0,NTRACE1              OVERRIDE DFAULT NTRACE1
         BAL   R14,NUM
         ST    R0,NTRACE2
PASI6    DS    0H
         L     R13,4(,R13)             OLD SAVE
         STM   R8,R10,20+8*4(R13)      SET CALLERS REGS
         LM    R14,R12,12(R13)         RESTORE
         BR    R14                     RETURN
         SPACE
NUM      TM    0(R8),C'0'              TEST NUMERIC
         BC    14,NUM2                 RETURN +4 IF NOT
         SR    R4,R4
         SR    R0,R0
NUM1     IC    R4,0(,R8)
         SH    R4,=Y(C'0')
         CL    R4,=A(9)
         BC    2,NUM3
         MH    R0,=Y(10)
         AR    R0,R4
         LA    R8,1(,R8)
         CR    R8,R9
         BC    10,PASI6
         B     NUM1
NUM2     LA    R14,4(,R14)
NUM3     CLI   0(R8),C','
         BCR   7,R14
         LA    R8,1(,R8)
         CR    R8,R9
         BC    10,PASI6
         BR    R14
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         SPACE
INITEXL  DC    0A(0),X'85',AL3(DCBEXIT)
DCBEXIT  DS    0H
         USING DFCB,R1
         SPACE
*        ENSURE RECFM IS GOOD
         CLI   DCBRECFM,0
         BC    7,DCBE4                 ASSUME ANYTHING IS OK
         MVI   DCBRECFM,X'44'          SET VA DEFAULT
DCBE4    DS    0H
         SPACE
*        ENSURE LRECL IS GOOD
         LH    R0,DCBLRECL
         LTR   R0,R0                   TEST IF LRECL SPEC
         BC    7,DCBE3                  BR IF SO
         LA    R0,137                  SET 137 DEFAULT
         TM    DCBRECFM,X'80'          UNLESS RECFM F OR U
         BC    8,DCBE2                 WHEN USE 133
         LA    R0,133
DCBE2    STH   R0,DCBLRECL
DCBE3    DS    0H
         SPACE
*        ENSURE BLKSI IS GOOD
         LH    R0,DCBBLKSI
         LTR   R0,R0
         BCR   7,R14                   RETURN IF ALREADY SET
         LH    R3,DCBLRECL             ELSE GET LRECL
         TM    DCBRECFM,X'10'
         BC    8,DCBE49                BR IF NOT BLOCKED
         LR    R0,R3                   GET LRECL MULTIPLE
         LA    R3,2000                 NEAR 2000
         SR    R2,R2
         DR    R2,R0
         LA    R2,1(,R2)
         MR    R2,R0
DCBE49   DS    0H
         CLI   DCBRECFM,X'80'          TEST IF V
         BC    10,DCBE5                BR IF NOT
         LA    R3,4(,R3)               ADD 4 FOR BDW IF RECFM=V
DCBE5    STH   R3,DCBBLKSI             SAVE IN BLKSI
         BR    R14
         DROP  R1
         EJECT
PASISAVE DC    18A(0)
         LTORG
PASDATA  DSECT
         PASDATA
         EJECT
         BLOCK
         SPACE
         PRINT NOGEN
DFCB     DCBD  DSORG=QS,DEVD=TA
DFCB     EQU   IHADCB
         END
