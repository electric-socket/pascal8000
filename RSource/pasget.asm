         TITLE 'PASCAL - PASGET ROUTINE'
PASGET   CSECT
*        IF A PASCAL STEP HAS BEEN DONE,
*        SAVE TTIMER RESULT AND CORE USAGE IN SCB
*        ENTRY: R8 --> CURRENT POSN IN PARM
*               R9 --> END OF PARM FIELD
*               R10 --> CURRENT SCB
*        FREEMAIN THE STACK ETC
*        GET NEXT STEPNAME
*        EXIT : R8, R10, UPDATED
*               DDNAME ROOT STORED
*               R0 IS ERROR NUMBER IF ANY
*               R15 IS ZERO TO FINISH UP, NON-ZERO TO PROCEED
         SPACE
PASGET   CSECT
         USING *,R15
         B     12(,R15)
         DC    AL1(7),CL7'PASGET'
         STM   R14,R12,12(R13)
         LR    R14,R13
         LA    R13,PASGSAVE
         ST    R13,8(,R14)
         ST    R14,4(,R13)
         LR    R12,R15
         USING PASGET,R12
         DROP  R15
         USING PASDATA,R11
         USING STEPS,R10
         L     R1,HEAD                 TEST IF 1ST STEP
         LTR   R1,R1
         BC    8,PASG5                 BR IF SO
         L     R0,STAKHEAD
         SR    R0,R1
         ST    R0,STPGM                SAVE PGM CODE SPACE
         L     R0,INTERVAL
         ST    R0,PASDTIM              INITIALISE CLOCK FOR 1ST STEP
         L     R0,TAIL
         S     R0,STAKHEAD
         ST    R0,STSTAK               INIT STSTAK IN CASE NONE FREE
         SPACE
*        NOW FIND END OF STACK AND START OF HEAP
         SR    R1,R1                   FOR TRT
         SR    R4,R4                   MODE SWITCH
         ST    R4,STFREE               INITIALISE
         ST    R4,STHEAP
         L     R6,STAKHEAD             PREPARE TO LOOK
SYSCON   EQU   1584
         LA    R6,SYSCON(,R6)          PAST SYSCON AREA
         SPACE
PASG1    L     R7,TAIL                 END
         SR    R7,R6                   LENGTH LEFT
         BC    12,PASG4                BR IF FIN
         BCTR  R7,0                    -1 FOR EX
         EX    R7,PASGTRT(R4)          SEARCH
         BC    7,PASG2                 BR IF FOUND
         N     R7,=A(255)
         LA    R6,1(R6,R7)
         B     PASG1
PASG2    LTR   R4,R4                   TEST MODE
         BC    8,PASG3                 BR IF LOOKING FOR FREE
         SR    R4,R4                   NON-FREE FOUND
         LR    R6,R1                   ADDR OF NON-FREE
         SR    R1,R5                   LENGTH OF FREE
         C     R1,STFREE               TEST VS PREVIOUS
         BC    4,PASG1                 CARRY ON IF LESS
         ST    R1,STFREE               SAVE THIS ONE
         LR    R1,R5
         S     R1,STAKHEAD
         ST    R1,STSTAK
         L     R1,TAIL
         SR    R1,R6
         ST    R1,STHEAP
         B     PASG1                   KEEP LOOKING
PASG3    LA    R4,6                    CHANGE MODE
         LR    R5,R1                   SAVE ADDR
         LR    R6,R1                   UPDATE PTR
         B     PASG1                   KEEP LOOKING
PASG4    LTR   R4,R4                   IF LOOKING FOR NON-FREE
         LR    R1,R6                   FAKE TRT RESULT AND
         BC    7,PASG2                 GO BACK AND PROCESS
         SPACE
         SR    R0,R0
         ST    R0,STLINK
         TTIMER
         ST    R0,STTIME
         SPACE
         ST    R0,PASDTIM              INITIALISE CLOCK FUNCTION
*        SEE IF ERROR REQUIRES US TO FINISH UP
         L     R6,4(,R13)              OLD SAVE
         L     R6,20(,R6)              OLD R0
         LTR   R6,R6                   TEST IF ERRORS
         BC    7,PASG5                 BR IF SO
*        DO NOT FREEMAIN IF ERRORS, PASTERM MAY DUMP VARIABLES
         L     R0,LENGTH
         L     R1,HEAD
         FREEMAIN R,LV=(0),A=(1)       FREE THE MEMORY
         SPACE
*        NOW LOOK FOR NEXT STEPNAME
         LA    R8,1(,R8)               PAST DELIM
PASG5    DS    0H
         SPACE
*        INITIALISE STD OUTPUT BLOCK AND BUFFER
         LA    R2,OUTBLK
         USING BLOCK,R2
         L     R3,BLKPS                START OF REC
         LH    R4,OUTDCB+82            LRECL
         AR    R4,R3                   END OF REC
         CLI   OUTDCB+36,X'80'         RECFM
         BC    10,PASG14               BR IF NOT V
         XC    0(4,R3),0(R3)           INITIALISE RDW
         LA    R3,4(0,R3)              POINT PAST IT
PASG14   STM   R3,R4,BLKPC             SAVE PTRS
         OI    BLKSTAT,BSPAGE          SET FOR NEW PAGE NEXT STEP
         SPACE
*        CLEAR LINE BUFFER
         SR    R4,R3
         BCTR  R4,0
         BCTR  R4,0
         MVI   0(R3),C' '
         EX    R4,PASGMVC
         DROP  R2
         SPACE
         L     R1,HEAD
         LTR   R1,R1                   TEST IF 1ST STEP
         BC    8,PASG6                 CARRY ON IF SO
         LTR   R0,R6                   TEST IF ERRORS
         BC    7,PASG5B                EXIT IF SO
         CR    R8,R9                   SEE IF ANY LEFT
         BC    4,PASG6                 BR IF SO
PASG5A   SR    R0,R0                   IND NO ERRORS AND RETURN
PASG5B   SR    R15,R15
         L     R13,4(,R13)
         STM   R15,R0,16(R13)
         LM    R14,R12,12(R13)
         BR    R14
PASG6    DS    0H
         LA    R2,7                    LOOK FOR 7-BYTE STEPNAME
         LR    R0,R9
         SR    R0,R8                   GET REM LENGTH
         BCTR  R0,0
         CR    R0,R2
         BC    10,PASG7                BR IF REM <7
         LR    R2,R0                   RESET LEN TO REM
PASG7    EX    R2,TRTX                 LOOK FOR NON-NATIONAL
         BC    7,PASG8                 BR IF FOUND
         CR    R0,R2                   SEE IF END
         LR    R1,R9
         BC    10,PASG8                BR IF END - OK
PASG75   LA    R0,ERRPARM              SET PARM FIELD ERROR
         B     PASG5B
PASG8    DS    0H
         SR    R1,R8                   GET LENGTH OF NAME
         BCTR  R1,0
         LTR   R2,R1
         BC    4,PASG75
         LA    R0,STEPSL
         GETMAIN R,LV=(0)
         ST    R1,STLINK
         LR    R10,R1
         MVI   STEPS,0
         MVC   STEPS+1(STEPSL-1),STEPS
         MVC   STDDN,=CL8' '
         EX    R2,MVCX
         LA    R8,1(R2,R8)
         MVC   INDDNAM,STDDN
         LA    R2,INDDNAM+1(R2)
         MVI   0(R2),C'I'
         CR    R8,R9                   SEE IF END
*        NOW FIND IF =NN IS PRESENT
         L     R0,NFSBLKS              DEFAULT # OF 2K BLOCKS FREE
         BC    10,PASG9
         CLI   0(R8),C'='
         BC    7,PASG9
         LA    R8,1(,R8)               PAST =
         SR    R0,R0                   FOR VALUE
PASG85   CR    R8,R9
         BC    10,PASG9
         SR    R1,R1                   FOR DIGIT
         IC    R1,0(,R8)
         S     R1,=A(C'0')
         CL    R1,=A(9)
         BC    2,PASG9
         MH    R0,=Y(10)
         ALR   R0,R1
         LA    R8,1(,R8)
         B     PASG85
PASG9    SLL   R0,11                   * 2K
         LR    R2,R0
         GETMAIN R,LV=(0)
         LR    R3,R1                   SAVE ADDR
         GETMAIN VC,A=HEAD,MF=(E,GML)
         LTR   R15,R15
         BC    8,PASG11
         LA    R0,ERRCORE              SET NO CORE
         B     PASG5B
PASG11   DS    0H
         FREEMAIN R,A=(R3),LV=(R2)
         SPACE
*        SET INITIALISING PATTERN IN GETMAINED AREA
         L     R1,HEAD
         L     R2,LENGTH
         LR    R6,R1                   SAVE IN R6
         LA    R7,0(R1,R2)             TAIL IN R7
         S     R7,=A(64)               EMERGENCY ROOM FOR SAVE REGS
         ST    R7,TAIL
         BCTR  R2,0                    -1
         MVI   0(R1),PAT               1ST BYTE
PASG12   DS    0H
         BCTR  R2,0                    - FOR EX
         EX    R2,MVCXX                PROPAGATE BYTE
         LA    R3,255                  MASK
         NR    R3,R2                   SEE HOW MUCH MOVED
         XR    R2,R3                   AMT LEFT
         BC    8,PASG13                EXIT IF FINISHED
         LA    R1,1(R1,R3)             POINT TO NEXT PART
         B     PASG12                  LOOP
MVCXX    MVC   1(0,R1),0(R1)           MVC FOR EX TO PROPAGATE PAT
PASG13   DS    0H
         SPACE
         LA    R15,4                   SET TO CONTINUE
         L     R13,4(,R13)
         ST    R15,16(,R13)
         STM   R6,R10,20+6*4(R13)
         LM    R14,R12,12(R13)
         BR    R14                     RETURN
         SPACE
PASGMVC  MVC   1(0,R3),0(R3)
         SPACE 3
GML      GETMAIN VC,LA=RANGE,MF=L
RANGE    DC    A(8,X'FFFFF8')
PAT      EQU   X'7F'
TRTF     DC    (PAT)AL1(0),AL1(1),(256-PAT-1)AL1(0)
TRTNF    DC    (PAT)AL1(1),AL1(0),(256-PAT-1)AL1(1)
TRTNAT   DC    (C'$')AL1(1),AL1(0)
         DC    (C'#'-C'$'-1)AL1(1),AL1(0,0)
         DC    (C'A'-C'@'-1)AL1(1),9AL1(0)
         DC    7AL1(1),9AL1(0),8AL1(1),8AL1(0)
         DC    6AL1(1),10AL1(0),6AL1(1)
PASGTRT  TRT   0(0,R6),TRTF
         TRT   0(0,R6),TRTNF
TRTX     TRT   0(0,R8),TRTNAT
MVCX     MVC   STDDN(0),0(R8)
PASGSAVE DC    18A(0)
         LTORG
         EJECT
PASDATA  DSECT
         PASDATA
         SPACE 3
STEPS    DSECT
         STEPS
         SPACE 2
         ERRNOS
         BLOCK
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
