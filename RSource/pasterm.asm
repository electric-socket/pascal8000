         TITLE 'PASTERM - PASCAL TERMINATION ROUTINE'
*        ROUTINE PRINTS STATISTICS FOR PASCAL STEPS
*        FOLLOWED BY ERROR MESSAGE AND TRACEBACK IF APPROPRIATE
         SPACE 2
         MACRO
&L       PR    &MSG,&OFFSET
         LCLC  &LQ
         LCLA  &MSGL,&OFFSL
         AIF   ('&OFFSET' EQ '').B
&OFFSL   SETA  &OFFSET
.B       ANOP
         AIF   ('&MSG'(1,1) NE '''').A
&MSGL    SETA  K'&MSG-2
&L       MVC   &OFFSL.(&MSGL,R1),=C&MSG
         LA    R1,&OFFSL+&MSGL.(,R1)
         MEXIT
.A       ANOP
&LQ      SETC  'L'''
&L       MVC   &OFFSL.(&LQ.&MSG,R1),&MSG
         LA    R1,&OFFSL+&LQ.&MSG.(,R1)
         MEND
         SPACE
         MACRO
&L       MSG   &M
         LCLA  &K
&K       SETA  K'&M-3
MSG&L    DC    AL1(&K),C&M
         MEND
         SPACE
         MACRO
         LAMSG
         LCLA  &I
.A       ANOP
&I       SETA  &I+1
         AIF   ('&SYSLIST(&I)' EQ '').B
         LA    R4,MSG&SYSLIST(&I)
         AGO   .A
.B       ANOP
         MEND
         SPACE 2
         EJECT
PASTERM  CSECT
         USING *,R12
         B     12(,R15)
         DC    AL1(7),CL7'PASTERM'
         STM   R14,R12,12(R13)
         LR    R12,R15
         USING PASDATA,R11
         LR    R14,R13
         LA    R13,SAVETERM
         USING SAVETERM,R13
         ST    R13,8(,R14)
         ST    R14,4(,R13)
         STM   R0,R1,ERRN              SAVE ERR NO AND EXTENDED INFO
         L     R1,OUTBLK+BLKPC-BLOCK
         SPACE 2
*        PRINT SUMMARY OF STORAGE AND TIME USAGE
         BAL   R10,PRSUMM
         SPACE 2
*        PRINT TERMINATION MESSAGE(S)
         BAL   R10,PRTERM
         SPACE 2
*        EXIT IF NO ERRORS
         L     R0,ERRN
         CH    R0,=Y(ERRTOVL)          TEST IF TRACEBACK REQD
         BC    2,PAST1                 BR IF SO
         LTR   R0,R0                   TEST IF ERRORS
         L     R13,4(,R13)
         L     R13,4(,R13)
         LM    R14,R12,12(R13)
         LA    R15,0                   CAREFUL OF COND CODE ..
         BCR   8,R14
         LA    R15,8                   SET COND CODE TO IND ERRORS
         BR    R14
         SPACE 2
*        PRINT TRACEBACK AND VARIABLE DUMP
PAST1    DS    0H
         ST    R1,WORK
         LA    R15,SPIEINT
         SPIE  (15),MF=(E,SPIEL)       TRAP INTERRUPTS DURING TRACEBACK
         L     R1,WORK                 RESTORE
         SPACE
         BAL   R10,PRTRACE
         SPACE 2
*        CLOSE FILES LEFT OPEN, SCRATCH LOCAL FILES
         BAL   R10,CLOSCR
         SPACE 2
*        EXIT  WITH COND CODE
         L     R13,4(,R13)
         L     R13,4(,R13)
         LM    R14,R12,12(R13)
         LA    R15,8
         BR    R14
         EJECT
*        PRINT PASCAL EXECUTION SUMMARY
         PRINT NOGEN
         SPACE
PRSUMM   MVI   0(R1),C'1'              PAGE
         PR    'PASCAL EXECUTION SUMMARY',48
         BAL   R14,PRNT
         PR    '------ --------- -------',48
         BAL   R14,PRNT
         MVI   0(R1),C'0'              DOUBLE SPACE
         PR    'STORAGE SUMMARY IN DECIMAL(HEX) BYTES',36
         BAL   R14,PRNT
         PR    'STEP',1
         PR    'TIME IN SECONDS',93
         BAL   R14,PRNT
         PR    'PROGRAM',19
         PR    'STACK',14
         PR    'HEAP',16
         PR    'UNUSED',15
         BAL   R14,PRNT
         SPACE
         L     R8,PASDLINK             1ST STEP DESCRIPTOR
         USING STEPS,R8
PRSL     DS    0H                      PRINT STEP LINE LOOPS TO HERE
         LTR   R8,R8
         BCR   8,R10                   RETURN WHEN FINISHED
         MVI   0(R1),C'0'              DBL SPACE
         PR    STDDN,1                 STEPNAME
         LA    R1,2(,R1)
         L     R0,STPGM
         BAL   R9,PRDH                 PRINT DEC(HEX) IN 20 BYTES
         L     R0,STSTAK
         BAL   R9,PRDH
         L     R0,STHEAP
         BAL   R9,PRDH
         L     R0,STFREE
         BAL   R9,PRDH
         L     R15,INTERVAL            TIME AT START OF STEP
         L     R14,STTIME              TIME AT END
         ST    R14,INTERVAL            RESET FOR NEXT STEP
         SR    R15,R14                 GET ELAPSED
         SR    R14,R14
         M     R14,=A(2604)            HUNDREDTHS OF U SEC
         D     R14,=A(1000000)         HUNDREDTHS OF SECONDS
         CVD   R15,WORK
         MVC   0(17,R1),=X'40202020202020202020202021204B2020'
         ED    0(17,R1),WORK
         LA    R1,17(,R1)
         BAL   R14,PRNT
         L     R8,STLINK               NEXT STEP BLOCK
         B     PRSL
         DROP  R8
         SPACE 2
PRDH     DS    0H                      PRINT R0 IN DEC(HEX) IN 20 BYTES
         MVC   0(12,R1),=X'402020202020202020202120'
         CVD   R0,WORK
         ED    0(12,R1),WORK+2
         ST    R0,WORK
         UNPK  13(7,R1),WORK+1(4)
         TR    14(6,R1),HEXTBL
         MVI   12(R1),C'('
         MVI   19(R1),C')'
         LA    R1,20(,R1)
         BR    R9
HEXTBL   EQU   *-240
         DC    C'0123456789ABCDEF'
         EJECT
*        PRINT TERMINATION ERROR MESSAGE
         ERRNOS
PRTERM   DS    0H
         L     R2,ERRN                 GET ERROR NUMBER
         LTR   R2,R2                   TEST IF ANY
         BCR   8,R10                   RETURN IF NONE
         MVI   0(R1),C'-'
         PR    'PASCAL TERMINATION LOG',49
         BAL   R14,PRNT
         PR    '------ ----------- ---',49
         BAL   R14,PRNT
         PR    '0 *** ERROR *** '
         EX    0,GETMSG(R2)            GET THE MESSAGE ADDR
         SR    R3,R3
         IC    R3,0(,R4)               L-1 OF MESSAGE
         EX    R3,MVCMSG               TO PRINT LINE
         LA    R1,11+1(R3,R1)          PAST MESSAGE
         L     R0,ERRINFO              RETN CODE REQD?
         CH    R2,=Y(ERRSVC32)
         BC    4,PRTERM1               BR IF NOT
         PR    'RETURN CODE',1
         BAL   R14,PRHEX3              PRINT 3 HEX DIGITS
PRTERM1  DS    0H
         BAL   R14,PRNT
         BR    R10
         SPACE
PRHEX3   ST    R0,WORK                 PRINT 3 HEX DIGS - 012 (HEX)
         UNPK  0(5,R1),WORK+2(3)
         TR    1(3,R1),HEXTBL
         MVI   0(R1),C' '
         PR    ' (HEX)',4
         BR    R14
         SPACE
MVCMSG   MVC   11(0,R1),1(R4)
         SPACE
         DC    0A(0)
GETMSG   EQU   *-4
         LAMSG OBJ,COMT,CORE,PARM,                                     X
               TOVL,                                                   X
               CASE,RANGE,PNTR,STACK,TOVR,LFDD
         BAL   R14,PRHALT
         BAL   R14,PRINTP              INT POWER ERROR
         BAL   R14,GETFILN
         BAL   R14,GETFILN
         BAL   R14,GETFILN
         BAL   R14,GETFILN
         BAL   R14,GETFILN
         BAL   R14,GETFILN
         BAL   R14,GETFILN
         BAL   R14,GETFILN
         BAL   R14,GETFILN
         BAL   R14,FUNMSG
         BAL   R14,FUNMSG
         BAL   R14,FUNMSG
         BAL   R14,FUNMSG
         BAL   R14,FUNMSG
         BAL   R14,FUNMSG
         LAMSG SVC32,SCR
         B     PRINTM                  PRINT INTERRUPT MESSAGE
         SPACE
PRHALT   LR    R15,R1
         SH    R15,=Y(15)
         MVC   1(14,R15),0(R15)        BLANK OUT *** ERROR ***
         LAMSG HALT
         BR R14
         SPACE
GETFILN  DS    0H
         PR    'FILE "',10
         L     R3,ERRINFO              ADDR OF FILE NAME
         MVC   0(8,R1),0(R3)
         MVC   8(2,R1),=C'",'
GETFILN1 DS    0H
         EX    0,GETIOMSG(R2)
         SR    R0,R0
         ST    R0,ERRINFO
         BR    R14
         SPACE
PRINTP   MVC   1(15,R1),=X'202020202020202020202020202120'
         L     R0,ERRINFO              NEG VALUE
         LCR   R0,R0                   +VE
         CVD   R0,WORK
         LR    R15,R1                  SAVE ACROSS EDMK
         LA    R1,15(,R1)              BACKSTOP
         EDMK  0(16,15),WORK
         BCTR  R1,0                    SPOT FOR -
         MVI   0(R1),C'-'
         LA    R1,6(,R15)
         B     GETFILN1
         SPACE
FUNMSG   DS    0H
         PR    'ARGUMENT OUT OF RANGE FOR FUNCTION '
         B     GETFILN1
         SPACE
GETIOMSG EQU   *-ERRINTP
         LAMSG INTP,OPN,RECFM,LRECL,                                   X
               IOE,EOF,MR,MW,NO,RINH,                                  X
               SIN,COS,EXP,SQRT,LOG,ATAN
         SPACE
OBJ      MSG   'OBJECT FILE ERROR'
COMT     MSG   'COMPILE TIME ERROR(S)'
CORE     MSG   'INSUFFICIENT STORAGE AVAILABLE'
PARM     MSG   'INVALID PARM FIELD'
TOVL     MSG   'TIME LIMIT EXCEEDED'
         SPACE
CASE     MSG   'UNDEFINED CASE LABEL'
RANGE    MSG   'VALUE OUT OF RANGE'
PNTR     MSG   'POINTER VALUE INVALID'
STACK    MSG   'STACK OVERFLOW'
MSGTOVR     EQU   MSGTOVL
LFDD     MSG   'LOCAL FILE DD STATEMENT MISSING OR INVALID'
HALT     MSG   'HALT INSTRUCTION'
         SPACE
INTP     MSG   'IS EXPONENT OF INTEGER EXPRN (MUST BE +VE)'
OPN      MSG   'OPEN FAILED'
RECFM    MSG   'RECFM MUST BE F OR FB'
LRECL    MSG   'RECORD LENGTH MISMATCH'
IOE      MSG   'I/O ERROR'
EOF      MSG   'GET OR READ ISSUED AFTER END OF DATA'
MR       MSG   'GET OR READ WAS NOT PRECEDED BY RESET'
MW       MSG   'PUT OR WRITE WAS NOT PRECEDED BY REWRITE'
NO       MSG   'READ FORMAT ERROR - DIGIT EXPECTED'
RINH     MSG   'RESET OR REWRITE WAS USED FOR STD FILE'
SIN      MSG   'SIN'
COS      MSG   'COS'
EXP      MSG   'EXP'
SQRT     MSG   'SQRT'
LOG      MSG   'LN'
ATAN     MSG   'ARCTAN'
SVC32    MSG   'LOCAL FILE ALLOCATION FAILED - SVC 32'
SCR      MSG   'SCRATCH OF LOCAL FILE FAILED - SCRATCH'
         EJECT
*        PRINT INTERRUPT MESSAGE
PRINTM   PR    'PROGRAM INTERRUPT',11
         SR    R2,R2
         IC    R2,SPIEOPSW+3           INT CODE
         C     R2,=A(15)
         BC    2,PRM1                  NO MORE MESSAGE IF UNKNOWN INT
         SLL   R2,2
         EX    0,GETIMSG-4(R2)
         MVI   2(R1),C'-'
         IC    R2,0(,R4)
         EX    R2,MVCIMSG
         AR    R1,R2
         PR    'EXCEPTION',7
PRM1     BAL   R14,PRNT
         SPACE
         PR    '0OLD PSW:'
         LA    R1,2(,R1)
         LA    R2,SPIEOPSW
         BAL   R14,PRHEX8
         BAL   R14,PRHEX8
         BAL   R14,PRNT
         SPACE
         PR    '0REGISTERS:'
         LM    R2,R3,SPIEREGS          OLD R14-R15
         MVC   SPIEREGS(56),SPIEREGS+8 REGS 0-13
         STM   R2,R3,SPIEREGS+56
         LA    R2,SPIEREGS             REGS NOW IN ORDER 0 TO 15
         BAL   R8,PR8HEX8
         LA    R1,11(,R1)
         BAL   R8,PR8HEX8
         SPACE
         BR    R10
         SPACE
MVCIMSG  MVC   5(0,R1),1(R4)
GETIMSG  DS    0H
         LAMSG I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I13,I14,I15
I1       MSG   'OPERATION'
I2       MSG   'PRIVILEGED OPERATION'
I3       MSG   'EXECUTE'
I4       MSG   'PROTECTION'
I5       MSG   'ADDRESSING'
I6       MSG   'SPECIFICATION'
I7       MSG   'DATA'
I8       MSG   'FIXED-PT OVERFLOW'
I9       MSG   'FIXED-PT DIVIDE'
I10      MSG   'DECIMAL OVERFLOW'
I11      MSG   'DECIMAL DIVIDE'
I12      MSG   'EXPONENT OVERFLOW'
I13      MSG   'EXPONENT UNDERFLOW'
I14      MSG   'SIGNIFICANCE'
I15      MSG   'FLOATING-PT DIVIDE'
         SPACE
PRHEX8   DS    0H                      PRINT HEX, 8 CHARS
         LA    R0,PRHEX81
         ST    R0,SPIEADR
         UNPK  1(9,R1),0(5,R2)
         BCR   15,0                    CATCH ANY IMPRECISE INT HERE
         TR    1(8,R1),HEXTBL
         MVI   9(R1),C' '
PRHEX81  DS    0H
         XC    SPIEADR,SPIEADR         CLR ADR AGAIN
         LA    R1,9(,R1)
         LA    R2,4(,R2)
         BR    R14
         SPACE
PR4HEX8  DS    0H                      PRINT 4 WORDS, 8 CHAR HEX
         BAL   R14,PRHEX8
         BAL   R14,PRHEX8
         BAL   R14,PRHEX8
         BAL   R14,PRHEX8
         BR    R9
         SPACE
PRLS     DS    0H                      PRINT A LINE OF STORAGE
         LR    R0,R2                   ADDRESS
         BAL   R14,PRHEX6
         MVI   0(R1),C':'
         LA    R1,3(,R1)
         SPACE
PR8HEX8  DS    0H                      PRINT 8 WORDS, 8 CHAR HEX
         BAL   R9,PR4HEX8
         LA    R1,1(,R1)
         BAL   R9,PR4HEX8
         BAL   R14,PRNT
         BR    R8
         EJECT
*        ROUTINE TO PRINT THE TRACEBACK AND POST-MORTEM DUMP INFO
PRTRACE  DS    0H
         ST    R10,R10S
         LA    R0,STKCORR
         ST    R0,SPIEADR              SET UP TRAP FOR PGM INT
         L     R0,ERRN                 ERROR NUMBER
         CH    R0,=Y(ERRINT)           SEE IF INTERRUPT TYPE
         BC    8,PRTI                  HANDLE SEPARATELY IF SO
         PR    '0PROGRAM TERMINATED AT OFFSET'
         L     R8,4(,R13)
         L     R8,20+R2*4(,R8)
PRTR00   DS    0H
         C     R8,STAKHEAD             SEE IF IN PASCAL CODE YET
         BC    8,PRTR6                 FINISH IF NOT
         ST    R8,OLDSTK
         L     R7,9*4(,R8)             RETURN ADDR FROM CALL
*        R8 CONTAINS SP VALUE AT CALL TO RUNTIME SYSTEM
PRTR0    DS    0H
         L     R0,STAKHEAD             MAIN STACK AND SYSCON AREA ADDR
         ST    R0,4(,R8)               SAVE IN CASE IT WAS LEVEL 1 CALL
         L     R6,14*4(,R8)            CODE BASE OF CALLING PROC
         LR    R0,R7
         SR    R0,R6
         BAL   R14,PRHEX6              6 BYTE HEX OFFSET VALUE
         PR    'IN SEGMENT',1
         LR    R15,R6                  CODE BASE FOR PROC
         BAL   R14,PROCNAME            GET PROC NAME
         BAL   R14,PRNT                PRINT THE LINE
         SPACE
*        NOW PRINT NTRACE1 INNER CALLS AND NTRACE2 OUTER ONES
         L     R10,NTRACE1
PRTR1    BAL   R9,PRTRS1               PRINT ONE CALL LEVEL
         BAL   R9,PRTRS2               ADVANCE TO NEXT LEVEL
         BC    8,PRTR6                 EXIT IF FINISHED
         BCT   R10,PRTR1               LOOP FOR NTRACE CALLS
         LR    R4,R8                   SAVE STK ADDR
         L     R10,NTRACE2             GET NUMBER OF OUTER PROCS REQD
PRTR2    LR    R3,R8                   SAVE PTR
         BAL   R9,PRTRS2               GET NEXT LEVEL
         ST    R8,0(,R3)               LINK SAVE AREAS IN R8 LOCN
         BC    8,PRTR4                 JUST PRINT THE REST IF FINISHED
         BCT   R10,PRTR2               LOOP FOR NTRACE2
         SR    R2,R2                   COUNTER
PRTR3    LR    R3,R8                   SAVE STK ADDR
         BAL   R9,PRTRS2               NEXT LEVEL
         ST    R8,0(,R3)               LINK TOGETHER
         L     R4,0(,R4)               BRING R4 ALONG NTRACE2 BEHIND
         LA    R2,1(,R2)               COUNT LOOPS
         BC    7,PRTR3                 LOOP IF MORE LEFT
         MVI   0(R1),C'0'              DBL SPACE
         BAL   R9,PRDOTS
         LR    R0,R2
         BAL   R14,PRDEC               PRINT NUMBER
         PR    'PROCEDURES NOT TRACED',1
         BAL   R14,PRNT
         BAL   R9,PRDOTS
PRTR4    LR    R8,R4                   GO BACK NTRACE2 PROCS
         LA    R4,1(,R8)
         ST    R4,OLDSTK
         L     R6,=F'-8'
         L     R6,0(R6,R8)             GET CODE BASE
PRTR5    BAL   R9,PRTRS1               PRINT TRACE
         BAL   R9,PRTRS2               GET NEXT LEVEL
         BC    7,PRTR5                 LOOP TO END
PRTR6    L     R10,R10S                RETURN REG
         XC    SPIEADR,SPIEADR         CLEAR PGM INT TRAP
         BR    R10
         SPACE
PRDOTS   LA    R10,4
PRDOTS1  MVI   15(R1),C'.'
         LA    R1,16(,R1)
         BAL   R14,PRNT
         BCT   R10,PRDOTS1
         BR    R9
         EJECT
*        FOLLOWING 2 VALUES MUST MATCH DISPL FROM STACK
*        IN RUNTIME ASSEMBLY
STKNP    EQU   X'8C'
STKRT    EQU   X'94'
         SPACE 2
PRTI     DS    0H                      PRINT TRACEBACK FOR PROG INT
         PR    '0CODE BASE ='
         L     R0,HEAD
         BAL   R14,PRHEX6
         PR    ', STACK BASE ='
         L     R0,STAKHEAD
         BAL   R14,PRHEX6
         PR    ', HEAP TOP ='
         LR    R2,R0
         L     R0,STKNP(,R2)
         BAL   R14,PRHEX6
         PR    ', RUNTIME SUPPORT ='
         L     R0,STKRT(,R2)
         BAL   R14,PRHEX6
         BAL   R14,PRNT
         SPACE
         PR    '0INTERRUPT'
         L     R2,SPIEOPSW+4
         LA    R2,0(,R2)
         CL    R2,HEAD
         BC    4,NOTCODE               INT NOT IN COMPILED CODE
         C     R2,TAIL
         BC    2,NOTCODE
         PR    'FROM COMPILED CODE, STORAGE NEAR INTERRUPT:',1
         BAL   R14,PRNT
         S     R2,=A(33)
         N     R2,=X'00FFFFE0'
         MVI   0(R1),C'0'              DBL SPACE
         BAL   R8,PRLS
         BAL   R8,PRLS
         BAL   R8,PRLS
         PR    '0INTERRUPT AT OFFSET'
PRTI1    LA    R8,SPIEREGS             ==> REGS 0-15
         MVC   OLDSTK,SPIEREGS+R8*4
         L     R7,SPIEOPSW+4           INT ADDR
         B     PRTR0                   COMPLETE TRACEBACK
         SPACE 2
NOTCODE  DS    0H
         L     R8,SPIEREGS+4*R8
         C     R8,R8*4(,R8)            SEE IF LOOKS LIKE RUNTIME ENVMT
         BC    7,NOTRT
         PR    'IN RUNTIME SYSTEM',1
         BAL   R14,PRNT
         PR    '0RUNTIME SYSTEM CALLED FROM OFFSET'
         B     PRTR00
         SPACE
NOTRT    PR    'ENVIRONMENT COULD NOT BE IDENTIFIED - NO TRACEBACK',1
         BAL   R14,PRNT
         ABEND 0,DUMP
         EJECT
*        PRINT ONE TRACEBACK LEVEL
*        R6 => CODE, R8 => REGS 0-6,  R6 DESTROYED.
PRTRS1   DS    0H
         PR    '-====> SEGMENT'
         LR    R15,R6                  PROC ADDR
         BAL   R14,PROCNAME
         SR    R5,R5
         IC    R5,5(,R6)               PROC LEVEL * 16
         SRL   R5,2                    DISPL TO DISPLAY REG
         L     R5,0(R5,R8)             DISPLAY REG TO R5
         C     R5,STAKHEAD             IS IT BOTTOM ONE?
         BC    8,PRTRS11               FINISHED IF SO
         L     R0,4(,R5)               RETURN ADDR TO PREV PROC
         S     R0,24(,R5)              MINUS PREV BASE (R14)
         PR    'CALLED FROM OFFSET',1
         BAL   R14,PRHEX6              PRINT OFFSET VALUE
         PR    'IN SEGMENT',1
         L     R15,24(,R5)             CALLING PROC NAME
         BAL   R14,PROCNAME
PRTRS11  BAL   R14,PRNT                PRINT THE LINE
         SPACE
         LR    R4,R6
         CLC   =X'4700',6(R6)         TEST LOCAL VBL OPTION
         BC    7,PRTRS12               BR IF NOT HERE
         AH    R4,8(,R4)
         B     PRTRS4
PRTRS12  CLC   =X'4700',10(R6)
         BC    7,PRTRS3
         AH    R4,12(,R4)
         B     PRTRS4
PRTRS3   CLC   =X'4700',14(R6)
         BCR   7,R9
         AH    R4,16(,R4)
PRTRS4   DS    0H
         PR    '0LOCAL VBLS: '
         LA    R0,PRLINT
         ST    R0,SPIEADR              SET TRAP FOR PGM INT
         SPACE
PRLOCAL  DS    0H
         L     R3,0(,R4)               ADDR OF VBL IN DISPLAY SPACE
         LTR   R3,R3                   TEST IF LIST END
         BC    7,PRLOC05
PRLOC04  BAL   R14,PRNT
         LA    R0,STKCORR              RESTORE PGM INT TRAP
         ST    R0,SPIEADR
         BR    R9
PRLOC05  DS    0H
         LA    R0,30(,R1)              TEST LINE OVFLO
         C     R0,BLKPE-BLOCK+OUTBLK
         BC    12,PRLOC1
         BAL   R14,PRNT                UNLOAD LAST LINE
         LA    R1,13(,R1)
PRLOC1   DS    0H
*        0(R3,R5) IS VBL VALUE, 4(,R4) IS ITS NAME, 0(R4) IS ITS TYPE
         MVC   0(8,R1),4(R4)           VBL NAME
         MVI   8(R1),C'='
         LA    R1,10(,R1)
         SR    R15,R15
         IC    R15,0(,R4)
         N     R15,=A(X'FE')           REMOVE INDIRECT BIT
         SLL   R15,1                   TYPE * 4
         LA    R14,0(R3,R5)            ADDRESS VBL
         CLC   =X'7F7F7F7F',0(R14)     TEST UNDEFINED
         BC    8,PRLOC15               BR IF SO
         TM    0(R4),1                 TEST IF PTR
         BC    8,PRLOC2                BR IF DIRECT
         L     R14,0(,R14)             GET PTR TO VALUE
         CLC   =X'7F7F7F7F',0(R14)     TEST UNDEFINED
         BC    7,PRLOC2                BR IF NOT
PRLOC15  DS    0H
         MVC   0(11,R1),=C'<UNDEFINED>'
         B     PRLOC3
PRLOC2   BAL   R15,PRVAL(R15)          GET VALUE TO PRLINE
PRLOC3   DS    0H
         LA    R1,20(,R1)              NEXT POSN ON LINE
         LA    R4,12(,R4)              NEXT NAME IN PMD LIST
         B     PRLOCAL                 LOOP THRU NAMES
         SPACE 2
*        PUT VARIABLE VALUES TO PRINT LINE
         SPACE
PRVAL    B     PRVP                    POINTER
         B     PRVI                    INTEGER
         B     PRVR                    REAL
         B     PRVC                    CHAR
         B     PRVB                    BOOLEAN
         B     PRVA                    ALFA
         B     PRVS                    SCALAR
         SPACE
PRVP     DS    0H
         L     R0,0(,R14)
         LTR   R0,R0
         BC    7,PRVP1
         MVC   0(5,R1),=C'<NIL>'
         BR    R15
PRVP1    DS    0H
         C     R0,STAKHEAD
         BC    12,PRLOC15
         C     R0,TAIL
         BC    10,PRLOC15              PTR IS UNDEF IF <STK OR >HEAP
         MVI   0(R1),C'@'
         UNPK  1(7,R1),1(4,R14)
         TR    1(6,R1),HEXTBL
         MVI   7(R1),C' '
         BR    R15
         SPACE
PRVI     L     R0,0(,R14)
         CVD   R0,WORK
         MVC   4(15,R1),=X'202020202020202020202020202120'
         LR    R14,R1                  SAVE ACROSS EDMK
         LA    R1,18(,R1)              BACKSTOP
         EDMK  3(16,R14),WORK
         BC    10,PRVI1                BR IF +VE
         BCTR  R1,0                    SPOT FOR MINUS
         MVI   0(R1),C'-'
PRVI1    MVC   0(16,R14),0(R1)         LEFT JUSTIFY
         LR    R0,R1
         SR    R0,R14
         LR    R1,R14
         LA    R14,19(,R14)
         BCTR  R14,0
         MVI   0(R14),C' '
         BCT   R0,*-6
         BR    R15
         SPACE
PRVR     DS    0H
         STM   R15,R6,PRVRS
         LA    R3,4                    IND E TYPE
         LA    R5,WORK
         LA    R2,19                   FIELD LENGTH
         SR    R4,R4
         LD    0,0(,R14)
         L     R15,=V(WRITFP)
         BALR  R14,R15
         MVC   0(20,R1),WORK
         LM    R15,R6,PRVRS
         BR    R15
PRVRS    DC    8A(0)
         SPACE
PRVC     MVI   0(R1),C''''
         L     R0,0(,R14)
         STC   R0,1(,R1)
         MVI   2(R1),C''''
         BR    R15
         SPACE
PRVB     L     R0,0(,R14)
         LA    R14,=C'<TRUE> '
         LTR   R0,R0
         BC    7,*+8
         LA    R14,=C'<FALSE>'
         MVC   0(7,R1),0(R14)
         BR    R15
         SPACE
PRVA     MVI   0(R1),C''''
         MVC   1(8,R1),0(R14)
         MVI   9(R1),C''''
         BR    R15
         SPACE
PRVS     ST    R15,PRVRS
         BAL   R15,PRVI
         MVC   0(3,R14),=C'(S)'
         L     R15,PRVRS
         BR    R15
         SPACE
PRLINT   BAL   R14,PRNT                UNLOAD PART LINE
         PR    '0PROGRAM INTERRUPT FINDING LOCAL VBLS'
         B     PRLOC04
         EJECT
*        ROUTINE SETS R6=> CODE AND R8=> REGS 0-6 FOR NEXT HIGHER CALL
PRTRS2   DS    0H
         SR    R5,R5
         IC    R5,5(,R6)               DISPLAY REG NO
         N     R5,=A(240)
         SRL   R5,2
         L     R8,0(R5,R8)             PREV STACK
         CL    R8,OLDSTK
         BC    10,STKCORR              BR IF STK NOT DECREASING
         ST    R8,OLDSTK
         L     R6,24(,R8)              PREV CODE BASE
         C     R8,STAKHEAD             TEST IF END YET
         LA    R8,32(,R8)              => REGS 0-6
         BR    R9
         SPACE
STKCORR  DS    0H                      STK PROBABLY CORRUPT - STOP LOOP
         BAL   R14,PRNT                UNLOAD WHATEVER IS ON LINE
         PR    '0STACK CORRUPTION, UNABLE TO COMPLETE TRACEBACK'
         BAL   R14,PRNT
         B     PRTR6
         SPACE 2
PRDEC    CVD   R0,WORK
         MVC   1(7,R1),=X'20202020202120'
         ED    0(8,R1),WORK+4
         LA    R1,8(,R1)
         BR    R14
         SPACE
PRHEX6   DS    0H
         ST    R0,WORK
         UNPK  1(7,R1),WORK+1(4)
         TR    1(6,R1),HEXTBL
         MVI   7(R1),C' '
         LA    R1,7(,R1)
         BR    R14
         SPACE
         SPACE
         EJECT
PRNT     DS    0H
         ST    R2,R2S                  SAVE
         LR    R0,R1                   SUBROU PRINTS THE PREPARED LINE
         LA    R1,OUTDCB               AND BLANKS OUT THE NEXT
         LA    R2,OUTBLK
         USING DFCB,R1
         USING BLOCK,R2
         L     R15,BLKPS               START OF REC
         TM    DCBRECFM,X'40'
         BC    8,PRNT2                 BR IF RECFM F
         SR    R0,R15                  LENGTH OF REC
         TM    DCBRECFM,X'80'
         BC    1,PRNT1                 BR IF RECFM U
         SLL   R0,16
         ST    R0,WORK
         MVC   0(4,R15),WORK           SET RDW UP
         B     PRNT2
PRNT1    STH   R0,DCBLRECL
PRNT2    ST    R14,PRNTS
         LR    R0,R15                  ADDR OF RECD TO R0
         PUT   (1)
         DROP  R1
         L     R14,OUTBLK
         USING DFCB,R14
         LH    R15,DCBLRECL
         TM    DCBRECFM,X'10'          TEST RECFM=B
         BC    1,*+8                   RECL IS LRECL IF SO
         LH    R15,DCBBLKSI            BLKSI IF NOT
         BCTR  R15,0
         BCTR  R15,0
         MVI   0(R1),C' '              BLANK LINE
         EX    R15,PRNTX
         LA    R15,2(R15,R1)           END OF RECORD
         ST    R15,BLKPE               SAVE IN BLK
         CLI   DCBRECFM,X'80'
         DROP  R14
         L     R14,PRNTS               RESTORE
         L     R2,R2S
         DROP  R2
         BCR   10,R14                  RETURN IF NOT RECFM V
         XC    0(4,R1),0(R1)
         LA    R1,4(,R1)               POINT PAST IT
         BR    R14
         SPACE
PRNTX    MVC   1(0,R1),0(R1)
PRNTS    DC    A(0)                    SAVE AREA
R2S      DC    A(0)
         EJECT
PROCNAME DS    0H
         STM   R14,R1,WORK
         CLI   PROCFLG,1              PROCS READ YET?
         BC    8,PROCN1                BR IF ALREADY READ NAMES
         MVI   PROCFLG,1               SET NAMES READ
         MVC   FCB2+40(8),INDDNAM
         LA    R1,FCB2+47
         CLI   0(R1),C'I'
         BC    8,*+8
         BCT   R1,*-8
         MVI   0(R1),C'2'
         OPEN  (FCB2,INPUT)
         ST    R10,R10S                SAVE
         LA    R10,PRCNAME
PRC1     GET   FCB2,(R10)
         CLI   0(R10),0                SEE IF END OF PROCNAMES
         BC    8,PRC2
         LA    R10,8(,R10)             SPOT FOR NEXT
         CLI   0(R10),C'?'             SEE IF END YET
         BC    7,PRC1
         SPACE
PRC2     DS    0H
         MVC   0(8,R10),=CL8'????????'
         BC    7,PRC2
         CLOSE FCB2
         L     R10,R10S
         LM    R14,R1,WORK
PROCN1   DS    0H
         L     R14,STAKHEAD
         L     R0,SYSCON+256*4(,R14)   SAVE STACK WORD
         ST    R15,SYSCON+256*4(,R14)  SET BACKSTOP
         LA    R14,SYSCON-4(,R14)
SYSCON   EQU   X'230'
PROCN2   LA    R14,4(,R14)
         C     R15,0(,R14)
         BC    7,PROCN2
         L     R15,STAKHEAD
         ST    R0,SYSCON+4*256(,R15)
         LA    R15,SYSCON(,R15)
         SR    R14,R15
         AR    R14,R14
         LA    R14,PRCNAME(R14)
         MVC   1(8,R1),0(R14)
         LM    R14,R1,WORK
         LA    R1,9(,R1)
         BR    R14
PROCFLG  DC    AL1(0)
         EJECT
CLOSCR   DS    0H                      CLOSE DCBS LEFT OPEN, SCRATCH
*                                      LOCAL FILES LEFT ALLOCATED
         LA    R7,OUTDCB
         CLOSE ((R7))                  CLOSE SYSPRINT
         L     R7,PASFLINK
CLOS1    LTR   R7,R7                   TEST IF ANY
         BCR   8,R10                   RETURN IF NONE LEFT
         USING DFCB,R7
         L     R6,DFCBBLK
         USING BLOCK,R6                ADDRESS BLOCK
         TM    BLKSTAT,BSREAD+BSWRITE
         BC    8,CLOS2                 BR IF NOT OPEN
         CLOSE ((R7))
CLOS2    TM    BLKSTAT,BSLOCAL         TEST IF LOCAL FILE
         BC    8,CLOS3                 BR IF NOT
         LA    R2,DFCBJ                MAKE UP CAMLST
         SR    R3,R3
         LA    R4,DFCBJ+132
         STM   R2,R4,CAMSCR+4
         SR    R0,R0
         SCRATCH        CAMSCR
CLOS3    L     R7,DFCBLINK
         B     CLOS1                   LOOP
         DROP  R6,R7
         SPACE
CAMSCR   CAMLST SCRATCH,00,,000,,OVRD
         SPACE 4
SPIEINT  DS    0H
         USING *,R15
         L     R2,SPIEADR              ADDR OF RECOVERY ROUTINE
         LTR   R2,R2
         BC    8,SPIEI1                BR IF NONE EXISTS
         ST    R2,8(,R1)               SET RETURN PSW
         BR    R14                     RETURN
SPIEI1   DS    0H
         ABEND 1,DUMP                  ISSUE ABEND FOR DEBUGGING
SPIEADR  DC    A(0)                    ADDR FOR PROCEEDING AFTER INT
         DROP  R15
         SPACE
R10S     DC    A(0)                    SAVE FOR R10
OLDSTK   DC    A(0)
         EJECT
SAVETERM DC    18A(0)
WORK     DC    D'0'
         DC    D'0'
         DC    3D'0'                   WRITFP NEEDS 5 DBLWDS AT WORK
ERRN     DC    A(0)
ERRINFO  DC    A(0)
         SPACE 2
         LTORG
         SPACE
FCB2     DCB   DSORG=PS,MACRF=GM,EODAD=PRC2
PRCNAME  DC    256CL8' '
         DC    C'????????'
         EJECT
PASDATA  DSECT
         PASDATA
         EJECT
         SPACE 2
         BLOCK
         SPACE 2
STEPS    DSECT
         STEPS
         SPACE 2
         PRINT NOGEN
         DCBD  DEVD=TA,DSORG=(QS)
DFCB     EQU   IHADCB
DFCBDDN  DC    CL8' '
DFCBLINK DC    A(0)
DFCBBLK  DC    A(0)
DFCBL    EQU   *-DFCB
DFCBJ    DC    XL176'0'
DFCBD    DC    XL140'0'
DFCBJDL  EQU   *-DFCB
         PRINT GEN
         EJECT
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
