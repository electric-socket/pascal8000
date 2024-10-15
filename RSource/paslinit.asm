         TITLE 'PASCAL - INITIALISE PASCAL SYSTEM'
PASINIT  CSECT
*        THIS ROUTINE OPENS SYSPRINT, DOES STIMER TO INITIALISE TTIMER
*        READS PARM FIELD PARAMETERS
*        GETS AND PATTERNS STACK
*        INITIALISES PROC TABLE
*        INITIALISES DATA FROM VALUE STATEMENTS
         SPACE
         SPACE 2
         USING *,R15
         B     12(,R15)
         DC    AL1(7),CL7'PASINIT'
         STM   R14,R12,12(R13)         SAVE
         LR    R14,R13
         LA    R13,PASSAVE
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
         ST    R1,PASDTIM
PASIST1  L     R0,=V(TIMEX)
         STIMER TASK,(R0),TUINTVL=INTERVAL
         SPACE
*        INITIALISE PARM FIELD REGISTERS
         DROP  R10
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
         ST    R0,NFSBLKS              NO OF 2K BLOCKS OF FREE STORE
         BAL   R14,NUM
         ST    R0,NTRACE1              OVERRIDE DFAULT NTRACE1
         BAL   R14,NUM
         ST    R0,NTRACE2
PASI6    DS    0H
         B     PASI7
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
         BC    4,NUM1
         B     NUM3
NUM2     LA    R14,4(,R14)
NUM3     CLI   0(R8),C','
         BCR   7,R14
         LA    R8,1(,R8)
         BR    R14
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
         SPACE
PASI7    DS    0H
         EJECT
         L     R0,NFSBLKS              GET FREE STORE AMNT
PASG9    SLL   R0,11                   * 2K
         LR    R2,R0
         GETMAIN R,LV=(0)
         LR    R3,R1                   SAVE ADDR
         GETMAIN VC,A=HEAD,MF=(E,GML)
         LTR   R15,R15
         BC    8,PASG11
         BAL   R14,ERROR
         DC    Y(ERRCORE)
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
         SPACE
         B     PASG15
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
PASSAVE  DC    18A(0)
         LTORG
PASG15   DS    0H
         EJECT
*        INITIALISE PROC TABLE
         SPACE
         L     R10,HEAD
         USING STACK,R10
         LA    R9,PROCTBLS             PROC TABLE START
         LA    R8,PROCTBLE             PROC TABLE END (FOR STACK AREA)
         LR    R10,R8                  FOR PROC NO LIMIT TEST
         DROP  R10
         L     R6,=V(P@MAIN)           CODE CSECT FOR MAIN ROUTINE
         BAL   R7,INITPROC             RECURSIVE ROUTINE TO SET UP
         B     INITPEND                PROCTABLE
         SPACE 2
*        THIS RECURSIVE ROUTINE SETS UP THE PROC ADDRESS TABLE IN THE
*        SYSTEM CONSTANT AREA
*        TO DO THIS IT FINDS ALL THE SEGMENTS BELONGING TO SEPARATELY
*        COMPILED PROCEDURES, RELOCATES THEM AND STORES THEM
*        TOGETHER IN THE MAIN TABLE
*        AT THE SAME TIME IT PREPARES ENTRIES IN THIS TABLE TO
*        POINT TO EXTERNAL PASCAL AND EXTERNAL FORTRAN(OR ASSEMBLER)
*        ROUTINES
*        THESE ARE TWO-WORD ENTRIES AND HAVE THE FOLLOWING FORMAT:
*        1) EXTERNAL PASCAL ENTRY
*              BEFORE INITIALISATION:
*              DC    A(1)              INDICATES EXT PASCAL
*              DC    A(EXTPAS-8)       PTR TO EXTERNAL ROUTINE
*              AFTER INITIALISATION:
*              DC    A(EXTPAS)         PTR TO LINKAGE CODE
*              L     14,*-*(15,1)      THIS INSTRUCTION WHEN PLACED
*              AT STACK+8 PROVIDES THE ENVIRONMENT NECESSARY FOR CALLS
*              TO AND WITHIN THE GIVEN EXTERNAL ROUTINE
*        2) EXTERNAL FORTRAN ENTRY
*              BEFORE INITIALISATION:
*              DC    A(IND)            2=SUBR, 3=INT FUN, 4=REAL*8 FUN
*              DC    A(FORTCODE)       PTR TO FORTRAN OBJ CODE
*              AFTER INITIALISATION:
*              DC    XL1'INS'          INSTR TO STORE RESULT:
*                                      NOP FOR SUBR, ST OR STD FOR FUN
*              DC    AL3(EXTFORT)      PTR TO LINKAGE CODE
*              DC    A(FORTCODE)       PTR TO FORTRAN-COMPILED CODE
         SPACE
INITPROC DS    0H
         STM   R0,R7,0(R8)             STACK REGISTERS
         LA    R8,32(,R8)              BUMP SP
         LA    R5,8(,R6)               CODE BASE FOR RELOCN
         L     R15,0(,R6)              PROC TABLE PATTERN
         BALR  R14,R15                 ENSURE RESIDENT
         LR    R4,R9                   PROC TABLE SEGMENT ADDR
         LH    R9,2(,R15)              NO OF PROC ENTRIES THIS MODULE
         LTR   R0,R9                   LOAD COUNTER
         BC    12,COMTERR              COMPILE ERRS IF NOT +VE
         SLL   R9,2                    *4
         AR    R9,R4                   FORM END OF THIS SEG IN GLBL REG
         LA    R3,4(,R15)              ADDR OF 1ST PROC PATTERN
INITP1   DS    0H                      LOOP FOR EACH ENTRY
         CR    R4,R10                  TEST TABLE LENGTH
         BC    10,PROCERR              TOO MANY PROCS TOTAL
         TM    3(R3),7                 TEST IF EXTERNAL ROUTINE
         L     R1,0(,R3)               GET TYPE OF EXT, ADDR OF INT
         BC    8,INITP4                BR IF INTERNAL
         CL    R1,=A(4)
         BC    2,INITP4                LONGJUMP IF NOT 1,2,3 OR 4
         BCT   R1,INITP3               BR IF FORT EXT
*        PASCAL EXT ENTRY
         L     R6,4(,R3)               GET ADDR OF ROUTINE
         L     R1,4(,R6)
         LTR   R1,R1                   SEE IF INITIALISED YET
         BC    7,INITP2                BR IF SO
         LR    R1,R9                   SAVE ADDR OF START
         SL    R1,HEAD                 DISPL FROM START OF STACK
         AL    R1,VBLINSTR             FORM INSTRN FOR STACK+8
         ST    R1,4(,R6)               INDICATE THIS EXT NOW INIT
         BAL   R7,INITPROC             INITIALISE PROC TABLE FOR EXTNLS
INITP2   ST    R1,4(,R4)               SAVE INSTR IN PROC TABLE
         LA    R6,EXTPAS
         ST    R6,0(,R4)               SAVE ADDR OF LINKAGE ROUTINE
         LA    R4,8(,R4)               TO NEXT POSN IN PROC TABLE
         LA    R3,8(,R3)               TO NEXT ENTRY FROM COMPILER
         BCT   R0,INITP5               TO PROCESS NEXT
         B     LOADERR
         SPACE
INITP3   DS    0H                      FORTRAN EXTERNAL ENTRY
         LA    R6,EXTFORT              LINKAGE CODE
         L     R7,4(,R3)               ADDR OF FORT ROUTINE
         STM   R6,R7,0(R4)             SAVE TWO-WORD ENTRY
         LA    R6,=X'00475060'         NOP, ST, STD
         IC    R1,0(R1,R6)
         STC   R1,0(,R4)               IND TYPE OF FORT ROUTINE
         LA    R4,8(,R4)
         LA    R3,8(,R3)
         BCT   R0,INITP5
         B     LOADERR
         SPACE
INITP4   DS    0H                      INTERNAL PASCAL PROC ENTRY
         AR    R1,R5                   RELOCATE
         ST    R1,0(,R4)
         LA    R4,4(,R4)               TO NEXT
         LA    R3,4(,R3)
INITP5   BCT   R0,INITP1               LOOP
         S     R8,=A(32)
         LM    R0,R7,0(R8)             RESTORE
         BR    R7                      RETURN
         SPACE
         DC    0A(0)
VBLINSTR L     R14,*-*(R15,R1)
         SPACE
INITPEND DS    0H
         EJECT
*        INITIALISE STACK IF VALUE STATEMENT PRESENT
         L     R15,=V(P@MAIN)
         L     R2,4(,R15)              NO OF VALUES
         LTR   R2,R2
         BC    8,NOINIT
         L     R15,WXINIT              GET ADDR OF INIT CSECT IF ANY
         LTR   R15,R15
         BC    8,LOADERR
         BALR  R14,R15                 ENSURE RESIDENT
         LA    R15,4(,R15)             TO 1ST ENTRY
INIT1    L     R3,0(,R15)              BYTES IN THIS VALUE
         L     R4,4(,R15)              DISPL OF THIS VBL
         LA    R15,8(,R15)             ADDR OF VALUE
         A     R4,HEAD                 RELOCATE
INIT2    BCTR  R3,0                    -1 FOR MVC
         EX    R3,MVCXL
         LA    R5,255
         NR    R5,R3                   L-1 JUST MOVED
         LA    R15,1(R5,R15)
         LA    R4,1(R5,R4)
         XR    R3,R5                   LENGTH LEFT
         BC    7,INIT2                 BR IF SOME
         BCT   R2,INIT1                LOOP THRU ALL VALUES
         B     NOINIT                  PAST CONSTS
         SPACE
MVCXL    MVC   0(0,R4),0(R15)          FOR EX
         WXTRN P@MAIN@V
WXINIT   DC    A(P@MAIN@V)
         SPACE
NOINIT   DS    0H
         EJECT
*        SET UP RUNTIME ENV'MENT FOR FORTRAN IF NECESSARY
         L     R15,AIBCOM
         LTR   R15,R15
         BC    8,NOFORT
         BAL   R14,64(,R15)            INITIALISE ENTRY
NOFORT   DS    0H
         SPACE
         SR    R0,R0                   IND NO ERRORS
RETN     L     R13,4(,R13)
         L     R14,12(,R13)
         LM    R1,R12,24(R13)
         BR    R14
         SPACE
ERROR    LH    R0,0(,R14)
         B     RETN
LOADERR  BAL   R14,ERROR
         DC    Y(ERRLOAD)
PROCERR  BAL   R14,ERROR
         DC    Y(ERRPROC)
COMTERR  BAL   R14,ERROR
         DC    Y(ERRCOMT)
         SPACE
         WXTRN IBCOM#
AIBCOM   DC    A(IBCOM#)
         LTORG
         SPACE
RSTACK   EQU   8
         PRINT NOGEN
PASDATA  DSECT
         PASLDATA
         BLOCK
         PRINT NOGEN
         DCBD  DSORG=QS,DEVD=TA
         PRINT GEN
DFCB     EQU   IHADCB
STACK    DSECT
PROCTBLS EQU   STACK+560
PROCTBLE EQU   STACK+1584
         ERRNOS
ERRLOAD  EQU   ERROBJ
ERRPROC  EQU   ERROBJ
         END
