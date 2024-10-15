         TITLE 'PASCAL RUNTIME SYSTEM'
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                     *
*        RUNTIME SUPPORT FOR THE PASCAL 8000 SYSTEM OF                *
*        HIKITA AND ISHIHATA, TOKYO UNIVERSITY                        *
*                                                                     *
*        RUNTIME SYSTEM WAS REWRITTEN FOR INCREASED ECONOMY AND SPEED *
*        WITH ADDITION OF SOME NEW FEATURES BY                        *
*        G. W. COX, AUSTRALIAN ATOMIC ENERGY COMMISSION               *
*                                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         SPACE
*        THE INTERFACE BETWEEN THE COMPILED CODE AND THE ROUTINE
*        SYSTEM IS THROUGH A SET OF 8-BYTE ENTRIES AT THE BOTTOM
*        OF THE MAIN STACK WHICH IS ADDRESSED THROUGH R1
*        THE CONTENTS OF THESE 8 BYTES DIFFERS FROM THAT OF HIKITA
*        AND IS FOR EXAMPLE
         SPACE
*        L     R1,=A(RUNTIME#)
*        BALR  R1,R1
*        DC    Y(SIN-RUNTIME$)
         SPACE
*        IN THE RUNTIME HEADER, WE USE THE Y CONST TO ENTER THE ROUTINE
         EJECT
*        MACROS FOR USE BY RUNTIME
         MACRO
&L       SAVEF
&L       STD   F0,SAVEFLOT
         STD   F2,SAVEFLOT+8
         STD   F4,SAVEFLOT+16
         STD   F6,SAVEFLOT+24
         MEND
         SPACE
         MACRO
&L       LOADF
&L       LD    F0,SAVEFLOT
         LD    F2,SAVEFLOT+8
         LD    F4,SAVEFLOT+16
         LD    F6,SAVEFLOT+24
         MEND
         SPACE
         MACRO
         STDFN &F,&E
P&F      BAL   RWK,ARITH
         WXTRN &F.@P
         AIF   ('&E' EQ '').A
         DC    AL1(ERR&E),AL3(&F.@P)
         AGO   .B
.A       DC    AL1(ERR&F),AL3(&F.@P)
.B       ANOP
         MEND
         SPACE
*        MACROS FOR USE BY PASRUN
         SPACE
         MACRO
&L       ENT   &POINT
&L       L     R1,STKRT
         BALR  R1,R1
         DC    Y(&POINT-RUNTIME$)
         MEND
         EJECT
RUNTIME# CSECT
         STM   R0,R15,0(RSTACK)        SAVE ON PASCAL STACK
         BALR  RBASE,0                 MAKE ADDRESSABLE
RUNTIME$ DS    0H                      FOR CALC DISPL OF ENTRY POINTS
         USING *,RBASE
         L     R13,=V(SAVEAREA)
         LR    RBLK,R15                LOAD I/O BLOCK PTR
         LR    RPARM,R0                LOAD PARAMETER REG
         LH    R1,0(,R1)               GET DISPL TO ENTRY
         LA    R6,4*16(,RSTACK)        NEW STACK TOP
         CLR   RNP,R6                  SEE IF OK
         BC    10,RUNTIME1
         LA    R1,ERROR4-RUNTIME$      SET FOR STACK ERROR
RUNTIME1 DS    0H
         BAL   RBASE,0(R1,RBASE)       ENTER ROUTINE, RESET BASE FOR
         USING *,RBASE                 (BR RBASE) RETURN SEQUENCE
*        RETURN SEQUENCE
         SPACE
*        EVERYTHING COMES BACK HERE EXCEPT RETURN, ERROR, AND LONGJUMP
RUNTIME2 DS    0H
         L     R1,RTSTAK               RESTORE R1 STACK
         LM    R2,R15,8(RSTACK)        RESTORE OTHER REGS
*                                      NO NEED TO RESTORE R0?
RUNTIME3 BR    R9                      THIS GETS CLOBBERED AT TIMEX
         EJECT
*        COMPILED CALLS TO ERROR MESSAGES
         SPACE
ERROR1   BAL   R14,ERROR               UNDEFINED CASE LABEL
         DC    Y(ERRCASE)
ERROR2   BAL   R14,ERROR               VALUE OUT OF RANGE
         DC    Y(ERRRANGE)
ERROR3   BAL   R14,ERROR               POINTER VALUE INVALID
         DC    Y(ERRPNTR)
ERROR4   BAL   R14,ERROR               STACK OVERFLOW
         DC    Y(ERRSTACK)
         SPACE 3
         ENTRY INTERUPT
INTERUPT DS    0H    ENTRY HERE IF PROGRAM INTERRUPT
*                    R1--> PIE = A(PICA),D'OPSW',R14,15,0,1,2
         USING *,R15
         L     R2,=V(SPIEOPSW)         ADDRESS PARM AREA
         MVC   0(28,R2),4(R1)          SAVE OPSW, REGS 14,15,0,1,2
         STM   R3,R13,28(R2)           SAVE REGS 3 - 13
         ST    R2,24(,R1)              RESTORE AS R1
         LA    R0,ERRORB               SET PSW SO RETURN
         ST    R0,8(,R1)               IS TO ERRORB
         LA    R0,ERRINT               INDICATE INTERRUPT ERROR
         ST    R0,20(,R1)              RESTORE AS R0
         L     RBASE,=A(RUNTIME2)      TO GET ADDRESSABILITY
         L     R13,=V(SAVEAREA)
         BR    R14                     GO TO ERRORB, UNDOING SYNCH
         DROP  R15
         EJECT
*        OPEN AND CLOSE ROUTINES FOR LOCAL AND EXTERNAL FILES
*        THESE ARE INVOKED AT START AND END OF THE PROGRAM(EXTERNAL)
*        OR PROCEDURE(LOCAL) WHICH USES THEN
         SPACE
*        OPEN DOES NOT OPEN IN THE O.S./360 SENSE, BUT PREPARES FOR
*        RESET OR REWRITE TO DO THIS LATER
*        OPEN GETS A DCB ON THE STACK PLUS A JFCB IF A LOCAL FILE
*        OPEN LOCAL ALLOCATES FILE USING SVC 32 WITH MODEL DSCB.
*        1ST RESET OR REWRITE BUILDS BUFFER POOL ON STACK,
*        SUBSEQUENT ONES RE-USE THIS AREA
*        CLOSE CLOSES THE DCB - SUBSEQUENT PROCEDURE OR PROGRAM RETURN
*        EFFECTIVELY FREES THE DCB AND BUFFERS
*        TEXT-FILES ARE PROCESSED USING QSAM (GL,PL),
*        NON-TEXT FILES USE QSAM (GM,PM)
         SPACE
OPNINPUT SR    R0,R0                   OPEN STD INPUT
         USING BLOCK,RBLK
         ST    R0,BLKDFCB              INITIALISE BLOCK,
         MVC   BLKDDN,=CL8'SYSIN'
         LR    RPARM,RBLK
         SPACE
OPENEXT  DS    0H
*        BLOCK IS INITIALISED BY CALLER AS FOLLOWS:
*        DISPL 0: -1 FOR TEXT-FILES, EXCEPT 0 FOR STD INPUT
*                 LRECL FOR NON-TEXT FILES
*        DISPL 4: DDNAME (8 BYTES) FOR PERMANENT FILES
*                 ZERO (1 BYTE) FOR LOCAL FILE
*        BLOCK ADR PASSED IN R0, NOW ALSO IN RPARM
         LR    RBLK,RPARM
         LA    R0,DFCBL
         CLI   BLKDDN,0                TEST IF LOCAL
         BC    7,OPENX1                BR IF NOT
         LA    R0,DFCBJDL              SPACE FOR DCB,JFCB,DSCB
OPENX1   BAL   R14,GETMAIN             GET SPACE REQD ON STACK
         BC    7,ERROR4                BR IF STK OVFLO
         LR    RDFCB,R1
         USING DFCB,RDFCB
         L     R2,BLKDFCB              GET IND FOR TEXT OR NOT
         MVC   DFCB(96),MODELFCB       SET UP DCB
         SPM   R14                     TEST IF LOCAL
         BC    8,OPENX2                BR IF SO - DDNAME ALREADY SET
         MVC   DCBDDNAM,BLKDDN
OPENX2   DS    0H
         MVC   DFCBDDN,DCBDDNAM
         LTR   R0,R2                   TEST IF TEXT
         BC    2,OPENX3                BR NON-TEXT, MACRF OK FROM MODEL
         MVI   DCBMACR1,X'48'          USE MACRF=(GL,PM) FOR TEXT
OPENX3   SLL   R0,16
         ST    R0,BLKLRECL             SET LRECL, CLR STATUS FLAGS
         ST    R1,BLKDFCB
         BC    7,*+8
         OI    BLKSTAT,BSSTD           IND STD INPUT
         LTR   R2,R2
         BC    12,*+8
         STH   R2,DCBLRECL             SET LRECL FOR NON-TEXT
         SPM   R14                     TEST IF LOCAL
         BC    7,OPENX7                BR IF NOT
         OI    BLKSTAT,BSLOCAL         INDICATE LOCAL
         LA    R0,DFCBJ                JFCB ADDR
         ST    R0,RTEXL                SET TO EXLST
         MVI   RTEXL,7                 SET TO JFCB ENTRY
         ST    RDFCB,OPENL
         MVI   OPENL,X'80'             IND END OF LIST
         RDJFCB MF=(E,OPENL)           GET JFCB
         LTR   R15,R15                 TEST RD JFCB 0K
         BC    7,ERRLF
         CLI   DFCBJ,0                 AND AGAIN
         BC    8,ERRLF
*        TEST TO ENSURE TEMP FILE WAS SPEC
         MVC   WORK(17),=X'FFFFFFF0F0F0F0F0FFFFF0F0F0F0F0F0FF'
         NC    WORK(17),DFCBJ
         CLC   WORK(17),=C'SYS00000.T000000.' TEST IF TEMP DS
         BC    7,ERRLF                 ERROR IF NOT
*        NOW MAKE UNIQUE DSN BY CHANGING JOBNAME PART OF TEMP DSN
*        TO 'PNNNNNNN' WHERE NNNNNNN IS THE LOCAL FILE NO.
         MVC   DFCBD+21(10),=X'D7202120202020202020'
         LA    R3,1
         A     R3,LOCFILNO
         ST    R3,LOCFILNO
         CVD   R3,WORK
         ED    DFCBD+21(10),WORK+3     PNNNNNNN
         MVC   DFCBD(23),DFCBJ         SYSNNNNN.TNNNNNN.RVNNN. PART
         LA    R3,DFCBJ+31             LOOKING FOR LAST '.'
         CLI   0(R3),C'.'
         BC    8,*+8
         BCT   R3,*-8
         MVC   DFCBD+31(13),0(R3)      '.RNNNNNNN    ' PART
         MVC   DFCBJ+23(17),DFCBD+23   DUPLICATE DSN BACK TO JFCB
*        DSN IS MADE - NOW FILL IN REST OF MODEL DSCB
         OI    DFCBJ+52,8              SET NO WRITEBACK FOR JFCB
         MVI   DFCBD+44,C'1'           FMT 1 BYTE
         MVC   DFCBD+45(6),DFCBJ+117   SET VOLSER
         XC    DFCBD+51(89),DFCBD+51
         MVC   DFCBD+53(6),DFCBJ+80    SET CREDT, EXPDT
         MVC   DFCBD+62(10),=C'PASCAL/LOC'
         MVI   DFCBD+82,X'40'          DSORG=PS
         L     R3,DFCBJ+152            GET PRI ALLOCN IF SPEC IN JCL
         LTR   R3,R3                   SEE IF SPEC
         BC    7,*+8                   BR IF SO
         LA    R3,X'580'               DEFAULT TO 5(X'5') TRK (X'80')
         STC   R3,DFCBD+94             SPACE UNITS
         SRL   R3,8                    QTTY
*        NOW GET UCB ADDR(WHICH IS REQD LATER) AND DEVTYPE TO CALC TRKS
*        FROM QTTY AND UNITS
         L     R6,16                   CVT
         L     R6,0(,R6)               TCBWORDS
         L     R6,4(,R6)               TCB
         L     R6,12(,R6)              TIOT
         LA    R6,24(,R6)              PAST JOBNAME ETC
OPENX4   CLI   0(R6),0                 SEE IF LAST
         BC    8,ERRLF
         CLC   MODELFCB+40(8),4(R6)    SEE IF THIS IS DD ENTRY
         BC    8,OPENX5                BR IF SO
         SR    R5,R5
         IC    R5,0(,R6)
         AR    R6,R5
         B     OPENX4
OPENX5   DS    0H
         LH    R4,18(,R6)              UCB ADDR
         LTR   R4,R4
         BC    8,ERRLF                 ERROR IF DUMMY
*        1ST WORD OF DEVTYPE IS AT 16(R4) - MAKE VOLLIST FOR SCRATCH
*        AT JFCB+132 (14 BYTES LONG) WHILE IT IS ACCESSIBLE
         LA    R0,1
         STH   R0,DFCBJ+132            H'1'
         MVC   DFCBJ+132+2(4),16(R4)   XL4'DEVTYPE'
         MVC   DFCBJ+132+6(6),DFCBJ+118 CL6'VOLSER'
         SR    R0,R0
         STH   R0,DFCBJ+132+12         H'0'
         ST    R4,OPENL
         MVI   OPENL,X'80'             UCB LIST FOR SVC 32
         TM    DFCBD+94,X'C0'          SEE IF CYL REQUEST
         BC    14,OPENX6               BR IF NOT - TREAT AS TRK
         L     R6,16
         L     R6,64(,R6)              DEVTABLE
         IC    R5,19(,R4)              UNIT TYPE
         LA    R0,15
         NR    R5,R0                   ISOLATE LAST 4 BITS
         IC    R5,0(R5,R6)             INDEX
         ALR   R6,R5                   ADD TO TABLE, NOW POINTS TO
*                                      DEVTYPE+8
         MH    R3,2(,R6)               CYL REQ * TRKS/CYL = TRKS REQ
OPENX6   DS    0H
         ST    R3,X'6C'+DFCBD          STORE REQ AMNT IN MODEL DSCB
         LA    R0,DFCBD                MODEL ADDR
         O     R0,=X'80000000'         INDICATE DSCB, NOT JFCB
         LA    R1,OPENL                UCB LIST
         SVC   32                      ALLOCATE SPACE ON DISK
         LTR   R1,R15                  TEST IF ERROR
         BC    7,SVC32ERR
         MVC   DFCBJ+X'35'(3),DFCBD+X'3E' PUT DSCBTTR IN JFCB
         MVC   DFCBD(64),0(RSTACK)     FREE DSCB AREA
         LA    RSTACK,DFCBD
         ST    RSTACK,4*RSTACK(,RSTACK)
OPENX7   DS    0H
         ST    RBLK,DFCBBLK            POINT BACK TO IOBLOCK
         L     RPARM,=V(PASFLINK)      AND CHAIN DFCB AREAS TOGETHER
         L     R0,0(,RPARM)            SO ABNORMAL TERMINATION
         ST    R0,DFCBLINK             CAN CLOSE DFCB'S AND SCRATCH
         ST    RDFCB,0(,RPARM)         LOCAL FILES
         TM    BLKSTAT,BSSTD           IS THIS STD INPUT
         BCR   14,RBASE                RETURN IF NOT
         BAL   RWK,OPENSUBI            OPEN STD INPUT
         BAL   RDIG,READIN             DO 1ST READ
         B     SETEOLN                 FINISH UP AND RETURN
         DROP  RBLK
         DROP  RDFCB
         SPACE
         PRINT NOGEN
MODELFCB DCB   DSORG=PS,MACRF=(GM,PM),DDNAME=LOCAL,                    X
               EODAD=EOFEX,SYNAD=IOEEX,EXLST=RTEXL,BFTEK=A
         PRINT GEN
         SPACE
CLOSEEXT DS    0H                      CLOSE EXTERNAL FILE
*        BLOCK ADR PASSED IN R0, NOW ALSO IN RPARM
         USING BLOCK,RBLK
         USING DFCB,RDFCB
         LR    RBLK,R0                 LOAD BLOCK BASE
         L     RDFCB,BLKDFCB           LOAD DFCB REG
         BAL   RWK,CLOSESUB            CLOSE THE DCB
         TM    BLKSTAT,BSLOCAL
         BC    8,CLOSEX1               BR IF NOT LOCAL FILE
*                        SCRATCH THE DSN FOR LOCAL FILE
         L     R15,CAMSCR              FLAGS
         LA    R0,DFCBJ                DSN
         SR    R1,R1
         LA    R2,DFCBJ+132            VOLLIST
         STM   R15,R2,CAMLST           SET UP CAMLST FOR SCRATCH
         SR    R0,R0
         SCRATCH CAMLST
         LTR   R1,R15
         BC    7,SCRERR                PROBLEM SCRATCHING LOCAL FILE
CLOSEX1  DS    0H
         L     RPARM,=V(PASFLINK)
         L     R0,DFCBLINK             REMOVE DCB AREA FROM LINKED LIST
         ST    R0,0(,RPARM)            ASSUMING LAST IN FIRST OUT
         BR    RBASE
CAMSCR   CAMLST SCRATCH,00,,000,,OVRD
         ORG   CAMSCR+4
*        DCB AND BUFFERS ON STACK WILL BE FREED BY PROCEDURE RETURN
         SPACE
CLOSESUB DS    0H
         ST    RDFCB,OPENL
         MVI   OPENL,X'80'
         CLOSE MF=(E,OPENL)
         NI    BLKSTAT,X'70'           CLEAR STATUS BITS
         BR    RWK
         SPACE
OPENSUBI LA    R0,128                  OPEN FOR INPUT
         OI    BLKSTAT,BSREAD          SET OPEN-READ
         B     *+12
OPENSUBO LA    R0,143                  OPEN FOR OUTPUT
         OI    BLKSTAT,BSWRITE+BSEOF   SET OPEN-WRITE
         ST    RDFCB,OPENL             SET UP
         STC   R0,OPENL                OPEN LIST
         ST    RSTACK,RSSAVE           DCBEX MAY CHANGE RSTACK
         LA    R1,OPENL
         TM    BLKSTAT,BSLOCAL
         BC    1,OPENS1
         OPEN  MF=(E,(1))              OPEN NON-LOCAL FILE
         B     OPENS2
OPENS1   LA    R0,DFCBJ
         ST    R0,RTEXL                JFCB ADDR TO EXIT LIST
         MVI   RTEXL,07                INDICATE JFCB TYPE
         OPEN  MF=(E,(1)),TYPE=J       OPEN LOCAL FILE
OPENS2   DS    0H
         SPACE
*        HERE CHANGE ALL THE DISPLAY SPACES (IF ANY) BETWEEN HERE AND
*        THE DCB (WHICH IS JUST ABOVE THE DISPLAY SPACE OF THE PROC
*        CONTAINING THE FILE DEFN) TO MAKE THEIR SAVED RSTACK POINT
*        TO AFTER THE BUFFER.  THIS IS SO THAT NO NEW STACK USAGE
*        BELOW THIS BUFFER IS ALLOWED UNTIL RETURN FROM THE PROC
*        WHICH DEFINED THE FILE OCCURS. (THESE INTERVENING DISPLAY
*        SPACES ARE OWNED BY PROCS INVOKED SINCE THE FILE DEFN PROC
*        AND INCLUDING THE PROC CONTG THE 1ST RESET OR REWRITE FOR
*        THE FILE.)  THIS MAY LEAVE A HOLE IN THE STACK FOR A WHILE
*        BUT IT IS THE BEST SOLUTION TO THE PROBLEM OF WHERE TO PUT
*        THE BUFFERS WHEN THE RESET IS IN A PROC AFTER THE FILE DEFN.
         C     RSTACK,RSSAVE           SEE IF ANY BUFFERS ALLOC
         BC    8,OPENS4                NO ACTION IF NOT
         L     RSTACK,RSSAVE           GET NEW SP
         L     R1,RTSTAK               REPAIR SAVED R1
         ST    R1,4*R1(,RSTACK)        ON STACK
         L     R1,4*R14(,RSTACK)       CURRENT CODE BASE
         LA    R2,32
         LR    R3,RSTACK
         SR    R3,R2                   SO 32(R3) => SAVED REGS 0-6
OPENS3   IC    R2,5(,R1)               16 * DISPL REG NO
         SRL   R2,2                    4 * ...
         L     R3,32(R2,R3)            ADDR OF LOC VAR SPACE
         CR    R3,RDFCB
         BC    4,OPENS4                FINISHED IF < DCB
         ST    RSTACK,0(,R3)           MODIFY  SAVED RSTACK
         L     R1,4*(R14-RSTACK)(,R3)  PREVIOUS CODE BASE
         B     OPENS3
OPENS4   DS    0H
         SPACE
         TM    DCBOFLGS,DCBOFOPN
         BCR   1,RWK                     RETURN IF OK
         NI    BLKSTAT,255-BSREAD-BSWRITE-BSEOF CLEAR BITS
         B     OPENERR
         SPACE
GETMAIN  DS    0H                      GET STORAGE IF AVAILABLE,
*                                      RETURN A CODE IF NOT
*                                      AMNT IN R0, ANSWER IN R1
         LR    R1,R0                   AMNT
         LA    R15,64+7(R1,RSTACK)     NEW STK TOP
         N     R15,=F'-8'              ROUND
         CL    R15,RNP*4(,RSTACK)      TEST
         BCR   2,R14                   RETURN IF BAD
         LA    R1,64(,RSTACK)
         LR    RSTACK,R15
         LA    R0,8                    PREPARE LOOP TO SHIFT REGS
GETM1    A     R1,=F'-8'
         A     RSTACK,=F'-8'
         MVC   0(8,RSTACK),0(R1)
         BCT   R0,GETM1                SHIFT STACK
         ST    RSTACK,RSTACK*4(,RSTACK) UPDATE NEW VALUE OF SP
         SPM   RSTACK                  SET CC FOR BC 8
         BR    R14                     RETURN SUCCESSFUL
         SPACE
         DROP  RBLK,RDFCB
         SPACE
RESETTXT DS    0H
RESET    DS    0H                      NON-TEXT FILES
*        BLOCK ADDR IN R15 AND RBLK
         USING BLOCK,RBLK
         L     RDFCB,BLKDFCB
         USING DFCB,RDFCB
         TM    BLKSTAT,BSSTD
         BC    1,INHIBIT
         BAL   RWK,CLOSESUB            CLOSE IF OPEN
         BAL   RWK,OPENSUBI            OPEN INPUT
         CLI   DCBMACF1,X'48'          SEE IF TEXT FILE
         BC    8,READCH2               GET 1ST REC IF SO
         TM    DCBRECFM,X'40'          TEST RECFM NOT V OR U
         BC    1,RECFMERR
         TM    DCBRECFM,X'80'          TEST F
         BC    8,RECFMERR
         B     GET1                    POINT TO 1ST RECORD
         SPACE
REWRITE  DS    0H                      NON-TEXT FILES
*        BLOCK ADDR IN R15 AND RBLK
         L     RDFCB,BLKDFCB
         BAL   RWK,CLOSESUB            CLOSE IF OPEN
         BAL   RWK,OPENSUBO            OPEN FOR OUTPUT
         BR    RBASE
         SPACE
GET      DS    0H                      NON TEXT GET
*        BLOCK ADR IN R15 AND RBLK
         L     RDFCB,BLKDFCB
         TM    BLKSTAT,BSREAD
         BC    8,MODERERR
         TM    BLKSTAT,BSEOF
         BC    1,OVEREOF
GET1     DS    0H
GET2     DS    0H
         GET   (RDFCB),BLKNTB          GET RECORD
         BR    RBASE
         SPACE
PUT      DS    0H
         L     RDFCB,BLKDFCB
         TM    BLKSTAT,BSWRITE
         BC    8,MODEWERR
         B     GET2                    GET GENERATES SAME CODE AS PUT
         SPACE
         ENTRY EOFEX
EOFEX    OI    BLKSTAT,BSEOF           SET EOF
         BR    RBASE                   RETURN
         SPACE
ERFIL    LA    R1,DFCBDDN              POINT TO DDN
         B     ERRORA
ERROR    SR    R1,R1                   INDICATE NO DDN
         DROP  RBLK,RDFCB
ERRORA   LH    R0,0(,R14)              GET ERROR NO
*        NOW RETURN TO CALLER FOR NORMAL OR ABNORMAL TERMINATION
ERRORB   DS    0H
         STM   R0,R1,R14S              SAVE REGS
         L     R11,=V(PASDATA)
         USING PASDATA,R11
         LA    RBLK,OUTBLK             PUT OUT LAST LINE
         USING BLOCK,RBLK
         L     RDFCB,BLKDFCB           WHICH MIGHT BE PARTLY FILLED
         L     RMAX,BLKPS
         USING DFCB,RDFCB
         CLI   DCBRECFM,X'80'          TEST IF RECFM V
         DROP  RDFCB
         BC    10,*+8
         LA    RMAX,4(,RMAX)           PAST PREFIX IF SO
         CR    RMAX,RP
         BC    8,*+8                   SKIP LAST PRINT IF NO INFO
         L     RP,BLKPC
         BAL   RSIGN,WRPUT
         ST    RP,BLKPC
         LM    R0,R1,R14S
         DROP  RBLK
         L     R13,4(,R13)             PREV SAVE
         LR    R2,RSTACK               SEND CURRENT STACK BACK
         LM    R3,R12,32(R13)          FOR TRACEBACK
         USING PASDATA,R11
         NI    PASDFLG,X'7F'           IND PGM NOT ACTIVE NOW
         STM   R0,R1,32(R13)           SAVE ACROSS SPIE
         L     R1,OLDPICA
         SPIE  MF=(E,(1))
         LM    R0,R1,32(R13)
         DROP  R11
         L     R14,12(,R13)            ACTION REQD
         BR    R14                     RETURN TO MAIN PROG
         SPACE
RETURN   BAL   R14,ERROR               NORMAL RETURN
         DC    Y(0)                    INDICATE NO ERROR
         SPACE
OPENERR  BAL   R14,ERFIL
         DC    Y(ERROPN)               'OPEN FAILED'
RECFMERR BAL   R14,ERFIL
         DC    Y(ERRRECFM)             'RECFM MUST BE F OR FB'
LRECLERR BAL   R14,ERFIL
         DC    Y(ERRLRECL)             'RECORD LENGTH MISMATCH
MODERERR BAL   R14,ERFIL
         DC    Y(ERRMR)                'RESET MUST PRECEDE GET '
OVEREOF  BAL   R14,ERFIL
         DC    Y(ERREOF)               'DATA EXHAUSTED'
IOEEX    BAL   R14,ERFIL
         DC    Y(ERRIOE)               'I/O ERROR'
SVC32ERR BAL   R14,ERRORA
         DC    Y(ERRSVC32)
ERRLF    BAL   R14,ERRORA
         DC    Y(ERRLFDD)
SCRERR   BAL   R14,ERRORA
         DC    Y(ERRSCR)
INHIBIT  BAL   R14,ERFIL
         DC    Y(ERRRINH)              RESET OR REWRITE BANNED FOR STD
HALT     BAL   R14,ERROR
         DC    Y(ERRHALT)
TLIMIT   BAL   R14,ERROR
         DC    Y(ERRTOVR)              TIME OVERFLO DURING RUN
         EJECT
*        STRING HANDLING
         SPACE
RELLONG  DS    0H
         MVC   LONGINS(6),CLCINS       SET CLC CODE IN ROUTINE
         B     LONGOP
         SPACE
ASSLONG  DS    0H
         MVC   LONGINS(6),MVCINS       SET MVC CODE IN ROUTINE
LONGOP   DS    0H
         SPACE
*        ON ENTRY R0 CONTAINS 0-24 LENGTH
*                            25-28 REG PTR DEST
*                            29-31 REG PTR SOURCE
         SPACE
         SPACE
         LA    RFR,15
         NR    RFR,RPARM               REG NO
         SLL   RFR,2
         L     RFR,0(RFR,RSTACK)       GET PTR TO SOURCE
         LA    RTO,X'F0'
         NR    RTO,RPARM               REG NO * 16
         SRL   RTO,2
         L     RTO,0(RTO,RSTACK)       PTR TO DEST
         SRA   RPARM,8                 GET LENGTH
         BCR   12,RBASE                RETURN IF 0 OR -VE
ASSL1    BCTR  RPARM,0                 -1 FOR MVC
LONGINS  DC    3Y(0)                   MOVE OR COMPARE SOME
         LA    RLEN,255
         NR    RLEN,RPARM              GET AMT MOVED -1
         XR    RPARM,RLEN              SEE IF ANY LEFT
         BCR   8,RBASE                 RETURN IF NONE
         LA    RTO,1(RLEN,RTO)         NEXT DEST
         LA    RFR,1(RLEN,RFR)         NEXT SOURCE
         B     ASSL1                   LOOP
         SPACE
ASSMVC   MVC   0(0,RTO),0(RFR)
RELCLC   CLC   0(0,RTO),0(RFR)
         SPACE
MVCINS   EX    RPARM,ASSMVC
         BCR   0,0
CLCINS   EX    RPARM,RELCLC
         BCR   7,RBASE                 RETURN TO CALLER IF UNEQUAL
         SPACE
         EJECT
MESAGE   LA    RFR,15                  PRINT MESSAGE TO USER
         NR    RFR,RPARM               REG NO OF MESSAGE
         SLL   RFR,2
         L     RFR,0(RFR,RSTACK)       PTR TO MESSAGE
         SRL   RPARM,4                 LEN OF MESSAGE
         CH    RPARM,*+10
         BC    4,*+8
         LA    RPARM,80                ENSURE <= 80
         BCTR  RPARM,0                 -1 FOR EX
         BALR  RWK2,0                    SET EXTRA ADDRESSABILITY
         USING *,RWK2
         EX    RPARM,MESAGEX
         LA    RPARM,5(,RPARM)         LEN+4
         STH   RPARM,WTOWORK
         MVC   WTOWORK+2(2),WTOL+2
         LA    RPARM,WTOWORK(RPARM)
         MVC   0(4,RPARM),WTOL+4       ROUT AND DESC
         WTO   MF=(E,WTOWORK)
         BR    RBASE
         SPACE
MESAGEX  MVC   WTOWORK+4(0),0(RFR)
WTOL     WTO   '',ROUTCDE=11,DESC=7,MF=L
         DROP  RWK2
         EJECT
*        LONGJUMP SUPPORT
*        LONGJUMP NEEDS SUPPORT FOR TWO REASONS WHICH DO NOT ARISE IN
*        HIKITA'S SYSTEM:
*        1)    ALLOCATION OF DCB, JFCB AND BUFFERS ON THE STACK MEANS
*              THAT ADDING THE DISPLAY SPACE SIZE TO THE CURRENT
*              DISPLAY REG MAY NOT GIVE THE RIGHT STACK VALUE
*        2)    IF A JUMP IS MADE FROM INSIDE TO OUTSIDE A ROUTINE
*              WHICH HAS ALLOCATED AND POSSIBLY OPENED LOCAL FILES,
*              THESE MUST BE CLOSED AND SCRATCHED.
*        ON ENTRY, RPARM => DISPLAY SPACE AT JUMP TARGET
*                  R14 => CODE BASE AT JUMP INSTRUCTION
LONGJUMP LA    R2,32
         SR    RSTACK,R2               SO 32(RSTACK) => REGS 0-6
LONGJ1   IC    R2,5(,R14)              16 * LEVEL AT JUMP
         SRL   R2,2                    4 * ...
         L     R3,32(R2,RSTACK)        DISPL SPACE PTR
         CR    R3,RPARM                DOWN TO PROPER LEVEL YET?
         BC    8,LONGJ2                BR IF SO
         BC    2,*+6                   *** DEBUG
         DC    Y(0)
         LR    RSTACK,R3
         L     R14,24(,RSTACK)         GET PREV CODE BASE
         B     LONGJ1                  LOOP
         SPACE
*        RSTACK IS CORRECT FOR TARGET ROUTINE EXCEPT WHEN BUFFER ALLOCN
*        HAS LEFT A HOLE IN THE STACK
LONGJ2   DS    0H
         L     RSTACK,0(,RSTACK)       THIS NORMALLY DOESNT CHANGE
*        RSTACK, BUT WILL CORRECT IT IF WE WANT A HOLE IN STACK
*        (REFER TO COMMENTS UNDER OPENSUB)
         L     RBLK,=V(PASFLINK)       PTR TO DFCB CHAIN
LONGJ3   L     R2,0(,RBLK)             NEXT DFCB
         LTR   R2,R2                   TEST IF ANY
         BC    8,LONGJ7                BR IF FIN
         CR    R2,RSTACK
         BC    4,LONGJ6                BR IF THIS DFCB IS 'OWNED'
         USING DFCB,R2
         USING BLOCK,R3
         L     R3,DFCBBLK
         ST    R14,R14S
         TM    BLKSTAT,BSREAD+BSWRITE  OPEN?
         BC    8,LONGJ4
         ST    R2,OPENL
         MVI   OPENL,X'80'
         CLOSE MF=(E,OPENL)
LONGJ4   TM    BLKSTAT,BSLOCAL
         BC    8,LONGJ5                BR NOT LOCAL
         LA    R4,DFCBJ
         SR    R5,R5
         LA    R6,DFCBJ+132            VOLLIST
         L     R3,CAMSCR
         STM   R3,R6,CAMLST
         SR    R0,R0
         SCRATCH CAMLST
LONGJ5   L     R14,R14S
         L     R0,DFCBLINK             TAKE DCB ETC OFF CHAIN
         ST    R0,0(,RBLK)
LONGJ6   LA    RBLK,DFCBLINK
         B     LONGJ3
         SPACE
LONGJ7   LM    R0,R6,32(RSTACK)      RESTORE DISPLAY AND EXT ADRS REGS
         B     RUNTIME3                TO LONGJ TARGET VIA TIMEX TRAP
         DROP  R2,R3
         EJECT
*        DATE AND TIME ROUTINES - ANS ADDR IN R0
         SPACE
TIME     DS    0H
         TIME  DEC
         ST    R0,TIMWK1               HHMMSSTH
         UNPK  TIMWK2(7),TIMWK1(4)     UNPK TO ZONED DIGITS
         MVC   0(2,RPARM),TIMWK2       HH
         MVI   2(RPARM),C':'
         MVC   3(2,RPARM),TIMWK2+2     MM
         MVI   5(RPARM),C':'
         MVC   6(2,RPARM),TIMWK2+4     SS
         BR    RBASE
         SPACE
DATE     TIME  BIN
         ST    R1,TIMWK1               00YYDDDF
         TM    TIMWK1+1,1              TEST YR ODD
         BC    1,DATE1                 BR IF ODD (NOT LEAP)
         TM    TIMWK1+1,X'12'          TEST MULT OF 4
         BC    9,DATE2                 BR LEAP
DATE1    CP    TIMWK1+2(2),MONTH+2     >= MAR?
         BC    4,DATE2                 BR NOT
         AP    TIMWK1+2(2),=P'1'       ADD 1 TO NON-LEAP AFTER FEB
DATE2    LA    RWK,MONTH-2
         SR    RWK2,RWK2               MONTH COUNTER
DATE3    LA    RWK,2(,RWK)
         LA    RWK2,1(,RWK2)
         CP    TIMWK1+2(2),2(2,RWK)
         BC    2,DATE3                 BR IF NOT THIS MONTH
         SP    TIMWK1+2(2),0(2,RWK)    SUBTR MONTH BASE
         MVC   TIMWK2(6),=X'402120202020'
         ED    TIMWK2(6),TIMWK1+1      YY0DD
         MVC   6(2,RPARM),TIMWK2+1     YY
         MVC   0(2,RPARM),TIMWK2+4     DD
         CVD   RWK2,TIMWK1             MONTH
         MVC   TIMWK2(4),=X'402120202020'
         ED    TIMWK2(4),TIMWK1+6      MM
         MVC   3(2,RPARM),TIMWK2+2
         MVI   2(RPARM),C'/'
         MVI   5(RPARM),C'/'
         BR    RBASE                   RETURN
         SPACE
MONTH    DC    PL2'0',PL2'31',PL2'60',PL2'91',PL2'121'
         DC    P'152',P'182',P'213',P'244'
         DC    P'274',P'305',P'335',P'999'
         EJECT
CLOCKF   DS    0H
         TTIMER
         L     R1,=V(PASDATA)
         USING PASDATA,R1
         L     R1,PASDTIM
         DROP  R1
         SR    R1,R0
         M     R0,=A(10)
         D     R0,=A(384)
         LR    R0,R1
         ST    R0,0(,RSTACK)
         BR    RBASE
         EJECT
*        INTERFACES TO TRIG FUNCTIONS ETC
         SPACE
*        ON ENTRY, R0 --> FP REG CONTAINING ARGUMENT, AND WHICH
*        MUST CONTAIN ANSWER
         SPACE
         CNOP  0,4
         STDFN SIN
         STDFN COS
         STDFN EXP
         STDFN SQRT
         STDFN LN,LOG
         STDFN ARCTAN,ATAN
         SPACE
ARITH    SAVEF                         SAVE REGS
         SLL   RPARM,2                 REG NO INDEX
         LA    RPARM,SAVEFLOT(RPARM)   MAKE ABSOLUTE
         ST    RPARM,ARITHPAR          FORM PARM
         MVI   ARITHPAR,X'80'
         LA    R1,ARITHPAR
         L     R15,0(,RWK)             GET VCONST
         BALR  R14,R15                 LINK TO ROUTINE
         STD   F0,0(,RPARM)            SAVE ANSWER IN PROPER REGISTER
         LOADF                         RESTORE REGS
         LTR   R15,R15
         BCR   8,RBASE                 RETURN IF OK
         SR    R1,R1
         SR    R0,R0
         IC    R0,0(RWK)               GET ERROR NO
         B     ERRORB
         EJECT
INTPOWER DS    0H                      CALCULATES A ** B FOR B INTEGER
*        RPARM SPECIFIES A REG NO IN BITS 24-27, B REG IN 28-31
*        ALSO SPECIFIES A FLOATING (BIT 23 = 1) OR INT (BIT 23 = 0)
*        FOR B = 0, ANS= 1 OR 1.0
*        FOR B < 0, ERROR IF A INT, ANS=1./(A**(-B)) IF A FLOATING
*        ALGORITHM USED IS FROM KNUTH VOL 2 PAGE 399
         LA    R6,15                   MASK
         NR    R6,RPARM                B REG NO
         SLL   R6,2
         L     R6,0(R6,RSTACK)         B VALUE
         LA    R5,240
         NR    R5,RPARM                B REG NO * 16
         SRL   R5,2                    * 4
         SRA   R0,8                    TEST FP OR INT BASE
         BC    7,FINTP                 BR IF FLOATING
         L     R3,0(R5,RSTACK)         A VALUE
         SR    R2,R2                   CLEAR EVEN REG FOR MULTIPLY
         LTR   R3,R3                   TEST A -VE
         BC    10,*+6                  BR +VE
         BCTR  R2,0                    1S COMPLEMENT
         SR    R0,R0                   REG PAIR IS Y OF ALGORTHM,
         LA    R1,1                    EVENTUALLY ANSWER
         LTR   R6,R6                   TEST SIGN OF POWER
         BC    2,INTP1                 GET ON IF +VE
         BC    8,INTP3                 ANS 1 IF ZERO
         LR    R1,R6                   POWER TO R1
         BAL   R14,ERRORA              ERROR - EXPONENT OF INTEGER -VE
         DC    Y(ERRINTP)
INTP1    DS    0H                      START OF MAIN LOOP
         SRDL  R6,1                    / 2
         LTR   R7,R7                   TEST FORMER PARITY
         BC    4,INTP2                 BR IF ODD
INTP15   MR    R2,R3                   Z * Z
         SLDA  R2,32                   FORCE OVFLO IF TOO BIG
         SRDA  R2,32
         B     INTP1                   LOOP
INTP2    MR    R0,R3                   Y * Z
         LTR   R6,R6
         BC    7,INTP15                LOOP IF MORE TO DO
INTP3    ST    R1,0(R5,RSTACK)         SAVE ANSWER
         SLDA  R0,32                   FORCE OVFLO IF TOO BIG
         BR    RBASE                   RETURN
         EJECT
FINTP    DS    0H                      FLOATING TO INT POWER
         SAVEF
         LD    F0,=D'1'                Y
         LD    F2,SAVEFLOT(R5)         Z <-- A
         LTR   R6,R6                   SIGN OF POWER
         BC    2,FINTP1                OK IF +VE
         BC    8,FINTP3                RETURN 1. IF ZERO
         LCR   R6,R6                   MAKE POWER +VE IF -VE
         DDR   F0,F2                   AND TAKE RECIPROCAL OF VALUE
         LDR   F2,F0                   RESTORE REGS
         LD    F0,=D'1'
FINTP1   SRDL  R6,1                    HALVE POWER
         LTR   R7,R7                   TEST IF WAS ODD
         BC    4,FINTP2                BR IF SO
FINTP15  MDR   F2,F2                   Z * Z
         B     FINTP1                  LOOP
FINTP2   MDR   F0,F2                   Y * Z
         LTR   R6,R6
         BC    7,FINTP15               LOOP
FINTP3   STD   F0,SAVEFLOT(R5)         SAVE ANSWER
         LOADF                         RESTORE REGS
         BR    RBASE                   RETURN
         EJECT
*        SERVICE ROUTINES FOR READ TEXT
         USING BLOCK,RBLK
         SPACE
INITR    DS    0H                      LOADS RDFCB, TESTS OPEN,
*                                      ISSUES ERROR IF EOF,
*                                      LOADS RP (POINTER), RMAX(LIMIT)
*                                      GETS NEW RECORD IF REQ
         L     RDFCB,BLKDFCB           DFCB BASE
         USING DFCB,RDFCB
         TM    BLKSTAT,BSREAD          TEST READ STATUS
         BC    8,MODERERR
         TM    BLKSTAT,BSEOF           TEST EOF STATUS
         BC    1,OVEREOF
         LM    RP,RMAX,BLKPC           CURRENT, END
         BR    RSIGN
         SPACE
SIGN     DS    0H                      GET 0 OR 2 TO RSIGN FOR + OR -
         CR    RP,RMAX                 RETURN ON R15
         BC    10,NERROR
         SR    RSIGN,RSIGN             INITIALIZE
         CLI   0(RP),C'+'
         BC    8,SIGN1                 SHIFT PTR IF +
         CLI   0(RP),C'-'
         BCR   7,R15                   RETURN NOT -
         LA    RSIGN,2                 SET VAL FOR -
SIGN1    LA    RP,1(,RP)               UPDATE PTR
         BR    R15
         SPACE
CNUMBER  DS    0H                      CHECK NEXT DIGIT IS NUMBER
         CR    RP,RMAX                 RETURN ON R15
         BC    10,NERROR
         CLI   0(RP),C'0'              C < '0' IS ERROR
         BC    4,NERROR
         CLI   0(RP),C'9'              C > '9' IS ERROR
         BCR   12,R15
NERROR   BAL   R14,ERFIL
         DC    Y(ERRNO)                FORMAT ERROR - NUMBER EXPECTED
         SPACE
NUMBER   DS    0H                      DECODE INTEGER INTO RVAL
         LA    R14,NUM2
         SR    RVAL,RVAL
         SPACE
NUM0     DS    0H
         SR    RDIG,RDIG
NUM1     CR    RP,RMAX
         BCR   10,R15                  RETURN IF END OF RECORD
         IC    RDIG,0(,RP)             GET NEXT DIG
         SL    RDIG,=A(C'0')           CONV TO INTEGER
         CL    RDIG,=A(9)              TEST FOR DIG
         BCR   2,R15                   RETURN IF NOT
         LA    RP,1(,RP)
         BR    R14                     SELECT ENDING ROUTINE
         SPACE
NUM2     LR    RWK2,RVAL+1             SAVE REG
         LA    RVAL+1,10
         MR    RVAL,RVAL
         SLDA  RVAL,32                 FORCE OVERFLOW TRAP IF TOO BIG
         LR    RVAL+1,RWK2             RESTORE
         AR    RVAL,RDIG               ACCUMULATE RESULT
         B     NUM1
         SPACE
NUM3     BCTR  RVAL,0                  ACCUMULATE EXP AFTER POINT
NUM4     ST    RDIG,CONVWORK+4
         MD    FVAL,=D'10.'
         AD    FVAL,CONVWORK
         B     NUM1
         SPACE
READIN   DS    0H                      READ NEXT CARD AND SET POINTERS
*                                      RETURN ON RDIG
         GET   (RDFCB)                 POINTER RETURNS IN RP=R1
         LA    RP,0(,RP)               REMOVE TOP BYTE (FOR MVS)
         ST    RP,BLKPS                STORE PTR TO START
         LH    RMAX,DCBLRECL
         ALR   RMAX,RP                 CALC END OF LINE
         CLI   DCBRECFM,DCBRECF
         BC    10,READ1                BR IF RECFM F OR U
         LA    RP,4(,RP)               PAST RDW FOR RECFM V
READ1    DS    0H
         STM   RP,RMAX,BLKPC           STORE CURRENT AND END
         NI    BLKSTAT,255-BSEOL
         BR    RDIG
         SPACE
GETNB1   BAL   RDIG,READIN
GETNB    TM    BLKSTAT,BSEOF           ROUTINE TO GET NON-BLANK
         BC    1,OVEREOF               RETURN ON RSIGN
GETNB2   CR    RP,RMAX
         BC    10,GETNB1
         CLI   0(RP),C' '
         BCR   7,RSIGN                 RETURN
         LA    RP,1(,RP)
         B     GETNB2
         EJECT
READINT  DS    0H                      READ AN INTEGER FROM THE TEXTFIL
         BAL   RSIGN,INITR             LOAD RDFCB, TEST MODE AND EOF
         USING DFCB,RDFCB
         USING BLOCK,RBLK
         BAL   RSIGN,GETNB             GET NON-BLANK
         BAL   R15,SIGN                SET SIGN IF ANY
         BAL   R15,CNUMBER             CHECK NUMBER EXISTS
         BAL   R15,NUMBER              GET NUMBER
         EX    0,SIGNEX(RSIGN)         SET SIGN
         ST    RVAL,0(,RPARM)          STORE ANSWER
         SPACE
SETEOLN  DS    0H                      TEST FOR END OF LINE
         TM    BLKSTAT,BSEOL
         BC    1,SETEOL1
         CR    RP,RMAX                 TEST IF EOL
         BC    4,SETEOL1
         OI    BLKSTAT,BSEOL           SET EOL BIT
         BCTR  RP,0                    BACK ONE
         BCTR  RMAX,0                  CHAR
         MVI   0(RP),C' '              FAKE A BLANK FOR EOL CHAR
SETEOL1  DS    0H
         STM   RP,RMAX,BLKPC           STORE REGS
         BR    RBASE                   RETURN TO PASCAL PROG
         SPACE
SIGNEX   BCR   0,0                     NOP
         LCR   RVAL,RVAL               SET -VE
         SPACE
READREAL DS    0H
         BAL   RSIGN,INITR
         BAL   RSIGN,GETNB             GET TO NON-BLANK
         BAL   R15,SIGN
         ST    RSIGN,MAINSIGN          SET SIGN OF NO
         CR    RP,RMAX                 TEST IF EOL
         BC    10,NERROR
         SAVEF
         SDR   FVAL,FVAL               INITIA;ISE ANSWER
         MVC   CONVWORK(4),=X'4E000000' INITIALISE CONVERT WK AREA
         SR    RVAL,RVAL               INITIALISE EXPONENT
         CLI   0(RP),C'.'              TEST IF POINT
         BC    8,READR1                OK IF SO
         BAL   R15,CNUMBER             CHECK IF NUMBER
         LA    R14,NUM4
         BAL   R15,NUM0
         CLR   RP,RMAX                 TEST EOL
         BC    10,READR6               BR IF SO
         CLI   0(RP),C'.'              TEST POINT
         BC    7,READR2                TEST FOR E IF NOT
READR1   LA    RP,1(,RP)               PAST POINT
         CR    RP,RMAX                 TEST EOL
         BC    10,READR25
         LA    R14,NUM3
         BAL   R15,NUM0
READR2   LR    REXP,RVAL
         CLI   0(RP),C'E'              SEE IF EXPONENT SPEC
         BC    7,READR25
         LA    RP,1(,RP)
         BAL   R15,SIGN
         BAL   R15,CNUMBER             INSIST ON NUMERIC AFTER 'E'
         BAL   R15,NUMBER              EVALUATE NO
         LTR   RSIGN,RSIGN
         BC    8,*+6
         LCR   RVAL,RVAL
         AR    REXP,RVAL
READR25  DS    0H
         LD    FWORK,=D'1.'
         LD    FEXP,=D'10.'
         LTR   REXP,REXP
         BC    8,READR6
         BC    2,READR3
         LD    FEXP,=D'.1'
         LPR   REXP,REXP
READR3   EX    REXP,TMODD              TEST IF ODD
         BC    8,READR4
         MDR   FWORK,FEXP
READR4   SRA   REXP,1
         BC    8,READR5
         MDR   FEXP,FEXP
         B     READR3
READR5   MDR   FVAL,FWORK
READR6   L     RSIGN,MAINSIGN          GET ANSWER SIGN
         EX    0,NOPLCDR(RSIGN)        NEGATE IF REQ
         STD   FVAL,0(RPARM)           SAVE ANSWER
         LOADF
         B     SETEOLN                 RETURN
         SPACE
TMODD    TM    =AL1(1),0               TEST REGISTER ODD
NOPLCDR  NOPR  0
         LCDR  FVAL,FVAL
         SPACE
READCHAR DS    0H                      READ ONE CHAR
         BAL   RSIGN,INITR
         SR    RDIG,RDIG
         IC    RDIG,0(RP)              GET CHAR
         ST    RDIG,0(,RPARM)          SAVE AS ANSWER
READCH1  LA    RP,1(,RP)               POINT TO NEXT
         CR    RP,RMAX
         BC    12,SETEOLN
READCH2  BAL   RDIG,READIN
         B     SETEOLN
         SPACE
         SPACE
GETTEXT  BAL   RSIGN,INITR
         B     READCH1
         SPACE
READLN   BAL   RSIGN,INITR
         B     READCH2
         SPACE 2
*        READ STRING - COPY MIN(DATALENGTH, BYTES REQ, RECORD REMAINDR)
*        AND PAD OUT WITH BLANKS TO DATA LENGTH
READSTRG BAL   RSIGN,INITR
         BAL   RSIGN,SETINT
         SRL   RPARM,8                 GET DATA LENGTH
         CR    RPARM,RLEN              GET MIN (RPARM, RLEN, RMAX-RP)
         BC    2,*+6
         LR    RLEN,RPARM
         LR    RWK,RMAX
         SR    RWK,RP
         CR    RWK,RLEN                CMP REC REM VS MIN
         BC    2,*+6
         LR    RLEN,RWK
         BCTR  RLEN,0                  BYTES-1 TO MOVE
         LR    RWK,RVAL                RVAL IS NO GOOD FOR MVC
         LTR   RLEN,RLEN
         BC    4,*+8
         EX    RLEN,MVCRS1             MOVE DATA
         LA    RP,1(RLEN,RP)           UPDATE RECORD POINTER
         SR    RPARM,RLEN              LEN + 1 TO PAD
         BCT   RPARM,*+8
         B     SETEOLN                 AWAY IF NONE TO PAD
         LA    RWK,1(RLEN,RWK)         PLACE FOR PADDING
         MVI   0(RWK),C' '
         BCT   RPARM,*+8
         B     SETEOLN                 BR IF ONLY 1 PAD
         BCTR  RPARM,0                 FOR EX MVC
         EX    RPARM,MVCRS2
         B     SETEOLN
         SPACE
MVCRS1   MVC   0(0,RWK),0(RP)
MVCRS2   MVC   1(0,RWK),0(RWK)
         EJECT
*        DCB EXIT ROUTINE FOR TEXT AND NON-TEXT DCBS
         USING BLOCK,RBLK
         USING DFCB,RDFCB
RTEXL    DC    A(0),X'85',AL3(DCBEXIT) SPACE FOR JFCB PTR, DCB EXIT
DCBEXIT  DS    0H
         SPACE
*        IF RECFM IS NOT SET, SET IT
         CLI   DCBRECFM,0
         BC    7,DCBE4                 IF ANY BITS SET, ASSUME OK
         MVI   DCBRECFM,X'90'          SET FB DEFAULT
         CLI   BLKLRECL,X'FF'          TEST IF TEXT FILE
         BC    7,DCBE4                 OK IF NOT
         MVI   DCBRECFM,X'54'          SET VBA DEFAULT IF TEXT
DCBE4    DS    0H
        SPACE
*        TEST LRECL IS SET
         LH    R0,DCBLRECL
         LTR   R0,R0
         BC    7,DCBE3                 BR IF SET
*        HERE LRECL NOT SET, NOW FIND A SUITABLE VALUE FOR IT
*        IT CAN ONLY BE STD INPUT OR A TEXT FILE
         TM    BLKSTAT,BSSTD           IF STD INPUT
         BC    8,DCBE1                 USE 80
         LA    R0,80
         B     DCBE2
DCBE1    DS    0H                      MUST BE TEXT FILE HERE
         LH    R0,DCBBLKSI
         LTR   R0,R0                   USE BLKSI IF SET
         BC    8,DCBE16
         CLI   DCBRECFM,X'80'
         BC    10,*+8                  BR IF NOT V
         SH    R0,=Y(4)                USE BLKSI - 4 FOR V
         B     DCBE25
DCBE16   DS    0H
         LA    R0,137                  USE LRECL=137 DEFAULT
         TM    DCBRECFM,X'80'          UNLESS RECFM F OR U
         BC    8,DCBE2
         LA    R0,133                  THEN USE 133
DCBE2    TM    DCBRECFM,4              TEST IF RECFM A
         BC    1,*+6                   BR IF SO
         BCTR  R0,0                    ELSE SUBTR 1
DCBE25   DS    0H
         STH   R0,DCBLRECL             SAVE LRECL
DCBE3    DS    0H
         SPACE
*        SET BLKSI IF NOT SET ALREADY
         LH    R0,DCBBLKSI
         LTR   R0,R0
         BC    7,DCBE6                 BR IF SET ALREADY
         LH    R3,DCBLRECL
         TM    DCBRECFM,X'10'          TEST IF RECFM=B
         BC    8,DCBE45                BR IF NOT BLOCKED
         TM    DCBRECFM,X'C0'          OR IF RECFM U
         BC    1,DCBE5
         LR    R0,R3
         SR    R2,R2
         LA    R3,2000                 GET BLKSI NEAR 2000
         DR    R2,R0
         LA    R3,1(,R3)
         MR    R2,R0
DCBE45   DS    0H
         CLI   DCBRECFM,X'80'
         BC    10,DCBE5                BR IF NOT RECFM V
         LA    R3,4(,R3)               SET BLKSI 4+MULT(LRECL) FOR VB
DCBE5    STH   R3,DCBBLKSI             SAVE BLKSI IN DCB
DCBE6    DS    0H
         SPACE
*        HERE RECFM,LRECL,BLKSI HAVE BEEN SET
*        NOW BUILD BUFFER POOLS ON THE PASCAL STACK SO THAT
*        NO O.S. GETMAIN WILL BE REQUIRED, AND HENCE INITIAL
*        STACK CAN BE ALL BUT A FEW K OF THE REGION
         SPACE
         STM   R14,R1,R14S             SAVE REGS
         SPACE
*        IF O/P TEXT FILE  GET A PUT MOVE BUFFER
         TM    BLKSTAT,BSWRITE
         BC    8,DCBE7                 BR NOT O/P
         CLI   BLKLRECL,X'FF'
         BC    7,DCBE7                 BR NOT TEXT
         TM    DCBBUFCB+3,1            SEE IF 1ST OPEN
         BC    8,DCBE7                 BR IF NOT
         LH    R0,DCBLRECL             GET BUFFER SIZE REQD
         BAL   R14,GETMAIN             GET BUFFER
         BC    7,DCBE7                 BR NO GOOD
         ST    RSTACK,RSSAVE           SAVE NEW STACK POSN
         ST    R1,BLKPS                SAVE BUFFER ADDR
DCBE7    DS    0H
         SPACE
         SR    R3,R3
         IC    R3,DCBBUFNO             GET NO OF BUFFERS
         LTR   R3,R3                   TEST IF ANY SPEC
         BC    2,*+8                   BR IF SO
         LA    R3,2                    DEFAULT 2 BUFFERS
         LA    R0,3                    ROUNDING CONST
         AH    R0,DCBBLKSI
         N     R0,=F'-4'               ROUND BLKSI UP TO FW
         STH   R0,DCBBUFL              SAVE AS BUFL
         SR    R4,R4                   INITIALISE RCD REQMNT
         CLI   DCBRECFM,X'80'          IS IT RECFM=V
         BC    10,DCBE11               BR IF NOT
         TM    DCBRECFM,8              IS IT ALSO S ( IE VS OR VBS)
         BC    8,DCBE11                BR IF NOT
*        HERE WE MUST ALLOC SOME SPACE FOR RECD AND USE BUILDRCD
         LH    R4,DCBLRECL
         LA    R4,32+4+3(,R4)          SPACE FOR RECD + 32 BYTE PREFIX
*                                     + 4 BYTE EXTRA BUFCB, ROUND TO FW
         N     R4,=F'-4'               ROUND
DCBE11   DS    0H
         TM    DCBBUFCB+3,1            TEST IF BUFCB GOT ALREADY
         L     R1,DCBBUFCB
         BC    8,DCBE13                BR IF SO - JUST BUILD AGAIN
*        HERE TRY TO GET BUFFER POOL ON STACK, REDUCING BUFNO TILL
*        SUCCESSFUL. IF NOT SUCCESSFUL, GIVE UP AND LET AUTO
*        BUFFER ACQUISITION TRY ITS LUCK
DCBE12   LR    R1,R3                   BUFNO
         MH    R1,DCBBUFL              BYTES FOR ALL BUFFERS
         LA    R0,8(R1,R4)             8 BYTE PREFIX + RCD AREA
         BAL   R14,GETMAIN             TRY TO GET ON STACK
         BC    8,DCBE13                BR IF GOT OK
         BCT   R3,DCBE12               TRY WITH FEWER BUFFERS
         B     DCBE15                  BR IF NOT ENOUGH, LET AUTO TRY
DCBE13   DS    0H
         ST    RSTACK,RSSAVE           SO RETURN FROM OPEN CORRECTS REG
         STC   R3,DCBBUFNO             SET BUFNO TO DCB
         SLL   R3,16                   FORM REG PARAMETER FOR BUILD
         AH    R3,DCBBUFL              MACRO
         LTR   R4,R4                   TEST IF BUILD OR BUILDRCD REQD
         BC    7,DCBE14                BR IF BUILDRCD
*        NOW DO BUILD TO FORMAT BUFFER POOL ON STACK
         NI    DCBBFTEK,X'8F'          REMOVE BFTEK=A INDICATION TO
*              STOP O.S. BUILDING CRAZY CHANNEL PROGRAMS /././.
         IC    R0,DCBBUFNO             SAVE NO OF BUFFERS
         ST    R1,DCBBUFCB             SAVE ADDR IN DCB
         STC   R0,DCBBUFNO             REPAIR NO OF BUFFERS
         LR    R0,R3
         BUILD (1),(0)
         B     DCBE15
         SPACE
DCBE14   DS    0H
         LR    R5,R4                   REC LENGTH INCL 4 OF BUFCB
         A     R5,=F'-4'               CORRECT FOR BUFCB EXTRA
         LR    R4,R1                   REC AREA ADDR
         LA    R2,0(R4,R5)             BUFCB ADDR
         IC    R0,DCBBUFNO             SAVE BUFNO
         ST    R2,DCBBUFCB             SAVE IN DCB
         STC   R0,DCBBUFNO             REPLACE BUFNO
         STM   R2,R5,BUILDRL           SAVE IN LIST FORM BUILDRCD
         OI    BUILDRL+12,X'80'        SEND END-OF-LIST BIT
         BUILDRCD MF=(E,BUILDRL)
DCBE15   DS    0H
         LM    R14,R1,R14S             RESTORE REGS
         BR    R14                     RETURN FROM DCB EXIT
         DROP  RDFCB,RBLK
         SPACE
         LTORG
         EJECT
*        WORK AREAS
         SPACE
         ENTRY RTSTAK                  INITIALISED BY PASRUN
RTSTAK   DC    A(0)
LOCFILNO DC    A(0)                    INCREMENTED EVERY LOCAL FILE OPN
SAVEFLOT DC    4D'0'
ARITHPAR DC    A(0)
         ORG   SAVEFLOT
*        WORK AREA FOR OPEN
R14S     DC    4A(0)                   REG SAVE IN DCBEY
RSSAVE   DC    A(0)                    ENABLES DCBEX TO ADJUST RSTACK
BUILDRL  DC    4A(0)                   BUILDRCD LIST FORM
CAMLST   EQU   BUILDRL                 CAMLST FOR SCRATCH
OPENL    DC    A(0)                    OPEN - LIST FORM
         ORG   SAVEFLOT+8
         DC    5D'0'                   5 DBLWDS FOR WRITFP WK AREA
         ORG
WORK     DC    D'0'
         SPACE
         ORG WORK
TIMWK1   DC    D'0'
TIMWK2   DC    D'0'
         SPACE
         ORG   WORK
CONVWORK DC    D'0'
MAINSIGN DC    A(0)
         ORG   WORK
WORKED   DC    CL16' '
WORK1    DC    D'0'
         DC    2D'0'                   ENOUGH SPACE FOR WRITFP
         ORG
         EJECT
         USING BLOCK,RBLK
         USING DFCB,RDFCB
*        SERVICE ROUTINES FOR WRITE TEXT
         SPACE
INITW    DS    0H                      LOADS RDFCB, TESTS OPEN
*                                      LOADS RP(CURRENT), RMAX(LIMIT)
         L     RDFCB,BLKDFCB
         TM    BLKSTAT,BSWRITE
         BCR   7,RSIGN
MODEWERR BAL   R14,ERFIL
         DC    Y(ERRMW)
         SPACE
SETINT   LA    RWK,240                 RETURNS RVAL AND RLEN
         NR    RWK,RPARM               FROM REG IND IN BITS
         SRL   RWK,2                   24-27 AND 28-31 OF R0
         L     RVAL,0(RWK,RSTACK)
         LA    RWK,15
         NR    RWK,RPARM
         SLL   RWK,2
         L     RLEN,0(RWK,RSTACK)
         BR    RSIGN
         SPACE 2
WRITBOOL BAL   RSIGN,INITW
         BAL   RSIGN,SETINT
         USING *,RSIGN
         LA    RPARM,5
         LTR   RVAL,RVAL               TEST T OR F
         LA    RVAL,=C'FALSE'
         BC    8,FINAL                 BR IF F
         LA    RVAL,=C'TRUE'
         BCT   RPARM,FINAL             BR T
         DROP  RSIGN
         SPACE 2
WRITEINT BAL   RSIGN,INITW
         BAL   RSIGN,SETINT            GET RVAL, RLEN
         CVD   RVAL,WORK1
         MVC   WORKED,EDPAT
         LA    R1,WORKED+15            BACKSTOP FOR EDMK
         EDMK  WORKED,WORK1
         BC    10,WRI1                 BR NOT MINUS
         BCTR  R1,0
         MVI   0(R1),C'-'              PUT SIGN IN
WRI1     DS    0H
         LR    RVAL,R1
         LA    RPARM,WORKED+16
         SR    RPARM,RVAL
         B     FINAL
EDPAT    DC    X'40202020202020202020202020202120'
         SPACE
WRTREALE LA    RTYP,4
         B     WRT1
WRTREALF SR    RTYP,RTYP
WRT1     DS    0H
         BAL   RSIGN,INITW
         SAVEF
*        RPARM CONTAINS 3 REG NUMBERS:
*        BITS 20-23 - FLOATING REG CONTAINING VALUE
*             24-27 - REG CONTG FIELD LENGTH
*             28-31 - REG CONTAINING NUMBER OF FRACTION DIGITS
         BAL   RSIGN,SETINT
         LA    RWK,X'F00'              MASK
         NR    RWK,RPARM
         SRL   RWK,6
         LD    FVAL,SAVEFLOT(RWK)
*        NOW INTERFACE TO WRITFP ROUTINE TO GET CHAR STRING
*        REPRESENTING FLOATING POINT NUMBER
         LR    R2,RVAL                 FIELD WIDTH
         CH    R2,*+10
         BC    12,*+8
         LA    R2,23                   SET 23 MAX FIELD WIDTH
         LR    R4,RLEN                 FRACTION DIGITS
         LA    R5,SAVEFLOT+8           5 DBLWDS WORK SPACE
         USING SAVEFLOT+8,R5
         L     R15,=V(WRITFP)
         DROP  R5
         BALR  R14,R15
*        WRITFP FORMATS NO TO SAVEFLOT+8
*        ENTRY: R2 = TOTAL FIELD SIZE REQ (<= 23) (WRITFP MAY
*                                      INCREASE IT)
*               R3 = 0/4 FOR F/E TYPE OUTPUT
*               R4 = NUMBER OF FRACTION DIGITS
*               R5 => 5 DOUBLEWORD WK AREAS
         LR    RLEN,RVAL               FIELD WIDTH REQ BY USER
         LR    RPARM,R2                SIG CHARS IN ANSWER
         LA    RVAL,SAVEFLOT+8         ADDR OF ANSWER
         B     FINAL
         SPACE
WRITCHAR BAL   RSIGN,INITW
         BAL   RSIGN,SETINT
         STC   RVAL,WORK
         LA    RVAL,WORK
         LA    RPARM,1
         B     FINAL
         SPACE
PUTTEXT  BAL   RSIGN,INITW
         LM    RP,RMAX,BLKPC
         LA    RP,1(,RP)
         B     FINAL1
         SPACE
WRITSTRG DS    0H                      WRITE CHAR STRING
         BAL   RSIGN,INITW
         BAL   RSIGN,SETINT
         SRL   RPARM,8                 LEN OF STRING
         CR    RPARM,RLEN
         BC    12,FINAL
         LR    RPARM,RLEN
         EJECT
*        ALL WRITE TEXT ROUTINES EXCEPT PUTTEXT FINISH HERE WITH
*        CHAR STRING READY TO OUTPUT
*        RLEN = FIELD WIDTH REQ BY USER (PERHAPS INCREASED IF TOO LOW)
*        RVAL => START OF CHAR STRING TO OUTPUT
*        RPARM = NO OF SIG CHARS TO BE WRITTEN
*        IF RLEN < RPARM, IT IS INCREASED TO RPARM
*        IF RLEN > RPARM, CHAR STRING IS RIGHT JUSTIFIED
         SPACE 2
FINAL    DS    0H
         CR    RLEN,RPARM              INCREASE RLEN IF TOO SMALL
         BC    10,*+6
         LR    RLEN,RPARM
         LM    RP,RMAX,BLKPC
         LA    RWK,0(RLEN,RP)          END OF PROPOSED
         SR    RWK,RPARM               START OF PROPOSED
         LR    RLEN,RPARM              LEN OF SIG PART
         LR    RPARM,RVAL              ADDR OF SOURCE
WRS1     DS    0H
         CR    RWK,RMAX                SEE IF START AFTER THIS REC
         BC    4,WRS2                  BR IF NOT
         SR    RWK,RMAX                EXCESS
         BAL   RSIGN,WRPUT             OUTPUT REC
         AR    RWK,RP                  START POSN IN THIS REC
         B     WRS1                    LOOP TILL IN THIS REC
WRS2     DS    0H
*        RWK --> START OF WHERE STRING LENGTH RLEN IS TO GO
         LR    RP,RWK
WRS3     DS    0H
         AR    RWK,RLEN                END OF STRING
         CR    RWK,RMAX                SEE IF STRING WITHIN RECD
         BC    12,WRS4                 BR IF SO
         SR    RWK,RMAX                EXCESS LENGTH
         SR    RLEN,RWK                LENGTH IN REC
         BCTR  RLEN,0                  FOR EX
         BC    12,*+8                  SKIP IF LEN <= 0
         EX    RLEN,WRSMVC             MOVE IT IN
*        NOTE NO PROVISION FOR LRECL > 256
         LA    RPARM,1(RPARM,RLEN)     UPDATE SOURCE PTR
         LR    RP,RMAX
         BAL   RSIGN,WRPUT             OUTPUT RECD
         LR    RLEN,RWK                LENGTH REMAINING
         LR    RWK,RP
         TM    DCBRECFM,4              TEST IF RECFM A
         BC    8,WRS3
         LA    RWK,1(,RWK)
         B     WRS3                    LOOP TO GET RID OF IT
WRS4     DS    0H
         LTR   RLEN,RLEN
         BCTR  RLEN,0
         BC    12,*+8                  SKIP IF LEN <= 0
         EX    RLEN,WRSMVC             MOVE LAST (OR ONLY) PART IN
         LA    RP,1(RLEN,RP)           UPDATE POINTER
         SPACE
FINAL1   DS    0H                      PUTTEXT FINISHES HERE
         ST    RP,BLKPC
         BR    RBASE                   RETURN
WRSMVC   MVC   0(0,RP),0(RPARM)        FOR EX TO MOVE STRING
         SPACE
WRPUT    DS    0H                      SUBROU OUTPUTS 1 REC
         L     RMAX,BLKPS
         TM    DCBRECFM,X'40'
         BC    8,WRPUT1                BR IF F
         SR    RP,RMAX
         TM    DCBRECFM,4              TEST IF RECFM A
         BC    8,WRPUT0                BR NOT
         LA    R14,6                   MIN LEN +1 FOR VA OR VBA
*        THE +1 IS ADDED BECAUSE SOME HASP SYSTEMS PRINT NOTHING
*        FOR MIN LEN RECORDS
         TM    DCBRECFM,X'C0'          TEST IF U
         BC    14,*+8                  BR NOT U
         LA    R14,2                   MIN LEN +1 FOR UA
         CR    RP,R14
         BC    10,*+6                  BR IF > MIN
         LR    RP,R14                  USE MIN LEN
WRPUT0   DS    0H
         TM    DCBRECFM,X'C0'          TEST IF U
         BC    14,WRPUT05              BR NOT
         STH   RP,DCBLRECL             SAVE LENGTH IN LRECL FOR U
         B     WRPUT1
WRPUT05  DS    0H
         SLL   RP,16                   SET UP RDW
         ST    RP,12(R13)
         MVC   0(4,RMAX),12(R13)
         LA    RMAX,4(,RMAX)           SPOT FOR PAGE CONTROL
WRPUT1   DS    0H
         TM    BLKSTAT,BSPAGE          SEE IF PAGE REQ
         BC    8,WRPUT3
         TM    DCBRECFM,4              TEST IF RECFM A
         BC    8,WRPUT2                IF NOT A, PAGE DOES NOTHING
         MVI   0(RMAX),C'1'            PAGE CARR CNTRL CHAR
WRPUT2   NI    BLKSTAT,255-BSPAGE      TURN PAGE BIT OFF
WRPUT3   DS    0H
         L     R0,BLKPS                IN CASE MOVE MODE
         PUT   (RDFCB)                 OUTPUT LAST BLK
WRPUT4   DS    0H
         BALR  R6,0
         USING *,R6
         LH    RMAX,DCBLRECL           GET LENGTH OF MAX RECD
         TM    DCBRECFM,X'C0'          IF RECFM U
         BC    14,*+8
         LH    RMAX,DCBBLKSI           USE BLKSI INSTEAD
         BCTR  RMAX,0
         BCTR  RMAX,0                  -2
         MVI   0(RP),C' '              BLANK OUT LINE
         EX    RMAX,WRXMVC3
         DROP  R6
         LA    RMAX,2(RMAX,RP)         POINT TO END OF LINE
         ST    RMAX,BLKPE
         CLI   DCBRECFM,X'80'          TEST FOR RECFM V
         BCR   10,RSIGN                DONE IF NOT
         LA    RP,4(,RP)               LEAVE SPACE FOR RDW IF V
         BR    RSIGN                   RETURN
         SPACE
WRXMVC3  MVC   1(0,RP),0(RP)
         LTORG
         SPACE
WTOWORK  DC    2A(0),CL80' '
         SPACE
REWRTTXT DS    0H
         L     RDFCB,BLKDFCB
         TM    BLKSTAT,BSSTD
         BC    7,INHIBIT
         BAL   RWK,CLOSESUB            CLOSE IF OPEN
         BAL   RWK,OPENSUBO            OPEN OUTPUT
         L     RP,BLKPS                BUFFER ADDR
         BAL   RSIGN,WRPUT4            CLEAR BUFFER
         ST    RP,BLKPC
         BR    RBASE
         SPACE
         SPACE
PAGE     BAL   RSIGN,INITW
         OI    BLKSTAT,BSPAGE          SET PAGE BIT
*        NOTE ISSUING PAGE IN A LOOP WITHOUT WRITELN MAKES ONLY 1 PAGE
         BR    RBASE
         SPACE
WRITELN  BAL   RSIGN,INITW
         L     RP,BLKPC
         BAL   RSIGN,WRPUT             PUT REC OUT NOW
         ST    RP,BLKPC
         BR    RBASE
         SPACE
         DROP  RDFCB,RBLK
         DROP  RBASE
         EJECT
PASRUN   CSECT
*        ON ENTRY, STACK ADDR IN R1
*        HEAP TOP ADDR IN RNP (NEW POINTER)
*        PROGRAM ENTRY POINT IS 1ST PROC ADDR AT SYSCON(,RSTACK)
*        THIS ROUTINE INITIALISES SYSTEM CONSTANTS ON STACK
*        AND ENTERS USER PROGRAM
         SPACE
         USING *,R15
         STM   R14,R12,12(R13)
         USING PASDATA,R11
         OI    PASDFLG,X'80'           SET RUN ACTIVE
         SR    R1,R1
         LA    R0,ERRTOVL              TIME OVFLO IN LOADING
         TM    PASDFLG,X'40'           TEST IF OVFLO DURING LOAD
         BCR   1,R14                   RETURN IF SO
         L     RSTACK,HEAD
         LR    R14,R13
         LA    R13,SAVEAREA
         ST    R13,8(,R14)
         ST    R14,4(,R13)             LINK SAVE AREAS
         SPACE
*        INITIALISE SYSTEM CONSTANTS
         L     R14,=V(RTSTAK)
         ST    RSTACK,0(,R14)          SAVE STACK ADDR IN RUNTIME
         MVC   0(256,RSTACK),STKPAT
         MVC   256(256,RSTACK),STKPAT+256
         MVC   512(STKL-512,RSTACK),STKPAT+512
         LR    R1,RSTACK
         USING STACK,R1
         L     RNP,TAIL                GET HEAP PTR
         ST    RNP,STKNP               SET UP NPINIT
         L     R15,=V(INTERUPT)
         DROP  R15
         SPIE  (15),MF=(E,SPIEL)
         ST    R1,OLDPICA
         DROP  R11
         BALR  R9,0
         USING *,R9
         LM    0,6,=7X'7F7F7F7F'
         LM    9,15,=7X'7F7F7F7F'
         DROP  R9
         LR    R1,RSTACK               SO 1ST ENTRY WILL WORK
         SR    R15,R15
         B     8(,R1)                  ENTER 1ST PROC, AVOIDING SAVE
         SPACE
         LTORG
         EJECT
*        PATTERNS FOR INITIALISING SYSTEM CONSTS
         SPACE
         USING STACK,R1
STKPAT   DC    0D'0'   FOLLOWING CODE LIVES AT BOTTOM OF SYSTEM CONST
*                      AREA (R1 BASE) SO THAT PROC CALLS CAN BE MADE
*                      VIA THE SEQUENCE  BALR R9,R1
*                                        DC   Y(DISPL)
*                      WHERE DISPL IS THE OFFSET FROM THE SYSCON BASE
*                      TO THE REQD PROC ADDRESS
STACK    DS    0D
         LH    R15,0(,R9)              GET DISPL TO PROC ADDRS IN TABLE
         STM   RNP+1,RNP-1,0(RSTACK)   SAVE REGS ON STACK
ENTRYVAR EQU   *-STACK                 VARIABLE PROC CALL ENTERS HERE
         L     R14,SYSCON+*-*(R15,R1)  LOAD PROC ADDRESS
*        EXTERNAL PROCEDURE LINKAGES CHANGE THE *-*
*        LONGJUMP EXECUTES ABOVE INSTR WITH EX 0,8(,1)
*        INITIAL ENTRY JUMPS TO HERE
         LR    R0,RSTACK               STACK TO R0
         A     RSTACK,0(,R14)          ALLOCATE DISPLAY SPACE
         CLR   RNP,RSTACK              TEST STK OVFLO OR EXTEND ADDRESS
STKTO1   DS    0AL4                    TIMEX CHANGES THIS TO BC 10,STK2
         BC    10,4(,R14)              ENTER ROUTINE IF NEITHER
         LA    RSTACK,0(,RSTACK)       STRIP TOP BYTE
         CLR   RNP,RSTACK              TEST STK OVFLO
         BC    4,STK3                  BR IF STK OFLO
*
*
*
STK1     DS    0H                      LOAD EXTENDED ADDRESSING REGS
         SR    R15,R15
         IC    R15,0(,R14)
         LA    R6,14
         EX    R6,4(,R14)              GET CODE BASE TO DISPL REG
         BALR  R9,0                    IN CASE LEVEL=1
         USING *,R9
         B     *(R15)                  BR TO * + 4*LEVEL
         DROP  R9
         LA    R2,4082(R6,R1)          LOAD EXTENDED
         LA    R3,4082(R6,R2)          ADDRESSING
         LA    R4,4082(R6,R3)          REGISTERS
         LA    R5,4082(R6,R4)
STK2     DS    0H                      TIMEX CHANGES TO EX 0,4(14)
         LA    R6,4082(R6,R5)
*              TIMEX CHANGES NEXT TO B TIMOV
         B     4(,R14)                 ENTER ROUTINE
STK3     LR    RSTACK,R0               STACK OVERFLOW, RESTORE STACK
         L     R14,24(,RSTACK)         RESTORE CALLER'S CODE BASE
         B     STKOV                   ENTER ERROR ROUTINE
         SPACE
*        RETURN SEQUENCES
         DC    0D'0'
         ENT   RETURN                  RETURN FROM PROGRAM (LEVEL 1)
STKR2    DS    0H                      TIMEX SETS ALL THE B 2(,9)'S
*                                      TO BC 15,TIMOV
         LM    RNP+1,RNP-1,0(R2)       RETURN FROM PROCEDURE LEVEL 2
         B     2(,R9)
         LM    RNP+1,RNP-1,0(R3)       RETURN FROM PROCEDURE LEVEL 3
         B     2(,R9)
         LM    RNP+1,RNP-1,0(R4)       RETURN FROM PROCEDURE LEVEL 4
         B     2(,R9)
         LM    RNP+1,RNP-1,0(R5)       RETURN FROM PROCEDURE LEVEL 5
         B     2(,R9)
         LM    RNP+1,RNP-1,0(R6)       RETURN FROM PROCEDURE LEVEL 6
         B     2(,R9)
         SPACE
         DC    A(0)                    SPARE
STKNP    DC    A(*-*)                  INITIAL NEW POINTER
STKOP    DC    V(OUTBLK)               IO BLOCK FOR STD OUTPUT
STKRT    DC    V(RUNTIME#)             ADDR OF RUNTIME SYSTEM
STKFW    DC    0D'0',X'4E00000000000000' FL PNT WK AREA
         EJECT
         SPACE
ENTPAT   DS    0D
         PRINT NOGEN
         ENT   PSIN
         ENT   PCOS
         ENT   PEXP
         ENT   PSQRT
         ENT   PLN
         ENT   PARCTAN
         SPACE
         ENT   OPENEXT                 OPEN EXTERNAL FILE
         ENT   CLOSEEXT
         ENT   GET
         ENT   PUT
         ENT   RESET
         ENT   REWRITE
         SPACE
         ENT   WRITBOOL
         ENT   WRITEINT
         ENT   WRTREALE
         ENT   WRTREALF
         ENT   WRITCHAR
         ENT   WRITSTRG
         SPACE
         ENT   READINT
         ENT   READREAL
         ENT   READCHAR
         ENT   READLN
         SPACE
         ENT   OPNINPUT
         ENT   GETTEXT
         ENT   PUTTEXT
         ENT   RESETTXT
         ENT   REWRTTXT
         ENT   ERROR1
         ENT   ERROR2
         ENT   ERROR3
STKOV    ENT   ERROR4
         SPACE
         ENT   ASSLONG
         ENT   RELLONG
         ENT   CLOCKF
         ENT   TIME
         ENT   DATE
         ENT   INTPOWER
         ENT   HALT                    HALT WITH TRACEBACK
         ENT   PAGE
         ENT   WRITELN
         ENT   MESAGE
         ENT   LONGJUMP
         ENT   READSTRG
TIMOV    ENT   TLIMIT
         DC    D'0'                    SPARE
         DC    D'0'                    SPARE
         DC    D'0'                    SPARE
STKPEND  DS    0D
STKL     EQU   *-STACK
SYSCON   EQU   560
         DC    0S(SYSCON-STKL)         ASM ERROR IF TOO LONG
         DROP  R1
         PRINT GEN
         EJECT
*        TIMER EXIT ROUTINE, USED TO GIVE TIME EXCEEDED CONDITION
         SPACE
*        AT TIMER EXIT, OS DOES NOT ALLOW US TO AVOID RETURNING TO
*        THE INTERRUPTED TASK, SO WE TAKE THE FOLLOWING APPROACH
*        1)    IF COMPILED CODE NOT IN EXECUTION, JUST SET BIT
*        2)    IF IN EXECUTION
*                 IF R1 => STACK AND PSW #> SYSCON AREA
*                 AND PSW #> EXTPAS OR EXTFORT INTERFACES
*                 (* THEN PSW => COMPILED CODE *)
*                 AND PSW #> 07FE (LONGJUMP)
*                    THEN IF OLD PSW => 18 ADD 2 TO IT
*                         IF OLD PSW => 4700 ADD 4 TO IT
*                    STORE BAL 9,TIMOV IN CODE WHERE PSW POINTS
*                 ELSE (*MUST BE IN RUNTIME SYSTEM SOMEWHERE,
*                        OR IN FORTRAN ROUTINE,
*                        OR IN PROC CALL OR RETURN*)
*                    SET B STKOV IN ENOUGH PLACES IN RUNTIME
*                    SYSTEM TO CATCH CONTROL WHEREVER IT IS AFTER
*                    FINISHING ITS CURRENT ACTIVITY.
         SPACE
TIMEX    CSECT
         USING *,R15
         STM   R14,R12,12(R13)
         LR    RBASE,R15
         USING TIMEX,RBASE
         L     R11,=V(PASDATA)
         USING PASDATA,R11
         TM    PASDFLG,X'80'           IS IT IN EXECUTION
         BC    1,TIMEX1                BR IF SO
         OI    PASDFLG,X'40'           ELSE SET BIT TO IND STOP
         B     TIMEXR                  FINISH
TIMEX1   DS    0H
         L     R1,HEAD
         USING STACK,R1
         L     R9,STKRT                GET BASE OF RUNTIME SYSTEM
         USING RUNTIME#,R9
         L     R2,16                   CVT
         L     R2,0(,R2)               TCBWORDS
         L     R2,4(,R2)               TCB
         L     R2,0(,R2)               IRB
         L     R3,32+4(,R2)            R1 AT INTERRUPT TIME
         L     R2,28(,R2)              PREVIOUS RB
         L     R2,20(,R2)              OLD PSW
         LA    R2,0(,R2)               TOP BYTE OFF
         CR    R3,R1                   IS R1 ==> STACK
         BC    7,TIMEX2                BR IF NOT
         C     R2,HEAD                 IS PSW ==> SYSCON AREA
         BC    4,TIMEX15               BR IF NOT
         LA    R0,SYSCON
         A     R0,HEAD
         CR    R2,R0
         BC    4,TIMEX2                BR IF SO
TIMEX15  LA    R0,EXTPAS               SEE IF IN EXT INTERFACES
         CR    R2,R0
         BC    4,TIMEX16               BR IF NOT
         LA    R0,EXTFTOV2
         BC    12,TIMEX2               BR IF SO
TIMEX16  DS    0H
*       HERE PSW MUST ==> PASCAL-COMPILED CODE
         CLC   BR14,0(R2)              SEE IF 07FE
         BC    8,TIMEX2                BR IF SO, ABOUT TO LONGJUMP
         CLI   0(R2),X'18'             SEE IF NEXT IS LR INSTR
         BC    7,*+8
         LA    R2,2(,R2)               ALLOW IT, MIGHT SET UP DISPL REG
         CLC   =X'4700',0(R2)          SEE IF BEFORE NAMELIST PTR
         BC    7,*+8
         LA    R2,4(,R2)               LEAVE THE NOP INTACT
         MVC   0(4,R2),BALTIMOV        SET TRAP
         B     TIMEXR
TIMEX2   DS    0H                      SET TRAPS IN RUNTIME SYSTEM
         MVC   RUNTIME3(4),BTIMOV
         LH    R0,BTIMOV+2             GET SCONST OF BRANCH
         MVC   STKTO1,BSTK2
         MVC   STK2(8),EX0414
*
         STH   R0,STKR2+6
         STH   R0,STKR2+6+8
         STH   R0,STKR2+6+16
         STH   R0,STKR2+6+24
         STH   R0,STKR2+6+32
         MVC   EXTFTOV1,=X'0700'
         STH   R0,EXTFTOV2+2           ZAP FORTRAN INTERFACE
TIMEXR   LM    R14,R12,12(R13)
BR14     BR    R14
         SPACE
BALTIMOV BAL   R9,TIMOV
EX0414   EX    0,4(,R14)               LOAD DISPLAY REGISTER
BTIMOV   B     TIMOV
BSTK2    B     STK2
         DROP  R1,R11,RBASE,R9
         LTORG
         SPACE
PASDATA  DSECT
         PRINT NOGEN
         PASLDATA
         PRINT GEN
         BLOCK
         SPACE 3
         PRINT NOGEN
         DCBD  DSORG=QS,DEVD=TA
         PRINT GEN
DFCB     EQU   IHADCB
DFCBDDN  DC    CL8' '
DFCBLINK DC    A(0)                    CHAIN OF ALLOCATED DCBS
DFCBBLK  DC    A(0)                    PTR BACK TO IOBLOCK
DFCBL    EQU   *-DFCB
DFCBJ    DC    XL176'0'                JFCB AREA FOR LOCAL FILE
DFCBD    DC    XL140'0'                DSCB MODEL AREA FOR LOCAL FILE
DFCBJDL  EQU   *-DFCB
         EJECT
*        REGISTER DEFINITIONS
R0       EQU   0                       RVAL ALSO
R1       EQU   1                       RP ALSO
R2       EQU   2                       RMAX ALSO
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R9       EQU   9                       RDFCB ALSO
R12      EQU   12
RNP      EQU   7                       NEW POINTER
RMAX     EQU   2                       RMAX
RTYP     EQU   3                       RDIG ALSO
RSIGN    EQU   4                       RSIGN
RLEN     EQU   5                       RLEN
RWK2     EQU   6                       RFP ALSO
RWK      EQU   7                       REXP ALSO
RSTACK   EQU   8
RDFCB    EQU   9
RBLK     EQU   10
RPARM    EQU   11
RBASE    EQU   12
RFR      EQU   RWK
RTO      EQU   RDFCB
R11      EQU   11
R13      EQU   13
R14      EQU   14
R15      EQU   15
F0       EQU   0                       FVAL
F2       EQU   2                       FEXP
F4       EQU   4                       FWORK
F6       EQU   6
RVAL     EQU   0
RP       EQU   1
RDIG     EQU   3
REXP     EQU   5
FVAL     EQU   0
FEXP     EQU   2
FWORK    EQU   4
         ERRNOS
         END
