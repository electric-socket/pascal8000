         TITLE 'PASCAL  -  PASLOAD ROUTINE'
PASLOAD  CSECT
*        LOADS USER'S OBJECT CODE INTO AREA GOT BY PASGET
         B     12(,R15)
         DC    AL1(7),CL7'PASLOAD'
         STM   R14,R12,12(R13)
         LR    R12,R15
         USING PASLOAD,R12
         USING PASDATA,R11
         LR    R14,R13
         LA    R13,PASLSAVE
         ST    R13,8(,R14)
         ST    R14,4(,R13)
         SPACE 2
*        SET DDNAMES UP
         LA    R1,INDDNAM+7
         CLI   0(R1),C'I'
         BC    8,*+8
         BCT   R1,*-8
         MVI   0(R1),C'1'
         MVC   FCB1+DCBDDNAM-DFCB(8),INDDNAM
         MVI   0(R1),C'2'
         MVC   FCB2+DCBDDNAM-DFCB(8),INDDNAM
         MVI   0(R1),C'I'              RESET DDNAME FOR STD INPUT
         SPACE 2
*        GET DIRECTORY IN
         SPACE
         OPEN  (FCB2,INPUT)
         TM    DCBOFLGS-DFCB+FCB2,DCBOFOPN TEST IF OPEN OK
         BZ    OBJERR
GETD     DS    0H
         GET   FCB2,DIRECTRY
         CLI   INITCNT,0               SEE IF GOT LOADER BLK YET
         BC    7,GETD                  FLUSH PROCNAMES IF NOT
         GET   FCB2,PROCCNT            GET NEXT 2 WORDS
         CLOSE FCB2
         FREEPOOL FCB2
         OC    ERRORS,ERRORS           SEE IF COMPILE ERRORS
         BC    7,COMTERR               PRINT 'COMPILE TIME ERROR'
         TM    OBJLENG+3,7             LENGTH MUST BE DBLWD
         BC    7,OBJERR
         L     R2,OBJLENG
         SPACE
         OPEN  FCB1                    OPEN OBJECT FILE
         TM    DCBOFLGS-DFCB+FCB1,DCBOFOPN TEST IF OPEN OK
         BZ    OBJERR
         L     R3,HEAD
         L     R0,OBJLENG
         AR    R0,R3
         ST    R0,STAKHEAD             SET STAK UP
         SPACE
*        READ INIT VALUES
         L     R2,INITCNT
         LTR   R2,R2
         BC    12,NOINIT
         SPACE
*        ARRANGE WORK AREA FOR READING INIT VALUES
*        STARTS AT PROG START AND MAY NOT GO PAST PROC TABLE
         LH    R1,DCBBLKSI-DFCB+FCB1
         SH    R1,=Y(SYSCON)
SYSCON   EQU   1584
         LTR   R1,R1
         BC    2,*+6
         SR    R1,R1
         L     R0,OBJLENG
         CR    R0,R1
         BC    2,*+6
         LR    R0,R1
         ALR   R0,R3                   FORM STAK ADDR
         ST    R0,STAKHEAD
*        AT LEAST BLKSI BYTES WORK AREA NOW AT (R3)
         SPACE
*        NOW LOOP TO READ AND SET INIT VALUES
         LH    R4,DCBBLKSI-DFCB+FCB1
         SH    R4,=Y(4)                INITIALISE FOR 1ST READ
INIT1    DS    0H
         BAL   R10,READNX              NEXT WORD
         L     R5,0(R3,R4)             LENGTH OF INIT AREA
         BAL   R10,READNX
         L     R6,0(R3,R4)             REL ADDR OF AREA
         A     R6,STAKHEAD             RELOCATE
         SRL   R5,2                    USE LENGTH AS WORD CTR
INIT2    DS    0H
         BAL   R10,READNX
         L     R0,0(R3,R4)             NEXT DATA
         ST    R0,0(,R6)               INIT VALUE ON STACK
         LA    R6,4(,R6)               ADDR NEXT WORD
         BCT   R5,INIT2                LOOP OVER ARRAY
         BCT   R2,INIT1                LOOP OVER ALL INITS
NOINIT   DS    0H
         SPACE 3
*        LOAD PROGRAM BODY
*        R3 --> START OF PROG
         L     R2,OBJLENG              LENGTH OF PROG
         LA    R9,CHN1                 INITIALISE I/O
PROG1    DS    0H
         LH    R4,DCBBLKSI-DFCB+FCB1   MAX BLKSI
         CR    R4,R2                   SEE IF > OR < 1 BLK LEFT
         BC    12,*+6                  SET MAX READ LEN
         LR    R4,R2
         BAL   R10,READOBJ             READ TO (R3), LENGTH (R4)
         AR    R3,R4                   READY FOR NEXT
         SR    R2,R4                   LENGTH REMAINING
         BC    7,PROG1                 LOOP TILL ALL READ
         SPACE
         BAL   R10,CHEKOBJ             FINISH READS
         SPACE 3
*        READ AND RELOCATE PROC ADDRESSES
         L     R2,PROCCNT              # OF PROCS
         SLL   R2,2                    # OF BYTES TO BE READ
         L     R3,STAKHEAD
         LA    R3,560(,R3)             ADDR OF PROC TABLE
PROC0    DS    0H
         READ  4(,R9),SF,,(R3),(R2),MF=E READ PROC TABLE
         CHECK 4(,R9)
         AH    R3,DCBBLKSI-DFCB+FCB1   FOR NEXT BLK IF ANY
         SH    R2,DCBBLKSI-DFCB+FCB1   ANY?
         BC    2,PROC0                 LOOP IF SO
         L     R3,STAKHEAD
         LA    R3,560(,R3)
         L     R2,PROCCNT
         L     R0,HEAD                 RELOCATION FACTOR
PROC1    DS    0H
         L     R1,0(,R3)               NEXT ADDR TO RELOCATE
         ALR   R1,R0
         ST    R1,0(,R3)
         LA    R3,4(,R3)               POINT TO NEXT
         BCT   R2,PROC1                LOOP TO RELOCATE ALL ADDRS
         SPACE
         SPACE 2
         SR    R5,R5                   INDICATE OK
RETURN   DS    0H
         CLOSE FCB1
         SPACE
         LTR   R0,R5                   GET ERROR INDICATOR
         L     R1,STAKHEAD             TO PASS TO PASRUN
         LA    R5,SPIEL                DITTO
RETURN1  DS    0H
         L     R13,4(,R13)
         L     R14,12(,R13)
         ST    R5,20+5*4(,R13)
         LM    R2,R12,28(R13)          RESTORE EXCEPT R0,R1,R5
         BCR   8,R14                   RETURN IF OK
         USING STEPS,R10
         MVC   STTIME,PASDTIM          SET FOR ZERO TIME USED IF ERROR
         BR    R14                     RETURN
        SPACE
OBJERR2  CLOSE FCB2
OBJERR   LA    R5,ERROBJ               LOADER ERROR
         B     RETURN
         SPACE
COMTERR  LA    R0,ERRCOMT              COMPILE TIME ERROR
         B     RETURN1
         SPACE
         EJECT
READNX   LA    R4,4(,R4)               NEXT WORD IN WK AREA
         CH    R4,DCBBLKSI-DFCB+FCB1
         BCR   4,R10
         LH    R15,DCBBLKSI-DFCB+FCB1
         SR    R4,R4
         READ  DECB1,SF,,(R3),(R15),MF=E READ BLK TO WK AREA
         CHECK DECB1
         BR    R10                     RETURN
         SPACE
READOBJ  TM    0(R9),X'80'             TEST IF THIS DECB FREE
         BC    8,RO1                   BR IF SO
         CHECK 4(,R9)                  WAIT FOR COMPLETION
         NI    0(R9),X'7F'             INDICATE FREE
RO1      DS    0H
         READ  4(,R9),SF,,(R3),(R4),MF=E  READ NEXT PART OF OBJ
         OI    0(R9),X'80'             INDICATE DECB BUSY
         L     R9,0(,R9)               CHAIN TO NEXT DECB
         BR    R10
         SPACE
CHEKOBJ  TM    0(R9),X'80'             LOOK FOR 1ST USED DECB
         BC    7,CO1                   BR WHEN FOUND
         L     R9,0(,R9)               CHAIN TO NEXT
         B CHEKOBJ                     KEEP LOOKING
CO1      TM    0(R9),X'80'             IS THIS ONE FREE
         BCR   8,R10                   RETURN IF SO
         CHECK 4(,R9)                  FREE IT
         NI    0(R9),X'7F'
         B     CO1
         SPACE
         PRINT NOGEN
CHN1     DC    A(CHN2)
         READ  DECB1,SF,FCB1,MF=L
CHN2     DC    A(CHN3)
         READ  DECB2,SF,FCB1,MF=L
CHN3     DC    A(CHN4)
         READ  DECB3,SF,FCB1,MF=L
CHN4     DC    A(CHN5)
         READ  DECB4,SF,FCB1,MF=L
CHN5     DC    A(CHN6)
         READ  DECB5,SF,FCB1,MF=L
CHN6     DC    A(CHN7)
         READ  DECB6,SF,FCB1,MF=L
CHN7     DC    A(CHN8)
         READ  DECB7,SF,FCB1,MF=L
CHN8     DC    A(CHN1)
         READ  DECB8,SF,FCB1,MF=L
         SPACE
FCB1     DCB   DSORG=PS,MACRF=R,SYNAD=OBJERR,EODAD=OBJERR,             X
               OPTCD=C,NCP=8,                                          X
               RECFM=U,DDNAME=*****
FCB2     DCB   DSORG=PS,MACRF=GM,SYNAD=OBJERR2,EODAD=OBJERR2,          X
               RECFM=FB,LRECL=8,DDNAME=*****
PASLSAVE DC    18A(0)
         LTORG
         EJECT
PASDATA  DSECT
         PASDATA
         SPACE
STEPS    DSECT
         STEPS
         SPACE
         PRINT NOGEN
DFCB     DCBD  DSORG=QS,DEVD=TA
DFCB     EQU   IHADCB
         SPACE 2
         ERRNOS
         SPACE
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
