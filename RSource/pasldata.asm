         MACRO
         PASLDATA
*        COMMON COMMUNICATION AREA BETWEEN MODULES
PASDFLG  DC    0AL1(0)                 TIMER FLAG 80 WHEN IN EXECUTION
PASDLINK DC    A(0)                    LINKED LIST OF STEPNAMES
HEAD     DC    A(0)                    START OF CODE
LENGTH   DC    A(0)                    LENGTH OF GETMAIN AREA
TAIL     DC    A(0)                    END OF GETMAIN, NPINIT
PASDTIM  DC    A(0)                    FOR STEP TIMING AND CLOCK FN
OLDPICA  DC    A(0)                    FOR SPIE
OUTDCB   DCB   MACRF=PM,DSORG=PS,DDNAME=SYSPRINT
         DC    A(0)                    SPARE
OUTBLK   DC    A(OUTDCB,0,0,0,OUTBUF)
SPIEL    SPIE  ,((1,13),15),MF=L
NFSBLKS  DC    A(4)                    DEFAULT NUMBER OF 2K BLOCKS FREE
NTRACE1  DC    A(5)                    NO OF INNER PROCS TO TRACE
NTRACE2  DC    A(5)                    NO OF OUTER PROCS TO TRACE
INTERVAL DC    F'1000000000'           FOR USE BY STIMER
         SPACE
PASFLINK DC    A(0)                    FOR LINKED LIST OF DCBS
         SPACE
SPIEOPSW DC    2A(0)
SPIEREGS DC    16A(0)
         SPACE
EXTPAS   DC    0A(0)              INTERFACE TO EXTERNAL PASCAL PROCS
         DC    A(0)                    ADD 0 TO STACK
         L     R0,8(,R1)               SAVE OLD ENVIRONMENT INSTRN
         ST    R0,32(,RSTACK)          IN R0 SAVE POSN
         LA    R15,4(,R15)             POINT TO NEXT IN PROC TABLE
         EX    0,8(,R1)                LOAD NEW ENVIRONMENT INSTRN
         ST    R14,8(,R1)              STORE IN PROC ENTRY CODE
         SR    R15,R15
         BC    15,8(,R1)               ENTER 1ST PROC OF EXT MODULE
         SPACE
EXTFORT  DC    0A(0)              INTERFACE TO EXTERNAL FORTRAN SUBS
         USING *,R14
         DC    A(0)                    ADD 0 TO STACK
         LA    R13,SAVEAREA            LOAD ADDR OF SAVE AREA
         LR    R0,R14
         SRL   R0,24
         STC   R0,EXTFINS              SET INS TO DISPOSE OF FUN RESLT
         DROP  R14
         LA    R15,4(,R15)             POINT TO ADDR IN PROC TABLE
         EX    0,8(,R1)                LOAD ENTRY POINT
         LR    R15,R14
         L     R1,60(,RSTACK)          ARG LIST PTR
EXTFTOV1 BALR  R14,R15                 LINK TO FORT ROUTINE
EXTFINS  ST    R0,64(,RSTACK)          NOP, ST OR STD FOR FUN RESLT
         LM    RSTACK,R6,0(RSTACK)     RESTORE REGS
EXTFTOV2 B     2(,R9)                  BACK TO PASCAL CALLER
*        NOTE EXTFTOV1 & 2 ARE ZAPPED BY TIMER EXIT ROUTINE
SAVEAREA DC    18A(0)
OUTBUF   DC    CL137' '
         MEND
