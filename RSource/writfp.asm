         TITLE 'PASCAL - FLOATING POINT CONVERSION FOR OUTPUT'
WRITFP   CSECT
         USING *,R15
         USING ANS,RD
         STM   R14,R12,12(R13)
*        ENSURE RLEN >= RFP+3/7 FOR RTYP = 0/4
         L     RWK,WRCFE(RTYP)         3/7
         AR    RWK,RFP
         CR    RLEN,RWK
         BC    10,*+6
         LR    RLEN,RWK                INCREASE RLEN IF TOO SMALL
         ST    RLEN,20+4*RLEN(,R13)
         LTR   RTYP,RTYP
         BC    8,WR1
         LR    RFP,RLEN
         S     RFP,WRCFE+4             SET RFP FOR E TYPE
WR1      DS    0H
         SPACE
*        DISCRIMINATE 0, + AND -
         SR    MSIGN,MSIGN
         LTDR  FVAL,FVAL
         BC    7,WR3                   BR NON-ZERO
         SPACE
*        HANDLE ZERO RESULT
WRZ      DS    0H
         LA    RWK,0(RD,RLEN)          END OF FIELD
         SR    RWK,RTYP                POSN FOR E+00 IF ANY
         SR    RWK,RFP                 POSN AFER .
         BCTR  RWK,0                   .
         BCTR  RWK,0                   DIG BEFORE .
         AR    RFP,RTYP                LENGTH-1 FOR MVC
         MVI   0(RWK),C'0'
         EX    RFP,WRMVCX1             PROPAGATE ZERO
         MVI   1(RWK),C'.'
         SR    RWK,RD                  NO OF BLANKS
         BC    8,WRZ1
         MVI   0(RD),C' '
         BCT   RWK,*+8
         B     WRZ1
         BCTR  RWK,0
         LR    RFP,RWK
         LR    RWK,RD
         EX    RFP,WRMVCX1
WRZ1     DS    0H
         LTR   RTYP,RTYP
         BC    8,WRR                   FINISH IF F TYPE
         AR    RD,RLEN                 END OF FIELD
         SR    RD,RTYP                 SPOT FOR E+
         MVC   0(2,RD),=C'E+'
         B     WRR
         SPACE
         SPACE
WR3      DS    0H
         BC    2,WR4
         LA    MSIGN,2                 SET SIGN IND FOR -VE
         LCDR  FVAL,FVAL               MAKE +VE
WR4      DS    0H
         SPACE
*        NOW REDUCE EXPONENT TO 4E BY MULT BY + OR - POWERS OF 10
         SR    RSIGN,RSIGN
         CD    FVAL,MIN10
         BC    2,WR5
         LA    RSIGN,WRXN-WRXP         IND -VE XPONENT
WR5      DS    0H
         EX    0,WRX1(RSIGN)           LA  0,6/7
         SR    REXP,REXP
         EX    0,WRX2(RSIGN)           LA RWK,TBLPP/TBLNP
WR6      CD    FVAL,0(,RWK)            TEST VS MAX/MIN * POWER OF 10
         SLL   REXP,1                  EXPONENT *2
         EX    0,WRX3(RSIGN)           BC 4/2,WR7
         MD    FVAL,8(,RWK)            REDUCE BY POWER
         LA    REXP,1(,REXP)           INCR EXPONENT
WR7      DS    0H
         LA    RWK,16(,RWK)            NEXT PAIR
         BCT   R0,WR6                  LOOP
         EX    0,WRX4(RSIGN)           NOP/LCR EXP,EXP
         SPACE
         STD   FVAL,WORK
         CLI   WORK,X'4E'              EXP SHOULD BE
         BC    8,*+6                   4E BY NOW
         DC    Y(0)
         MVI   WORK,0
         LM    R0,R1,WORK
         D     R0,=F'1000000000'       SPLIT IN 2 PARTS
         MH    R1,=Y(10)
         CVD   R1,WORK1
         CVD   R0,WORK2                LOW ORDER 9 DIGITS
         MVC   WORK1+7(5),WORK2+3      23 DIGITS TOTAL, 12 BYTES,
*                                      16 OR 17 SIG DIGS. WHEN DEC PT
*                                      AT RIGHT, REXP IS EXPONENT
         TM    WORK1+3,X'F0'          TEST 17TH DIG
         BC    7,WR8
         MP    WORK1(12),=P'10'
         BCTR  REXP,0
WR8      DS    0H                      EXACTLY 17 DIGS NOW
         TM    WORK1+3,X'F0'
         BC    7,*+6
         DC    Y(0)                    ***** CONSISTENCY CHECK
         SPACE 2
*        NOW DO ROUNDING UP WHERE NECESSARY
*        THERE ARE 17+REXP(F) OR 1(E) DIGS < DEC PT, RFP DIGS > DEC PT,
*        CALC NO OF SIG DIGS IN ANSWER SO CAN ROUND
         LTR   RTYP,RTYP
         BC    8,WR81                  BR IF F
WR80     DS    0H
         LA    RWK,1
         B     WR82
WR81     DS    0H
         LA    RWK,17
         AR    RWK,REXP
WR82     DS    0H
         AR    RWK,RFP                 # OF SIG DIGS
         LTR   RWK,RWK
         BC    4,WRZ                   BR IF NO ROUNDING, FIELD ALL 0
         ZAP   WORK3,=P'0'             PREPARE ROUND AMNT FOR ADDITION
         C     RWK,=A(17)              *
         BC    10,WRO25                BR IF NO ROUND REQD
         EX    RWK,TMODD               SEE IF ODD DIGITS
         SRL   RWK,1                   /2
         LA    RWK,WORK3+3(RWK)        BYTE FOR ROUNDING DIGIT
         BC    7,WRO1                  BR IF ODD DIGITS
         OI    0(RWK),X'50'
         B     WRO2
WRO1     OI    0(RWK),X'05'
WRO2     DS    0H
         AP    WORK1(12),WORK3(12)     DO THE ROUNDING
WRO25    DS    0H
         LTR   RTYP,RTYP
*        CONDITION IS BASED ON DIGS BEFORE DEC PT AND IS
*        RLEN-RFP-2 >= 17+REXP OR RLEN-RFP-REXP >= 19
         BC    7,WRO26                 BR IF E
         SR    RWK,RWK
         TM    WORK1+2,255
         BC    8,*+8
         LA    RWK,1
         AR    RWK,RLEN
         SR    RWK,RFP
         SR    RWK,REXP
         C     RWK,=A(19)
         BC    4,WRE1                  BR TO CHANGE TO E-TYPE
WRO26    DS    0H
         TM    WORK1+2,255             TEST IF EXTRA DIG NOW
         BC    8,WRO3                  BR IF NOT
         MP    WORK1(12),=P'10'        GET TO BYTE BOUNDARY AGAIN
         A     REXP,=A(1)
         B     WRO4
WRO3     MP    WORK1(12),=P'100'
WRO4     DS    0H                      19 DIGS NOW AT WORK1+2
         OI    WORK1+10,X'0C'          17 DIGS NOW
         ZAP   WORK1+11(5),=P'0'       9 DIG ZERO FOLLOWS
         SPACE
         LTR   RTYP,RTYP
         BC    7,WRE                   BR IF E TYPE
         BAL   R14,WRF                 GET F-TYPE DECODED
         B     WRR                     RETURN
         SPACE
WRE1     LA    RTYP,4                  SET TO E-TYPE
         L     RWK,WRCFE+4
         CR    RLEN,RWK
         BC    10,*+6
         LR    RLEN,RWK
         LR    RFP,RLEN
         S     RFP,WRCFE+4
         ST    RLEN,20+4*RLEN(,R13)
         SP    WORK1(12),WORK3(12)     UNDO ROUNDING
         B     WR80                    DO ROUNDING AGAIN AS E-TYPE
         SPACE
WRE      DS    0H
         SR    RLEN,RTYP               FIELD WIDTH W/OUT E+NN
         LR    REXPS,REXP              SAVE EXPONENT
         L     REXP,=F'-16'            ARRANGE FOR 1 DIGIT BEFORE .
         BAL   R14,WRF
         LA    RWK,1(RD,RLEN)
         MVC   0(4,RWK),=X'40212020'
         SR    REXPS,REXP              GET EXPONENT
         CVD   REXPS,WORK
         ED    0(4,RWK),WORK+6
         MVC   0(2,RWK),=C'E+'
         BC    10,*+8                  BR IF +VE EXPONENT
         MVI   1(RWK),C'-'
         SPACE
WRR      LM    R14,R12,12(R13)
         BR    R14
         EJECT
WRF      DS    0H                      DECODE F-TYPE NUMBER
*        THERE ARE 19 PACKED DIGITS AT WORK1+2
         LA    RWK,17
         AR    RWK,REXP
         AR    RWK,RFP
         LTR   RWK,RWK                 NO OF SIG DIGS IN ANS
         BC    12,WRZ                  PRINT ZERO IF NONE ( F ONLY)
         MVC   ANS(24),=CL24' '        PREPARE ANSWER FIELD
         LA    R1,ANS(RLEN)
         BCTR  R1,0                    LAST CHAR OF ANSWER AREA
         LCR   RFP,RFP
         AR    RFP,R1                  SPOT FOR DEC PT
WRF1     MVI   0(R1),X'20'             EDIT PAT CHAR
         BCTR  R1,0
         BCT   RWK,WRF1
         CR    R1,RFP                  TEST IF 1ST DIG < DEC PT
         BC    10,WRF15                BR NOT
         MVI   0(R1),X'20'             FOR EXTRA DIG < DEC PT
         MVI   0(RFP),C'.'             PUT . IN
WRF15    DS    0H
         BCTR  RLEN,0
         EX    RLEN,EDANS              EDIT SIG DIGS TO ANS
         CR    R1,RFP
         BC    4,WRF3                 BR IF < DEC PT
         BCTR  RFP,0
WRF2     MVI   0(R1),C'0'
         BCTR  R1,0
         CR    R1,RFP
         BC    10,WRF2                 LOOP TILL 0 BEFORE .
         MVI   1(RFP),C'.'             PUT . IN
WRF3     LTR   MSIGN,MSIGN
         BC    8,WRF4
         BCTR  R1,0
         MVI   0(R1),C'-'              SET - SIGN
WRF4     DS    0H
         BR    R14
         SPACE
EDANS    ED    ANS(0),WORK1+2
         EJECT
TBLPP    DC    0D'0'                   MAX IS 4EFFFFFF000000,
*                                      MIN IS MAX/10.
         DC    X'687E37BDA1EB0270'     10**32 * MIN
         DC    D'1E-32'
         DC    X'5B38D7EA139015B3'     10**16 * MIN
         DC    D'1E-16'
         DC    X'5498967F67698000'     10**8 * MIN
         DC    D'1E-8'
         DC    X'513E7FFFC1800000'     10**4 * MIN
         DC    D'1E-4'
         DC    X'4F9FFFFF60000000'     10**2 * MIN
         DC    D'1E-2'
MIN10    DS    0D
         DC    X'4EFFFFFF00000000'     10**1 * MIN
         DC    D'1E-1'
TBLNP    DC    X'19A87FE97F254FC2'     10**-64 * MAX
         DC    D'1E64'
         DC    X'3433EC47776506B9'     10**-32 * MAX
         DC    D'1E32'
         DC    X'41734AC9EC175C91'     10**-16 * MAX
         DC    D'1E16'
         DC    X'482AF31D996DFAAF'     10**-8 * MAX
         DC    D'1E8'
         DC    X'4B68DB8B43958106'     10**-4 * MAX
         DC    D'1E4'
         DC    X'4D28F5C266666666'     10**2 * MAX
         DC    D'1E2'
         DC    X'4E19999980000000'     10**1 * MAX
         DC    D'1E1'
         SPACE
WRXP     DS    0H
WRX1     LA    R0,6
WRX3     BC    4,WR7
WRX4     NOPR  0
WRX2     LA    RWK,TBLPP               +VE POWERS
WRXN     LA    R0,7
         BC    2,WR7
         LCR   REXP,REXP
         LA    RWK,TBLNP               -VE POWERS
WRCFE    DC    A(3,7)
WRMVCX1  MVC   1(0,RWK),0(RWK)
TMODD    TM    =AL1(1),0               FOR TESTING IF REG ODD
         EJECT
FVAL     EQU   0                       FLOATING REG
R0       EQU   0
R1       EQU   1
RLEN     EQU   2                       FIELD WIDTH
RTYP     EQU   3                       0 FOR F-TYPE, 4 FOR E-TYPE
RFP      EQU   4                       NUMBER OF FRACTION DIGITS
RD       EQU   5                       ANSWER AREA, DBLWD WORK AREAS
REXP     EQU   6                       EXPONENT WORK REG
REXPS    EQU   7                       EXPONENT SAVE REGISTER
RWK      EQU   9
RSIGN    EQU   10
MSIGN    EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         SPACE 2
ANS      DSECT ,                       ANSWER, WORK AREAS
WORK3    DC    0PL12'0'                FOR BUILDING ROUNDING DATA
         DC    3D'0'                   24 DIGS FOR ANSWER
WORK     DC    0D'0'
WORK1    DC    D'0'
WORK2    DC    D'0'
         SPACE 2
         END
