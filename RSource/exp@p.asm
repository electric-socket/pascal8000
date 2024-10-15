LEXP   TITLE   'EXPONENTIAL FUNCTION (LONG)'
EXP@P    CSECT
*
*      EXPONENTIAL FUNCTION (LONG)
*              1. WRITE X = (4A-B-C/16)*LOGE(2)+R, WHERE A,B, AND
*                   C ARE INTEGERS, B BETWEEN 0 AND 3, C BETWEEN 0
*                     AND 15.  R IS A FRACTION BETWEEN -1/16 AND 0.
*              2. THEN E**X = 2**Y = (16**A)(2**-B)(2**-C/16)(E**R).
       SPACE
       SPACE
GRA    EQU     1               ARGUMENT POINTER
GRS    EQU     13              SAVE AREA POINTER
GRR    EQU     14              RETURN REGISTER
GRL    EQU     15              LINK REGISTER
GR0    EQU     0               SCRATCH REGISTERS
GR1    EQU     1
GR2    EQU     14
* REGISTERS 2 AND 3 ARE SAVED AND USED IN 'ERROR'
REG2     EQU   2
REG3     EQU   3
FR0    EQU     0               ANSWER REGISTER
FR2    EQU     2               SCRATCH REGISTER
       SPACE
       USING   *,GRL
DEXP   BC      15,LEXP
       DC      AL1(4)
       DC      CL5'DEXP'
       SPACE
LEXP   STM     GRR,GRL,12(GRS) SAVE REGISTERS
       L       GR2,0(GRA)
BEGIN  LD      FR0,0(GR2)      OBTAIN ARGUMENT
       CE      FR0,MAX         MAX = 63*LOG16 = 174.67309
       BC      2,ERROR           IF ARG GREATER THAN THIS, ERROR
       CE      FR0,MIN         MIN = -65*LOG16 = -180.21867
       BC      12,SMALL        IF ARG LESS THAN THIS, GIVE UNDERFLOW
       SPACE
       LER     FR2,FR0         DECOMPOSE X = P*LOG2+R,
       DE      FR2,LOG2H         P MULTIPLE OF 1/16, ACCURATELY
       AU      FR2,SCALER      FIRST (UNDER)ESTIMATE P BY
       STE     FR2,FIELDS        DIVIDING HIGH PART X BY LOG2H
       ME      FR2,LOG2H
       SDR     FR0,FR2         LOG(2) = LOG2H+LOG2L,
       LD      FR2,FIELDS        WHERE LOG2H IS ROUNDED UP.
       MD      FR2,LOG2L           TOTAL PRECISION 80 BITS
       SDR     FR0,FR2         X = P'*LOG2+R', /R'/ MAY BE
       L       GR0,FIELDS        SLIGHTLY OVER (LOG2)/16
       BC      12,ZMINUS
       SPACE
       LCR     GR0,GR0         CASE WHEN X AND R' ARE POSITIVE
PLUS   BCTR    GR0,0             CHANGE SIGN OF P AND SUBTRACT
       AD      FR0,ML216           (LOG2)/16 UNTIL R BECOMES NEGATIVE,
       BC      2,PLUS                EACH TIME SUBTRACT 1 FROM -P
       BC      15,READY
       SPACE
ZMINUS CD      FR0,ML216       CASE WHEN X AND R' 0 OR NEGATIVE
       BC      2,READY           IF R' SMALLER THAN -(LOG2)/16,
       SD      FR0,ML216           ADD (LOG2)/16, AND INCREMENT
       SH      GR0,INCR              GR0 WHOSE LOW PART IS -P
       SPACE
READY  SR      GR1,GR1         GR1 = -P = -4A+B+C/16
       SRDL    GR0,4           C IN HIGH GR1
       SRL     GR1,25
       SRDL    GR0,2           B IN HIGH GR1, C IN LOW GR1
       SLL     GR0,24
       LCR     GR2,GR0         A (IN SCALE B7) IN GR2, CHAR MODIFIER
       SR      GR0,GR0
       SLDL    GR0,2           B IN GR0, 8*C IN GR1
       SPACE
       CE      FR0,NEAR0       IF /R/ IS LESS THAN 2**-60, AVOID
       BC      2,SKIP1           UNDERFLOW BY TAKING  E**R = 1
       LDR     FR2,FR0         COMPUTE E**R FOR R BETWEEN
       ME      FR0,C6            -(LOG2)/16 AND 0 BY MINIMAX
       AD      FR0,C5              POLYNOMIAL APPROX OF DEGREE 6
       MDR     FR0,FR2
       AD      FR0,C4
       MDR     FR0,FR2
       AD      FR0,C3
       MDR     FR0,FR2
       AD      FR0,C2
       MDR     FR0,FR2
       AD      FR0,C1
       MDR     FR0,FR2         E**R-1 READY
       MD      FR0,MCONST(GR1)
       SPACE
SKIP1  AD      FR0,MCONST(GR1) (E**R)*2**(-C/16) READY
       SPACE
       LTR     GR0,GR0         MULTIPLY BY 2**(-B)
       BC      8,SKIP2           BY HALVING B TIMES
       HDR     FR0,FR0
       BCT     GR0,*-2
       SPACE
SKIP2  STE     FR0,FIELDS      ADD A TO CHARACTERISTIC
       A       GR2,FIELDS
       ST      GR2,FIELDS
       LE      FR0,FIELDS
       SPACE
EXIT     DS    0H
         SR    GRL,GRL                 ZERO COND. CODE
ERROR    DS    0H
         L     GRR,12(GRS)
       MVI     12(GRS),X'FF'
         BCR   8,GRR
         LA    GRL,4
         BCR   15,GRR                  RETURN WITH ERROR
       SPACE
SMALL  LE      FR0,BOMB        LOAD BOMB
       MDR     FR0,FR0           AND GO OUT WITH A BANG!
       BC      15,EXIT
       SPACE
         DC    0A(0)                   ALIGNMENT
LOG2H  DC      X'40B17218'     LOG(2) ROUNDED UP
MAX    DC      X'42AEAC4E'     174.6731
MIN    DC      X'C2B437DF'    -180.2187
FIELDS DC      D'0'
NEAR0  DC      X'B2100000'    -2**60
C6     DC      X'3E591893'             0.13594970E-2
C5     DC      X'3F2220559A15E158'     0.8331617720039062E-2
C4     DC      X'3FAAAA9D6AC1D734'     0.4166661730788750E-1
C3     DC      X'402AAAAAA794AA99'     0.1666666659481656
C2     DC      X'407FFFFFFFFAB64A'     0.4999999999951906
C1     DC      X'40FFFFFFFFFFFCFC'     0.9999999999999892
LOG2L  DC      X'B982E308654361C4'     LOG(2)-LOG2H TO 80 BITS
MCONST DC      X'4110000000000000'     2**(-0/16)
       DC      X'40F5257D152486CD'     2**(-1/16) +F
       DC      X'40EAC0C6E7DD243A'     2**(-2/16) +F
       DC      X'40E0CCDEEC2A94E1'     2**(-3/16)
       DC      X'40D744FCCAD69D6B'     2**(-4/16)
       DC      X'40CE248C151F8481'     2**(-5/16)
       DC      X'40C5672A115506DB'     2**(-6/16)
       DC      X'40BD08A39F580C37'     2**(-7/16)
       DC      X'40B504F333F9DE65'     2**(-8/16)
       DC      X'40AD583EEA42A14B'     2**(-9/16)
       DC      X'40A5FED6A9B15139'     2**(-10/16)
       DC      X'409EF5326091A112'     2**(-11/16)
       DC      X'409837F0518DB8AA'     2**(-12/16)+F
       DC      X'4091C3D373AB11C4'     2**(-13/16)+F
       DC      X'408B95C1E3EA8BD7'     2**(-14/16)
       DC      X'4085AAC367CC487C'     2**(-15/16)+F
ML216  DC      X'BFB17217F7D1CF7A'    -LOG(2)/16   ROUNDED UP
SCALER DC      X'45000000'
BOMB   EQU     C5+4
INCR   EQU     C1+2
       END
