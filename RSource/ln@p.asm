LLOG   TITLE   'LOGARITHMIC FUNCTION (LONG)'
LN@P     CSECT
*
*      LOGARITHMIC FUNCTION (LONG)
*              1. WRITE X = (M*2**-Q)*16**P, M MANTISSA BETWEEN 1/2
*                   AND 1, Q INTEGER BETWEEN 0 AND 3. DEFINE A=1, B=0
*                     IF M GREATER THAN SQRT2/2, OTHERWISE A=1/2, B=1.
*              2. WRITE Z = (M-A)/(M+A), THEN
*                   LOG(X) = (4P-Q-B)LOG(2)+LOG((1+Z)/(1-Z)).
       SPACE
       SPACE
GR0    EQU     0               SCRATCH REGISTERS
GR1    EQU     1
GR2    EQU     2
GR3    EQU     3
GRA    EQU     1               ARGUMENT POINTER
GRS    EQU     13              SAVE AREA POINTER
GRR    EQU     14              RETURN REGISTER
GRL    EQU     15              LINK REGISTER
FR0    EQU     0               ANSWER REGISTER
FR2    EQU     2               SCRATCH REGISTER
FR4    EQU     4
FR6    EQU     6
       SPACE
       USING   *,GRL
DLOG   BC      15,LLOG         NATURAL LOG ENTRY
       DC      AL1(4)
       DC      CL5'DLOG'
LLOG   STM     GRR,GR3,12(GRS) SAVE REGISTERS
         L     GR2,0(GRA)
       LM      GR2,GR3,0(GR2)  OBTAIN ARGUMENT IN GR2, GR3
       LTR     GR0,GR2
       BC      12,ERROR        IF ARG IS 0 OR NEGATIVE, ERROR
       SRDL    GR0,24          CHAR IN LOW GR0, 1ST DIGIT IN HIGH GR1
       SLL     GR0,2
       STH     GR0,IPART+2     FLOAT 4*CHAR AND SAVE IT
       SRL     GR1,29          1ST THREE BITS OF M IN GR1
       IC      GR1,TABLE(GR1)  NUMBER OF LEADING ZEROS (=Q) IN GR1
       SLDL    GR2,0(GR1)
       STM     GR2,GR3,BUFF
       MVI     BUFF,X'40'      M = FRACTION*2**Q IN CELL BUFF
       SPACE
       SR      GR2,GR2         IF M LESS THAN SQRT2/2, GR2=0
       LD      FR0,BUFF        PICK UP M IN FR0
       CE      FR0,LIMIT       IF M GREATER THAN SQRT2/2, GR2=8
       BC      2,READY
       LA      GR2,8
       LA      GR1,1(GR1)      CRANK GR1 BY 1. Q+B IN GR1
       SPACE
READY  HDR     FR2,FR0         COMPUTE 2Z = (M-A)/(0.5M+0.5A),
       SD      FR0,ONE(GR2)      A = 1 OR 1/2
       AD      FR2,HALF(GR2)   0.5M+0.5A HAS 56 BITS
       DDR     FR0,FR2
       SPACE
       LDR     FR2,FR0         COMPUTE LOG((1+Z)/(1-Z)) BY MINIMAX
       MDR     FR2,FR2           APPROXIMATION OF THE FORM
       LD      FR4,C6              W+C1*W**3(W**2+C2+C3/
       ADR     FR4,FR2               (W**2+C4+C5/(W**2+C6)))
       LD      FR6,C5
       DDR     FR6,FR4
       AD      FR6,C4
       ADR     FR6,FR2
       LD      FR4,C3
       DDR     FR4,FR6
       AD      FR4,C2
       ADR     FR4,FR2
       MD      FR4,C1
       MDR     FR4,FR2
       MDR     FR4,FR0
       ADR     FR4,FR0
       SPACE
       LD      FR0,IPART       4*(P+64)
       LA      GR1,256(GR1)    4*64+Q+B
       STH     GR1,IPART+2     FLOAT THIS AND SUBTRACT FROM FR0
       SE      FR0,IPART         TO OBTAIN 4P-Q-B
       MD      FR0,LOGE2       MULTIPLY BY LOG(2) BASE E
       ADR     FR0,FR4           AND ADD TO LOG((1+Z)/(1-Z))
       SPACE
EXIT  EQU      *
         SR    GRL,GRL                 ZERO COND. CODE
EXITE    DS    0H
       LM      GR2,GR3,28(GRS)
       MVI     12(GRS),X'FF'
         BCR   8,GRR                   RETURN IF OK
         LA    GRL,4                   SET ERROR
         BCR   15,GRR                  RETURN WITH ERROR
       SPACE
ERROR    DS    0H
         LTR   GRL,GRL                 SET CC
         BC    15,EXITE
       SPACE
BUFF   DS      D
C6     DC      X'C158FA4E0E40C0A5'    -0.5561109595943017E+1
C5     DC      X'C12A017578F548D1'    -0.2625356171124214E+1
C4     DC      X'C16F2A64DDFCC1FD'    -0.6947850100648906E+1
C3     DC      X'C38E5A1C55CEB1C4'    -0.2277631917769813E+4
C2     DC      X'422FC604E13C20FE'     0.4777351196020117E+2
C1     DC      X'3DDABB6C9F18C6DD'     0.2085992109128247E-3
IPART  DC      X'4600000000000000'
LOGE2  DC      X'40B17217F7D1CF7B'     LOG(2) BASE E + FUDGE 1
ONE    DC      X'4110000000000000'     THESE THREE
HALF   DC      X'4080000000000000'       CONSTANTS MUST
       DC      X'4040000000000000'         BE CONSECUTIVE
TABLE  DC      X'0302010100000000'
LIMIT  DC      X'40B504F3'               1/SQRT 2
       END
