LSCN   TITLE   'SINE-COSINE FUNCTIONS (LONG)'
COS@P    CSECT
*
*      SINE-COSINE FUNCTIONS (LONG)
*              1. DIVIDE MAGNITUDE OF ARG BY PI/4 TO FIND OCTANT
*                   AND FRACTION.
*              2. IF COSINE, ADD 2 TO OCTANT NUMBER.
*              3. IF SINE, ADD 0(4) TO OCTANT NUMBER FOR +ARG(-ARG).
*              4. COMPUTE SINE OR COSINE OF FRACTION*PI/4 DEPENDING
*                   ON THE OCTANT.
*              5. IF OCTANT NUMBER IS FOR LOWER PLANE, MAKE SIGN -.
       SPACE
         ENTRY SIN@P
       SPACE
GR0    EQU     0               SCRATCH REGISTERS
GR1    EQU     1
GR2    EQU     14
GRA    EQU     1               ARGUMENT POINTER
GRS    EQU     13              SAVE AREA POINTER
GRR    EQU     14              RETURN REGISTER
GRL    EQU     15              LINK REGISTER
FR0    EQU     0               ANSWER REGISTER
FR2    EQU     2               SCRATCH REGISTERS
FR4    EQU     4
       SPACE
       USING   *,GRL
DCOS   BC      15,LCOS         COSINE ENTRY
       DC      AL1(4)
       DC      CL5'DCOS'
LCOS   STM     GRR,GR0,12(GRS) SAVE REGISTERS
       LA      GR0,2             FOR COSINE, OCTANT CRANK IS 2
       L       GR2,0(GRA)          COS(X) = SIN(PI/2+X)
       BAL     GRL,MERGE             ADJUST BASE REGISTER AND MERGE
       SPACE
       USING   *,GRL
SIN@P    DS    0H
DSIN   BC      15,LSIN         SINE ENTRY
       DC      AL1(4)
       DC      CL5'DSIN'
LSIN   STM     GRR,GR0,12(GRS) SAVE REGISTERS
         L     GR2,0(GRA)         ADDR OF DATA
       SR      GR0,GR0           FOR SINE, OCTANT CRANK IS 0 IF +ARG
         TM    0(GR2),X'80'       OCTANT CRANK IS 4 IF ARG NEG
         BC    8,*+8              SIN(-X) = SIN(PI+X)
       LA      GR0,4
       SPACE
MERGE  LD      FR0,0(GR2)      PICK UP THE ARGUMENT
       LPER    FR0,FR0         FORCE SIGN OF ARG TO +
       CE      FR0,MAX
       BC      10,ERROR        ERROR IF /X/ GRT THAN OR = PI*2**50
       SPACE
       DD      FR0,PIOV4       DIVIDE BY PI/4 AND SEPARATE INTEGER
       LDR     FR2,FR0           PART AND FRACTION PART OF QUOTIENT
       AW      FR2,SCALER      FORCE CHARACTERISTIC X'4E'
       STD     FR2,BUFF        INTEGER PART UNNORMALIZED = OCTANT
       AD      FR2,SCALER      INTEGER PART NORMALIZED
       SDR     FR0,FR2         FRACTION PART TO FR0
       AL      GR0,BUFF+4      ADJUST OCTANT NUMBER WITH CRANK
       ST      GR0,OCTNT         AND SAVE IT
       SPACE
       TM      OCTNT+3,X'01'   IF ODD OCTANT, TAKE COMPLEMENT
       BC      8,EVEN            OF FRACTION TO OBTAIN MODIFIED ARG
       SD      FR0,C0
       SPACE
EVEN   LPDR    FR4,FR0
       SR      GR1,GR1         GR1 = 0 FOR COSINE POLYNOMIAL
       TM      OCTNT+3,X'03'     THIS IS FOR OCTANT 2, 3, 6, OR 7
       BC      4,*+8           GR1 = 8 FOR SINE POLYNOMIAL
       LA      GR1,8             THIS IS FOR OCTANT 1, 4, 5, OR 8
       SPACE
       CE      FR4,UNFLO       IF X IS LESS THAN 16**-7, SET X TO 0
       BC      2,*+6             THIS PREVENTS UNDERFLOW
       SDR     FR0,FR0
       SPACE
       MDR     FR0,FR0         COMPUTE SINE OR COSINE OF MODIFIED
       LDR     FR2,FR0           ARG USING PROPER CHEBYSHEV
       MD      FR0,C7(GR1)         INTERPOLATION POLYNOMIAL
       AD      FR0,C6(GR1)
       MDR     FR0,FR2         SIN(X)/X POLYNOMIAL OF DEG 6 IN X**2
       AD      FR0,C5(GR1)     COS(X) POLYNOMIAL OF DEG 7 IN X**2
       MDR     FR0,FR2
       AD      FR0,C4(GR1)
       MDR     FR0,FR2
       AD      FR0,C3(GR1)
       MDR     FR0,FR2
       AD      FR0,C2(GR1)
       MDR     FR0,FR2
       AD      FR0,C1(GR1)
       SPACE
       LTR     GR1,GR1
       BC      8,COSF
       MDR     FR0,FR4         COMPLETE SINE POLYNOMIAL BY
       BC      15,SIGN           MULTIPLYING BY X
       SPACE
COSF   MDR     FR0,FR2         COMPLETE COSINE POLYNOMIAL
       AD      FR0,C0            (ONE MORE DEGREE)
       SPACE
SIGN   TM      OCTNT+3,X'04'   IF MODIFIED OCTANT IS IN
       BC      8,*+6             LOWER PLANE, SIGN IS NEGATIVE
       LNER    FR0,FR0
EXIT   EQU     *
         SR    GRL,GRL                 ZERO COND. CODE
EXITE    DS    0H
       SPACE
         L     GRR,12(GRS)         RETURN POINT
       MVI     12(GRS),X'FF'   RETURN
         BCR   8,GRR                   RETURN W/OUT ERROR
         LA    GRL,4
         BCR   15,GRR                  RETURN WITH ERROR
       SPACE
ERROR    DS    0H
         LTR   GRL,GRL                 SET CC TO INDICATE ERROR
         B     EXITE
       SPACE
BUFF   DS      D
OCTNT  EQU     BUFF
C7     DC      X'B66C992E84B6AA37'     COS C7
       DC      X'3778FCE0E5AD1685'     SIN C6
C6     DC      X'387E731045017594'     COS C6
       DC      X'B978C01C6BEF8CB3'     SIN C5
C5     DC      X'BA69B47B1E41AEF6'     COS C5
       DC      X'3B541E0BF684B527'     SIN C4
C4     DC      X'3C3C3EA0D06ABC29'     COS C4
       DC      X'BD265A599C5CB632'     SIN C3
C3     DC      X'BE155D3C7E3C90F8'     COS C3
       DC      X'3EA335E33BAC3FBD'     SIN C2
C2     DC      X'3F40F07C206D6AB1'     COS C2
       DC      X'C014ABBCE625BE41'     SIN C1
C1     DC      X'C04EF4F326F91777'     COS C1 -2F
PIOV4  DC      X'40C90FDAA22168C2'     SIN C0
C0     DC      X'4110000000000000'     COS C0
SCALER DC      X'4E00000000000000'
UNFLO  DC      X'3A100000'
MAX    DC      X'4DC90FDA'
       END
