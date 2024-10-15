LAT2   TITLE   'ARCTANGENT FUNCTION (LONG, 2 ENTRY POINTS)'
ARCTAN@P CSECT
*HJNX 013600,0126-0130,0210-0234                                  18303
*
*        ARCTANGENT FUNCTION
*              1. REDUCE THE CASE TO THE 1ST OCTANT BY USING
*                   ATAN(-X)=-ATAN(X), ATAN(1/X)=PI/2-ATAN(X)
*              2. REDUCE FURTHER TO THE CASE /X/ LESS THAN TAN(PI/12)
*                   BY ATAN(X)=PI/6+ATAN((X*SQRT3-1)/(X+SQRT3)).
*              3. FOR THE BASIC RANGE (X LESS THAN TAN(PI/12)), USE
*                   A CONTINUED FRACTION APPROXIMATION
       SPACE
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
FR6    EQU     6
       SPACE
       USING   *,GRL
DATAN  BC      15,LATN         DATAN ENTRY POINT
       DC      AL1(5)
       DC      CL5'DATAN'
       SPACE
LATN   STM     GRR,GRL,12(GRS) SAVE REGISTERS
       MVI     XFLAG,X'00'       PRESET XFLAG TO PLUS
       SPACE
         L     GR2,0(GRA)
       LD      FR0,0(GR2)      OBTAIN 1ST (OR ONLY) ARGUMENT X1
       L       GR0,0(GR2)        SAVE ITS SIGN
       LPER    FR0,FR0         FORCE SIGN POSITIVE
         LD    FR4,ONE                 MAIN CIRCUIT
       SR      GR1,GR1         GR1, GR2 FOR DISTINGUISHING CASES
       LA      GR2,ZERO
       CER     FR0,FR4
       BC      12,SKIP1
       LDR     FR2,FR4         IF X GREATER THAN 1, TAKE INVERSE
       DDR     FR2,FR0           AND INCREMEMENT GR1 BY 16
       LDR     FR0,FR2
       LA      GR1,16
       SPACE
SKIP1  CE      FR0,SMALL       IF ARG LESS THAN 16**-7, ANS=ARG.
       BC      12,READY          THIS AVOIDS UNDERFLOW EXCEPTION
       CE      FR0,TAN15
       BC      12,SKIP2
       LDR     FR2,FR0         IF X GREATER THAN TAN(PI/12),
       MD      FR0,RT3M1         REDUCE X TO (X*SQRT3-1)/(X+SQRT3)
       SDR     FR0,FR4         COMPUTE X*SQRT3-1 AS
       ADR     FR0,FR2           X*(SQRT3-1)-1+X
       AD      FR2,RT3             TO GAIN ACCURACY
       DDR     FR0,FR2
       LA      GR2,8(GR2)      INCREMENT GR2 BY 8
       SPACE
SKIP2  LDR     FR6,FR0         COMPUTE ATAN OF REDUCED ARGUMENT BY
       MDR     FR0,FR0           ATAN(X) = X+X*X**2*F, WHERE
       LD      FR4,C7              F = C1+C2/(X**2+C3+C4/
       ADR     FR4,FR0               (X**2+C5+C6/(X**2+C7)))
       LD      FR2,C6
       DDR     FR2,FR4
       AD      FR2,C5
       ADR     FR2,FR0
       LD      FR4,C4
       DDR     FR4,FR2
       AD      FR4,C3
       ADR     FR4,FR0
       LD      FR2,C2
       DDR     FR2,FR4
       AD      FR2,C1
       MDR     FR0,FR2
       MDR     FR0,FR6
       ADR     FR0,FR6
       SPACE
READY  AD      FR0,0(GR1,GR2)  DEPENDING ON THE CASE,
       LNR     GR1,GR1           EITHER ADD 0 OR PI/6, OR
       SD      FR0,ZERO(GR1)       SUBTRACT FROM PI/3 OR PI/2
       LPER    FR0,FR0         DO THE LATTER IN TWO STEPS
         LTR   GR0,GR0                 SIGN OF ANS SHOULD AGREE
       BC      10,*+6            WITH SIGN OF ARG
       LCER    FR0,FR0
       SPACE
         SR    GRL,GRL                 SUCCESSFUL RETURN
EXIT   L       GRR,12(GRS)
       MVI     12(GRS),X'FF'   RETURN
       BCR     15,GRR
       SPACE
       SPACE
         EJECT
       DS      0D
C1     DC      X'BF1E31FF1784B965'    -0.7371899082768562E-2
C2     DC      X'C0ACDB34C0D1B35D'    -0.6752198191404210
C3     DC      X'412B7CE45AF5C165'     0.2717991214096480E+1
C4     DC      X'C11A8F923B178C78'    -0.1660051565960002E+1
C5     DC      X'412AB4FD5D433FF6'     0.2669186939532663E+1
C6     DC      X'C02298BB68CFD869'    -0.1351430064094942
C7     DC      X'41154CEE8B70CA99'     0.1331282181443987E+1
RT3M1  DC      X'40BB67AE8584CAA8'     SQRT(3)-1
ONE    DC      X'4110000000000000'             THESE
RT3    DC      X'411BB67AE8584CAB'     SQRT(3)   SIX
ZERO   DC      D'0'                    0           CONSTANTS
       DC      X'40860A91C16B9B2C'     PI/6          MUST
MPO2P1 DC      X'C0921FB54442D184'     -PI/2+1         BE
       DC      X'BFC152382D736574'     -(PI/3-F)+1       CONSECUTIVE
SMALL  DC      X'3A100000'
XFLAG  DS      F
TAN15  DC      X'40449851'
       END
