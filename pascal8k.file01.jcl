//*      THIS FILE CONTAINS CONTROL CARDS WHICH MAY BE USED TO          00001000
//*      IMPLEMENT THE PASCAL 8000 SYSTEM WRITTEN BY HIKITA AND         00002000
//*      ISHIHATA FOR HITAC 8000 COMPUTERS, AND MODIFIED BY             00003000
//*      COX AND TOBIAS FOR IBM360/370 COMPUTERS                        00004000
//*                                                                     00005000
//*      CARDS WITH COMMENTS +VOLSER+ OR +DSN+ MAY NEED TO BE CHANGED   00006000
//*      TO SUIT LOCAL CONVENTIONS                                      00007000
//*                                                                     00008000
//*      THE 'QUICK IMPLEMENTATION' DECK STARTS HERE                    00009000
//*                                                                     00010000
//*      THIS STEP CATALOGS NEW DATASETS REQUIRED                       00011000
//A  EXEC  PGM=IEFBR14                                                  00012000
//DSK    DD    UNIT=SYSDA,DISP=OLD,VOL=SER=VOLSER              +VOLSER+ 00013000
//A      DD    DSN=PASCAL.PASLIB,                                 +DSN+ 00014000
//             SPACE=(TRK,(18,5,5)),    THIS IS ENOUGH FOR 3330 DISKS   00015000
//             VOL=REF=*.DSK,                                           00016000
//             DISP=(NEW,CATLG)                                         00017000
//B      DD    DSN=PASCAL.PASOBJ1,                                +DSN+ 00018000
//             SPACE=(TRK,(9,2)),       THIS IS ENOUGH FOR 3330         00019000
//             VOL=REF=*.DSK,                                           00020000
//             DCB=(RECFM=F,BLKSIZE=1024),                              00021000
//             DISP=(NEW,CATLG)                                         00022000
//C      DD    DSN=PASCAL.PASOBJ2,                                +DSN+ 00023000
//             SPACE=(TRK,(1,1)),      THIS IS ENOUGH FOR ANY DISK      00024000
//             VOL=REF=*.DSK,                                           00025000
//             DCB=(RECFM=FB,LRECL=8,BLKSIZE=800),                      00026000
//             DISP=(NEW,CATLG)                                         00027000
//D      DD    DSN=PASCAL.PASMSGS,                                +DSN+ 00028000
//             SPACE=(TRK,(1,1)),      THIS IS ENOUGH FOR 3330          00029000
//             VOL=REF=*.DSK,                                           00030000
//             DCB=(RECFM=VB,LRECL=84,BLKSIZE=800),                     00031000
//             DISP=(NEW,CATLG)                                         00032000
//*                                                                     00033000
//*                                                                     00034000
//*      THE NEXT STEP MOVES ONTO THE SPECIFIED DISK, THE PDS           00035000
//*      CONTAINING LOAD MODULES OF THE RUNTIME SYSTEM FOR THE          00036000
//*      COMPILE-AND-GO VERSION, AND OF THE COMPILER AND STANDARD       00037000
//*      FUNCTION LIBRARY FOR THE LINKAGE-EDIT VERSION.                 00038000
//*                                                                     00039000
//MVE  EXEC  PGM=IEHMOVE                                                00040000
//SYSUT1  DD  VOL=REF=*.A.DSK,DISP=OLD   WORK DISK FOR IEHMOVE          00041000
//SYSPRINT  DD  SYSOUT=A                                                00042000
//D  DD  VOL=REF=*.A.DSK,DISP=OLD        TARGET DISK FOR PDS            00043000
//T  DD  UNIT=(TAPE,,DEFER),VOL=(,RETAIN,SER=PASCAL),LABEL=(2,NL),      00044000
//  DCB=(RECFM=FB,BLKSIZE=800,LRECL=80)                                 00045000
//*                                                                     00046000
C  COPY PDS=PASCAL.PASLIB,TO=3330=VOLSER,FROMDD=T,             +VOLSER+*00047000
               RENAME=PASCAL.PASLIB,FROM=TAPE=(PASCAL,2)          +DSN+ 00048000
//*                                                                     00049000
//*      THE NEXT STEP COPIES THE 3 COMPILER FILES TO DISK              00050000
//*      THE FIRST TWO OF THESE ARE THE COMPILER CODE FOR USE BY        00051000
//*      THE COMPILE-AND-GO VERSION; THE THIRD IS THE ERROR             00052000
//*      MESSAGE DATASET COMMON TO BOTH VERSIONS.                       00053000
//*                                                                     00054000
//PASCAL  EXEC  PGM=PASCAL,PARM='CMP,$PASOBJ',REGION=160K               00055000
//STEPLIB  DD  DSN=PASCAL.PASLIB,DISP=SHR                         +DSN+ 00056000
//SYSPRINT  DD  SYSOUT=A,DCB=(RECFM=FA,BLKSIZE=133)                     00057000
//CMP1  DD  LABEL=(3,NL),DCB=(RECFM=F,BLKSIZE=1024),                    00058000
//    UNIT=TAPE,VOL=(,RETAIN,SER=PASCAL)                                00059000
//CMP2  DD  LABEL=(4,NL),DCB=(RECFM=FB,LRECL=8,BLKSIZE=800),            00060000
//    UNIT=TAPE,VOL=(,RETAIN,SER=PASCAL)                                00061000
//$PASMSGS  DD  LABEL=(5,NL),DCB=(RECFM=VB,LRECL=84,BLKSIZE=800),       00062000
//    UNIT=TAPE,VOL=(,RETAIN,SER=PASCAL)                                00063000
//$PASOBJ1  DD  UNIT=SYSDA,SPACE=(TRK,(5,5)),DCB=(RECFM=F,BLKSIZE=1024) 00064000
//$PASOBJ2  DD  UNIT=SYSDA,SPACE=(TRK,1)                                00065000
//F3  DD  DISP=OLD,DSN=PASCAL.PASOBJ1                             +DSN+ 00066000
//F4  DD  DISP=OLD,DSN=PASCAL.PASOBJ2                             +DSN+ 00067000
//F5  DD  DISP=OLD,DSN=PASCAL.PASMSGS                             +DSN+ 00068000
//CMPI  DD  *                                                           00069000
(*$U+  TELL COMPILER TO IGNORE COLS 73-80 *)                            00070000
PROGRAM INSTALPASCAL(OUTPUT, CMP1, CMP2, $PASMSGS, F3, F4, F5);         00071000
                                                                        00072000
  PROCEDURE COPYTEXT( VAR T1,T2:TEXT);                                  00073000
  BEGIN                                                                 00074000
  RESET (T1); REWRITE (T2);                                             00075000
  WHILE NOT EOF(T1) DO                                                  00076000
    BEGIN                                                               00077000
      WHILE NOT EOLN(T1) DO                                             00078000
        BEGIN T2@ := T1@; PUT(T2); GET(T1); END;                        00079000
      WRITELN (T2);                                                     00080000
      READLN (T1);                                                      00081000
    END;                                                                00082000
  END;                                                                  00083000
                                                                        00084000
  PROCEDURE COPY3;                                                      00085000
  VAR CMP1,F3: FILE OF ARRAY (.1..256.) OF INTEGER;                     00086000
  BEGIN                                                                 00087000
    RESET (CMP1); REWRITE(F3);                                          00088000
    REPEAT BEGIN F3@:=CMP1@; PUT(F3); GET(CMP1); END                    00089000
    UNTIL EOF(CMP1);                                                    00090000
  END;                                                                  00091000
                                                                        00092000
  PROCEDURE COPY4;                                                      00093000
  VAR CMP2,F4: FILE OF PACKED ARRAY (.1..8.) OF CHAR;                   00094000
  BEGIN                                                                 00095000
    RESET (CMP2); REWRITE (F4);                                         00096000
    REPEAT BEGIN F4@:=CMP2@; PUT(F4); GET(CMP2); END                    00097000
    UNTIL EOF(CMP2);                                                    00098000
  END;                                                                  00099000
                                                                        00100000
  PROCEDURE COPY5;                                                      00101000
  VAR $PASMSGS,F5: TEXT;                                                00102000
  BEGIN COPYTEXT($PASMSGS,F5) END;                                      00103000
                                                                        00104000
 BEGIN                                                                  00105000
  COPY3; COPY4; COPY5                                                   00106000
 END .                                                                  00107000
/*                                                                      00108000
//*                                                                     00109000
//*      THE 'QUICK IMPLEMENTATION' DECK ENDS HERE                      00110000
//*                                                                     00111000
//                                                                      00112000
//*                                                                     00113000
//*      IMPLEMENTATION BY ASSEMBLY AND COMPILATION STARTS HERE         00114000
//*                                                                     00115000
//*                                                                     00116000
//*      THE FIRST STEP ALLOCATES REQUIRED DATASETS                     00117000
//*                                                                     00118000
//B      EXEC  PGM=IEFBR14                                              00119000
//DSK    DD    UNIT=SYSDA,DISP=OLD,VOL=SER=VOLSER              +VOLSER+ 00120000
//*      CHANGE THE ABOVE CARD TO SPECIFY A TARGET DISK                 00121000
//A        DD  DCB=SYS1.MACLIB,                                         00122000
//             DSN=PASCAL.RSOURCE,                                +DSN+ 00123000
//             SPACE=(TRK,(60,10,10)),  THIS IS OK FOR A 3330           00124000
//             DISP=(,CATLG),                                           00125000
//             VOL=REF=*.B.DSK                                          00126000
//B        DD  VOL=REF=*.B.DSK,                                         00127000
//             SPACE=(TRK,(10,5,5)),                                    00128000
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=400),                     00129000
//             DSN=PASCAL.ROBJ,                                   +DSN+ 00130000
//             DISP=(,CATLG)                                            00131000
//*                                                                     00132000
//*                                                                     00133000
//*      THE FOLLOWING STEP LOADS THE RUNTIME SOURCE PDS                00134000
//*      CONTAINING MODULES FOR BOTH VERSIONS OF THE COMPILER           00135000
//*                                                                     00136000
//UPD    EXEC  PGM=IEBUPDTE,PARM=NEW                                    00137000
//SYSPRINT DD  DUMMY                                                    00138000
//SYSUT2   DD  DSN=PASCAL.RSOURCE,DISP=OLD                        +DSN+ 00139000
//SYSIN    DD  LABEL=(6,NL),DCB=(RECFM=FB,LRECL=80,BLKSIZE=2000),       00140000
//             UNIT=(TAPE,,DEFER),VOL=(,RETAIN,SER=PASCAL),DISP=OLD     00141000
//*                                                                     00142000
//*                                                                     00143000
//*      THE FOLLOWING STEPS ASSEMBLE ALL SOURCES OF THE RUNTIME SYSTEM 00144000
//*      TO THE PDS OF OBJECT MODULES, PASCAL.ROBJ                      00145000
//*                                                                     00146000
//ASM    PROC                                                           00147000
//ASM    EXEC  PGM=ASMBLR,PARM='LOAD,NODECK'                            00148000
//SYSLIB   DD  DISP=SHR,DSN=PASCAL.RSOURCE                        +DSN+ 00149000
//         DD  DISP=SHR,DSN=SYS1.MACLIB                                 00150000
//SYSPRINT DD  SYSOUT=A                                                 00151000
//SYSUT1   DD  UNIT=SYSDA,SPACE=(TRK,(10,10))                           00152000
//SYSUT2   DD  UNIT=SYSDA,SPACE=(TRK,(10,10))                           00153000
//SYSUT3   DD  UNIT=SYSDA,SPACE=(TRK,(10,10))                           00154000
//SYSGO    DD  DSN=PASCAL.ROBJ(&M),DISP=OLD                       +DSN+ 00155000
//SYSIN    DD  DISP=SHR,DSN=PASCAL.RSOURCE(&M)                    +DSN+ 00156000
//       PEND                                                           00157000
//*      FIRST ASSEMBLE MODULES COMMON TO BOTH VERSIONS OF              00158000
//*      THE SYSTEM                                                     00159000
//B1     EXEC  ASM,M=WRITFP                                             00160000
//B2     EXEC  ASM,M=ARCTAN@P                                           00161000
//B3     EXEC  ASM,M=EXP@P                                              00162000
//B4     EXEC  ASM,M=LN@P                                               00163000
//B5     EXEC  ASM,M=SIN@P                                              00164000
//B6     EXEC  ASM,M=SQRT@P                                             00165000
//*                                                                     00166000
//*      NEXT ASSEMBLE MODULES SPECIFIC TO COMPILE-AND-GO VERSION       00167000
//*                                                                     00168000
//C1     EXEC  ASM,M=PASCAL                                             00169000
//C2     EXEC  ASM,M=PASGET                                             00170000
//C3     EXEC  ASM,M=PASINIT                                            00171000
//C4     EXEC  ASM,M=PASLOAD                                            00172000
//C5     EXEC  ASM,M=PASTERM                                            00173000
//C6     EXEC  ASM,M=RUNTIME                                            00174000
//*                                                                     00175000
//*      NEXT ASSEMBLE MODULES SPECIFIC TO LINKAGE-EDIT VERSION         00176000
//*                                                                     00177000
//L1     EXEC  ASM,M=PASCALL                                            00178000
//L2     EXEC  ASM,M=PASLINIT                                           00179000
//L3     EXEC  ASM,M=PASLTERM                                           00180000
//L4     EXEC  ASM,M=RUNTIMEL                                           00181000
//*                                                                     00182000
//*      THE NEXT STEPS COMPILE THE PASCAL SOURCE CODE FOR BOTH         00183000
//*      VERSIONS OF THE COMPILER                                       00184000
//*                                                                     00185000
//*      1.  COMPILE THE COMPILE-AND-GO VERSION                         00186000
//P1     EXEC  PGM=PASCALO,PARM=CMP,REGION=220K                         00187000
//STEPLIB  DD  DISP=SHR,DSN=PASCAL.PASLIB                         +DSN+ 00188000
//SYSPRINT DD  SYSOUT=A                                                 00189000
//$PASMSGS DD  DISP=SHR,DSN=PASCAL.PASMSGS                        +DSN+ 00190000
//CMPI     DD  DISP=SHR,UNIT=(TAPE,,DEFER),LABEL=(7,NL),                00191000
//             DCB=(RECFM=VB,LRECL=116,BLKSIZE=2324),                   00192000
//             VOL=(,RETAIN,SER=PASCAL)                                 00193000
//CMP1     DD  DISP=SHR,DSN=PASCAL.PASOBJ1                        +DSN+ 00194000
//CMP2     DD  DISP=SHR,DSN=PASCAL.PASOBJ2                        +DSN+ 00195000
//$PASOBJ1 DD  DISP=(,CATLG),DSN=PASCAL.COBJ1,                    +DSN+ 00196000
//             UNIT=SYSDA,SPACE=(TRK,(9,2)),                            00197000
//             DCB=(RECFM=F,BLKSIZE=1024)                               00198000
//$PASOBJ2 DD  DISP=(,CATLG),DSN=PASCAL.COBJ2,                    +DSN+ 00199000
//             UNIT=SYSDA,SPACE=(TRK,(1,1)),                            00200000
//             DCB=(RECFM=FB,LRECL=8,BLKSIZE=400)                       00201000
//*                                                                     00202000
//*                                                                     00203000
//*      2.  COMPILE THE LINKAGE-EDIT VERSION                           00204000
//P2     EXEC  PGM=PASCALC,REGION=240K                                  00205000
//STEPLIB  DD  DISP=SHR,DSN=PASCAL.PASLIB                         +DSN+ 00206000
//SYSPRINT DD  SYSOUT=A                                                 00207000
//$PASMSGS DD  DISP=SHR,DSN=PASCAL.PASMSGS                        +DSN+ 00208000
//SYSIN    DD  DISP=SHR,UNIT=(TAPE,,DEFER),LABEL=(8,NL),                00209000
//             DCB=(RECFM=VB,LRECL=116,BLKSIZE=2324),                   00210000
//             VOL=(,RETAIN,SER=PASCAL)                                 00211000
//SYSGO    DD  DISP=(,CATLG),DSN=PASCAL.LOBJ,                     +DSN+ 00212000
//             UNIT=SYSDA,SPACE=(TRK,(15,2)),                           00213000
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=400)                      00214000
//*                                                                     00215000
//*                                                                     00216000
//*                                                                     00217000
//*      THE NEXT STEP USES THE LINKAGE EDITOR TO CREATE OVERLAY AND    00218000
//*      NON-OVERLAY LOAD MODULES OF THE RUNTIME SYSTEM FOR THE         00219000
//*      COMPILE-AND-GO VERSION                                         00220000
//*                                                                     00221000
//LKED   EXEC  PGM=IEWL,PARM='OVLY,LIST,MAP'                            00222000
//SYSUT1   DD  UNIT=SYSDA,SPACE=(TRK,(5,5))                             00223000
//SYSPRINT DD  SYSOUT=A                                                 00224000
//SYSLMOD  DD  DSN=PASCAL.PASLIB,                                 +DSN+ 00225000
//             DISP=SHR                                                 00226000
//SYSLIB   DD  DISP=SHR,DSN=PASCAL.ROBJ                           +DSN+ 00227000
//SYSLIN  DD  *                                                         00228000
         INCLUDE SYSLIB(PASCAL,RUNTIME)                                 00229000
         INSERT PASCAL                                                  00230000
         INSERT TIMEX                                                   00231000
         INSERT WRITFP                                                  00232000
         OVERLAY A                                                      00233000
         INSERT PASINIT,PASGET,PASLOAD                                  00234000
         OVERLAY A                                                      00235000
         INSERT RUNTIME#                                                00236000
         OVERLAY B                                                      00237000
         INSERT PASRUN                                                  00238000
         OVERLAY A                                                      00239000
         INSERT PASTERM                                                 00240000
         NAME PASCALO(R)                                                00241000
         INCLUDE SYSLMOD(PASCALO)                                       00242000
         NAME PASCAL(R)                                                 00243000
//*                                                                     00244000
//*                                                                     00245000
//*      THE NEXT STEP USES THE LINKAGE EDITOR TO CREATE LOAD MODULES   00246000
//*      OF THE LINKAGE-EDITOR VERSION OF THE PASCAL SYSTEM.            00247000
//*      THESE MODULES ARE:                                             00248000
//*      1. PASCALL   THIS IS THE BARE RUNTIME SYSTEM WHICH MUST BE     00249000
//*                   LINKED WITH A PASCAL-COMPILED CSECT.  THE MAIN    00250000
//*                   PROGRAM ALWAYS HAS THE CSECT NAME P@MAIN          00251000
//*      2. PASCALC   THIS IS THE COMPILER MODULE.  ITS OVERLAY         00252000
//*                   STRUCTURE GIVES THE MINIMUM SIZE MODULE POSSIBLE  00253000
//*      3. STANDARD FUNCTIONS.  THESE ARE REQUIRED BY THE LINKAGE      00254000
//*                   EDITOR WHENEVER A COMPILED PROGRAM REFERS TO      00255000
//*                   A STANDARD FUNCTION.                              00256000
//*                                                                     00257000
//L  EXEC  PGM=IEWL,PARM='NCAL,LET,OVLY,LIST,MAP'                       00258000
//SYSPRINT DD  SYSOUT=A                                                 00259000
//SYSUT1   DD  UNIT=SYSDA,SPACE=(TRK,(5,5))                             00260000
//SYSLMOD  DD  DISP=SHR,DSN=PASCAL.PASLIB                         +DSN+ 00261000
//ROBJ     DD  DISP=SHR,DSN=PASCAL.ROBJ                           +DSN+ 00262000
//LOBJ     DD  DISP=SHR,DSN=PASCAL.LOBJ                           +DSN+ 00263000
//SYSLIN  DD  *                                                         00264000
 INCLUDE ROBJ(PASCALL,RUNTIMEL,PASLINIT,PASLTERM,WRITFP)                00265000
 ENTRY PASCAL                                                           00266000
 NAME PASCALL(R)      RUNTIME SYSTEM, NEEDED BY COMPILED PROGRAMS       00267000
 INCLUDE ROBJ(PASCALL,RUNTIMEL,PASLINIT,PASLTERM,WRITFP)                00268000
 INCLUDE LOBJ                                                           00269000
 ENTRY PASCAL                                                           00270000
 INSERT PASCAL,TIMEX,WRITFP,P@MAIN                                      00271000
 OVERLAY A                                                              00272000
 INSERT PASINIT,P@MAIN@V,P@MAIN@                                        00273000
 OVERLAY A                                                              00274000
 INSERT RUNTIME#,PASRUN                                                 00275000
 OVERLAY A                                                              00276000
 INSERT PASTERM                                                         00277000
 NAME PASCALC(R)                                                        00278000
 INCLUDE ROBJ(SIN@P)                                                    00279000
 ALIAS COS@P                                                            00280000
 NAME SIN@P(R)                                                          00281000
 INCLUDE ROBJ(ARCTAN@P)                                                 00282000
 NAME ARCTAN@P(R)                                                       00283000
 INCLUDE ROBJ(SQRT@P)                                                   00284000
 NAME SQRT@P(R)                                                         00285000
 INCLUDE ROBJ(EXP@P)                                                    00286000
 NAME EXP@P(R)                                                          00287000
 INCLUDE ROBJ(LN@P)                                                     00288000
 NAME LN@P(R)                                                           00289000
//*                                                                     00290000
//*      IMPLEMENTATION BY ASSEMBLY AND COMPILATION ENDS HERE           00291000
//*                                                                     00292000
//                                                                      00293000
//*                                                                     00294000
//*      A SUGGESTED PASCAL COMPILE-AND-GO PROCEDURE FOLLOWS            00295000
//*                                                                     00296000
//*      USE THIS PROCEDURE AS FOLLOWS:                                 00297000
//*                                                                     00298000
//*      //A   EXEC  PASCAL                                             00299000
//*      //SYSIN DD  *                                                  00300000
//*            PUT PASCAL SOURCE PROGRAM HERE                           00301000
//*      /*                                                             00302000
//*      //DATA  DD  *                                                  00303000
//*            PUT DATA FOR PASCAL PROGRAM HERE                         00304000
//*      /*                                                             00305000
//*                                                                     00306000
//PASCAL PROC                                                           00307000
//PASCAL EXEC  PGM=PASCALO,REGION=128K,PARM='CMP,$PASOBJ'               00308000
//STEPLIB  DD  DISP=SHR,DSN=PASCAL.PASLIB                     +DSN+     00309000
//SYSPRINT  DD  SYSOUT=A                                                00310000
//CMP1     DD  DISP=SHR,DSN=PASCAL.PASOBJ1                    +DSN+     00311000
//CMP2     DD  DISP=SHR,DSN=PASCAL.PASOBJ2                    +DSN+     00312000
//CMPI     DD  DDNAME=SYSIN                                             00313000
//LOCAL    DD  UNIT=SYSDA,DISP=OLD,VOL=SER=VOLSER             +VOLSER+  00314000
//$PASMSGS DD  DISP=SHR,DSN=PASCAL.PASMSGS                    +DSN+     00315000
//$PASOBJI DD  DDNAME=DATA                                              00316000
//$PASOBJ1 DD  UNIT=SYSDA,SPACE=(TRK,(10,5)),                           00317000
//             DCB=(RECFM=F,BLKSIZE=1024,BUFNO=1)                       00318000
//$PASOBJ2 DD  UNIT=SYSDA,SPACE=(TRK,(1,1)),                            00319000
//             DCB=(RECFM=FB,LRECL=8,BLKSIZE=200,BUFNO=1)               00320000
//       PEND  (* DO NOT ADD PEND CARD TO PROCLIB *)                    00321000
//*                                                                     00322000
//*                                                                     00323000
//*                                                                     00324000
//*      SUGGESTED PROCEDURES FOR USE OF THE LINKAGE-EDITOR VERSION     00325000
//*      ARE GIVEN HERE                                                 00326000
//*                                                                     00327000
//*      1.  COMPILE ONLY                                               00328000
//*                                                                     00329000
//*      USE THIS PROCEDURE AS FOLLOWS:                                 00330000
//*      //    EXEC  PASC                                               00331000
//*      //PASC.SYSIN  DD  *                                            00332000
//*        PUT PASCAL SOURCE PROGRAM HERE                               00333000
//*      /*                                                             00334000
//*                                                                     00335000
//PASC   PROC                                                           00336000
//PASC   EXEC  PGM=PASCALC,REGION=128K                                  00337000
//$PASMSGS DD  DISP=SHR,DSN=PASCAL.PASMSGS                        +DSN+ 00338000
//STEPLIB  DD  DISP=SHR,DSN=PASCAL.PASLIB                         +DSN+ 00339000
//SYSGO    DD  DISP=(,PASS),UNIT=SYSDA,SPACE=(TRK,(5,10)),              00340000
//             DSN=&&OBJ,DCB=(RECFM=FB,LRECL=80,BLKSIZE=400)            00341000
//SYSPRINT DD  SYSOUT=A                                                 00342000
//       PEND  (* DO NOT ADD PEND CARD TO PROCLIB *)                    00343000
//*                                                                     00344000
//*                                                                     00345000
//*      FOR THE COMPILE-LINK AND COMPILE-LINK-GO PROCEDURES,           00346000
//*      THE FOLLOWING CARD SHOULD BE ADDED TO SYS1.PROCLIB             00347000
//*      AND GIVEN THE MEMBER NAME PASCLCC                              00348000
 INCLUDE SYSLIB(PASCALL)                                                00349000
//*                                                                     00350000
//*                                                                     00351000
//*      2.  COMPILE AND LINK                                           00352000
//*                                                                     00353000
//*      USE THIS PROCEDURE AS FOLLOOWS                                 00354000
//*      //   EXEC  PASCL                                               00355000
//*      //PASC.SYSIN  DD  *                                            00356000
//*         PUT PASCAL SOURCE PROGRAM HERE                              00357000
//*      /*                                                             00358000
//*      //LKED.SYSIN  DD  *                                            00359000
//*        PUT LINKAGE-EDIT CONTROL STATEMENTS AND/OR OBJECT MODULES    00360000
//*        OF EXTERNALLY COMPILED ROUTINES HERE                         00361000
//*      /*                                                             00362000
//*                                                                     00363000
//PASCL  PROC                                                           00364000
//PASC   EXEC  PGM=PASCALC,REGION=128K                                  00365000
//$PASMSGS DD  DISP=SHR,DSN=PASCAL.PASMSGS                        +DSN+ 00366000
//STEPLIB  DD  DISP=SHR,DSN=PASCAL.PASLIB                         +DSN+ 00367000
//SYSGO    DD  DISP=(,PASS),UNIT=SYSDA,SPACE=(TRK,(5,10)),              00368000
//             DSN=&&OBJ,DCB=(RECFM=FB,LRECL=80,BLKSIZE=400)            00369000
//SYSPRINT DD  SYSOUT=A                                                 00370000
//LKED   EXEC  PGM=IEWL,PARM='LIST,MAP',COND=(0,NE,PASC)                00371000
//SYSLIB   DD  DISP=SHR,DSN=PASCAL.PASLIB                         +DSN+ 00372000
//*      THE FOLLOWING MAKES ALL FORTRAN LIBRARY ROUTINES ACCESSIBLE TO 00373000
//*      PASCAL PROGRAMS BY AUTOMATIC LIBRARY CALL                      00374000
//         DD  DISP=SHR,DSN=SYS1.FORTLIB                                00375000
//SYSLIN   DD  DISP=SHR,DSN=SYS1.PROCLIB(PASCLCC)                       00376000
//         DD  DISP=SHR,DSN=*.PASC.SYSGO                                00377000
//SYSLMOD  DD  DISP=(,PASS),DSN=&&LOAD(USERSPGM),SPACE=(TRK,(2,10,2)),  00378000
//             UNIT=SYSDA                                               00379000
//SYSPRINT DD  SYSOUT=A                                                 00380000
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1700,(50,100))                         00381000
//       PEND  (* DO NOT ADD PEND CARD TO PROCLIB *)                    00382000
//*                                                                     00383000
//*                                                                     00384000
//*      3. COMPILE, LINK AND GO                                        00385000
//*                                                                     00386000
//*      USE THIS PROCEDURE AS FOLLOWS:                                 00387000
//*      //    EXEC  PASCLG                                             00388000
//*      //PASC.SYSIN  DD  *                                            00389000
//*         PASCAL SOURCE PROGRAM                                       00390000
//*      /*                                                             00391000
//*      //LKED.SYSIN  DD  *                                            00392000
//*         LINK-EDIT CONTROL CARDS AND OBJECT MODULES                  00393000
//*      /*                                                             00394000
//*      //GO.SYSIN  DD  *                                              00395000
//*         DATA TO BE READ FROM FILE 'INPUT' (IF ANY)                  00396000
//*      /*                                                             00397000
//*                                                                     00398000
//PASCLG PROC                                                           00399000
//PASC   EXEC  PGM=PASCALC,REGION=128K                                  00400000
//$PASMSGS DD  DISP=SHR,DSN=PASCAL.PASMSGS                        +DSN+ 00401000
//STEPLIB  DD  DISP=SHR,DSN=PASCAL.PASLIB                         +DSN+ 00402000
//SYSGO    DD  DISP=(,PASS),UNIT=SYSDA,SPACE=(TRK,(5,10)),              00403000
//             DSN=&&OBJ,DCB=(RECFM=FB,LRECL=80,BLKSIZE=400)            00404000
//SYSPRINT DD  SYSOUT=A                                                 00405000
//LKED   EXEC  PGM=IEWL,PARM='LIST,MAP',COND=(0,NE,PASC)                00406000
//SYSLIB   DD  DISP=SHR,DSN=PASCAL.PASLIB                         +DSN+ 00407000
//         DD  DISP=SHR,DSN=SYS1.FORTLIB                                00408000
//SYSLMOD  DD  DISP=(,PASS),DSN=&&LOAD(USERSPGM),SPACE=(TRK,(2,10,2)),  00409000
//             UNIT=SYSDA                                               00410000
//SYSLIN   DD  DISP=SHR,DSN=SYS1.PROCLIB(PASCLCC)                       00411000
//         DD  DISP=SHR,DSN=*.PASC.SYSGO                                00412000
//SYSPRINT DD  SYSOUT=A                                                 00413000
//SYSUT1   DD  UNIT=SYSDA,SPACE=(TRK,(5,5))                             00414000
//GO     EXEC  PGM=*.LKED.SYSLMOD,COND=(0,NE,LKED)                      00415000
//LOCAL    DD  DISP=OLD,UNIT=SYSDA,VOL=SER=VOLSER          +VOLSER+     00416000
//SYSPRINT DD  SYSOUT=A                                                 00417000
//*        THE FOLLOWING CARD IS REQUIRED IF FORTRAN ROUTINES WITH      00418000
//*        STANDARD OUTPUT ARE CALLED                                   00419000
//FT03F001 DD  SYSOUT=A                                                 00420000
//       PEND  (* DO NOT ADD PEND CARD TO PROCLIB *)                    00421000
