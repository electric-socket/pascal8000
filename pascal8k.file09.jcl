//IEBGENER JOB (001),'PRINT FILE #9       ',                            00000100
//             CLASS=A,MSGCLASS=X                                       00000200
//********************************************************************* 00000300
//* PRINT PASCAL SOURCE FOR EDITOR PROGRAM.                             00000400
//********************************************************************* 00000500
//REPRO001 EXEC PGM=IEBGENER,REGION=512K                                00000600
//SYSIN    DD  DUMMY                                                    00000700
//SYSPRINT DD  SYSOUT=*                                                 00000800
//SYSUT1   DD  UNIT=TAPE,DISP=(OLD,PASS),VOL=SER=NONONO,DSN=NONE,       00000900
//             LABEL=(9,NL),DCB=(RECFM=VB,LRECL=80,BLKSIZE=2314)        00001000
//SYSUT2   DD  SYSOUT=*                                                 00001100
//                                                                      00001200
