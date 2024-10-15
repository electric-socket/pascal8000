//IEBGENER JOB (001),'PUNCH/PRINT FILE #1 ',                            00000100
//             CLASS=A,MSGCLASS=X                                       00000200
//********************************************************************* 00000300
//* PRINT AND PUNCH FILE #1 FROM PASCAL 8000 TAPE. THE OUTPUT IS THE    00000400
//* JOBSTREAM NEEDED TO RESTORE FILES #2 THROUGH #8. YOU WILL NEED TO   00000500
//* EXAMINE THE JCL CHANGING STATEMENTS NOTED WITH +DSN+ AND +VOLSER+   00000600
//* TO TAILOR THE JCL FOR YOUR SYSTEM.                                  00000700
//********************************************************************* 00000800
//REPRO001 EXEC PGM=IEBGENER,REGION=512K                                00000900
//SYSIN    DD  DUMMY                                                    00001000
//SYSPRINT DD  DUMMY                                                    00001100
//SYSUT1   DD  UNIT=TAPE,DISP=(OLD,PASS),VOL=SER=NONONO,DSN=NONE,       00001200
//             LABEL=(1,NL),DCB=(RECFM=FB,LRECL=80,BLKSIZE=2000)        00001300
//SYSUT2   DD  SYSOUT=*                                                 00001400
//REPRO002 EXEC PGM=IEBGENER,REGION=512K                                00001500
//SYSIN    DD  DUMMY                                                    00001600
//SYSPRINT DD  DUMMY                                                    00001700
//SYSUT1   DD  UNIT=TAPE,DISP=OLD,VOL=SER=NONONO,DSN=NONE,              00001800
//             LABEL=(1,NL),DCB=(RECFM=FB,LRECL=80,BLKSIZE=2000)        00001900
//SYSUT2   DD  SYSOUT=B                                                 00002000
//                                                                      00002100
