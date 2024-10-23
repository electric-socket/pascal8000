// Modified version of thr AAEC compiler to compile under Free Pascal
(*0001*)(***********************************************************************
  0002  *                                                                      *
  0003  *                                                                      *
  0004  *                                                                      *
  0005  *                                                                      *
  0006  *                                                                      *
  0007  *                   PASCAL 8000 - IBM 360/370 VERSION                  *
  0008  *                   ---------------------------------                  *
  0009  *                                                                      *
  0010  *                          LINKAGE EDITOR VERSION                      *        
  0011  *                          ----------------------                      *        
  0012  *                                                                      *
  0013  *                                                                      *
  0014  *        VERSION                       1.2                             *        
  0015  *        -------                                                       *
  0016  *                                                                      *
  0017  *        DATE:                         FEBRUARY 1,1978                 *        
  0018  *        ----                                                          *
  0019  *                                                                      *
  0020  *        ORIGINAL AUTHORS:             TERUO HIKITA                    *
  0021  *        (HITAC VERSION)               KIYOSHI ISHIHATA                *
  0022  *        ---------------               (UNIVERSITY OF TOKYO)           *
  0023  *                                                                      *
  0024  *        CURRENT AUTHORS:              JEFFREY TOBIAS                  *
  0025  *        (IBM VERSION)                 GORDON COX                      *
  0026  *        -------------                 (AUSTRALIAN ATOMIC ENERGY COMM.)*
  0027  *                                                                      *
  0028  *                                                                      *
  0029  *                                                                      *
  0030  *                                                                      *
  0031  *        IMPLEMENTATION LANGUAGE:      PASCAL 8000/370                 *
  0032  *        -----------------------                                       *
  0033  *                                                                      *
  0034  *                                                                      *
  0035  *                                                                      *
  0036  *                                                                      *
  0037  *        DESCRIPTION:                                                  *
  0038  *        -----------                                                   *
  0039  *           THIS IS A PASCAL 8000 COMPILER DESIGNED TO EXECUTE ON      *
  0040  *           AN IBM SERIES 360 OR 370 COMPUTER (OR EQUIVALENT)          *
  0041  *           UNDER THE OPERATING SYSTEMS OS/MFT,OS/MVT,SVS,VS1,VS2      *
  0042  *           AND MVS.                                                   *
  0043  *                                                                      *
  0044  *           THE CODE PRODUCED BY THIS VERSION OF THE COMPILER CAN      *
  0045  *           BE PASSED THROUGH THE STANDARD IBM LINKEAGE EDITOR.        *
  0046  *                                                                      *
  0047  *                                                                      *
  0048  *        DATASETS:             "INPUT" - PROGRAM TO COMPILE            *
  0049  *        --------             "OUTPUT" - PROGRAM LISTING               *
  0050  *                           "$PASMSGS" - ERROR MESSAGE DATASET         *
  0051  *                              "SYSGO" - CODE FILE PRODUCED            *
  0052  *                                                                      *
  0053  ************************************************************************)
// $TITLE PASCAL 8000 COMPILER
// (*0054*)     PROGRAM COMPILER(INPUT,OUTPUT,$PASMSGS,SYSGO);
(*0054*)     PROGRAM AAEC_COMPILER(INPUT,OUTPUT,_PASMSGS,SYSGO);
(*0055*)(* $L+,P-,T-,N-,U-*)
uses sysutils; // time and date
(*0056*) 
(*0057*) 
(*0058*)LABEL 9999;
(*0059*)CONST
(*0060*)     DISPLIMIT = 20;       (*MAX NUMBER OF NESTED SCOPES OF IDENTIFIERS*)
(*0061*)     MAXLEVEL  = 6;        (*MAX NUMBER OF NESTED PROC/FUNCT*)
(*0062*)     MAXCHCNT = 121; (* MAX NO OF CHARS ON AN INPUT LINE + 1 *)
(*0063*) 
(*0064*) 
(*0065*)     RESWORDS  = 38;       (*NUMBER OF RESERVED WORDS*)
(*0066*)     NRSTARITH = 6; (* NUMBER OF ARITH FUNCTIONS *)                             
(*0067*)     NRSTDPROC = 18; NRSTDFUNC = 19;
(*0068*)                           (*NO OF STANDARD PROC,FUNC*)
(*0069*)     NRSTDNAMES = 37; (* = PROCS + FUNCS *)
(*0070*)     ALFALENG  = 8;        (*NO OF SIGNIFICANT CHAR IN AN IDENTIFIER*)
(*0071*) 
(*0072*) 
(*0073*)     ORDCHARMAX= 255;       (*ORDINAL NUMBER OF THE LAST CHARACTER*)
(*0074*)     SETMIN    = 0;  SETMAX    = 63; (*SMALLEST AND LARGEST ELEMENT OF A SET *)
(*0075*)     STRGFRL   = 4;        (*NUMBER OF CHARACTERS IN A STRING FRAGMENT*)
(*0076*)     NILVAL    = 0;        (* = ORD(NIL) *)
(*0077*)     MXINT     = 2147483647;     (*LARGEST INTEGER VALUE*)
(*0078*)     MAX10     = 214748364;      (* = MXINT DIV 10 *)
(*0079*)     LCSTART   = 1584;     (*INITIAL VALUE OF LOCATION COUNTER IN MAIN PROGR*)
(*0080*)     MAXPROCFUNC=256;
(*0081*)     MAXPR1 = 257 ;     (* = MAXPROCFUNC + 1 *)
(*0082*)     CIXMAX    = 256;     (*MAXIMUM NUMBER OF CASE LABEL*)
(*0083*)     TEXTSIZE = 20; (*   NEW RUN TIME SYSTEM FILE STRUCTURE *)
(*0084*) 
(*0085*)     LINESPERPAGE = 60;
(*0086*)     INDENT = 14;                                                               
(*0087*)     MAXMSGSDIV64 = 7;
(*0088*)     VERSION = 'AAEC (1ST FEB 78) ';                                            
(*0089*)     CODEBLCK= 63;            (* 1024 DIV 16 -1          *)
(*0090*)     NCODESEGS=96;            (* NUMBER OF CODE SEGMENTS *)
(*0091*)     CODEPERSEG = 256;        (* BYTES PER SEGMENT       *)
(*0092*)     PTROUTBLCK = 144;        (* POINTER TO OUTPUT BLOCK *)
(*0093*)     OBJLENGTH = 14;
(*0094*)     OBJLENPL1 = 15;
(*0095*)     ALLSPACES = 1077952576;   (* = INTEGER('    ') *)
(*0096*)     RELOC2    = 65536;
(*0097*)     RELOC1    = 16777216;     (* 256 * 65536 *)
(*0098*)     BYTE1SPACE= 1073741824;    (* 256 * 65536 * 56(X'40) *)
(*0099*)     Z7FE =  134086656;  (* X'07FE *)
(*0100*)     BYTE2SPACE= 4194304;      (* 65536 * 56(X'40)       *)
(*0101*)     SD = 0;
(*0102*)     ER = 2;
(*0103*) 
(*0104*)
// moved from VALUES section
// PACKED ARRAY(.0..255,1..4.) OF CHAR
          MNEMONIC: ARRAY(.0..255,1..4.) OF CHAR=
(*0470*)    ('    ', '    ', '    ', 'TRSK', 'SPM ', 'BALR', 'BCTR', 'BCR ',
(*0471*)      'SSK ', 'ISK ', 'SVC ', 'SKC ', '    ', 'BASR', 'SCFR', 'ICFR',
(*0472*)      'LPR ', 'LNR ', 'LTR ', 'LCR ', 'NR  ', 'CLR ', 'OR  ', 'XR  ',
(*0473*)      'LR  ', 'CR  ', 'AR  ', 'SR  ', 'MR  ', 'DR  ', 'ALR ', 'SLR ',
(*0474*)      'LPDR', 'LNDR', 'LTDR', 'LCDR', 'HDR ', 'LRDR', 'MXR ', 'MXDR',
(*0475*)      'LDR ', 'CDR ', 'ADR ', 'SDR ', 'MDR ', 'DDR ', 'AWR ', 'SWR ',
(*0476*)      'LPER', 'LNER', 'LTER', 'LCER', 'HER ', 'LRER', 'AXR ', 'SXR ',
(*0477*)      'LER ', 'CER ', 'AER ', 'SER ', 'MER ', 'DER ', 'AUR ', 'SUR ',
(*0478*)      'STH ', 'LA  ', 'STC ', 'IC  ', 'EX  ', 'BAL ', 'BCT ', 'BC  ',
(*0479*)      'LH  ', 'CH  ', 'AH  ', 'SH  ', 'MH  ', 'BAS ', 'CVD ', 'CVB ',
(*0480*)      'ST  ', 'LAE ', 'LS  ', 'ICE ', 'N   ', 'CL  ', 'O   ', 'X   ',
(*0481*)      'L   ', 'C   ', 'A   ', 'S   ', 'M   ', 'D   ', 'AL  ', 'SL  ',
(*0482*)      'STD ', '    ', '    ', '    ', '    ', '    ', '    ', 'MXD ',
(*0483*)      'LD  ', 'CD  ', 'AD  ', 'SD  ', 'MD  ', 'DD  ', 'AW  ', 'SW  ',
(*0484*)      'STE ', '    ', '    ', '    ', '    ', '    ', '    ', '    ',
(*0485*)      'LE  ', 'CE  ', 'AE  ', 'SE  ', 'ME  ', 'DE  ', 'AU  ', 'SU  ',
(*0486*)      'IDL ', 'FGP ', 'PC  ', 'DIG ', 'WRD ', 'RDD ', 'BXH ', 'BXLE',
(*0487*)      'SRL ', 'SLL ', 'SRA ', 'SLA ', 'SRDL', 'SLDL', 'SRDA', 'SLDA',
(*0488*)      'STM ', 'TM  ', 'MVI ', 'TS  ', 'NI  ', 'CLI ', 'OI  ', 'XI  ',
(*0489*)      'LM  ', '    ', '    ', '    ', 'SDV ', 'TDV ', 'HDV ', 'CKC ',
(*0490*)      'STMA', 'SKB ', 'PCAS', 'GSK ', '    ', '    ', '    ', '    ',
(*0491*)      'LMA ', 'RTN ', 'TRC ', '    ', '    ', '    ', '    ', '    ',
(*0492*)      'STMC', 'LRA ', '    ', '    ', '    ', '    ', '    ', '    ',
(*0493*)      'LMC ', 'FSK ', '    ', '    ', '    ', '    ', '    ', '    ',
(*0494*)      '    ', '    ', '    ', '    ', '    ', '    ', '    ', '    ',
(*0495*)      '    ', '    ', '    ', '    ', '    ', '    ', '    ', '    ',
(*0496*)      '    ', 'MVN ', 'MVC ', 'MVZ ', 'NC  ', 'CLC ', 'OC  ', 'XC  ',
(*0497*)      '    ', '    ', '    ', '    ', 'TR  ', 'TRT ', 'ED  ', 'EDMK',
(*0498*)      '    ', '    ', '    ', '    ', '    ', '    ', '    ', '    ',
(*0499*)      '    ', '    ', '    ', '    ', '    ', '    ', '    ', '    ',
(*0500*)      '    ', 'MVO ', 'PACK', 'UNPK', '    ', '    ', '    ', '    ',
(*0501*)      'ZAP ', 'CP  ', 'AP  ', 'SP  ', 'MP  ', 'DP  ', '    ', '    ');
(*0502*)  LRW: ARRAY (.0..ALFALENG.) OF 0..RESWORDS=( 0, 0, 6, 14, 22,
                                                    29, 34, 35, 38 );
 TYPE
(*0107*)    ALFA = PACKED ARRAY(.1..ALFALENG.) OF CHAR;

CONST
          RW:  ARRAY (.1..RESWORDS.) OF ALFA=
(*0504*)       ('IF      ', 'DO      ', 'OF      ', 'TO      ', 'IN      ', 'OR      ',
(*0505*)        'END     ', 'FOR     ', 'VAR     ', 'DIV     ', 'MOD     ', 'SET     ',
(*0506*)        'AND     ', 'NOT     ', 'THEN    ', 'ELSE    ', 'WITH    ', 'GOTO    ',
(*0507*)        'CASE    ', 'TYPE    ', 'FILE    ', 'LOOP    ', 'BEGIN   ', 'UNTIL   ',
(*0508*)        'WHILE   ', 'ARRAY   ', 'CONST   ', 'LABEL   ', 'VALUE   ', 'REPEAT  ',
(*0509*)        'RECORD  ', 'DOWNTO  ', 'PACKED  ', 'FORALL  ', 'PROGRAM ', 'FUNCTION',
(*0510*)        'POSTLUDE', 'PROCEDUR' );
TYPE
  (*0175*)
  (*0176*)                       (* CODE GENERATION STRUCTURES *)
  (*0177*)                       (******************************)
  (*0178*)
  (*0179*)
  (*0180*)
  (*0181*)TXTBUF = RECORD
  (*0182*)           PRELUDE : PACKED ARRAY (.1..4.) OF CHAR;
  (*0183*)           ADDRESS : INTEGER;
  (*0184*)           LENGTH  : INTEGER;
  (*0185*)               ID  : INTEGER;
  (*0186*)          TEXTDATA : ARRAY (.1..OBJLENGTH.) OF INTEGER;
  (*0187*)           SEQNOS  : ALFA
  (*0188*)        END;
  (*0189*)
  (*0190*)
  (*0191*)
  (*0192*)ENDBUF = RECORD
  (*0193*)           PRELUDE : PACKED ARRAY (.1..28.) OF CHAR;
  (*0194*)           LENGTH  : INTEGER;
  (*0195*)           PSTLUDE : PACKED ARRAY (.1..48.) OF CHAR
  (*0196*)        END;
  (*0197*)
  (*0198*)
  (*0199*)
  (*0200*)ESDDATA = RECORD
  (*0201*)            NAME : ALFA;
  (*0202*)            ADDRESS:INTEGER;
  (*0203*)            LENGTH : INTEGER
  (*0204*)         END;
  (*0205*)
  (*0206*)
  (*0207*)
  (*0208*)ESDBUF = RECORD
  (*0209*)           PRELUDE : ALFA;
  (*0210*)           BYTES   : INTEGER;
  (*0211*)           ID      : INTEGER;
  (*0212*)           DATAITEM:ARRAY (.1..3.) OF ESDDATA;
  (*0213*)           FILLER : ALFA;
  (*0214*)           SEQNOS  : ALFA
  (*0215*)        END;
  (*0216*)
  (*0217*)
  (*0218*)
  (*0219*)
  (*0220*)
  (*0221*)RLDDATA = RECORD
  (*0222*)            RELPOS : INTEGER;
  (*0223*)            FLAGADDRESS : INTEGER
  (*0224*)          END;
  (*0225*)
  (*0226*)
  (*0227*)RLDBUF = RECORD
  (*0228*)           PRELUDE : ALFA;
  (*0229*)           BYTES   : INTEGER;
  (*0230*)           DUMMY   : PACKED ARRAY (. 1..4 .) OF CHAR;
  (*0231*)           RLDITEMS: ARRAY (. 1..7 .) OF RLDDATA;
  (*0232*)           SEQNOS  : ALFA
  (*0233*)         END;
  (*0234*)

// CONST
// Can't preload these buffers, the IBM is a big endian EBCDIC machine
// [initialize in code] no, just postfix before writing
//
VAR
    //ESD : ESDBUF;

      
    RLD : RLDBUF=       // MAIN RLD BUFFER
          (prelude:' RLD    '; bytes:0; dummy: '    ';
              rlditems:((relpos:0; flagaddress: 0),
                       (relpos:0; flagaddress: 0),
                       (relpos:0; flagaddress: 0),
                       (relpos:0; flagaddress: 0),
                       (relpos:0; flagaddress: 0),
                       (relpos:0; flagaddress: 0),
                       (relpos:0; flagaddress: 0));
              seqnos:'        ');

(*0402*)  ESD : ESDBUF =       // MAIN ESD BUFFER
               (prelude:' ESD    '; bytes:16; id: 1;
               dataitem:((name:'P@MAIN@V'; address:0; length:0),
                         (name:'        '; address:0; length:0),
                         (name:'        '; address:0; length:0));
                       filler: '        '; seqnos:'        ' );



   TXT : TXTBUF=       // MAIN TEXT BUFFER
         (prelude: ' TXT'; address:0; length:0; id:0;
           textdata:(0, 0, 0, 0, 0, 0, 0,
                     0, 0, 0, 0, 0, 0, 0);
                   seqnos:'        ' );

(*0538  TXT := (#' TXT',0,0,0,   0,0,0,0,
                          0,0,0,0,
                         0,0,0,0,
                          0,0, '        ' #);
 *)



  ENDC: ENDBUF=     //    MAIN END BUFFER
             (prelude:' END                        '; length: 0;
              pstlude:'                                                ' );



 PROCADDRESS : ARRAY (.1..MAXPR1.) OF INTEGER=
(*0550*)               (  0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
          (*0551*)        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
          (*0552*)        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
          (*0553*)        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
          (*0554*)        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
          (*0555*)        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
          (*0556*)        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
          (*0557*)        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
          (*0558*)        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
          (*0559*)        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
          (*0560*)        1,1,1,1,1,1,1   );
          (*0561*)

TYPE
  (*0114*)   SYMBOL = (IDENT,INTCONST,REALCONST,CHARCONST,STRINGCONST,NOTSY,MULOP,
  (*0115*)        ADDOP,RELOP,LPARENT,RPARENT,LBRACK,RBRACK,LCBRACK,RCBRACK,COMMA,SEMICOLON,
  (*0116*)        PERIOD,ARROW,COLON,BECOMES,LABELSY,CONSTSY,TYPESY,VARSY,VALUESY,
  (*0117*)        FUNCTSY,PROCSY,SETSY,PACKEDSY,ARRAYSY,RECORDSY,FILESY,
  (*0118*)        BEGINSY,IFSY,CASESY,REPEATSY,WHILESY,LOOPSY,FORSY,FORALLSY,WITHSY,
  (*0119*)        GOTOSY,ENDSY,ELSESY,POSTSY,UNTILSY,OFSY,DOSY,TOSY,DOWNTOSY,
  (*0120*)        THENSY,PROGRAMSY,EXPONOP,OTHERSY);

CONST
 (*0511*) RSY: ARRAY (.1..RESWORDS.) OF SYMBOL =
                ( IFSY,     DOSY,     OFSY,     TOSY,     RELOP,    ADDOP,
 (*0512*)         ENDSY,    FORSY,    VARSY,    MULOP,    MULOP,    SETSY,
 (*0513*)         MULOP,    NOTSY,    THENSY,   ELSESY,   WITHSY,   GOTOSY,
 (*0514*)         CASESY,   TYPESY,   FILESY,   LOOPSY,   BEGINSY,  UNTILSY,
 (*0515*)         WHILESY,  ARRAYSY,  CONSTSY,  LABELSY,  VALUESY,  REPEATSY,
 (*0516*)         RECORDSY, DOWNTOSY, PACKEDSY, FORALLSY, PROGRAMSY,FUNCTSY,
 (*0517*)         POSTSY,   PROCSY );
 (*0454*)  NA: ARRAY (.1..NRSTDNAMES.) OF ALFA=
 (*0518*)       ('GET     ', 'PUT     ', 'RESET   ', 'REWRITE ',
 (*0519*)        'PAGE    ', 'READ    ', 'READLN  ', 'WRITE   ',
 (*0520*)        'WRITELN ', 'TIME    ', 'DATE    ', 'NEW     ',
 (*0521*)        'MARK    ', 'RELEASE ', 'PACK    ', 'UNPACK  ',
 (*0522*)        'MESSAGE ', 'HALT    ', 'EOF     ', 'EOLN    ',
 (*0523*)        'ODD     ', 'ROUND   ', 'TRUNC   ', 'ABS     ',
 (*0524*)        'SQR     ', 'ORD     ', 'CHR     ', 'PRED    ',
 (*0525*)        'SUCC    ', 'SIN     ', 'COS     ', 'EXP     ',
 (*0526*)        'SQRT    ', 'LN      ', 'ARCTAN  ', 'CLOCK   ',
 (*0527*)        'CARD    ');

// $TITLE  GLOBAL TYPES
(*0105*)TYPE
(*0106*) 
(*0107*) //  ALFA = PACKED ARRAY(.1..ALFALENG.) OF CHAR;
(*0108*)   LEVRANGE = 0..8; ADDRRANGE = INTEGER;
(*0109*) 
(*0110*) 
(*0111*)                              (*BASIC SYMBOLS*)
(*0112*)                              (***************)
(*0113*)

(*0121*)   &OPERATOR = (MUL,RDIV,ANDOP,IDIV,IMOD,PLUS,MINUS,OROP,LTOP,LEOP,GEOP,
(*0122*)         GTOP,NEOP,EQOP,INOP,NOOP);
(*0123*)   SETOFSYS = SET OF SYMBOL;
(*0124*) 
(*0125*)                              (*CONSTANTS*)
(*0126*)                              (***********)
(*0127*) 
(*0128*)   LOCOFREF = ^LOCREC;
(*0129*)   LOCREC=RECORD NXTREF:LOCOFREF;
(*0130*)              LOC: ADDRRANGE
(*0131*)            END;
(*0132*) 
(*0133*)   CSTCLASS = (INT,REEL,PSET,STRG);
(*0134*)   CTAILP = ^ CSTTAILREC;
(*0135*)   STRGFRAG=PACKED ARRAY(.1..4.) OF 0..255;
(*0136*)   CSTTAILREC = RECORD NXTCSP: CTAILP; STFR : INTEGER END;
(*0137*) 
(*0138*)   BASICSET=SET OF SETMIN..SETMAX;
(*0139*)   CELLUNIT=1..8;
(*0140*)   VALU=RECORD CASE CKIND:CSTCLASS OF
(*0141*)           INT:  (IVAL: INTEGER);
(*0142*)           REEL: (RVAL: REAL);
(*0143*)           PSET: (PVAL: BASICSET);
(*0144*)           STRG: (VALP: CTAILP)
(*0145*)          END;
(*0146*) 
(*0147*)                              (*DATA STRUCTURES*)
(*0148*)                              (*****************)
(*0149*)   STRUCTFORM = (SCALAR,PACKDTYPE,SUBRANGE,POINTER,POWER,ARRAYS,RECORDS,FILES,
(*0150*)          TAGFIELD,VARIANT);
(*0151*)   DECLKIND = (STANDARD,DECLARED);
(*0152*)   WBSIZE=RECORD WBLENGTH:INTEGER; BOUNDARY:CELLUNIT END;
(*0153*)   STP = ^ STRUCTREC; CTP = ^ IDENTREC;
(*0154*) 
(*0155*)   STRUCTREC=RECORD
(*0156*)          FTYPE: BOOLEAN; (* TRUE IFF THE STRUCTURE CONTAINS OR IS A FILE *)
(*0157*)          SIZE: WBSIZE;
(*0158*)          CASE FORM: STRUCTFORM OF
(*0159*)           SCALAR:   (CASE SCALKIND: DECLKIND OF
(*0160*)                      DECLARED: (FCONST: CTP));
(*0161*)           PACKDTYPE:(BASETYPE:STP);
(*0162*)           SUBRANGE: (RANGETYPE: STP; MIN,MAX: INTEGER);
(*0163*)           POINTER:  (ELTYPE: STP);
(*0164*)           POWER:    (PCKDSET: BOOLEAN; ELSET: STP);
(*0165*)           ARRAYS:   (AELTYPE,INXTYPE: STP; AELLENG:INTEGER);
(*0166*)           RECORDS:  (FIELDS,FSTFLD: CTP;
(*0167*)                      RECVAR: STP);
(*0168*)           FILES:    (TEXTFILE:BOOLEAN; FILTYPE:STP);
(*0169*)           TAGFIELD: (TGFLDP: CTP; FSTVAR: STP);
(*0170*)           VARIANT:  (FSTVARFLD: CTP; NXTVAR,SUBVAR: STP;
(*0171*)                      VARVAL: INTEGER)
(*0172*)          END;
(*0173*) 
(*0174*)

(*0235*)
(*0236*) 
(*0237*) 
(*0238*)CARD = PACKED ARRAY (. 1..80 .) OF CHAR;
(*0239*) 
(*0240*) 
(*0241*) 
(*0242*) 
(*0243*) 
(*0244*)                              (*NAMES*)
(*0245*)                              (*******)
(*0246*) 
(*0247*)   IDCLASS = (TYPES,KONST,VARS,FIELD,EVENT,PROC,FUNC);
(*0248*)   SETOFIDS = SET OF IDCLASS;
(*0249*)   IDKIND = (ACTUAL,FORMAL);
(*0250*)   DRCTINDRCT = (DRCT,INDRCT);        (*INDRCT: VARIABLE PARAMETER, WITH STATEMENT*)
(*0251*) 
(*0252*)   IDENTREC=RECORD
(*0253*)          NAME: ALFA; LLINK,RLINK: CTP;
(*0254*)          IDTYPE: STP; NEXT: CTP;
(*0255*)          CASE KLASS: IDCLASS OF
(*0256*)           KONST: (VALUES: VALU);
(*0257*)           VARS:  (VKIND: DRCTINDRCT; VLEV: LEVRANGE;
(*0258*)                   VADDR,PARADDR: ADDRRANGE);
(*0259*)           FIELD: (FLDADDR: ADDRRANGE);
(*0260*)           EVENT: (EVENTJUMP:LOCOFREF; EVENTDEF:BOOLEAN);
(*0261*)           PROC,
(*0262*)           FUNC:  (CASE PFDECKIND: DECLKIND OF
(*0263*)                   STANDARD: (KEY: 1..NRSTDNAMES);
(*0264*)                   DECLARED: (PFLEV: LEVRANGE; PARAMS:CTP;
(*0265*)                              CASE PFKIND: IDKIND OF
(*0266*)                               ACTUAL: (PFCNT:INTEGER; LCSAVE:ADDRRANGE);
(*0267*)                               FORMAL: (PFADDR:ADDRRANGE)))
(*0268*)          END;
(*0269*) 
(*0270*) 
(*0271*)   CEP=^CSTEXPREC;
(*0272*)   CSTEXPREC=RECORD ELEMTYPE:STP; ELEMVALUE:VALU;
(*0273*)                    NEXTELEM:CEP
(*0274*)             END;
(*0275*)   FILEP = ^ FILEREC;
(*0276*)   FILEREC = RECORD
(*0277*)               FILENAME: ALFA; ADDR:ADDRRANGE;
(*0278*)               NXTP: FILEP;
(*0279*)               DECLARED: BOOLEAN
(*0280*)             END;
(*0281*) 
(*0282*) 
(*0283*)   DISPRANGE = 0..DISPLIMIT;
(*0284*)   WHERE = (BLCK,REC);
(*0285*) 
(*0286*) 
(*0287*)                              (*LABELS*)
(*0288*)                              (********)
(*0289*)   LBP = ^LABREC;
(*0290*)   LABREC=RECORD
(*0291*)              LABVAL: INTEGER; NEXTLAB: LBP;
(*0292*)              LCNT: 0..MAXPROCFUNC;
(*0293*)              CASE DEFINED: BOOLEAN OF
(*0294*)               TRUE:  (LABADDR: ADDRRANGE);
(*0295*)               FALSE: (FSTOCC: LOCOFREF)
(*0296*)            END;
(*0297*)                              (*MISCELLANEOUS*)
(*0298*)                              (***************)
(*0299*)   PCRP = ^ PTRCOMPREC;     (*POINTER COMPARISON*)
(*0300*)   PTRCOMPREC = RECORD NEXT : PCRP;  (*TO AVOID INFINITE RECURSION
  0301                                         IN 'COMPTYPES'*)
(*0302*)                PTR1,PTR2 : STP
(*0303*)               END;
(*0304*)  MARKP = ^BOOLEAN;        (*MARK AND RELEASE*)
(*0305*)   REGNO=(R10,R11,R12,R13,F0,F2,F4,F6);

// copied from VALUES
CONST
(*0453*)  REALREG: ARRAY (. REGNO .) OF INTEGER =
(*0503*)            ( 10, 11, 12, 13, 0, 2, 4, 6 );

(*0306*) 
(*0307*) 
(*0308*)(*--------------------------------------------------------------------*)
(*0309*) 
// $TITLE  GLOBAL VARIABLES
(*0310*) 
(*0311*)VAR
(*0312*)                  (*RETURNED BY SOURCE PROGRAM SCANNER
  0313                     INSYMBOL:
  0314                     **********)
(*0315*) 
(*0316*)  SY: SYMBOL;                     (*LAST SYMBOL*)
(*0317*)  OP: &OPERATOR;                   (*CLASSIFICATION OF LAST SYMBOL*)
(*0318*)  IVAL: INTEGER;                  (*VALUE OF LAST INTEGER CONSTANT*)
(*0319*)  RVAL: REAL;                     (*VALUE OF LAST REAL CONSTANT*)
(*0320*)  CONSTP: CTAILP;                 (*POINTER TO LAST STRING*)
(*0321*)  LGTH: INTEGER;                  (*LENGTH OF LAST STRING CONSTANT*)
(*0322*)  ID: ALFA;                       (*LAST IDENT (POSSIBLY TRUNCATED)*)
(*0323*)  CH: CHAR;                       (*LAST CHARACTER*)
(*0324*)  SWEOL: BOOLEAN;                   (*END OF LINE CONDITION*)
(*0325*)  DOTDOT : BOOLEAN;
(*0326*)  DOTFLG : BOOLEAN;
(*0327*) 
(*0328*) 
(*0329*)                  (*COUNTERS:*)
(*0330*)                  (***********)
(*0331*) 
(*0332*)  CHCNT: INTEGER;                 (*CHARACTER COUNTER*)
(*0333*)  LC,IC: ADDRRANGE;               (*DATA LOCATION AND INSTR COUNTER*)
(*0334*)  PCNT: INTEGER;                  (*NUMBER OF PROCSY/FUNCTIONS*)
(*0335*)  PROGCOUNT:INTEGER;              (*GLOBAL INSTRUCTION COUNTER*)
(*0336*) 
(*0337*) 
(*0338*)                  (*SWITCHES:*)
(*0339*)                  (***********)
(*0340*) 
(*0341*)  PRTERR: BOOLEAN;                (*TO ALLOW FORWARD REFERENCES
  0342                                    BY SUPPRESSING ERROR MESSAGE*)
(*0343*)  DEBUG,LISTON,PMD,PRINTCODE,EXTWARN : BOOLEAN; (* $ SWITCHES *)
(*0344*) 
(*0345*) 
(*0346*)                  (*POINTERS:*)
(*0347*)                  (***********)
(*0348*)  INTPTR,REALPTR,CHARPTR,ALFAPTR,
(*0349*)  BOOLPTR,NILPTR,TEXTPTR: STP;    (*POINTERS TO ENTRIES OF STD IDS*)
(*0350*)  PACKDINTPTR,PACKDCHARPTR:STP;
(*0351*)  UTYPPTR,UCSTPTR,UVARPTR,
(*0352*)  UFLDPTR,UPRCPTR,UFCTPTR,        (*POINTERS TO ENTRIES FOR UNDECL IDS*)
(*0353*)  UEVENTPTR,
(*0354*)  INPUTPTR,OUTPUTPTR,             (*ENTRIES FOR INPUT AND OUTPUT*)
(*0355*)  FWPTR: CTP;                     (*HEAD OF CHAIN OF FORW  TYPE IDS*)
(*0356*)  FSTLABP : LBP;                  (*HEAD OF LABEL CHAIN*)
(*0357*)  FEXFILP,LOCFILP: FILEP;         (*HEAD OF LIST OF EXTERNAL/LOCAL FILES*)
(*0358*)  FSTPCRP : PCRP;                 (*HEAD OF LIST OF POINTER COMPARISON*)
(*0359*) 
(*0360*) 
(*0361*)                  (*BOOKKEEPING OF DECLARATION LEVELS:*)
(*0362*)                  (************************************)
(*0363*) 
(*0364*)  LEVEL: LEVRANGE;                (*CURRENT STATIC LEVEL*)
(*0365*)  DISX,                           (*LEVEL OF LAST ID SRCHD BY SEARCHID*)
(*0366*)  TOP: DISPRANGE;                 (*TOP OF DISPLAY*)
(*0367*) 
(*0368*)  DISPLAY:                        (*WHERE:   MEANS:*)
(*0369*)   ARRAY (.DISPRANGE.) OF
(*0370*)           RECORD               (*=BLCK:   ID IS VARIABLE ID*)
(*0371*)     FNAME: CTP;               (*=REC:   ID IS FIELD ID IN RECORD*)
(*0372*)     CASE OCCUR: WHERE OF
(*0373*)       REC: (DADRS:ADDRRANGE;
(*0374*)             CASE DISPKIND:DRCTINDRCT OF
(*0375*)                   DRCT: (DLEVEL:LEVRANGE);
(*0376*)                  INDRCT: (DBASEL:LEVRANGE; DBASEA:ADDRRANGE))
(*0377*)     END;
(*0378*) 
(*0379*)         (* LISTING CONTROLS *)
(*0380*)         (********************)
(*0381*) 
(*0382*)LEFT,RIGHT,PROCLEV : CHAR;             (*NESTING LEVEL INDICATORS *)
(*0383*)LOCATION           : INTEGER;          (*OFFSET AT EOL            *)
(*0384*)DDATE,TTIME        : ALFA;             (*DATE AND TIME FOR NEWPAGE*)
(*0385*)PAGEE              : INTEGER;          (*PAGE COUNTER             *)
(*0386*)ZLEV               : INTEGER;          (*NESTING LEVEL COUNTER    *)
(*0387*)TTL                : PACKED ARRAY(.1..40.)
(*0388*)                        OF CHAR;       (*TITLE BUFFER             *)
(*0389*)PRINTED            : INTEGER;          (*INTEGER                  *)
(*0390*)MAXLINE            : 0..MAXCHCNT;      (* MAX NO OF INPUT CHARACTERS *)
(*0391*)LINEE              : INTEGER;           (* NO OF LINES PRINTED      *)
(*0392*)DP                 : BOOLEAN;
(*0393*)MAXLN              : BOOLEAN;
(*0394*) 
(*0395*) 
(*0396*) 
(*0397*)                    (* OBJECT FILE GENERATION *)
(*0398*)                    (**************************)
(*0399*) 
(*0400*) 

(*0405*)
(*0406*)ESDID : 1..256;     (* ESD IDENTIFIER *)
(*0407*)ESDCNT:0..256;
(*0408*)RLDPOS:INTEGER;
(*0409*)CURRADDRESS : INTEGER;    (* OFFSET FROM START OF TXT BLOCK FOR SD *)
(*0410*)OBJECTCODE : ARRAY (.1..OBJLENGTH.) OF INTEGER;  (* CODE STORE *)
(*0411*)EXTPROCS : 0..MAXPROCFUNC;                                                      
(*0412*)EXTARRAY : PACKED ARRAY (.0..MAXPROCFUNC.) OF                                   
(*0413*)              RECORD                                                            
(*0414*)                ENAME:ALFA;                                                     
(*0415*)                ECNT : 0..MAXPROCFUNC                                           
(*0416*)              END;                                                              
(*0417*)EXTRNL : BOOLEAN;  (* SET FOR EXTERNAL COMPILATIONS *)
(*0418*)PROCNAMES : BOOLEAN;    (* FOR PROC NAME INFORMATION *)
(*0419*)PROCREF : ALFA;    (* STORES LAST PROC COMPILED *)
(*0420*)INITFLAG : BOOLEAN;
(*0421*)STDPRCS : ARRAY(.1..NRSTARITH.) OF ALFA; (* FOR ARITH FUNCTS *)                 
(*0422*) 
(*0423*) 
(*0424*)                  (*ERROR MESSAGES:*)
(*0425*)                  (*****************)
(*0426*) 
(*0427*)  ERRINX: 0..10;                  (*NR OF ERRORS IN CURR SOURCE LINE*)
(*0428*)  ERRORS: BOOLEAN;                (*TRUE IFF THE PROGRAM CONTAINS AN ERROR*)
(*0429*)  ERRLIST:
(*0430*)   ARRAY (.1..10.) OF
(*0431*)     RECORD POS: 1..MAXCHCNT;
(*0432*)            NMR: 1..400
(*0433*)        END;
(*0434*) 
(*0435*)ERRORTOT : INTEGER;     (* TOTAL NUMBER OF LINES IN ERROR *)
(*0436*) _PASMSGS : TEXT;        (* PASCAL ERROR MESSAGE FILE      *)
(*0437*)ERRMSGS  : ARRAY (. 0 .. MAXMSGSDIV64 .)
(*0438*)                   OF SET OF SETMIN..SETMAX;
(*0439*) 
(*0440*) 
(*0441*) 
(*0442*)                  (*STRUCTURED CONSTANTS:*)
(*0443*)                  (***********************)
(*0444*) 
(*0445*)  CONSTBEGSYS,SIMPTYPEBEGSYS,TYPEBEGSYS,BLOCKBEGSYS,SELECTSYS,FACBEGSYS,
(*0446*)  STATBEGSYS,TYPEDELS: SETOFSYS;
(*0447*) // LRW: ARRAY (.0..ALFALENG.) OF 0..RESWORDS;
(*0448*) // RW:  ARRAY (.1..RESWORDS.) OF ALFA;
(*0449*) // RSY: ARRAY (.1..RESWORDS.) OF SYMBOL;
(*0450*)  ROP: ARRAY (.1..RESWORDS.) OF &OPERATOR;
(*0451*)  SSY : ARRAY (.' '..'A'.) OF SYMBOL;                                           
(*0452*)  SOP: ARRAY (.' '..'"'.) OF &OPERATOR;
(*0453*)//  REALREG: ARRAY (. REGNO .) OF INTEGER;
(*0454*)//  NA: ARRAY (.1..NRSTDNAMES.) OF ALFA;
(*0455*)  BMASK: ARRAY(.LTOP..EQOP.) OF INTEGER;
(*0456*)  DUALOP: ARRAY(.LTOP..EQOP.) OF LTOP..EQOP;
(*0457*) // MNEMONIC: PACKED ARRAY(.0..255,1..4.) OF CHAR;
(*0458*)  CHTYPE: PACKED ARRAY(.CHAR.) OF (SPCHAR,LETTER,DIGIT);
(*0459*)                    (*OUTPUT BUFFER:*)
(*0460*)                    (****************)
(*0461*)   LINE  : PACKED ARRAY(.1..MAXCHCNT.) OF CHAR;
(*0462*) 
(*0464*)   SYSGO : FILE OF CARD;     (* OUTPUT FILE *)
(*0465*)   INITNUMBER,OBPOINTER: INTEGER;
(*0466*) 
(*0467*) 
// $TITLE  GLOBAL VALUES

(*0468  VALUE
 0469  MNEMONIC:=
 0470    (#'    ', '    ', '    ', 'TRSK', 'SPM ', 'BALR', 'BCTR', 'BCR ',
0471      'SSK ', 'ISK ', 'SVC ', 'SKC ', '    ', 'BASR', 'SCFR', 'ICFR',
0472      'LPR ', 'LNR ', 'LTR ', 'LCR ', 'NR  ', 'CLR ', 'OR  ', 'XR  ',
0473      'LR  ', 'CR  ', 'AR  ', 'SR  ', 'MR  ', 'DR  ', 'ALR ', 'SLR ',
0474      'LPDR', 'LNDR', 'LTDR', 'LCDR', 'HDR ', 'LRDR', 'MXR ', 'MXDR',
0475      'LDR ', 'CDR ', 'ADR ', 'SDR ', 'MDR ', 'DDR ', 'AWR ', 'SWR ',
0476      'LPER', 'LNER', 'LTER', 'LCER', 'HER ', 'LRER', 'AXR ', 'SXR ',
0477      'LER ', 'CER ', 'AER ', 'SER ', 'MER ', 'DER ', 'AUR ', 'SUR ',
0478      'STH ', 'LA  ', 'STC ', 'IC  ', 'EX  ', 'BAL ', 'BCT ', 'BC  ',
0479      'LH  ', 'CH  ', 'AH  ', 'SH  ', 'MH  ', 'BAS ', 'CVD ', 'CVB ',
0480      'ST  ', 'LAE ', 'LS  ', 'ICE ', 'N   ', 'CL  ', 'O   ', 'X   ',
0481      'L   ', 'C   ', 'A   ', 'S   ', 'M   ', 'D   ', 'AL  ', 'SL  ',
0482      'STD ', '    ', '    ', '    ', '    ', '    ', '    ', 'MXD ',
0483      'LD  ', 'CD  ', 'AD  ', 'SD  ', 'MD  ', 'DD  ', 'AW  ', 'SW  ',
0484      'STE ', '    ', '    ', '    ', '    ', '    ', '    ', '    ',
0485      'LE  ', 'CE  ', 'AE  ', 'SE  ', 'ME  ', 'DE  ', 'AU  ', 'SU  ',
0486      'IDL ', 'FGP ', 'PC  ', 'DIG ', 'WRD ', 'RDD ', 'BXH ', 'BXLE',
0487      'SRL ', 'SLL ', 'SRA ', 'SLA ', 'SRDL', 'SLDL', 'SRDA', 'SLDA',
0488      'STM ', 'TM  ', 'MVI ', 'TS  ', 'NI  ', 'CLI ', 'OI  ', 'XI  ',
0489      'LM  ', '    ', '    ', '    ', 'SDV ', 'TDV ', 'HDV ', 'CKC ',
0490      'STMA', 'SKB ', 'PCAS', 'GSK ', '    ', '    ', '    ', '    ',
0491      'LMA ', 'RTN ', 'TRC ', '    ', '    ', '    ', '    ', '    ',
0492      'STMC', 'LRA ', '    ', '    ', '    ', '    ', '    ', '    ',
0493      'LMC ', 'FSK ', '    ', '    ', '    ', '    ', '    ', '    ',
0494      '    ', '    ', '    ', '    ', '    ', '    ', '    ', '    ',
0495      '    ', '    ', '    ', '    ', '    ', '    ', '    ', '    ',
0496      '    ', 'MVN ', 'MVC ', 'MVZ ', 'NC  ', 'CLC ', 'OC  ', 'XC  ',
0497      '    ', '    ', '    ', '    ', 'TR  ', 'TRT ', 'ED  ', 'EDMK',
0498      '    ', '    ', '    ', '    ', '    ', '    ', '    ', '    ',
0499      '    ', '    ', '    ', '    ', '    ', '    ', '    ', '    ',
0500      '    ', 'MVO ', 'PACK', 'UNPK', '    ', '    ', '    ', '    ',
0501      'ZAP ', 'CP  ', 'AP  ', 'SP  ', 'MP  ', 'DP  ', '    ', '    '#); *)

(*0502*)//  LRW:=(# 0, 0, 6, 14, 22, 29, 34, 35, 38 #);
(*0503*)//  REALREG:=(# 10, 11, 12, 13, 0, 2, 4, 6 #);
{
(*0504*)  RW:=(#'IF      ', 'DO      ', 'OF      ', 'TO      ', 'IN      ', 'OR      ',
(*0505*)        'END     ', 'FOR     ', 'VAR     ', 'DIV     ', 'MOD     ', 'SET     ',
(*0506*)        'AND     ', 'NOT     ', 'THEN    ', 'ELSE    ', 'WITH    ', 'GOTO    ',
(*0507*)        'CASE    ', 'TYPE    ', 'FILE    ', 'LOOP    ', 'BEGIN   ', 'UNTIL   ',
(*0508*)        'WHILE   ', 'ARRAY   ', 'CONST   ', 'LABEL   ', 'VALUE   ', 'REPEAT  ',
(*0509*)        'RECORD  ', 'DOWNTO  ', 'PACKED  ', 'FORALL  ', 'PROGRAM ', 'FUNCTION',
(*0510*)        'POSTLUDE', 'PROCEDUR' #);
(*0511*)  RSY:=(#IFSY,     DOSY,     OFSY,     TOSY,     RELOP,    ADDOP,
(*0512*)         ENDSY,    FORSY,    VARSY,    MULOP,    MULOP,    SETSY,
(*0513*)         MULOP,    NOTSY,    THENSY,   ELSESY,   WITHSY,   GOTOSY,
(*0514*)         CASESY,   TYPESY,   FILESY,   LOOPSY,   BEGINSY,  UNTILSY,
(*0515*)         WHILESY,  ARRAYSY,  CONSTSY,  LABELSY,  VALUESY,  REPEATSY,
(*0516*)         RECORDSY, DOWNTOSY, PACKEDSY, FORALLSY, PROGRAMSY,FUNCTSY,
(*0517*)         POSTSY,   PROCSY #);
(*0518*)  NA:=(#'GET     ', 'PUT     ', 'RESET   ', 'REWRITE ',
(*0519*)        'PAGE    ', 'READ    ', 'READLN  ', 'WRITE   ',
(*0520*)        'WRITELN ', 'TIME    ', 'DATE    ', 'NEW     ',
(*0521*)        'MARK    ', 'RELEASE ', 'PACK    ', 'UNPACK  ',
(*0522*)        'MESSAGE ', 'HALT    ', 'EOF     ', 'EOLN    ',
(*0523*)        'ODD     ', 'ROUND   ', 'TRUNC   ', 'ABS     ',
(*0524*)        'SQR     ', 'ORD     ', 'CHR     ', 'PRED    ',
(*0525*)        'SUCC    ', 'SIN     ', 'COS     ', 'EXP     ',
(*0526*)        'SQRT    ', 'LN      ', 'ARCTAN  ', 'CLOCK   ',
(*0527*)        'CARD    '#);
}
(*0528*) 
(*0529*) 
(*0530*) 
(*0562*)
(*0563*)(*--------------------------------------------------------------------*)
(*0564*)
(*0565*)

procedure page(Var f:text); begin end;

// $TITLE  LEXICAL ANALYSER
(*0566*)PROCEDURE NEWPAGE;
(*0567*) VAR I,J : INTEGER;
(*0568*)BEGIN
(*0569*)  PAGE(OUTPUT);
(*0570*) WRITELN('   PASCAL 8000/1.2',                                                  
(*0571*)    VERSION :22,' ':3,TTL:40,' ':11,DDATE,' AT ',                               
(*0572*)          TTIME,'     PAGE ',PAGEE:4);
(*0573*)  WRITELN;
(*0574*)  PAGEE := PAGEE+1;
(*0575*)  LINEE := 2;
(*0576*)END; (* NEWPAGE *)
(*0577*) 
(*0578*) 
(*0579*)PROCEDURE  ENDOFLINE; FORWARD;
(*0580*) 
(*0581*) 
(*0582*) 
(*0583*)PROCEDURE RIGHTCHECK;
(*0584*)BEGIN
(*0585*)  RIGHT := CHR(ORD('0') + ZLEV MOD 10);
(*0586*)  ZLEV := ZLEV - 1
(*0587*) END;
(*0588*)PROCEDURE LEFTCHECK;
(*0589*)BEGIN
(*0590*)  ZLEV:=ZLEV+1;
(*0591*)  IF LEFT =  '-' THEN LEFT:=CHR(ORD('0')+ZLEV MOD 10)
(*0592*)END;
(*0593*) 
(*0594*) 
(*0595*)PROCEDURE WRITEHEX(X:INTEGER);
(*0596*) VAR I,N,C : INTEGER;
(*0597*)  L:INTEGER;                                                                    
(*0598*)BEGIN                                                                           
(*0599*)   IF X < 65636 THEN BEGIN L:=4;N:=4096 END ELSE                                
(*0600*)    IF X < 1050176 THEN BEGIN L := 5; N:=65636 END ELSE                         
(*0601*)     BEGIN L := 6; N := 1050176 END;                                            
(*0602*)   WRITE(' ':6-L);                                                              
(*0603*)   FOR I :=  1 TO L DO                                                          
(*0604*)    BEGIN C:=X DIV N;
(*0605*)      IF C>= 10 THEN WRITE(CHR(C-10+ORD('A')))
(*0606*)                ELSE WRITE(CHR(C+ORD('0')));
(*0607*)      X:=X MOD N; N:=N DIV 16
(*0608*)    END
(*0609*)END; (*WRITEHEX *)
(*0610*) 
(*0611*) 
(*0612*)PROCEDURE OPTCARD;
(*0613*) VAR I:INTEGER;
(*0614*) C : CHAR;
(*0615*)BEGIN (*OPTCARD*)
(*0616*)  READ(CH);
(*0617*)  IF NOT EOLN(INPUT) THEN
(*0618*)  BEGIN
(*0619*)   READ(CH);
(*0620*)   IF CH = 'E' THEN LINEE := LINESPERPAGE (*EJECT *)
(*0621*)  ELSE
(*0622*)   IF CH = 'S' THEN
(*0623*)   BEGIN IF LISTON THEN BEGIN C:=' ';WHILE NOT EOLN(INPUT) AND (C=' ') DO
(*0624*)     BEGIN READ(CH); IF (CH>='0') AND (CH<='9') THEN
(*0625*)       FOR C:='1' TO CH DO BEGIN ENDOFLINE;WRITELN END
(*0626*)     END
(*0627*)     END
(*0628*)    END
(*0629*)  ELSE
(*0630*)  IF CH = 'T' THEN
(*0631*)  BEGIN
(*0632*)    REPEAT READ(CH) UNTIL (CH = ' ') OR EOLN(INPUT);
(*0633*)    WHILE (NOT EOLN(INPUT)) AND (CH=' ') DO READ(CH);
(*0634*)    FOR I:=1 TO 40 DO BEGIN TTL(.I.):=CH;
(*0635*)    IF EOLN(INPUT) THEN CH:=' ' ELSE READ(CH);
(*0636*)     END; LINEE := LINESPERPAGE;
(*0637*)  END ELSE IF CH ='U' THEN LINEE:=LINESPERPAGE+1
(*0638*)  END; READLN
(*0639*)END; (*OPTCARD *)
(*0640*) 
(*0641*) 
(*0642*)PROCEDURE WRITERRORS;
(*0643*)   VAR I : INTEGER;
(*0644*)       LASTPOS,FREEPOS,CURRPOS,CURRNMR,F,K: INTEGER;
(*0645*)         FLAG:BOOLEAN;
(*0646*)BEGIN
(*0647*)            FLAG:=FALSE;
(*0648*)            ENDOFLINE;
(*0649*)            FOR K:=1 TO ERRINX DO
(*0650*)             IF ERRLIST(. K .).NMR <> 291 THEN FLAG:=TRUE;
(*0651*)            IF FLAG THEN WRITE(' ***ERROR***':INDENT) ELSE
(*0652*)             WRITE(' **WARNING**':INDENT);
(*0653*)             LASTPOS := 0; FREEPOS := 1;
(*0654*)             FOR K := 1 TO ERRINX DO
(*0655*)               BEGIN
(*0656*)                 WITH ERRLIST(.K.) DO
(*0657*)                   BEGIN CURRPOS := POS; CURRNMR := NMR END;
(*0658*)                 IF CURRPOS = LASTPOS THEN WRITE(',')
(*0659*)                 ELSE
(*0660*)                   BEGIN
(*0661*)                      IF FREEPOS > CURRPOS THEN
(*0662*)                      BEGIN
(*0663*)                        WRITELN;
(*0664*)                        ENDOFLINE; WRITE(' ':INDENT);
(*0665*)                        FREEPOS:=1
(*0666*)                     END;
(*0667*)                     WHILE FREEPOS < CURRPOS DO
(*0668*)                       BEGIN WRITE(' '); FREEPOS := FREEPOS + 1 END;
(*0669*)                     WRITE('^'); LASTPOS:=CURRPOS;
(*0670*)                   END;
(*0671*)                 IF CURRNMR < 10 THEN F := 1
(*0672*)                   ELSE IF CURRNMR < 100 THEN F := 2
(*0673*)                     ELSE F := 3;
(*0674*)                 WRITE(CURRNMR:F);
(*0675*)                 FREEPOS := FREEPOS + F + 1
(*0676*)               END;
(*0677*)             WRITELN; ERRINX := 0
(*0678*)   END;
(*0679*) 
(*0680*) PROCEDURE ERROR(FERRNR: INTEGER);
(*0681*)   VAR
(*0682*)      ERRCNT : INTEGER;
(*0683*)    BEGIN
(*0684*)      IF FERRNR <> 291 THEN
(*0685*)             ERRORS:=TRUE;
(*0686*)     IF FERRNR = 400 THEN
(*0687*)     BEGIN
(*0688*)       ENDOFLINE; WRITELN(' ***** COMPILER ERROR *****'); HALT
(*0689*)     END;
(*0690*)     IF ERRINX = 0 THEN ERRORTOT := ERRORTOT + 1;
(*0691*)     IF ERRINX >= 9 THEN
(*0692*)       BEGIN ERRLIST(.10.).NMR := 255; ERRINX := 10 END
(*0693*)     ELSE
(*0694*)       BEGIN ERRINX := ERRINX + 1;
(*0695*)         ERRLIST(.ERRINX.).NMR := FERRNR
(*0696*)       END;
(*0697*)      ERRCNT := ERRLIST(. ERRINX .).NMR;
(*0698*)      ERRMSGS(. ERRCNT DIV  64 .) :=
(*0699*)            ERRMSGS(. ERRCNT DIV 64 .) + (.ERRCNT MOD 64 .);
(*0700*)     ERRLIST(.ERRINX.).POS := CHCNT
(*0701*)   END;
(*0702*) 
(*0703*)    FUNCTION BYTEPACK(X:STRGFRAG):INTEGER;
(*0704*)      BEGIN BYTEPACK:=256*(256*(256*X(.1.)+X(.2.))+X(.3.))+X(.4.);
(*0705*)      END;
(*0706*) 
(*0707*)    PROCEDURE BYTEUNPACK(VAR X:STRGFRAG; V:INTEGER);
(*0708*)      VAR W: RECORD CASE FLAG:BOOLEAN OF
(*0709*)               TRUE:  (STR: STRGFRAG);
(*0710*)               FALSE: (INT: INTEGER)
(*0711*)             END;
(*0712*)      BEGIN W.INT:=V; X:=W.STR; END;
(*0713*) 
(*0714*)    PROCEDURE SETVALUE(X:BASICSET; VAR I1,I2:INTEGER);
(*0715*)      VAR W: RECORD DUMMY:INTEGER;
(*0716*)               CASE FLAG:BOOLEAN OF
(*0717*)                 FALSE: (S: BASICSET);
(*0718*)                 TRUE:  (A1,A2: INTEGER)
(*0719*)             END;
(*0720*)      BEGIN W.S:=X; I1:=W.A1; I2:=W.A2; END;
(*0721*) 
(*0722*)    PROCEDURE HALFWORD(X:INTEGER; VAR X1,X2:INTEGER);
(*0723*)      BEGIN
(*0724*)        IF X>=0 THEN
(*0725*)          BEGIN X1:=X DIV 65536;
(*0726*)                X2:=X MOD 65536;
(*0727*)          END
(*0728*)        ELSE IF X MOD 65536=0 THEN
(*0729*)          BEGIN X1:=X DIV 65536+65536;
(*0730*)                X2:=0;
(*0731*)          END
(*0732*)        ELSE
(*0733*)          BEGIN X1:=X DIV 65536+65535;
(*0734*)                X2:=X MOD 65536+65536;
(*0735*)          END;
(*0736*)      END;
(*0737*) 
(*0738*)PROCEDURE ENDOFLINE;
(*0739*) VAR I : INTEGER;
(*0740*)BEGIN
(*0741*)  IF CHCNT > PRINTED THEN
(*0742*)  BEGIN
(*0743*)    IF LISTON OR (ERRINX>0) THEN
(*0744*)    BEGIN
(*0745*)      IF LINEE = LINESPERPAGE THEN NEWPAGE;
(*0746*)      LINEE := LINEE + 1;
(*0747*)      WRITE(' ');
(*0748*)      WRITEHEX(LOCATION);
(*0749*)      WRITE(' ',LEFT,RIGHT,' ',PROCLEV,' ':PRINTED+2);
(*0750*)      FOR I:=PRINTED+1 TO CHCNT DO
(*0751*)         IF I <= MAXCHCNT THEN WRITE(LINE(.I.));
(*0752*)      WRITELN
(*0753*)   END;
(*0754*)   LEFT:='-'; RIGHT:='-';PROCLEV:=' ';PRINTED:=CHCNT;
(*0755*)   IF ERRINX>0 THEN WRITERRORS
(*0756*)  END ELSE
(*0757*)  BEGIN
(*0758*)    IF LINEE = LINESPERPAGE THEN NEWPAGE;
(*0759*)    LINEE:=LINEE+1;
(*0760*)  END;
(*0761*)  IF DP THEN LOCATION:= LC ELSE LOCATION := IC;
(*0762*)END;(*ENDOFLINE*)
(*0763*) PROCEDURE INSYMBOL;         // Scanner
(*0764*)  LABEL 1,2;
(*0765*)  (*READ NEXT BASIC SYMBOL OF SOURCE PROGRAM AND RETURN ITS DESCRIPTION
  0766    IN THE GLOBAL VARIABLES SY, OP, ID, IVAL, RVAL, SVAL AND LGTH*)
(*0767*)  CONST ONE=1E0; TEN=10E0;
(*0768*)  VAR I,K,SCALE,EXP: INTEGER;
(*0769*)      R,FAC: REAL; SIGN: BOOLEAN; SF:STRGFRAG;
(*0770*)      NXTP,TAILP:CTAILP;
(*0771*) 
(*0772*)PROCEDURE NEXTCH;
(*0773*)BEGIN
(*0774*)  REPEAT
(*0775*)  IF SWEOL THEN
(*0776*)  BEGIN
(*0777*)    ENDOFLINE;
(*0778*)    REPEAT
(*0779*)     IF EOF(INPUT) THEN
(*0780*)     BEGIN
(*0781*)       ENDOFLINE; WRITELN;
(*0782*)       ENDOFLINE; WRITELN(' *** WARNING - PREMATURE PROGRAM EOF ***');
(*0783*)         ERRORS:=TRUE;
(*0784*)        { GOTO 9999 } Halt(1);
(*0785*)     END ELSE
(*0786*)     BEGIN
// options at start of line not used
//(*0787*)       IF INPUT^ = '$' THEN OPTCARD ELSE
// (*0788*)       BEGIN

                          CHCNT:=0; PRINTED:=0; SWEOL:=FALSE;
//             END;
(*0789*)     END
(*0790*)   UNTIL NOT SWEOL
(*0791*)  END;
(*0792*)  SWEOL:=EOLN(INPUT); READ(CH); IF CHCNT=MAXCHCNT THEN ERROR(180);
(*0793*)  CHCNT:=CHCNT+1; IF CHCNT <= MAXCHCNT THEN LINE(.CHCNT.):=CH;
(*0794*)  UNTIL SWEOL OR (CHCNT<= MAXLINE);
(*0795*)END; (*NEXTCH *)
(*0796*) 
(*0797*)    PROCEDURE OPTIONS;
(*0798*)      VAR CH1:CHAR;
(*0799*) 
(*0800*)      PROCEDURE SETOPTION(VAR F:BOOLEAN; C:CHAR);
(*0801*)        BEGIN IF CH1=C THEN
(*0802*)                IF (CH='+') OR (CH='-') THEN F:=(CH='+');
(*0803*)        END;
(*0804*) 
(*0805*)      BEGIN
(*0806*)        REPEAT NEXTCH; CH1:=CH; NEXTCH;
(*0807*)          SETOPTION(PRINTCODE,'C'); SETOPTION(LISTON,'L');
(*0808*)          SETOPTION(PMD,'P'); SETOPTION(DEBUG,'T');
(*0809*)          SETOPTION(MAXLN,'U');
(*0810*)              IF EXTRNL AND (CH1='E') THEN                                      
(*0811*)                  ERROR(382) ELSE SETOPTION(EXTRNL,'E');                        
(*0812*)       SETOPTION(PROCNAMES,'N');
(*0813*)          SETOPTION(EXTWARN,'S');
(*0814*)            IF MAXLN THEN MAXLINE:=72 ELSE MAXLINE:=MAXCHCNT;
(*0815*)          NEXTCH;
(*0816*)        UNTIL CH<>',';
(*0817*)      END;
(*0818*) 
(*0819*) BEGIN (*INSYMBOL*)    // Scanner
(*0820*) 1:
(*0821*)  WHILE CH=' ' DO NEXTCH;            // skip whitespace
(*0822*)  IF CHTYPE(.CH.)=LETTER THEN        // if it's an identifier
(*0823*)    BEGIN K:=0; ID:='        ';
(*0824*)      REPEAT
(*0825*)        IF K < ALFALENG THEN
(*0826*)          BEGIN K:=K+1; ID(.K.):=CH; END;
(*0827*)        NEXTCH;
(*0828*)      UNTIL CHTYPE(.CH.)=SPCHAR;
(*0829*)      FOR I := LRW(.K-1.) + 1 TO LRW(.K.) DO
(*0830*)        IF RW(.I.) = ID THEN             // if it matches a keyword
(*0831*)          BEGIN SY := RSY(.I.); OP := ROP(.I.); GOTO 2 END;
(*0832*)      SY := IDENT; OP := NOOP;
(*0833*) 2: END
(*0834*)  ELSE IF (CH>='0') AND (CH<='9') THEN
(*0835*)    BEGIN SY := INTCONST; OP := NOOP;
(*0836*)      IVAL:=0;
(*0837*)      REPEAT
(*0838*)        IF IVAL<MAX10
(*0839*)          THEN IVAL:=IVAL*10+(ORD(CH)-ORD('0'))
(*0840*)          ELSE IF (IVAL>MAX10) OR (CH>='8')
(*0841*)                 THEN BEGIN ERROR(203); IVAL:=0; END
(*0842*)                 ELSE IVAL:=IVAL*10+(ORD(CH)-ORD('0'));
(*0843*)        NEXTCH;
(*0844*)      UNTIL (CH<'0') OR (CH>'9');
(*0845*)       SCALE := 0;
(*0846*)       IF CH = '.' THEN
(*0847*)        BEGIN NEXTCH;
(*0848*)         IF CH = '.' THEN BEGIN DOTFLG:=TRUE; CH:=':' END
(*0849*)         ELSE IF CH=')' THEN CH:='%'
(*0850*)         ELSE
(*0851*)          BEGIN RVAL := IVAL; SY := REALCONST;
(*0852*)           IF (CH<'0') OR (CH>'9') THEN ERROR(201)
(*0853*)           ELSE
(*0854*)            REPEAT RVAL := TEN*RVAL + (ORD(CH)-ORD('0'));
(*0855*)             SCALE := SCALE - 1; NEXTCH
(*0856*)            UNTIL (CH<'0') OR (CH>'9')
(*0857*)          END
(*0858*)        END;
(*0859*)       IF CH = 'E' THEN
(*0860*)         BEGIN
(*0861*)           IF SCALE = 0 THEN
(*0862*)             BEGIN RVAL := IVAL; SY := REALCONST END;
(*0863*)           SIGN := FALSE; NEXTCH;
(*0864*)           IF CH = '+' THEN NEXTCH
(*0865*)           ELSE
(*0866*)             IF CH = '-' THEN
(*0867*)               BEGIN SIGN := TRUE; NEXTCH END;
(*0868*)           EXP := 0;
(*0869*)           IF (CH<'0') OR (CH>'9') THEN ERROR(201)
(*0870*)           ELSE
(*0871*)             REPEAT EXP := 10*EXP + (ORD(CH)-ORD('0'));
(*0872*)               NEXTCH
(*0873*)             UNTIL (CH<'0') OR (CH>'9');
(*0874*)           IF SIGN THEN SCALE := SCALE - EXP
(*0875*)                   ELSE SCALE := SCALE + EXP
(*0876*)         END;
(*0877*)       IF SCALE<>0 THEN
(*0878*)         BEGIN R:=ONE; SIGN:=FALSE;
(*0879*)           IF SCALE<0 THEN BEGIN SIGN:=TRUE; SCALE:=-SCALE; END;
(*0880*)           FAC:=TEN;
(*0881*)           REPEAT IF ODD(SCALE) THEN R:=R*FAC;
(*0882*)                  FAC:=SQR(FAC); SCALE:=SCALE DIV 2;
(*0883*)           UNTIL SCALE=0;
(*0884*)           IF SIGN THEN RVAL:=RVAL/R ELSE RVAL:=RVAL*R;
(*0885*)         END;
(*0886*)    END
(*0887*)ELSE IF (ORD(CH)<=73) OR (ORD(CH)>=190)                                         
(*0888*)    THEN BEGIN OP:=NOOP; SY:=OTHERSY; NEXTCH; END
(*0889*)    ELSE CASE ORD(CH) OF
(*0890*)(*'*) 125:
(*0891*)        BEGIN OP:=NOOP; LGTH:=0; I:=0;
(*0892*)          CONSTP:=NIL; NEXTCH;
// (*0893*)          LOOP
                   while true do begin
//(*0894*)            IF SWEOL THEN BEGIN ERROR(202); EXIT; END;
                      IF SWEOL THEN
                           BEGIN
                               ERROR(202);
                               break;
                           END;

(*0895*)            IF CH='''' THEN
//(*0896*)              BEGIN NEXTCH; IF CH<>'''' THEN EXIT; END;
(*0896*)              BEGIN
                          NEXTCH;
                          IF CH<>'''' THEN break;
                      END;

(*0897*)            IF I = STRGFRL THEN
(*0898*)              BEGIN NEW(TAILP);
(*0899*)                WITH TAILP^ DO
(*0900*)                  BEGIN NXTCSP := CONSTP; STFR := BYTEPACK(SF) END;
(*0901*)                CONSTP := TAILP; I := 0;
(*0902*)              END;
(*0903*)            I := I + 1; LGTH := LGTH + 1;
(*0904*)            SF(.I.):=ORD(CH);
(*0905*)            NEXTCH
(*0906*)          END;
(*0907*)          IF LGTH = 1 THEN
(*0908*)            BEGIN SY:=CHARCONST; IVAL:=SF(.1.); END
(*0909*)          ELSE
(*0910*)            BEGIN FOR I:=I+1 TO STRGFRL DO SF(.I.):=ORD(' ');
(*0911*)              NEW(TAILP);
(*0912*)              WITH TAILP^ DO
(*0913*)                BEGIN NXTCSP := CONSTP; STFR := BYTEPACK(SF) END;
(*0914*)              (*REVERSE POINTERS:*)
(*0915*)              CONSTP := NIL;
(*0916*)              WHILE TAILP <> NIL DO
(*0917*)                WITH TAILP^ DO
(*0918*)                  BEGIN NXTP := NXTCSP; NXTCSP := CONSTP;
(*0919*)                    CONSTP := TAILP; TAILP := NXTP
(*0920*)                  END;
(*0921*)              SY:=STRINGCONST;
(*0922*)            END
(*0923*)        END;
(*0924*)(*:*) 122: BEGIN OP:=NOOP; NEXTCH;
(*0925*)                 IF CH='=' THEN BEGIN SY:=BECOMES; NEXTCH; END
(*0926*)             ELSE BEGIN SY:=COLON; IF DOTFLG THEN
(*0927*)                BEGIN DOTFLG:=FALSE; DOTDOT:=TRUE END
(*0928*)              ELSE DOTDOT:=FALSE;
(*0929*)            END;
(*0930*)           END;
(*0931*)(*.*) 75:  BEGIN OP:=NOOP; NEXTCH;
(*0932*)          IF CH='.' THEN BEGIN SY:=COLON;DOTDOT:=TRUE;NEXTCH END
(*0933*)                   ELSE IF CH=')' THEN BEGIN SY:=RBRACK; NEXTCH END
(*0934*)                     ELSE SY:=PERIOD;
(*0935*)           END;
(*0936*)(*( *) 77:
(*0937*)    BEGIN NEXTCH;
(*0938*)      IF CH = '*' THEN
(*0939*)        BEGIN NEXTCH;
(*0940*)          IF CH = '$' THEN OPTIONS;
(*0941*)          REPEAT
(*0942*)            WHILE CH<>'*' DO NEXTCH;
(*0943*)            NEXTCH
(*0944*)          UNTIL CH = ')';
(*0945*)          NEXTCH; GOTO 1
(*0946*)        END;
(*0947*)      OP:=NOOP;
(*0948*)      IF CH='.' THEN BEGIN SY:=LBRACK; NEXTCH END
(*0949*)                ELSE IF CH='#' THEN BEGIN SY:=LCBRACK; NEXTCH; END
(*0950*)                  ELSE SY:=LPARENT;
(*0951*)    END;
(*0952*)(*<*) 76:  BEGIN NEXTCH; SY:=RELOP;
(*0953*)             IF CH='=' THEN BEGIN OP:=LEOP; NEXTCH; END
(*0954*)               ELSE IF CH='>' THEN BEGIN OP:=NEOP; NEXTCH; END
(*0955*)                 ELSE OP:=LTOP;
(*0956*)           END;
(*0957*)(*>*) 110: BEGIN NEXTCH; SY:=RELOP;
(*0958*)             IF CH='=' THEN BEGIN OP:=GEOP; NEXTCH; END
(*0959*)                       ELSE OP:=GTOP;
(*0960*)           END;
(*0961*)(*#*) 123: BEGIN NEXTCH; OP:=NOOP;
(*0962*)                 IF CH=')' THEN BEGIN SY:=RCBRACK; NEXTCH; END
(*0963*)                           ELSE SY:=OTHERSY;
(*0964*)           END;
(*0965*)(* ** *)
(*0966*)      92 : BEGIN   NEXTCH; SY:=MULOP;OP:=MUL;
(*0967*)            IF CH = '*' THEN
(*0968*)            BEGIN
(*0969*)             NEXTCH; SY:=EXPONOP
(*0970*)            END;
(*0971*)          END;
(*0972*)(* /+-=)%,;@XXXX *)
(*0973*)    173,189,79,80,                                                              
(*0974*)      97,78,96,126,93,108,107,94,124:
(*0975*)        BEGIN SY:=SSY(.CH.); OP:=SOP(.CH.); NEXTCH; END;
(*0976*)  74,91,98..102,                                                                
(*0977*)      103,104,105,106,109,111,112,113,114,115,116,117,118,119,120,121,127:
(*0978*)   BEGIN OP:=NOOP; SY:=OTHERSY; NEXTCH; END;                                    
(*0979*)(* ^ *)   95:BEGIN                                                              
(*0980*)               NEXTCH; SY :=NOTSY;                                              
(*0981*)                 IF CH = '=' THEN                                               
(*0982*)                 BEGIN                                                          
(*0983*)                   SY := RELOP;                                                 
(*0984*)                   OP := NEOP;                                                  
(*0985*)                   NEXTCH;                                                      
(*0986*)                  END ELSE OP := NOOP;                                          
(*0987*)               END;                                                             
(*0988*)(*   *)   139: BEGIN                                                            
(*0989*)                 NEXTCH; IF CH='$' THEN OPTIONS;                                
(*0990*)                 WHILE ORD(CH) <> 155 DO NEXTCH;                                
(*0991*)                 NEXTCH; GOTO 1;                                                
(*0992*)               END;                                                             
(*0993*)  END (*CASE*);
(*0994*) END (*INSYMBOL*) ;
(*0995*) 
 // $TITLE  IDENTIFIER TABLE ENTERING
(*0996*) PROCEDURE ENTERID(FCP: CTP);
(*0997*)  (*ENTER ID POINTED AT BY FCP INTO THE NAME-TABLE,
  0998     WHICH ON EACH DECLARATION LEVEL IS ORGANISED AS
  0999     AN UNBALANCED BINARY TREE*)
(*1000*)   VAR NAM: ALFA; LCP, LCP1: CTP; LLEFT: BOOLEAN;
(*1001*)   BEGIN NAM := FCP^.NAME;
(*1002*)     LCP := DISPLAY(.TOP.).FNAME;
(*1003*)     IF LCP = NIL THEN
(*1004*)       DISPLAY(.TOP.).FNAME := FCP
(*1005*)     ELSE
(*1006*)       BEGIN
(*1007*)         REPEAT LCP1 := LCP;
(*1008*)           IF LCP^.NAME = NAM THEN   (*NAME CONFLICT, FOLLOW RIGHT LINK*)
(*1009*)             BEGIN ERROR(101); LCP := LCP^.RLINK; LLEFT := FALSE END
(*1010*)           ELSE
(*1011*)             IF LCP^.NAME < NAM
(*1012*)               THEN BEGIN LCP := LCP^.RLINK; LLEFT := FALSE END
(*1013*)               ELSE BEGIN LCP := LCP^.LLINK; LLEFT := TRUE END
(*1014*)         UNTIL LCP = NIL;
(*1015*)         IF LLEFT THEN LCP1^.LLINK := FCP ELSE LCP1^.RLINK := FCP
(*1016*)       END;
(*1017*)     FCP^.LLINK := NIL; FCP^.RLINK := NIL
(*1018*)   END;
(*1019*) 
// $TITLE  SEARCHSECTION,SEARCHID
(*1020*) PROCEDURE SEARCHSECTION(FCP: CTP; VAR FCP1: CTP);
(*1021*)  LABEL 1;
(*1022*) BEGIN
(*1023*)  WHILE FCP <> NIL DO
(*1024*)   IF FCP^.NAME = ID THEN GOTO 1
(*1025*)   ELSE IF FCP^.NAME < ID THEN FCP := FCP^.RLINK
(*1026*)    ELSE FCP := FCP^.LLINK;
(*1027*)1:  FCP1 := FCP
(*1028*) END;
(*1029*) 
(*1030*) PROCEDURE SEARCHID(FIDCLS: SETOFIDS; VAR FCP: CTP);
(*1031*)  LABEL 1;
(*1032*)  VAR LCP: CTP;
(*1033*) BEGIN
(*1034*)  FOR DISX := TOP DOWNTO 0 DO
(*1035*)   BEGIN LCP := DISPLAY(.DISX.).FNAME;
(*1036*)    WHILE LCP <> NIL DO
(*1037*)      WITH LCP^ DO
(*1038*)        IF NAME = ID THEN
(*1039*)          IF KLASS IN FIDCLS THEN GOTO 1
(*1040*)          ELSE
(*1041*)            BEGIN IF PRTERR THEN ERROR(103);
(*1042*)              LCP := RLINK
(*1043*)            END
(*1044*)        ELSE
(*1045*)          IF NAME<ID THEN LCP:=RLINK
(*1046*)                     ELSE LCP:=LLINK;
(*1047*)   END;
(*1048*)  (*SEARCH NOT SUCCSESSFUL; SUPPRESS ERROR MESSAGE IN CASE
  1049     OF FORWARD REFERENCED TYPE ID IN POINTER TYPE DEFINITION
  1050     OR VARIANTS WITHOUT TAGFIELDS
  1051     --> PROCEDURE FIELDLIST
  1052     --> PROCEDURE TYP*)
(*1053*)  IF PRTERR THEN
(*1054*)   BEGIN ERROR(104);
(*1055*)    (*TO AVOID RETURNING NIL, REFERENCE AN ENTRY
  1056       FOR AN UNDECLARED ID OF APPROPRIATE CLASS
  1057       --> PROCEDURE ENTERUNDECL*)
(*1058*)    IF TYPES IN FIDCLS THEN LCP := UTYPPTR
(*1059*)      ELSE IF VARS IN FIDCLS THEN LCP:=UVARPTR
(*1060*)        ELSE IF FIELD IN FIDCLS THEN LCP:=UFLDPTR
(*1061*)          ELSE IF KONST IN FIDCLS THEN LCP:=UCSTPTR
(*1062*)            ELSE IF PROC IN FIDCLS THEN LCP:=UPRCPTR
(*1063*)              ELSE IF FUNC IN FIDCLS THEN LCP:=UFCTPTR
(*1064*)                ELSE LCP:=UEVENTPTR;
(*1065*)   END;
(*1066*)1:  FCP := LCP
(*1067*) END (*SEARCHID*) ;
(*1068*) 
// $TITLE GETBOUNDS ROUTINE ,SKIP,OBCLEAR
(*1069*) 
(*1070*)    PROCEDURE GETBOUNDS(FSP: STP; VAR FMIN,FMAX: INTEGER);
(*1071*)      (*GET INTERNAL BOUNDS OF SUBRANGE OR SCALAR TYPE*)
(*1072*)      (*ASSUME (FSP <> INTPTR) AND (FSP <> REALPTR)*)
(*1073*)      BEGIN
(*1074*)        IF FSP <> NIL THEN
(*1075*)         BEGIN
(*1076*)           IF FSP^.FORM = PACKDTYPE THEN FSP:=FSP^.BASETYPE;
(*1077*)          WITH FSP^ DO
(*1078*)            BEGIN
(*1079*)              IF FORM = SUBRANGE THEN
(*1080*)                BEGIN FMIN := MIN; FMAX := MAX END
(*1081*)              ELSE
(*1082*)                BEGIN FMIN := 0; FMAX := 0;
(*1083*)                  IF FORM = SCALAR THEN
(*1084*)                    BEGIN
(*1085*)                      IF SCALKIND = STANDARD THEN
(*1086*)                        BEGIN IF FSP = CHARPTR THEN FMAX := ORDCHARMAX
(*1087*)                          ELSE IF FSP=BOOLPTR THEN FMAX:=1;
(*1088*)                        END
(*1089*)                      ELSE
(*1090*)                        IF FSP^.FCONST <> NIL THEN
(*1091*)                         FMAX := FSP^.FCONST^.VALUES.IVAL
(*1092*)                    END
(*1093*)                END
(*1094*)            END;
(*1095*)           END
(*1096*)      END;
(*1097*) 
(*1098*) PROCEDURE SKIP(FSYS: SETOFSYS);
(*1099*)  (*SKIP INPUT STRING UNTIL RELEVANT SYMBOL FOUND*)
(*1100*)   BEGIN WHILE NOT (SY IN FSYS) DO INSYMBOL; END;
(*1101*) 
(*1102*) 
// $TITLE PUTESD,PUTRLD,OBCLEAR,DATA1
//(*1103*)PROCEDURE PUTESD(NAM:ALFA; ETYPE:0..2;CLEAR:BOOLEAN);
          procedure fixesd;   // convert numbers in ESD to big endian
                              // characters to ebcdic
          begin
              writeln;writeln('ESD writer inoperative');
              readln;
              halt;
          end;

          procedure fixrld;   // convert numbers in RLD to big endian
                              // characters to ebcdic
          begin
              writeln;writeln('RLD writer inoperative');
              readln;
              halt;
          end;

          procedure fixTXT;   // convert numbers in TXT to big endian
                              // characters to ebcdic
          begin
              writeln;writeln('TXT writer inoperative');
              readln;
              halt;
          end;

          procedure fixend;   // convert numbers in END to big endian
                              // characters to ebcdic
          begin
              writeln;writeln('END writer inoperative');
              readln;
              halt;
          end;



type EEType = 0..2;

(*1103*)PROCEDURE PUTESD(NAM:ALFA; ETYPE:EEtype;CLEAR:BOOLEAN);
(*1104*) 
(*1105*)BEGIN (* PUTESD *)
(*1106*)   WITH ESD.DATAITEM(.ESDCNT+1.) DO
(*1107*)   BEGIN
(*1108*)     NAME:=NAM;
(*1109*)     ADDRESS := RELOC1*ETYPE;
(*1110*)     IF ETYPE = SD THEN LENGTH := BYTE1SPACE ELSE
(*1111*)      LENGTH := ALLSPACES;
(*1112*)     ESDCNT:=ESDCNT+1;
(*1113*)     ESD.BYTES:=BYTE1SPACE+BYTE2SPACE+ESDCNT*16;
(*1114*)  END;
(*1115*)  IF (ESDCNT = 3) OR CLEAR THEN
(*1116*)  WITH ESD DO
(*1117*)  BEGIN
(*1118*)    ID:=BYTE1SPACE+BYTE2SPACE+ESDID;
(*1119*)     ESDID:=ESDID+ESDCNT;
(*1120*)    ESDCNT:=0;
// **FIXME** write esd
//           SYSGO^:=CARD(ESD);
// (*1121*)    PUT(SYSGO);
               fixesd;
(*1122*)  END;
(*1123*)END; (*PUTESD*)
(*1124*) 
(*1125*) 
(*1126*) 
(*1127*) 
(*1128*)PROCEDURE PUTRLD(R,P,ADDRESS:INTEGER; CLEAR:BOOLEAN);
(*1129*)BEGIN (* PUTRLD *)
(*1130*)  WITH RLD.RLDITEMS(.RLDPOS.) DO
(*1131*)  BEGIN
(*1132*)    RELPOS:=RELOC2*R+P;
(*1133*)    FLAGADDRESS:= RELOC1 * 28 + ADDRESS;
(*1134*)  END;
(*1135*)  RLDPOS:=RLDPOS+1;
(*1136*)  IF (RLDPOS=8) OR CLEAR THEN
(*1137*)  BEGIN
(*1138*)    RLD.BYTES:=BYTE1SPACE+BYTE2SPACE+(RLDPOS-1)*8;
{ **FIXME** write RLD buffer
  1139      SYSGO^:=CARD(RLD);
  1140     PUT(SYSGO);
}
            fixrld;
            RLDPOS:=1;
(*1141*)  END;
(*1142*)END; (* PUTRLD *)
(*1143*) 
(*1144*) 
(*1145*) 
(*1146*) 
(*1147*) 
(*1148*)PROCEDURE OBCLEAR;
(*1149*)BEGIN (* OBCLEAR *)
(*1150*)  WITH TXT DO
(*1151*)  BEGIN
(*1152*)    ADDRESS := BYTE1SPACE + CURRADDRESS;
(*1153*)    LENGTH := BYTE1SPACE+BYTE2SPACE + 4*(OBPOINTER-1);
(*1154*)    ID := 1+BYTE1SPACE+BYTE2SPACE;
(*1155*)    TEXTDATA := OBJECTCODE;
{ **FIXME** write TXT record
(*1156*)    SYSGO^:=CARD(TXT);
(*1157*)    PUT(SYSGO);
}
(*1158*)    CURRADDRESS:=CURRADDRESS+ 4*(OBPOINTER-1);
(*1159*)    OBPOINTER := 1;
(*1160*)  END;
(*1161*)END; (* OBCLEAR *)
(*1162*) 
(*1163*) 
(*1164*)PROCEDURE DATA1(X:INTEGER);
(*1165*)   BEGIN
(*1166*)     OBJECTCODE (.OBPOINTER.) := X;
(*1167*)     OBPOINTER := OBPOINTER + 1;
(*1168*)     IF OBPOINTER = OBJLENPL1 THEN
(*1169*)        OBCLEAR;
(*1170*)    END;
(*1171*) 
// $TITLE   TEST1 , TEST2
(*1172*)PROCEDURE TEST1(X:SYMBOL; Y:INTEGER);
(*1173*) 
(*1174*) (*  REPLACES 'IF <COND> THEN INSYMBOL ELSE ERROR(<NUM>) *)
(*1175*) 
(*1176*) BEGIN (*TEST1*)
(*1177*)   IF SY = X THEN INSYMBOL ELSE ERROR(Y)
(*1178*) END;   (*TEST1*)
(*1179*) 
(*1180*) 
(*1181*) 
(*1182*)PROCEDURE TEST2(X:SETOFSYS; Y:INTEGER; Z:SETOFSYS);
(*1183*) BEGIN(*TEST2*)
(*1184*)   IF NOT (SY IN X) THEN
(*1185*)   BEGIN
(*1186*)     ERROR(Y);
(*1187*)     SKIP(X+Z)
(*1188*)   END
(*1189*) END;(*TEST2*)
(*1190*) 
(*1191*) 
(*1192*) 
// $TITLE  BLOCK , INITSIZE AND ALIGNMENT
(*1193*) 
(*1194*) PROCEDURE BLOCK(FSYS: SETOFSYS; FSY: SYMBOL; FPROCP: CTP);
(*1195*)  VAR LSY:SYMBOL; FLABP:LBP; FWPROCS:CTP;
(*1196*) 
(*1197*)    PROCEDURE INITSIZE(VAR FSIZE : WBSIZE);
(*1198*)      BEGIN FSIZE.WBLENGTH:=4; FSIZE.BOUNDARY:=4;
(*1199*)      END;
(*1200*) 
(*1201*)    PROCEDURE ALIGNMENT(VAR COUNTER:INTEGER; CUNIT:CELLUNIT);
(*1202*)      BEGIN IF COUNTER MOD CUNIT>0 THEN
(*1203*)        COUNTER:=(COUNTER+CUNIT) DIV CUNIT*CUNIT;
(*1204*)      END;
(*1205*)
// old code


// $TITLE COMPTYPES,COMPLISTS,EQUALBOUNDS
(*1206*)  FUNCTION COMPTYPES(FSP1,FSP2: STP) : BOOLEAN;
(*1207*)   LABEL 1;
(*1208*)   (*DECIDE WHETHER STRUCT POINTED AT BY FSP1 AND FSP2 ARE COMPATIBLE*)
(*1209*)   VAR NXT1,NXT2: CTP; COMP: BOOLEAN; LPCRP : PCRP; LP : MARKP;
(*1210*) 
(*1211*)    FUNCTION COMPLISTS(FCP1,FCP2:CTP; FSP1,FSP2:STP):BOOLEAN;
(*1212*)    (*DECIDE WHETHER FIELDLISTS ARE COMPATIBLE*)
(*1213*)    (*FCP1, FCP2: HEADS OF FIELDLISTS; FSP1, FSP2: POINTERS TO HEAD OF
  1214       VARIANT CHAIN*)
(*1215*)      VAR COMP:BOOLEAN; NXT1,NXT2:STP;
(*1216*)      BEGIN COMP := TRUE;
(*1217*)        WHILE COMP AND (FCP1 <> NIL)AND (FCP2 <> NIL) DO
(*1218*)          BEGIN COMP := COMPTYPES(FCP1^.IDTYPE,FCP2^.IDTYPE);
(*1219*)            FCP1 := FCP1^.NEXT; FCP2 := FCP2^.NEXT
(*1220*)          END;
(*1221*)        COMP := COMP AND (FCP1 = FCP2);
(*1222*)        IF (FSP1 <> NIL)AND (FSP2 <> NIL) THEN
(*1223*)          BEGIN
(*1224*)            IF (FSP1^.TGFLDP <> NIL)AND (FSP2^.TGFLDP <> NIL) THEN
(*1225*)              COMP := COMP AND COMPTYPES(FSP1^.TGFLDP^.IDTYPE,
(*1226*)                  FSP2^.TGFLDP^.IDTYPE);
(*1227*)            NXT1 := FSP1^.FSTVAR; NXT2 := FSP2^.FSTVAR;
(*1228*)            WHILE COMP AND (NXT1 <> NIL)AND (NXT2 <> NIL) DO
(*1229*)              BEGIN COMP := COMPLISTS(NXT1^.FSTVARFLD,NXT2^.FSTVARFLD,
(*1230*)                     NXT1^.SUBVAR,NXT2^.SUBVAR);
(*1231*)                NXT1 := NXT1^.NXTVAR; NXT2 := NXT2^.NXTVAR
(*1232*)              END;
(*1233*)            COMPLISTS := COMP AND (NXT1 = NXT2)
(*1234*)          END
(*1235*)        ELSE COMPLISTS := COMP AND (FSP1 = FSP2)
(*1236*)      END (*COMPLISTS*) ;
(*1237*) 
(*1238*)    FUNCTION EQUALBOUNDS(FSP1,FSP2: STP) : BOOLEAN;
(*1239*)      VAR LMIN1,LMIN2,LMAX1,LMAX2: INTEGER;
(*1240*)      BEGIN GETBOUNDS(FSP1,LMIN1,LMAX1);
(*1241*)            GETBOUNDS(FSP2,LMIN2,LMAX2);
(*1242*)            EQUALBOUNDS := (LMIN1 = LMIN2)AND (LMAX1 = LMAX2)
(*1243*)      END;
(*1244*) 
(*1245*)  BEGIN (*COMPTYPES*)
(*1246*)    IF FSP1 = FSP2 THEN COMPTYPES := TRUE
(*1247*)    ELSE IF (FSP1=NIL) OR (FSP2=NIL) THEN COMPTYPES:=TRUE
(*1248*)    ELSE
(*1249*)     BEGIN
(*1250*)       IF FSP1^.FORM=PACKDTYPE THEN FSP1:=FSP1^.BASETYPE;
(*1251*)       IF FSP2^.FORM=PACKDTYPE THEN FSP2:=FSP2^.BASETYPE;
(*1252*)       IF FSP1^.FORM=SUBRANGE THEN FSP1:=FSP1^.RANGETYPE;
(*1253*)       IF FSP2^.FORM=SUBRANGE THEN FSP2:=FSP2^.RANGETYPE;
(*1254*)       IF FSP1=FSP2 THEN COMPTYPES:=TRUE ELSE
(*1255*)       IF FSP1^.SIZE.WBLENGTH<>FSP2^.SIZE.WBLENGTH THEN COMPTYPES:=FALSE
(*1256*)       ELSE
(*1257*)       IF FSP1^.FORM<>FSP2^.FORM THEN COMPTYPES:=FALSE
(*1258*)       ELSE CASE FSP1^.FORM OF
(*1259*)         SCALAR:
(*1260*)           IF (FSP1^.SCALKIND = STANDARD)OR(FSP2^.SCALKIND = STANDARD) THEN
(*1261*)             COMPTYPES := FALSE
(*1262*)           ELSE
(*1263*)             BEGIN NXT1 := FSP1^.FCONST; NXT2 := FSP2^.FCONST;
(*1264*)              COMP := TRUE;
(*1265*)              WHILE COMP AND (NXT1 <> NIL)AND (NXT2 <> NIL) DO
(*1266*)                BEGIN COMP := (NXT1^.NAME = NXT2^.NAME);
(*1267*)                  NXT1 := NXT1^.NEXT; NXT2 := NXT2^.NEXT
(*1268*)                END;
(*1269*)              COMPTYPES := COMP AND (NXT1 = NXT2)
(*1270*)             END;
(*1271*)         PACKDTYPE,SUBRANGE,TAGFIELD,VARIANT: ERROR(400);
(*1272*)         POINTER:
(*1273*)           BEGIN
(*1274*)             LPCRP := FSTPCRP; COMP := TRUE;
(*1275*)             WHILE LPCRP <> NIL DO
(*1276*)               WITH LPCRP^ DO
(*1277*)                 BEGIN
(*1278*)                   IF (FSP1 = PTR1) AND (FSP2 = PTR2) THEN GOTO 1
(*1279*)                   ELSE
(*1280*)                     IF (FSP1 = PTR2) AND (FSP2 = PTR1) THEN GOTO 1;
(*1281*)                   LPCRP := NEXT
(*1282*)                 END;
//(*1283*)             IF FSTPCRP = NIL THEN MARK(LP); NEW(LPCRP);
(*1283*)
                              NEW(LPCRP);

(*1284*)             WITH LPCRP^ DO
(*1285*)               BEGIN NEXT := FSTPCRP;
(*1286*)                 PTR1 := FSP1; PTR2 := FSP2
(*1287*)               END;
(*1288*)             FSTPCRP := LPCRP;
(*1289*)             COMP := COMPTYPES(FSP1^.ELTYPE,FSP2^.ELTYPE);
(*1290*)             FSTPCRP := FSTPCRP^.NEXT;
//(*1291*)             IF FSTPCRP = NIL THEN RELEASE(LP);

(*1292*)       1:    COMPTYPES := COMP
(*1293*)           END;
(*1294*)         POWER:
(*1295*)           COMPTYPES := (FSP1^.PCKDSET = FSP2^.PCKDSET)
(*1296*)                 AND COMPTYPES(FSP1^.ELSET,FSP2^.ELSET);
(*1297*)         ARRAYS:
(*1298*)           COMPTYPES:=COMPTYPES(FSP1^.INXTYPE,FSP2^.INXTYPE)
(*1299*)                  AND COMPTYPES(FSP1^.AELTYPE,FSP2^.AELTYPE)
(*1300*)                  AND EQUALBOUNDS(FSP1^.INXTYPE,FSP2^.INXTYPE);
(*1301*)         RECORDS:
(*1302*)           COMPTYPES:= COMPLISTS(FSP1^.FSTFLD,FSP2^.FSTFLD,
(*1303*)                 FSP1^.RECVAR,FSP2^.RECVAR);
(*1304*)         FILES:
(*1305*)           COMPTYPES:=COMPTYPES(FSP1^.FILTYPE,FSP2^.FILTYPE);
(*1306*)        END (*CASE*)
(*1307*)    END
(*1308*)  END (*COMPTYPES*) ;
(*1309*) 
// $TITLE  STRING,STRINGTYPE,REVERSE
(*1310*)    FUNCTION xSTRING(FSP: STP) : BOOLEAN;
(*1311*)      BEGIN result := FALSE;
(*1312*)        IF FSP <> NIL THEN
(*1313*)          WITH FSP^ DO
(*1314*)            IF SIZE.WBLENGTH<=256 THEN
(*1315*)              IF FORM = ARRAYS THEN
(*1316*)                IF AELTYPE<>NIL THEN
(*1317*)                  IF AELTYPE^.FORM=PACKDTYPE THEN
(*1318*)                    IF AELTYPE^.BASETYPE=CHARPTR THEN result :=TRUE;
(*1319*)      END;
(*1320*) 
(*1321*)    PROCEDURE STRINGTYPE(VAR FSP: STP);
(*1322*)   (*ENTER TYPE OF STRINGCONST (PACKED ARRAY (.1..LGTH.) OF CHAR) INTO
  1323      STRUCTURE TABLE*)
(*1324*)      VAR LSP,LSP1: STP;
(*1325*)      BEGIN NEW(LSP{,SUBRANGE});
(*1326*)        WITH LSP^ DO
(*1327*)          BEGIN RANGETYPE:=INTPTR;
(*1328*)            MIN := 1; MAX := LGTH ; FTYPE := FALSE;
(*1329*)            SIZE.WBLENGTH:=4; SIZE.BOUNDARY:=4;
(*1330*)          END;
(*1331*)        NEW(LSP1{,ARRAYS});
(*1332*)        WITH LSP1^ DO
(*1333*)          BEGIN
(*1334*)            AELTYPE := PACKDCHARPTR; INXTYPE := LSP;
(*1335*)            FTYPE:=FALSE; AELLENG:=1;
(*1336*)            SIZE.WBLENGTH:=LGTH; SIZE.BOUNDARY:=1;
(*1337*)          END;
(*1338*)        FSP := LSP1
(*1339*)      END;
(*1340*) 
(*1341*)    PROCEDURE REVERSE(A:CTP; VAR B:CTP);
(*1342*)      VAR WORK,ANSWER:CTP;
(*1343*)      BEGIN ANSWER:=NIL;
(*1344*)        WHILE A<>NIL DO
(*1345*)          WITH A^ DO
(*1346*)            BEGIN WORK:=NEXT; NEXT:=ANSWER;
(*1347*)                  ANSWER:=A; A:=WORK;
(*1348*)            END;
(*1349*)        B:=ANSWER;
(*1350*)      END;
(*1351*) 
// $TITLE CONSTANT,SETELEMENT
(*1352*)  PROCEDURE CONSTANT(FSYS: SETOFSYS; VAR FSP: STP; VAR FVALU: VALU);
(*1353*)   VAR LSP: STP; LCP: CTP; SIGN: (NONE,POS,NEG);
(*1354*)       SETTYPE1,SETTYPE2:STP; SETVAL1,SETVAL2:VALU;
(*1355*)       N:INTEGER; NOERROR:BOOLEAN;
(*1356*) 
(*1357*)   PROCEDURE SETELEMENT(SETTYPE:STP; SETVALUE:VALU);
(*1358*)     VAR X:BOOLEAN;
(*1359*)     BEGIN X:=FALSE;
(*1360*)       IF SETTYPE=REALPTR THEN ERROR(109)
(*1361*)         ELSE IF SETTYPE^.FORM>SUBRANGE THEN ERROR(136)
(*1362*)           ELSE IF NOT COMPTYPES(LSP^.ELSET,SETTYPE) THEN ERROR(137)
(*1363*)             ELSE IF (SETVALUE.IVAL<SETMIN) OR (SETVALUE.IVAL>SETMAX) THEN ERROR(304)
(*1364*)               ELSE X:=TRUE;
(*1365*)       NOERROR:=NOERROR AND X;
(*1366*)     END;
(*1367*) 
(*1368*)  BEGIN LSP := NIL; FVALU.IVAL := 0; FVALU.CKIND:=INT;
(*1369*)   TEST2(CONSTBEGSYS,50,FSYS);
(*1370*)   IF SY IN CONSTBEGSYS THEN
(*1371*)    BEGIN
(*1372*)     IF SY = CHARCONST THEN
(*1373*)       BEGIN LSP:=CHARPTR; FVALU.CKIND:=INT; FVALU.IVAL:=IVAL; INSYMBOL END
(*1374*)     ELSE IF SY=STRINGCONST THEN
(*1375*)       BEGIN STRINGTYPE(LSP);
(*1376*)         FVALU.CKIND:=STRG; FVALU.VALP:=CONSTP;
(*1377*)         INSYMBOL
(*1378*)       END
(*1379*)     ELSE IF SY=LBRACK THEN
(*1380*)       BEGIN NEW(LSP{,POWER});
(*1381*)         WITH LSP^ DO
(*1382*)           BEGIN ELSET:=NIL; PCKDSET:=FALSE; FTYPE:=FALSE;
(*1383*)                 SIZE.WBLENGTH:=8; SIZE.BOUNDARY:=8;
(*1384*)           END;
(*1385*)         FVALU.CKIND:=PSET; FVALU.PVAL:=(..);
(*1386*)         INSYMBOL;
(*1387*)         IF SY=RBRACK THEN INSYMBOL
(*1388*)         ELSE
(*1389*)           BEGIN
//(*1390*)             LOOP NOERROR:=TRUE;
(*1390*)             while true do begin
                       NOERROR:=TRUE;

(*1391*)               CONSTANT(FSYS+(.COMMA,COLON,RBRACK.),SETTYPE1,SETVAL1);
(*1392*)               SETELEMENT(SETTYPE1,SETVAL1);
(*1393*)               IF SY=COLON THEN
(*1394*)                 BEGIN INSYMBOL; CONSTANT(FSYS+(.COMMA,RBRACK.),SETTYPE2,SETVAL2);
(*1395*)                   SETELEMENT(SETTYPE2,SETVAL2);
(*1396*)                   IF NOERROR THEN
(*1397*)                     BEGIN FOR N:=SETVAL1.IVAL TO SETVAL2.IVAL DO
(*1398*)                              FVALU.PVAL:=FVALU.PVAL+(.N.);
(*1399*)                       LSP^.ELSET:=SETTYPE1;
(*1400*)                     END;
(*1401*)                 END
(*1402*)               ELSE IF NOERROR THEN
(*1403*)                 BEGIN FVALU.PVAL:=FVALU.PVAL+(.SETVAL1.IVAL.);
(*1404*)                       LSP^.ELSET:=SETTYPE1;
(*1405*)                 END;
//(*1406*)               IF SY<>COMMA THEN EXIT; INSYMBOL;
(*1406*)               IF SY<>COMMA THEN
                            break;
                          INSYMBOL;
(*1407*)             END;   // end of loop
(*1408*)             TEST1(RBRACK,12);
(*1409*)           END;
(*1410*)       END
(*1411*)     ELSE
(*1412*)       BEGIN
(*1413*)         SIGN := NONE;
(*1414*)         IF OP IN (.PLUS,MINUS.) THEN
(*1415*)           BEGIN IF OP = PLUS THEN SIGN := POS ELSE SIGN := NEG;
(*1416*)             INSYMBOL
(*1417*)           END;
(*1418*)         IF SY = IDENT THEN
(*1419*)           BEGIN SEARCHID((.KONST.),LCP);
(*1420*)             WITH LCP^ DO
(*1421*)               BEGIN LSP := IDTYPE; FVALU := VALUES END;
(*1422*)             IF SIGN <> NONE THEN
(*1423*)               IF LSP = INTPTR THEN
(*1424*)                 BEGIN IF SIGN = NEG THEN FVALU.IVAL := -FVALU.IVAL END
(*1425*)               ELSE
(*1426*)                 IF LSP = REALPTR THEN
(*1427*)                   BEGIN
(*1428*)                     IF SIGN = NEG THEN FVALU.RVAL := -FVALU.RVAL
(*1429*)                   END
(*1430*)               ELSE ERROR(105);
(*1431*)             INSYMBOL;
(*1432*)           END
(*1433*)         ELSE
(*1434*)           IF SY = INTCONST THEN
(*1435*)             BEGIN IF SIGN = NEG THEN IVAL := -IVAL;
(*1436*)               LSP:=INTPTR; FVALU.CKIND:=INT; FVALU.IVAL:=IVAL; INSYMBOL
(*1437*)             END
(*1438*)           ELSE
(*1439*)             IF SY = REALCONST THEN
(*1440*)               BEGIN IF SIGN = NEG THEN RVAL := -RVAL;
(*1441*)                 LSP:=REALPTR; FVALU.CKIND:=REEL; FVALU.RVAL:=RVAL; INSYMBOL
(*1442*)               END
(*1443*)             ELSE
(*1444*)               BEGIN ERROR(106); SKIP(FSYS) END
(*1445*)       END;
(*1446*)       TEST2(FSYS,6,(. .));
(*1447*)     END;
(*1448*)   FSP := LSP
(*1449*)  END (*CONSTANT*) ;
(*1450*) 
// $TITLE CONSTEXPRESSION,CONSTIMAGE,ERROR1
(*1451*)    PROCEDURE CONSTEXPRESSION(FSYS:SETOFSYS; VAR P:CEP);
(*1452*)      VAR X1,X2,T,W:CEP; LSP:STP; LVALU:VALU;
(*1453*)      BEGIN X1:=NIL;
(*1454*)        REPEAT INSYMBOL;
(*1455*)          CONSTANT(FSYS+(.COMMA,RCBRACK.),LSP,LVALU);
(*1456*)          IF (LSP^.FORM>=ARRAYS) AND (NOT xSTRING(LSP)) THEN ERROR(224)
(*1457*)          ELSE
(*1458*)            BEGIN NEW(X2);
(*1459*)              WITH X2^ DO
(*1460*)                BEGIN ELEMTYPE:=LSP; ELEMVALUE:=LVALU;
(*1461*)                      NEXTELEM:=X1;
(*1462*)                END;
(*1463*)              X1:=X2;
(*1464*)            END;
(*1465*)        UNTIL SY<>COMMA;
(*1466*)        T:=NIL;
(*1467*)        WHILE X1<>NIL DO WITH X1^ DO
(*1468*)          BEGIN W:=NEXTELEM; NEXTELEM:=T;
(*1469*)                T:=X1; X1:=W;
(*1470*)          END;
(*1471*)        P:=T;
(*1472*)        TEST1(RCBRACK,225);
(*1473*)        TEST2(FSYS,6,(..));
(*1474*)      END;
(*1475*) 
(*1476*)    PROCEDURE CONSTIMAGE(FSP:STP; FEP:CEP; VAR FVALU:VALU);
(*1477*)      VAR ERRFLAG:BOOLEAN; ANSWER,WORK,XX:CTAILP; CURRENT:ADDRRANGE;
(*1478*)          BYTEFLAG:BOOLEAN; BYTEPART,BUFFER:STRGFRAG;
(*1479*) 
(*1480*)     PROCEDURE ERROR1(N:INTEGER);
(*1481*)       BEGIN IF ERRFLAG THEN ERROR(N);
(*1482*)         ERRFLAG:=FALSE;
(*1483*)       END;
(*1484*) 
// $TITLE WORDCONST,BUFFEROUT,BYTECONST,UNITCONST
(*1485*)     PROCEDURE WORDCONST(V:INTEGER);
(*1486*)       BEGIN NEW(WORK);
(*1487*)         WORK^.NXTCSP:=ANSWER; WORK^.STFR:=V;
(*1488*)         ANSWER:=WORK; CURRENT:=CURRENT+4;
(*1489*)       END;
(*1490*) 
(*1491*)     PROCEDURE BUFFEROUT;
(*1492*)       BEGIN NEW(WORK);
(*1493*)         WORK^.NXTCSP:=ANSWER; WORK^.STFR:=BYTEPACK(BYTEPART);
(*1494*)         ANSWER:=WORK; BYTEFLAG:=FALSE;
(*1495*)            CURRENT := (CURRENT+3) DIV 4*4;                                     
(*1496*)       END;
(*1497*) 
(*1498*)     PROCEDURE BYTECONST(V:INTEGER);
(*1499*)       BEGIN BYTEPART(.CURRENT MOD 4+1.):=V;
(*1500*)         BYTEFLAG:=TRUE; CURRENT:=CURRENT+1;
(*1501*)         IF CURRENT MOD 4=0 THEN BUFFEROUT;
(*1502*)       END;
(*1503*) 
(*1504*)     PROCEDURE UNITCONST(DISPL:ADDRRANGE; FSP:STP);
(*1505*)       VAR X:CTAILP; I,A1,A2:INTEGER;
(*1506*)       BEGIN IF FEP=NIL THEN ERROR1(222)
(*1507*)    ELSE IF NOT COMPTYPES(FEP^.ELEMTYPE,FSP) THEN
(*1508*)    BEGIN
(*1509*)      IF NOT COMPTYPES(FSP,NILPTR) THEN ERROR1(223)
(*1510*)    END
(*1511*)         ELSE
(*1512*)           BEGIN
(*1513*)             IF DISPL>CURRENT THEN
(*1514*)               BEGIN IF BYTEFLAG THEN
(*1515*)                     BUFFEROUT;                                                 
(*1516*)                 IF DISPL>CURRENT THEN WORDCONST(0);
(*1517*)               END;
(*1518*)             IF FSP^.FORM=ARRAYS THEN
(*1519*)               BEGIN X:=FEP^.ELEMVALUE.VALP;
(*1520*)                 FOR I:=0 TO FSP^.SIZE.WBLENGTH-1 DO
(*1521*)                   BEGIN IF (I MOD 4)=0 THEN BYTEUNPACK(BUFFER,X^.STFR);
(*1522*)                         BYTECONST(BUFFER(.I MOD 4+1.));
(*1523*)                         IF (I MOD 4)=3 THEN X:=X^.NXTCSP;
(*1524*)                   END;
(*1525*)               END
(*1526*)             ELSE IF FSP^.SIZE.WBLENGTH=1 THEN BYTECONST(FEP^.ELEMVALUE.IVAL)
(*1527*)             ELSE IF (FEP^.ELEMVALUE.CKIND=REEL) OR (FEP^.ELEMVALUE.CKIND=PSET) THEN
(*1528*)               BEGIN SETVALUE(FEP^.ELEMVALUE.PVAL,A1,A2);
(*1529*)                     WORDCONST(A1); WORDCONST(A2);
(*1530*)               END
(*1531*)             ELSE WORDCONST(FEP^.ELEMVALUE.IVAL);
(*1532*)           FEP:=FEP^.NEXTELEM;
(*1533*)         END;
(*1534*)       END;
(*1535*) 
// $TITLE STCONST,BODY OF CONSTIMAGE
(*1536*)     PROCEDURE STCONST(DISPL:ADDRRANGE; FSP:STP);
(*1537*)       VAR LMIN,LMAX,I:INTEGER; LCP:CTP;
(*1538*)       BEGIN
(*1539*)         IF FSP<>NIL THEN
(*1540*)           CASE FSP^.FORM OF
(*1541*)             SCALAR,PACKDTYPE,SUBRANGE,POWER:
(*1542*)               UNITCONST(DISPL,FSP);
(*1543*)       POINTER: IF NOT COMPTYPES(NILPTR,FSP) THEN ERROR1(226)
(*1544*)                   ELSE UNITCONST(DISPL,FSP);
(*1545*)       FILES,TAGFIELD,VARIANT:
(*1546*)               ERROR1(226);
(*1547*)             ARRAYS:
(*1548*)               IF xSTRING(FSP) THEN UNITCONST(DISPL,FSP)
(*1549*)                 ELSE
(*1550*)                   BEGIN GETBOUNDS(FSP^.INXTYPE,LMIN,LMAX);
(*1551*)                     FOR I:=LMIN TO LMAX DO
(*1552*)                       BEGIN STCONST(DISPL,FSP^.AELTYPE);
(*1553*)                             DISPL:=DISPL+FSP^.AELLENG;
(*1554*)                       END;
(*1555*)                   END;
(*1556*)             RECORDS:
(*1557*)               IF FSP^.RECVAR<>NIL THEN ERROR1(227)
(*1558*)               ELSE
(*1559*)                 BEGIN LCP:=FSP^.FSTFLD;
(*1560*)                   WHILE LCP<>NIL DO
(*1561*)                     BEGIN STCONST(DISPL+LCP^.FLDADDR,LCP^.IDTYPE);
(*1562*)                           LCP:=LCP^.NEXT;
(*1563*)                     END;
(*1564*)                 END
(*1565*)           END;
(*1566*)       END;
(*1567*) 
(*1568*)      BEGIN (*CONSTIMAGE*)
(*1569*)        ERRFLAG:=TRUE; CURRENT:=0;
(*1570*)        ANSWER:=NIL; BYTEFLAG:=FALSE;
(*1571*)        STCONST(0,FSP);
(*1572*)        IF BYTEFLAG THEN BUFFEROUT;
(*1573*)        IF FSP^.SIZE.WBLENGTH>CURRENT THEN WORDCONST(0);
(*1574*)        IF FEP<>NIL THEN ERROR1(222);
(*1575*)        WORK:=NIL;
(*1576*)        WHILE ANSWER<>NIL DO WITH ANSWER^ DO
(*1577*)          BEGIN XX:=NXTCSP; NXTCSP:=WORK; WORK:=ANSWER; ANSWER:=XX; END;
(*1578*)        FVALU.CKIND:=STRG; FVALU.VALP:=WORK;
(*1579*)      END;
(*1580*) 
// $TITLE  TYP - TYPE HANDLING ROUTINES,CHECKPACK
(*1581*)  PROCEDURE TYP(FSYS: SETOFSYS; VAR FSP: STP; PACKFLAG:BOOLEAN);
(*1582*)   VAR LSP,LSP1,LSP2: STP; OLDTOP: DISPRANGE; LCP: CTP;
(*1583*)     LMIN,LMAX: INTEGER;
(*1584*)     LFILTYP: BOOLEAN;
(*1585*)     DISPL : ADDRRANGE;      (*LOCATION COUNTER WITHIN A RECORD*)
(*1586*)     LSIZE:CELLUNIT;         (*BOUNDARY OF THE RECORD*)
(*1587*) 
(*1588*)    PROCEDURE CHECKPACK(VAR ORG:STP);
(*1589*)      VAR W:STP; XMIN,XMAX:INTEGER;
(*1590*)      BEGIN
(*1591*)        IF ORG<>NIL THEN
(*1592*)          IF (ORG^.FORM=SCALAR) OR (ORG^.FORM=SUBRANGE) THEN
(*1593*)            IF ORG<>INTPTR THEN
(*1594*)              IF ORG<>REALPTR THEN
(*1595*)                BEGIN GETBOUNDS(ORG,XMIN,XMAX);
(*1596*)                  IF (XMIN>=0) AND (XMAX<=255) THEN
(*1597*)                    BEGIN NEW(W{,PACKDTYPE});
(*1598*)                      WITH W^ DO
(*1599*)                        BEGIN SIZE.WBLENGTH:=1; SIZE.BOUNDARY:=1;
(*1600*)                              BASETYPE:=ORG; FTYPE:=FALSE;
(*1601*)                        END;
(*1602*)                      ORG:=W;
(*1603*)                    END;
(*1604*)                END;
(*1605*)      END;
(*1606*) 
// $TITLE  SIMPLETYPE,SUBRNGS
(*1607*)   PROCEDURE SIMPLETYPE(FSYS: SETOFSYS; VAR FSP: STP; PACKFLAG:BOOLEAN);
(*1608*)    VAR LSP,LSP1: STP; LCP,LCP1: CTP; TTOP: DISPRANGE;
(*1609*)        LVAL:INTEGER; LVALU:VALU;
(*1610*) 
(*1611*)    PROCEDURE SUBRNGS(FSP: STP; FVALU: VALU);
(*1612*)      BEGIN NEW(LSP{,SUBRANGE});
(*1613*)        WITH LSP^ DO
(*1614*)          BEGIN RANGETYPE:=FSP;
(*1615*)            MIN := FVALU.IVAL; FTYPE := FALSE
(*1616*)          END;
(*1617*)        TEST1(COLON,5);
(*1618*)        CONSTANT(FSYS,LSP1,LVALU);
(*1619*)        WITH LSP^ DO
(*1620*)          BEGIN MAX := LVALU.IVAL;
(*1621*)            INITSIZE(SIZE);
(*1622*)            IF FSP<>NIL THEN
(*1623*)              IF NOT COMPTYPES(FSP,LSP1) THEN ERROR(107)
(*1624*)              ELSE IF (FSP=REALPTR) OR (FSP^.FORM>=POWER) THEN
(*1625*)                  BEGIN ERROR(148); RANGETYPE:=NIL; END
(*1626*)                ELSE IF MIN>MAX THEN ERROR(102);
(*1627*)          END
(*1628*)      END;
(*1629*) 
(*1630*)   BEGIN (*SIMPLETYPE*)
(*1631*)    TEST2(SIMPTYPEBEGSYS,1,FSYS);
(*1632*)    IF SY IN SIMPTYPEBEGSYS THEN
(*1633*)     BEGIN
(*1634*)      IF SY = LPARENT THEN
(*1635*)       BEGIN TTOP := TOP;   (*DECL. CONSTS LOCAL TO INNERMOST BLOCK*)
(*1636*)        WHILE DISPLAY(.TOP.).OCCUR <> BLCK DO TOP := TOP - 1;
(*1637*)        NEW(LSP{,SCALAR,DECLARED});
(*1638*)        WITH LSP^ DO
(*1639*)          BEGIN FTYPE:=FALSE;
(*1640*)            FCONST := NIL; INITSIZE(SIZE)
(*1641*)          END;
(*1642*)        LCP1 := NIL; LVAL := -1;
(*1643*)        REPEAT INSYMBOL;
(*1644*)          IF SY = IDENT THEN
(*1645*)            BEGIN NEW(LCP{,KONST}); LVAL := LVAL + 1;
(*1646*)              WITH LCP^ DO
(*1647*)                BEGIN NAME := ID; IDTYPE := LSP; NEXT := LCP1;
(*1648*)                      VALUES.CKIND:=INT; VALUES.IVAL:=LVAL;
(*1649*)                END;
(*1650*)              ENTERID(LCP);
(*1651*)              LCP1 := LCP; INSYMBOL
(*1652*)            END
(*1653*)          ELSE ERROR(2);
(*1654*)        TEST2(FSYS+(.COMMA,RPARENT.),6,(..));
(*1655*)        UNTIL SY <> COMMA;
(*1656*)        LSP^.FCONST:=LCP1;
(*1657*)        TOP := TTOP;
(*1658*)        TEST1(RPARENT,4);
(*1659*)       END
(*1660*)      ELSE
(*1661*)        BEGIN
(*1662*)          IF SY = IDENT THEN
(*1663*)            BEGIN SEARCHID((.TYPES,KONST.),LCP);
(*1664*)              INSYMBOL;
(*1665*)              WITH LCP^ DO
(*1666*)                IF KLASS = KONST THEN SUBRNGS(IDTYPE,VALUES)
(*1667*)                                 ELSE LSP:=IDTYPE;
(*1668*)            END (*SY = IDENT*)
(*1669*)          ELSE
(*1670*)            BEGIN CONSTANT(FSYS+(.COLON.),LSP1,LVALU);
(*1671*)              SUBRNGS(LSP1,LVALU)
(*1672*)            END;
(*1673*)        END;
(*1674*)      IF PACKFLAG THEN CHECKPACK(LSP);
(*1675*)      FSP:=LSP;
(*1676*)      TEST2(FSYS,6,(..));
(*1677*)     END
(*1678*)      ELSE FSP := NIL
(*1679*)   END (*SIMPLETYPE*) ;
(*1680*) 
// $TITLE  FIELDLIST,FIELDADDRESS
(*1681*)   PROCEDURE FIELDLIST(FSYS: SETOFSYS; VAR FRECVAR: STP;
(*1682*)             VAR FFSTFLD: CTP; VAR FTYP: BOOLEAN);
(*1683*)             (* FTYP IS TRUE IFF A FIELD OF THE LIST IS OR CONTAINS A FILE *)
(*1684*)    VAR A, LCP,LCP1,NXT,NXT1: CTP; LSP,LSP1,LSP2,LSP3,LSP4: STP;
(*1685*)      SAVEDISPL,MAXDISPL : ADDRRANGE; SAVESIZE,MAXSIZE: CELLUNIT;
(*1686*)      LVALU : VALU;
(*1687*)      LFILTYP: BOOLEAN;
(*1688*) 
(*1689*)    PROCEDURE FIELDADDRESS(FCP: CTP; FSP: STP);
(*1690*)      BEGIN
(*1691*)        WITH FCP^,FSP^ DO
(*1692*)          IF FSP=NIL THEN FLDADDR:=DISPL
(*1693*)          ELSE BEGIN ALIGNMENT(DISPL,SIZE.BOUNDARY); FLDADDR:=DISPL;
(*1694*)                     DISPL:=DISPL+SIZE.WBLENGTH;
(*1695*)                     IF LSIZE<SIZE.BOUNDARY THEN LSIZE:=SIZE.BOUNDARY;
(*1696*)              END;
(*1697*)      END;
(*1698*) 
(*1699*)   BEGIN (*FIELDLIST*) NXT1 := NIL; LSP := NIL;
(*1700*)     LSP1 := NIL;                                                               
(*1701*)    FTYP := FALSE;
(*1702*)    TEST2(FSYS+(.IDENT,CASESY.),19,(..));
(*1703*)    WHILE SY = IDENT DO
(*1704*)     BEGIN NXT := NXT1;
//(*1705*)      LOOP
                while true do begin

(*1706*)        IF SY = IDENT THEN
(*1707*)          BEGIN NEW(LCP{,FIELD});
(*1708*)            WITH LCP^ DO
(*1709*)              BEGIN NAME:=ID; IDTYPE:=NIL; NEXT:=NXT; END;
(*1710*)            NXT:=LCP; ENTERID(LCP); INSYMBOL;
(*1711*)          END
(*1712*)        ELSE ERROR(2);
(*1713*)        TEST2((.COMMA,COLON.),6,FSYS+(.SEMICOLON,CASESY.));
// (*1714*)       IF SY<>COMMA THEN EXIT; INSYMBOL;
(*1714*)       IF SY<>COMMA THEN
                 break;
               INSYMBOL;

(*1715*)     END; // end of loop
(*1716*)      TEST1(COLON,5);
(*1717*)      TYP(FSYS+(.CASESY,SEMICOLON.),LSP,PACKFLAG);
(*1718*)      IF LSP<>NIL THEN FTYP:=FTYP OR LSP^.FTYPE;
(*1719*)      WHILE NXT <> NXT1 DO
(*1720*)        WITH NXT^ DO
(*1721*)          BEGIN IDTYPE := LSP;
(*1722*)            NXT:=NEXT;
(*1723*)          END;
(*1724*)       NXT1:=LCP;
(*1725*)       IF SY = SEMICOLON THEN
(*1726*)         BEGIN INSYMBOL;
(*1727*)           TEST2(FSYS+(.IDENT,CASESY.),19,(..));
(*1728*)         END
(*1729*)      END (*WHILE*);
(*1730*)    REVERSE(NXT1,FFSTFLD);
(*1731*)    NXT:=FFSTFLD;
(*1732*)    WHILE NXT<>NIL DO
(*1733*)      BEGIN FIELDADDRESS(NXT,NXT^.IDTYPE); NXT:=NXT^.NEXT; END;
(*1734*)    IF SY = CASESY THEN
(*1735*)     BEGIN NEW(LSP{,TAGFIELD});
(*1736*)      WITH LSP^ DO
(*1737*)       BEGIN TGFLDP:=NIL; FSTVAR:=NIL;
(*1738*)             FTYPE:=FALSE;
(*1739*)       END;
(*1740*)      FRECVAR := LSP;
(*1741*)      INSYMBOL;
(*1742*)      IF SY = IDENT THEN
(*1743*)       BEGIN PRTERR := FALSE; SEARCHID((.TYPES.),LCP1); PRTERR := TRUE;
(*1744*)        NEW(LCP{,FIELD});
(*1745*)        WITH LCP^ DO
(*1746*)         BEGIN IDTYPE:=NIL; NEXT:=NIL END;
(*1747*)        IF LCP1 = NIL THEN   (*EXPLICIT TAGFIELD*)
(*1748*)         BEGIN LCP^.NAME := ID; ENTERID(LCP);
(*1749*)          INSYMBOL;
(*1750*)          TEST1(COLON,5);
(*1751*)          IF SY = IDENT THEN SEARCHID((.TYPES.),LCP1)
(*1752*)          ELSE
(*1753*)           BEGIN ERROR(2); SKIP(FSYS+(.OFSY,LPARENT.));
(*1754*)            LCP1 := NIL
(*1755*)           END
(*1756*)         END
(*1757*)        ELSE LCP^.NAME := '        ';
(*1758*)        INSYMBOL;
(*1759*)        IF LCP1<>NIL THEN LSP1:=LCP1^.IDTYPE;
(*1760*)        IF PACKFLAG THEN CHECKPACK(LSP1);
(*1761*)        IF LSP1 <> NIL THEN
(*1762*)          BEGIN
(*1763*)            IF LSP1^.FORM>SUBRANGE THEN ERROR(110)
(*1764*)              ELSE IF LSP1=REALPTR THEN ERROR(109)
(*1765*)              ELSE
(*1766*)                BEGIN LSP^.TGFLDP := LCP;
(*1767*)                  WITH LCP^ DO
(*1768*)                    BEGIN IDTYPE := LSP1;
(*1769*)                      IF NAME <> '        ' THEN
(*1770*)                        FIELDADDRESS(LCP,LSP1)
(*1771*)                    END
(*1772*)                END
(*1773*)           END;
(*1774*)       END
(*1775*)      ELSE
(*1776*)       BEGIN ERROR(2); SKIP(FSYS+(.OFSY,LPARENT.)) END;
(*1777*)      LSP^.SIZE.WBLENGTH := DISPL;
(*1778*)      LSP^.SIZE.BOUNDARY := LSIZE;
(*1779*)      TEST1(OFSY,8);
(*1780*)      LSP1 := NIL; SAVEDISPL := DISPL; MAXDISPL := DISPL;
(*1781*)      SAVESIZE := LSIZE; MAXSIZE := LSIZE;
(*1782*)      (*LOOP UNTIL SY <> SEMICOLON:*)
// (*1783*)      LOOP
(*1783*)      while true do begin    // loop1

(*1784*)       IF NOT (SY IN FSYS+(.SEMICOLON.)) THEN
(*1785*)         BEGIN LSP2 := NIL;
//(*1786*)           LOOP CONSTANT(FSYS+(.COMMA,COLON,LPARENT.),LSP3,LVALU);
(*1786*)           while true do begin // loop2

                     CONSTANT(FSYS+(.COMMA,COLON,LPARENT.),LSP3,LVALU);
(*1787*)             IF LSP^.TGFLDP <> NIL THEN
(*1788*)               IF NOT COMPTYPES(LSP^.TGFLDP^.IDTYPE,LSP3) THEN
(*1789*)                 ERROR(111);
(*1790*)            NEW(LSP3{,VARIANT});
(*1791*)            WITH LSP3^ DO
(*1792*)              BEGIN NXTVAR := LSP1; SUBVAR := LSP2; VARVAL := LVALU.IVAL;
(*1793*)                    FTYPE:=FALSE;
(*1794*)              END;
(*1795*)            LSP1 := LSP3; LSP2 := LSP3;
//(*1796*)            IF SY<>COMMA THEN EXIT; INSYMBOL;
(*1796*)            IF SY<>COMMA THEN
                      break;
                    INSYMBOL;

(*1797*)           END;  // end of loop 2
(*1798*)           TEST1(COLON,5);
(*1799*)           TEST1(LPARENT,9);
(*1800*)           FIELDLIST(FSYS+(.RPARENT,SEMICOLON.),LSP2,LCP,LFILTYP);
(*1801*)           IF LFILTYP THEN BEGIN ERROR(108); FTYP:=TRUE; END;
(*1802*)           IF DISPL>MAXDISPL THEN MAXDISPL:=DISPL;
(*1803*)           IF LSIZE>MAXSIZE THEN MAXSIZE:=LSIZE;
(*1804*)           WHILE LSP3 <> NIL DO
(*1805*)             WITH LSP3^ DO
(*1806*)               BEGIN LSP4 := SUBVAR; SUBVAR := LSP2;
(*1807*)                 SIZE.WBLENGTH:=DISPL;SIZE.BOUNDARY:=LSIZE;
(*1808*)                 FSTVARFLD := LCP;
(*1809*)                 LSP3 := LSP4
(*1810*)               END;
(*1811*)           IF SY = RPARENT THEN
(*1812*)            BEGIN INSYMBOL;
(*1813*)              TEST2(FSYS+(.SEMICOLON.),6,(. .));
(*1814*)            END
(*1815*)           ELSE ERROR(4);
(*1816*)         END (*NOT (SY IN ...*) ;
// (*1817*)       IF SY<>SEMICOLON THEN EXIT;
(*1817*)       IF SY<>SEMICOLON THEN
                 break;

(*1818*)       DISPL:=SAVEDISPL; LSIZE:=SAVESIZE; INSYMBOL;
(*1819*)     END;   // end of loop 1
(*1820*)      DISPL := MAXDISPL; LSIZE := MAXSIZE;
(*1821*)      LSP^.FSTVAR := LSP1;
(*1822*)     END
(*1823*)    ELSE
(*1824*)     FRECVAR := NIL
(*1825*)   END (*FIELDLIST*) ;
(*1826*) 
// $TITLE  FILETYPE
(*1827*)    PROCEDURE FILETYPE;
(*1828*)      VAR COMPONENT,S:STP;
(*1829*)      BEGIN INSYMBOL;
(*1830*)        TEST1(OFSY,8);
(*1831*)        NEW(LSP{,FILES});
(*1832*)        WITH LSP^ DO
(*1833*)          BEGIN FILTYPE:=NIL; FTYPE:=TRUE;
(*1834*)                TEXTFILE:=FALSE; SIZE.WBLENGTH:=16; SIZE.BOUNDARY:=4;
(*1835*)          END;
(*1836*)        TYP(FSYS,COMPONENT,PACKFLAG);
(*1837*)        IF COMPONENT<>NIL THEN
(*1838*)          IF COMPONENT^.FTYPE THEN
(*1839*)            BEGIN ERROR(108); COMPONENT:=NIL; END
(*1840*)          ELSE IF COMPONENT^.SIZE.WBLENGTH>=4096 THEN
(*1841*)            BEGIN ERROR(184); COMPONENT:=NIL; END;
(*1842*)        IF COMPONENT<>NIL THEN
(*1843*)          WITH LSP^ DO
(*1844*)            BEGIN FILTYPE:=COMPONENT; TEXTFILE:=COMPTYPES(COMPONENT,CHARPTR);
(*1845*)              IF TEXTFILE
(*1846*)                THEN BEGIN SIZE.WBLENGTH:=TEXTSIZE; SIZE.BOUNDARY:=4;
(*1847*)                       IF COMPONENT^.FORM=PACKDTYPE
(*1848*)                         THEN S:=COMPONENT
(*1849*)                         ELSE BEGIN NEW(S{,PACKDTYPE});
(*1850*)                                WITH S^ DO
(*1851*)                                  BEGIN SIZE.WBLENGTH:=1; SIZE.BOUNDARY:=1;
(*1852*)                                    FTYPE:=FALSE; BASETYPE:=COMPONENT;
(*1853*)                                  END;
(*1854*)                              END;
(*1855*)                            FILTYPE:=S;
(*1856*)                     END
(*1857*)                ELSE BEGIN SIZE.WBLENGTH:=COMPONENT^.SIZE.WBLENGTH+8;
(*1858*)                           SIZE.BOUNDARY:=COMPONENT^.SIZE.BOUNDARY;
(*1859*)                           ALIGNMENT(
                                   SIZE.WBLENGTH,4);

//                                  ALIGNMENT( SIZE.BOUNDARY,4);
(*1201    PROCEDURE ALIGNMENT(VAR COUNTER:INTEGER; CUNIT:CELLUNIT);
  1202       BEGIN*) IF SIZE.BOUNDARY MOD 4>0 THEN
(*1203*)        SIZE.BOUNDARY:=(SIZE.BOUNDARY+4) DIV 4*4;
(*1204      END; *)


(*1860*)                     END;
(*1861*)            END;
(*1862*)      END;
(*1863*) 
//// $TITLE  TYP - (BODY)
(*1864*)  BEGIN (*TYP*) LSP := NIL;
(*1865*)   TEST2(TYPEBEGSYS,10,FSYS);
(*1866*)   IF SY IN TYPEBEGSYS THEN
(*1867*)    BEGIN
(*1868*)     IF SY IN SIMPTYPEBEGSYS THEN SIMPLETYPE(FSYS,LSP,PACKFLAG)
(*1869*)     ELSE
(*1870*)  (*@*)
(*1871*)      IF SY = ARROW THEN
(*1872*)       BEGIN NEW(LSP{,POINTER});
(*1873*)        WITH LSP^ DO
(*1874*)         BEGIN ELTYPE := NIL; FTYPE := FALSE;
(*1875*)          INITSIZE(SIZE)
(*1876*)         END;
(*1877*)        INSYMBOL;
(*1878*)        IF SY = IDENT THEN
(*1879*)         BEGIN PRTERR := FALSE;   (*NO ERROR IF SEARCH NOT SUCCESSFUL*)
(*1880*)          SEARCHID((.TYPES.),LCP); PRTERR := TRUE;
(*1881*)          IF LCP = NIL THEN   (*FORWARD REFERENCED TYPE ID*)
(*1882*)           BEGIN NEW(LCP{,TYPES});
(*1883*)            WITH LCP^ DO
(*1884*)             BEGIN NAME := ID; IDTYPE := LSP;
(*1885*)              NEXT := FWPTR
(*1886*)             END;
(*1887*)            FWPTR := LCP
(*1888*)           END
(*1889*)          ELSE
(*1890*)           BEGIN
(*1891*)            IF LCP^.IDTYPE <> NIL THEN
(*1892*)             IF LCP^.IDTYPE^.FTYPE THEN ERROR(108)
(*1893*)               ELSE LSP^.ELTYPE:=LCP^.IDTYPE;
(*1894*)           END;
(*1895*)          INSYMBOL;
(*1896*)         END
(*1897*)        ELSE ERROR(2);
(*1898*)       END
(*1899*)      ELSE
(*1900*)       BEGIN
(*1901*)        IF SY = PACKEDSY THEN
(*1902*)         BEGIN PACKFLAG := TRUE; INSYMBOL END;
(*1903*)         TEST2(TYPEDELS,10,FSYS);
(*1904*)  (*ARRAY*)
(*1905*)        IF SY = ARRAYSY THEN
(*1906*)         BEGIN INSYMBOL;
(*1907*)          TEST1(LBRACK,11);
(*1908*)          LSP1 := NIL;
//(*1909*)          LOOP NEW(LSP,ARRAYS);
(*1909*)          while true do begin
                     NEW(LSP{,ARRAYS});

(*1910*)           WITH LSP^ DO
(*1911*)            BEGIN AELTYPE := LSP1; INXTYPE := NIL;
(*1912*)             FTYPE := FALSE; INITSIZE(SIZE)
(*1913*)            END;
(*1914*)           LSP1 := LSP;
(*1915*)           SIMPLETYPE(FSYS+(.COMMA,RBRACK,OFSY.),LSP2,FALSE);
(*1916*)           IF LSP2 <> NIL THEN
(*1917*)            IF LSP2^.FORM <= SUBRANGE THEN
(*1918*)             IF LSP2 = INTPTR THEN ERROR(149)
(*1919*)             ELSE
(*1920*)              IF LSP2=REALPTR THEN ERROR(112)
(*1921*)             ELSE LSP^.INXTYPE := LSP2
(*1922*)            ELSE ERROR(113);
//(*1923*)           IF SY<>COMMA THEN EXIT; INSYMBOL;
(*1923*)           IF SY<>COMMA THEN
                     break;
                   INSYMBOL;

(*1924*)         END;   // end of loop
(*1925*)          TEST1(RBRACK,12);
(*1926*)          TEST1(OFSY,8);
(*1927*)          TYP(FSYS,LSP,PACKFLAG);
(*1928*)          (*REVERSE POINTERS, COMPUTE SIZE *)
(*1929*)          IF LSP <> NIL THEN
(*1930*)           BEGIN
(*1931*)            REPEAT
(*1932*)             WITH LSP1^ DO
(*1933*)              BEGIN LSP2 := AELTYPE; AELTYPE := LSP;
(*1934*)               FTYPE := LSP^.FTYPE;
(*1935*)               IF INXTYPE<>NIL THEN
(*1936*)                 BEGIN AELLENG:=AELTYPE^.SIZE.WBLENGTH;
(*1937*)                   ALIGNMENT(AELLENG,AELTYPE^.SIZE.BOUNDARY); GETBOUNDS(INXTYPE,LMIN,LMAX);
(*1938*)                   SIZE.WBLENGTH:=AELLENG*(LMAX-LMIN+1);
(*1939*)                   SIZE.BOUNDARY:=AELTYPE^.SIZE.BOUNDARY;
(*1940*)                 END;
(*1941*)              END (*WITH LSP1^*) ;
(*1942*)             LSP := LSP1; LSP1 := LSP2
(*1943*)            UNTIL LSP1 = NIL
(*1944*)           END (*LSP <> NIL*)
(*1945*)         END
(*1946*)        ELSE
(*1947*)  (*RECORD*)
(*1948*)         IF SY = RECORDSY THEN
(*1949*)          BEGIN INSYMBOL;
(*1950*)           OLDTOP := TOP;
(*1951*)           IF TOP < DISPLIMIT THEN
(*1952*)            BEGIN TOP := TOP + 1;
(*1953*)             WITH DISPLAY(.TOP.) DO
(*1954*)              BEGIN FNAME := NIL; OCCUR := REC END
(*1955*)            END
(*1956*)           ELSE ERROR(250);
(*1957*)           DISPL:=0; LSIZE:=1;
(*1958*)           FIELDLIST(FSYS-(.SEMICOLON.)+(.ENDSY.),LSP1,LCP,LFILTYP);
(*1959*)           NEW(LSP{,RECORDS});
(*1960*)           WITH LSP^ DO
(*1961*)            BEGIN FIELDS := DISPLAY(.TOP.).FNAME; FTYPE := LFILTYP;
(*1962*)             FSTFLD := LCP; RECVAR := LSP1;
(*1963*)             SIZE.WBLENGTH:=DISPL; SIZE.BOUNDARY:=LSIZE;
(*1964*)            END;
(*1965*)           TOP := OLDTOP;
(*1966*)           TEST1(ENDSY,13);
(*1967*)          END
(*1968*)         ELSE
(*1969*)  (*SET*)
(*1970*)         IF SY = SETSY THEN
(*1971*)           BEGIN INSYMBOL;
(*1972*)            TEST1(OFSY,8);
(*1973*)            NEW(LSP{,POWER});
(*1974*)            WITH LSP^ DO
(*1975*)              BEGIN ELSET:=NIL; PCKDSET:=FALSE; FTYPE:=FALSE;
(*1976*)                    SIZE.WBLENGTH:=8; SIZE.BOUNDARY:=8;
(*1977*)              END;
(*1978*)            SIMPLETYPE(FSYS,LSP1,FALSE);
(*1979*)            IF LSP1 <> NIL THEN
(*1980*)              IF LSP1^.FORM > SUBRANGE THEN ERROR(115)
(*1981*)                ELSE IF LSP1=REALPTR THEN ERROR(114)
(*1982*)                  ELSE IF LSP1=INTPTR THEN ERROR(169)
(*1983*)                    ELSE
(*1984*)                      BEGIN GETBOUNDS(LSP1,LMIN,LMAX);
(*1985*)                        IF (LMIN < SETMIN)OR (LMAX > SETMAX) THEN ERROR(169);
(*1986*)                        LSP^.ELSET:=LSP1;
(*1987*)                      END
(*1988*)                END
(*1989*)          ELSE
(*1990*)  (*FILE*) IF SY = FILESY THEN FILETYPE;
(*1991*)       END;
(*1992*)       TEST2(FSYS,6,(. .));
(*1993*)    END;
(*1994*)   FSP := LSP
(*1995*)  END (*TYP*) ;
(*1996*) 
// $TITLE LABEL DECLARATIONS
(*1997*)  PROCEDURE LABELDECLARATION;
(*1998*)    LABEL 1;
(*1999*)    VAR LLP: LBP;
(*2000*)    BEGIN
(*2001*)      REPEAT INSYMBOL;
(*2002*)        IF SY = INTCONST THEN
(*2003*)          BEGIN LLP := FSTLABP;
(*2004*)            WHILE LLP <> FLABP DO
(*2005*)              IF LLP^.LABVAL = IVAL THEN
(*2006*)                BEGIN ERROR(166); GOTO 1 END
(*2007*)              ELSE LLP := LLP^.NEXTLAB;
(*2008*)            NEW(LLP);
(*2009*)            WITH LLP^ DO
(*2010*)              BEGIN LABVAL := IVAL; DEFINED := FALSE; NEXTLAB := FSTLABP;
(*2011*)                    LCNT:=0; FSTOCC:=NIL;
(*2012*)              END;
(*2013*)            FSTLABP := LLP;
(*2014*)        1:  INSYMBOL
(*2015*)          END
(*2016*)        ELSE ERROR(15);
(*2017*)        TEST2(FSYS+(.COMMA,SEMICOLON.),6,(. .));
(*2018*)      UNTIL SY<>COMMA;
(*2019*)      IF SY = SEMICOLON THEN INSYMBOL ELSE ERROR(14)
(*2020*)    END (*LABELDECLARATION*) ;
(*2021*) 
// $TITLE CONST DECLARATIONS
(*2022*)  PROCEDURE CONSTDECLARATION;
(*2023*)    VAR LCP:CTP; LSP:STP; LVALU:VALU; EXPR:CEP;
(*2024*)    BEGIN
(*2025*)      IF SY <> IDENT THEN
(*2026*)        BEGIN ERROR(2); SKIP(FSYS+(.IDENT.)) END;
(*2027*)      WHILE SY = IDENT DO
(*2028*)        BEGIN NEW(LCP{,KONST});
(*2029*)          WITH LCP^ DO
(*2030*)            BEGIN NAME := ID; IDTYPE := NIL; NEXT := NIL;
(*2031*)            END;
(*2032*)          INSYMBOL;
(*2033*)          IF OP = EQOP THEN INSYMBOL ELSE ERROR(16);
(*2034*)          IF SY=LCBRACK THEN
(*2035*)          BEGIN
(*2036*)              IF EXTWARN THEN ERROR(291);
(*2037*)              CONSTEXPRESSION(FSYS+(.COLON,SEMICOLON.),EXPR);
(*2038*)              IF SY=COLON THEN INSYMBOL ELSE ERROR(5);
(*2039*)              TYP(FSYS+(.SEMICOLON.)+TYPEDELS,LSP,FALSE);
(*2040*)              CONSTIMAGE(LSP,EXPR,LVALU);
(*2041*)            END
(*2042*)          ELSE CONSTANT(FSYS+(.SEMICOLON.),LSP,LVALU);
(*2043*)          ENTERID(LCP);
(*2044*)          LCP^.IDTYPE := LSP; LCP^.VALUES := LVALU;
(*2045*)          IF SY = SEMICOLON THEN
(*2046*)            BEGIN INSYMBOL;
(*2047*)              TEST2(FSYS+(.IDENT.),6,(. .));
(*2048*)            END
(*2049*)          ELSE ERROR(14)
(*2050*)        END
(*2051*)    END (*CONSTDECLARATION*) ;
(*2052*) 
// $TITLE  UNDEFINED
 type string9 =  PACKED ARRAY(.1..9.) OF CHAR;
(*2053*)    PROCEDURE UNDEFINED(VAR F:CTP; xSTRING:string9);
(*2054*)      VAR I,SAVECNT:INTEGER;
(*2055*)      BEGIN
(*2056*)        IF F<>NIL THEN
(*2057*)          BEGIN ERROR(117); SAVECNT:=CHCNT; ENDOFLINE;
(*2058*)            REPEAT ENDOFLINE;
(*2059*)                   WRITELN(' UNDEFINED ',xSTRING,'  ',F^.NAME);
(*2060*)                   F:=F^.NEXT;
(*2061*)            UNTIL F=NIL;
(*2062*)          END;
(*2063*)      END;
(*2064*) 
// $TITLE TYPE DECLARATIONS
(*2065*)  PROCEDURE TYPEDECLARATION;
(*2066*)    VAR LCP,LCP1,LCP2: CTP; LSP: STP;
(*2067*)    BEGIN
(*2068*)      IF SY <> IDENT THEN
(*2069*)        BEGIN ERROR(2); SKIP(FSYS+(.IDENT.)) END;
(*2070*)      WHILE SY = IDENT DO
(*2071*)        BEGIN NEW(LCP{,TYPES});
(*2072*)          WITH LCP^ DO
(*2073*)            BEGIN NAME := ID; IDTYPE := NIL END;
(*2074*)          INSYMBOL;
(*2075*)          IF OP = EQOP THEN INSYMBOL ELSE ERROR(16);
(*2076*)          TYP(FSYS+(.SEMICOLON.),LSP,FALSE);
(*2077*)          ENTERID(LCP);
(*2078*)          LCP^.IDTYPE := LSP;
(*2079*)          (*HAS ANY FORWARD REFERENCE BEEN SATISFIED:*)
(*2080*)          LCP1 := FWPTR;
(*2081*)          WHILE LCP1 <> NIL DO
(*2082*)            BEGIN
(*2083*)              IF LCP1^.NAME = LCP^.NAME THEN
(*2084*)                BEGIN
(*2085*)                  WITH LCP^ DO
(*2086*)                    BEGIN LCP1^.IDTYPE^.ELTYPE := IDTYPE;
(*2087*)                      IF IDTYPE <> NIL THEN
(*2088*)                        IF IDTYPE^.FTYPE THEN ERROR(108)
(*2089*)                    END;
(*2090*)                  IF LCP1 <> FWPTR THEN
(*2091*)                    LCP2^.NEXT := LCP1^.NEXT
(*2092*)                  ELSE FWPTR := LCP1^.NEXT;
(*2093*)                END;
(*2094*)              LCP2 := LCP1; LCP1 := LCP1^.NEXT
(*2095*)            END;
(*2096*)          IF SY = SEMICOLON THEN
(*2097*)            BEGIN INSYMBOL;
(*2098*)              TEST2(FSYS+(.IDENT.),6,(. .));
(*2099*)            END
(*2100*)          ELSE ERROR(14)
(*2101*)        END;
(*2102*)      UNDEFINED(FWPTR,'TYPE-ID  ');
(*2103*)    END (*TYPEDECLARATION*) ;
(*2104*) 
// $TITLE VAR DECLARATIONS, ADDRESS
(*2105*)    PROCEDURE ADDRESS(FCP:CTP);
(*2106*)      BEGIN ALIGNMENT(LC,4);
(*2107*)        WITH FCP^ DO
(*2108*)          IF KLASS=VARS THEN
(*2109*)            IF VKIND=DRCT THEN
(*2110*)              BEGIN IF IDTYPE<>NIL
(*2111*)                THEN BEGIN ALIGNMENT(LC,IDTYPE^.SIZE.BOUNDARY); VADDR:=LC;
(*2112*)                           LC:=VADDR+IDTYPE^.SIZE.WBLENGTH;
(*2113*)                     END
(*2114*)                ELSE BEGIN VADDR:=LC; LC:=LC+4; END
(*2115*)              END
(*2116*)            ELSE BEGIN PARADDR:=LC; LC:=LC+4 END
(*2117*)          ELSE IF (KLASS=PROC) OR (KLASS=FUNC)
(*2118*)           THEN BEGIN PFADDR:=LC; LC:=LC+8 END
(*2119*)            ELSE ERROR(400);
(*2120*)      END;
(*2121*) 
(*2122*)  PROCEDURE VARDECLARATION;
(*2123*)    VAR LCP,NXT: CTP; LSP: STP;
(*2124*)    BEGIN NXT := NIL;
(*2125*)      REPEAT
//(*2126*)        LOOP
(*2126*)        while true do begin

(*2127*)          IF SY = IDENT THEN
(*2128*)            BEGIN NEW(LCP{,VARS});
(*2129*)              WITH LCP^ DO
(*2130*)                BEGIN NAME := ID; NEXT := NXT;
(*2131*)                      IDTYPE := NIL; VKIND := DRCT; VLEV := LEVEL
(*2132*)                END;
(*2133*)              ENTERID(LCP); NXT:=LCP; INSYMBOL;
(*2134*)            END
(*2135*)          ELSE ERROR(2);
(*2136*)          TEST2(FSYS+(.COMMA,COLON.)+TYPEDELS,6,(.SEMICOLON.));
//(*2137*)          IF SY<>COMMA THEN EXIT; INSYMBOL;
(*2137*)          IF SY<>COMMA THEN
                    break;
                  INSYMBOL;

(*2138*)        END;    // end of loop
(*2139*)        TEST1(COLON,5);
(*2140*)        TYP(FSYS+(.SEMICOLON.)+TYPEDELS,LSP,FALSE);
(*2141*)        WHILE NXT <> NIL DO
(*2142*)          WITH  NXT^ DO
(*2143*)            BEGIN IDTYPE := LSP; ADDRESS(NXT);
(*2144*)                 NXT := NEXT
(*2145*)            END;
(*2146*)        IF SY = SEMICOLON THEN
(*2147*)          BEGIN INSYMBOL;
(*2148*)            TEST2(FSYS+(.IDENT.),6,(. .));
(*2149*)          END
(*2150*)        ELSE ERROR(14)
(*2151*)      UNTIL (SY <> IDENT)AND NOT (SY IN TYPEDELS);
(*2152*)      UNDEFINED(FWPTR,'TYPE-ID  ');
(*2153*)    END (*VARDECLARATION*);
(*2154*) 
// $TITLE  VARINIT,DATA1,INITDATA
(*2155*) 
(*2156*)  PROCEDURE VARINITIALIZATION;
(*2157*)   VAR LCP:CTP; LSP:STP; LVALU:VALU; EXPR:CEP;
(*2158*) 
(*2159*)   PROCEDURE INITDATA(FCP:CTP; FVALU:VALU);
(*2160*)     VAR A1,A2,X:INTEGER; P:CTAILP;
(*2161*)     BEGIN
(*2162*)       CASE FVALU.CKIND OF
(*2163*)        INT : BEGIN                                                             
(*2164*)                IF FCP^.IDTYPE^.FORM=SUBRANGE THEN                              
(*2165*)                  IF (FVALU.IVAL>FCP^.IDTYPE^.MAX) OR                           
(*2166*)                     (FVALU.IVAL<FCP^.IDTYPE^.MIN) THEN ERROR(303);             
(*2167*)                DATA1(4); DATA1(FCP^.VADDR);                                    
(*2168*)                DATA1(FVALU.IVAL);                                              
(*2169*)              END;                                                              
(*2170*)         REEL,PSET:
(*2171*)               BEGIN SETVALUE(FVALU.PVAL,A1,A2); DATA1(8);
(*2172*)                     DATA1(FCP^.VADDR); DATA1(A1); DATA1(A2);
(*2173*)               END;
(*2174*)         STRG: IF FCP^.IDTYPE<>NIL THEN
(*2175*)                 BEGIN P:=FVALU.VALP; X:=FCP^.IDTYPE^.SIZE.WBLENGTH;
(*2176*)                   ALIGNMENT(X,4); DATA1(X); DATA1(FCP^.VADDR);
(*2177*)                   WHILE P<>NIL DO
(*2178*)                     BEGIN DATA1(P^.STFR); P:=P^.NXTCSP; END;
(*2179*)                 END
(*2180*)       END;
(*2181*)     END;
(*2182*) 
// $TITLE  VARINIT - BODY
(*2183*)    BEGIN (*VARINITIALIZATION*)
(*2184*)      IF LEVEL<>1 THEN
(*2185*)        BEGIN ERROR(220); SKIP(FSYS); END
(*2186*)      ELSE
(*2187*)        BEGIN
(*2188*)          IF SY<>IDENT THEN
(*2189*)            BEGIN ERROR(2); SKIP(FSYS+(.IDENT.)); END;
(*2190*)          PUTESD('P@MAIN@V',SD,TRUE);
(*2191*)          ESDID:=1;
(*2192*)          DATA1(Z7FE);
(*2193*)          WHILE SY=IDENT DO
(*2194*)            BEGIN SEARCHID((.VARS.),LCP); INSYMBOL;
(*2195*)              INITNUMBER:=INITNUMBER+1;
(*2196*)              TEST1(BECOMES,51);
(*2197*)              IF SY IN CONSTBEGSYS THEN
(*2198*)                BEGIN CONSTANT(FSYS+(.SEMICOLON.),LSP,LVALU);
(*2199*)                  IF COMPTYPES(LSP,LCP^.IDTYPE)
(*2200*)                    THEN INITDATA(LCP,LVALU)
(*2201*)                    ELSE ERROR(221);
(*2202*)                END
(*2203*)              ELSE IF SY=LCBRACK THEN
(*2204*)                BEGIN CONSTEXPRESSION(FSYS+(.SEMICOLON.),EXPR);
(*2205*)                  CONSTIMAGE(LCP^.IDTYPE,EXPR,LVALU);
(*2206*)                  INITDATA(LCP,LVALU);
(*2207*)                END
(*2208*)              ELSE BEGIN ERROR(50); SKIP(FSYS+(.SEMICOLON.)); END;
(*2209*)              IF SY<>SEMICOLON THEN ERROR(14)
(*2210*)                ELSE BEGIN INSYMBOL;
(*2211*)                      TEST2(FSYS+(.IDENT.),6,(. .));
(*2212*)                     END;
(*2213*)            END;
(*2214*)      OBCLEAR; ENDC.LENGTH:=CURRADDRESS;
{**FIXME** write END card
(*2215*)      SYSGO^:=CARD(ENDC);
(*2216*)      PUT(SYSGO);
}
(*2217*)      CURRADDRESS:=0;
(*2218*)    ESDCNT:=0; ESDID:=1;
(*2219*)        END;
(*2220*)    END;
(*2221*) 
(*2222*) 
// $TITLE PROCEDURE/FUNCTION DECLARATIONS
(*2223*)  PROCEDURE PROCDECLARATION(FSY: SYMBOL);
(*2224*)   (* 'FSY' WILL BE EITHER 'PROCSY' OR 'FUNCTSY' *)
(*2225*)   VAR OLDLEV: 0..MAXLEVEL; LSY: SYMBOL; LCP,LCP1,COMPARE,SAVE: CTP; LSP: STP;
(*2226*)     FORW: BOOLEAN; OLDTOP: DISPRANGE;
(*2227*)     LLC: ADDRRANGE; LP : MARKP;
(*2228*)    TP:INTEGER;
(*2229*) 
(*2230*)   PROCEDURE PARAMETERLIST(FSY: SETOFSYS; VAR FPAR: CTP);
(*2231*)    VAR LCP,LCP1,LCP2,LCP3: CTP; LSP: STP; LKIND: DRCTINDRCT;
(*2232*) 
(*2233*)    PROCEDURE DEFINESKELETON(FID:IDCLASS);
(*2234*)      VAR LCP,WORK1,WORK2,SKLTOP:CTP; LSP:STP;
(*2235*)          LCSAVE:ADDRRANGE;
(*2236*)      BEGIN INSYMBOL;
(*2237*)        IF SY<>IDENT THEN ERROR(2)
(*2238*)        ELSE
(*2239*)          BEGIN LCSAVE:=LC;
(*2240*)            IF FID=PROC THEN BEGIN NEW(LCP{,PROC,DECLARED,FORMAL}); LC:=64; END
(*2241*)                        ELSE BEGIN NEW(LCP{,FUNC,DECLARED,FORMAL}); LC:=72; END;
(*2242*)            WITH LCP^ DO
(*2243*)              BEGIN NAME:=ID; IDTYPE:=NIL; NEXT:=LCP1;
(*2244*)                    PFLEV:=LEVEL; PARAMS:=NIL;
(*2245*)              END;
(*2246*)            INSYMBOL;
(*2247*)            IF SY=LPARENT THEN
(*2248*)             BEGIN
(*2249*)                IF EXTWARN THEN ERROR(291);
(*2250*)                SKLTOP:=NIL;
(*2251*)                REPEAT INSYMBOL;
(*2252*)                  TYP(FSYS+(.COMMA,RPARENT.),LSP,FALSE);
(*2253*)                  IF LSP<>NIL THEN IF LSP^.FTYPE THEN ERROR(121);
(*2254*)                  NEW(WORK1{,VARS});
(*2255*)                  WITH WORK1^ DO
(*2256*)                    BEGIN NAME:='        '; IDTYPE:=LSP; VKIND:=DRCT;
(*2257*)                          NEXT:=SKLTOP; VLEV:=LEVEL+1;
(*2258*)                    END;
(*2259*)                  SKLTOP:=WORK1; ADDRESS(WORK1);
(*2260*)                UNTIL SY<>COMMA;
(*2261*)                REVERSE(SKLTOP,LCP^.PARAMS);
(*2262*)                TEST1(RPARENT,4);
(*2263*)              END;
(*2264*)            ENTERID(LCP); LCP1:=LCP; LC:=LCSAVE;
(*2265*)          END;
(*2266*)      END;
(*2267*) 
(*2268*)   BEGIN (*PARAMETERLIST*)
(*2269*)    LCP1:=NIL;
(*2270*)    TEST2(FSY+(.LPARENT.),7,FSYS);
(*2271*)    IF SY = LPARENT THEN
(*2272*)     BEGIN IF FORW THEN ERROR(119);
(*2273*)      INSYMBOL;
(*2274*)      IF NOT (SY IN (.IDENT,VARSY,PROCSY,FUNCTSY.)) THEN
(*2275*)       BEGIN ERROR(7); SKIP(FSYS+(.IDENT,RPARENT.)) END;
(*2276*)      WHILE SY IN (.IDENT,VARSY,PROCSY,FUNCTSY.) DO
(*2277*)       BEGIN
(*2278*)        IF SY = PROCSY THEN
(*2279*)         BEGIN
(*2280*)          REPEAT
(*2281*)            DEFINESKELETON(PROC);
(*2282*)            TEST2(FSYS+(.COMMA,SEMICOLON,RPARENT.),7,(. .));
(*2283*)          UNTIL SY <> COMMA
(*2284*)         END
(*2285*)        ELSE
(*2286*)         BEGIN LCP2 := LCP1; LSP := NIL;
(*2287*)          IF SY = FUNCTSY THEN
(*2288*)           BEGIN
(*2289*)            REPEAT
(*2290*)              DEFINESKELETON(FUNC);
(*2291*)              IF NOT (SY IN (.COMMA,COLON.)+FSYS) THEN
(*2292*)                BEGIN ERROR(7); SKIP(FSYS+(.COMMA,SEMICOLON,RPARENT.))
(*2293*)                END
(*2294*)            UNTIL SY <> COMMA;
(*2295*)            IF SY = COLON THEN
(*2296*)              BEGIN INSYMBOL;
(*2297*)                 IF SY <> IDENT THEN
(*2298*)                  IF EXTWARN THEN ERROR(291);
(*2299*)                    TYP(FSYS+(.SEMICOLON,RPARENT.),LSP,FALSE);
(*2300*)                    IF LSP<>NIL THEN
(*2301*)                      IF NOT (LSP^.FORM IN (.SCALAR,SUBRANGE,POINTER.)) THEN
(*2302*)                        BEGIN ERROR(120); LSP:=NIL; END;
(*2303*)              END
(*2304*)            ELSE ERROR(5)
(*2305*)           END
(*2306*)          ELSE
(*2307*)           BEGIN
(*2308*)            IF SY=VARSY THEN BEGIN LKIND:=INDRCT; INSYMBOL; END
(*2309*)                        ELSE LKIND:=DRCT;
//(*2310*)            LOOP
(*2310*)            while true do begin

(*2311*)             IF SY = IDENT THEN
(*2312*)               BEGIN NEW(LCP{,VARS});
(*2313*)                 WITH LCP^ DO
(*2314*)                   BEGIN NAME := ID; IDTYPE := NIL;
(*2315*)                     VKIND := LKIND; NEXT := LCP1; VLEV := LEVEL;
(*2316*)                   END;
(*2317*)                 ENTERID(LCP); LCP1:=LCP; INSYMBOL;
(*2318*)               END
(*2319*)             ELSE ERROR(2);
(*2320*)             IF NOT (SY IN (.COMMA,COLON.)+FSYS) THEN
(*2321*)               BEGIN ERROR(7); SKIP(FSYS+(.COMMA,SEMICOLON,RPARENT.))
(*2322*)               END;
//(*2323*)             IF SY<>COMMA THEN EXIT; INSYMBOL;
(*2323*)             IF SY<>COMMA THEN
                       break;
                     INSYMBOL;

(*2324*)           END; // end of loop
(*2325*)            IF SY = COLON THEN
(*2326*)              BEGIN INSYMBOL;
(*2327*)                TYP(FSYS+(.RPARENT,SEMICOLON.),LSP,FALSE);
(*2328*)                IF LSP<>NIL THEN
(*2329*)                  IF (LKIND=DRCT) AND LSP^.FTYPE THEN ERROR(121);
(*2330*)              END
(*2331*)            ELSE ERROR(5);
(*2332*)           END;
(*2333*)          LCP3 := LCP1;
(*2334*)          WHILE LCP3 <> LCP2 DO
(*2335*)            BEGIN LCP3^.IDTYPE:=LSP; LCP3:=LCP3^.NEXT; END;
(*2336*)         END;
(*2337*)        IF SY = SEMICOLON THEN
(*2338*)         BEGIN INSYMBOL;
(*2339*)          IF NOT (SY IN FSYS+(.IDENT,VARSY,PROCSY,FUNCTSY.)) THEN
(*2340*)           BEGIN ERROR(7); SKIP(FSYS+(.IDENT,RPARENT.)) END
(*2341*)         END
(*2342*)       END (*WHILE*) ;
(*2343*)      IF SY = RPARENT THEN
(*2344*)       BEGIN INSYMBOL;
(*2345*)        TEST2(FSY+FSYS,6,(. .));
(*2346*)       END
(*2347*)      ELSE ERROR(4);
(*2348*)      REVERSE(LCP1,LCP3);
(*2349*)      LCP1 := LCP3;
(*2350*)      WHILE LCP1 <> NIL DO
(*2351*)        BEGIN ADDRESS(LCP1); LCP1:=LCP1^.NEXT; END;
(*2352*)      FPAR := LCP3
(*2353*)     END
(*2354*)      ELSE FPAR := NIL
(*2355*)  END (*PARAMETERLIST*) ;
(*2356*) 
(*2357*)  BEGIN (*PROCDECLARATION*)
(*2358*)   LLC:=LC; FORW:=FALSE;
(*2359*)   DP := TRUE;
(*2360*)   IF FSY=PROCSY THEN LC:=64 ELSE LC:=72;
(*2361*)   IF SY<>IDENT
(*2362*)     THEN BEGIN ERROR(2); LCP:=UFCTPTR; END
(*2363*)     ELSE
(*2364*)       BEGIN COMPARE:=FWPROCS; LCP:=NIL;
(*2365*)         WHILE COMPARE<>NIL DO
(*2366*)           BEGIN
(*2367*)             IF ID=COMPARE^.NAME THEN
(*2368*)               BEGIN LCP:=COMPARE;
(*2369*)                 IF COMPARE=FWPROCS THEN FWPROCS:=COMPARE^.NEXT
(*2370*)                                    ELSE SAVE^.NEXT:=COMPARE^.NEXT;
(*2371*)               END;
(*2372*)             SAVE:=COMPARE; COMPARE:=COMPARE^.NEXT;
(*2373*)           END;
(*2374*)         IF LCP=NIL
(*2375*)           THEN FORW:=FALSE
(*2376*)           ELSE
(*2377*)             BEGIN IF LCP^.KLASS=PROC THEN FORW:=(FSY=PROCSY)
(*2378*)                                      ELSE FORW:=(FSY=FUNCTSY);
(*2379*)                   IF NOT FORW THEN ERROR(160);
(*2380*)             END;
(*2381*)         IF FORW
(*2382*)           THEN LC:=LCP^.LCSAVE
(*2383*)           ELSE
(*2384*)             BEGIN
(*2385*)               IF FSY=PROCSY THEN NEW(LCP{,PROC,DECLARED,ACTUAL})
(*2386*)                             ELSE NEW(LCP{,FUNC,DECLARED,ACTUAL});
(*2387*)               WITH LCP^ DO
(*2388*)                 BEGIN NAME:=ID; IDTYPE:=NIL; NEXT:=NIL; PFLEV:=LEVEL; PARAMS:=NIL;
(*2389*)                  IF PCNT<MAXPROCFUNC THEN
(*2390*)                  BEGIN
(*2391*)                    PCNT:=PCNT+1;
(*2392*)               IF (LEVEL = 1 ) AND EXTRNL THEN
(*2393*)               BEGIN
(*2394*)                 PUTESD(ID,SD,FALSE);
(*2395*)                 PROCREF:=ID; TP:=8;
(*2396*)                 WHILE PROCREF(.TP.) = ' ' DO TP:=TP-1;
(*2397*)                 IF TP=8 THEN PROCREF(.8.) := '@'
(*2398*)                   ELSE PROCREF(.TP+1.) := '@';
(*2399*)                 PUTESD(PROCREF,ER,TRUE);
(*2400*)              END;
(*2401*)                  END
(*2402*)                  ELSE BEGIN ERROR(261); PCNT:=1 END;
(*2403*)                   PFCNT:=PCNT;
(*2404*)                 END;
(*2405*)               ENTERID(LCP);
(*2406*)             END;
(*2407*)         INSYMBOL;
(*2408*)       END;
(*2409*)   OLDLEV := LEVEL; OLDTOP := TOP;
(*2410*)   IF LEVEL < MAXLEVEL THEN LEVEL := LEVEL + 1 ELSE ERROR(251);
(*2411*)   IF TOP>=DISPLIMIT
(*2412*)     THEN ERROR(250)
(*2413*)     ELSE BEGIN TOP:=TOP+1;
(*2414*)            WITH DISPLAY(.TOP.) DO
(*2415*)              BEGIN OCCUR:=BLCK;
(*2416*)                IF FORW THEN FNAME:=LCP^.PARAMS ELSE FNAME:=NIL;
(*2417*)              END;
(*2418*)          END;
(*2419*)   IF FSY = PROCSY THEN
(*2420*)     BEGIN PARAMETERLIST((.SEMICOLON.),LCP1);
(*2421*)       IF NOT FORW THEN LCP^.PARAMS := LCP1
(*2422*)     END
(*2423*)   ELSE
(*2424*)     BEGIN PARAMETERLIST((.SEMICOLON,COLON.),LCP1);
(*2425*)       IF NOT FORW THEN LCP^.PARAMS := LCP1;
(*2426*)       IF SY=COLON THEN
(*2427*)         BEGIN INSYMBOL; IF FORW THEN ERROR(122);
(*2428*)           TYP(FSYS+(.SEMICOLON.),LSP,FALSE);
(*2429*)           LCP^.IDTYPE:=LSP;
(*2430*)           IF LSP<>NIL THEN
(*2431*)             IF NOT (LSP^.FORM IN (.SCALAR,SUBRANGE,POINTER.)) THEN
(*2432*)               BEGIN ERROR(120); LCP^.IDTYPE:=NIL; END;
(*2433*)         END
(*2434*)       ELSE IF NOT FORW THEN ERROR(123);
(*2435*)     END;
(*2436*)   TEST1(SEMICOLON,14);
(*2437*)   IF (SY = IDENT) AND (ID='FORWARD ') THEN
(*2438*)     BEGIN IF FORW THEN ERROR(161);
(*2439*)       LCP^.LCSAVE:=LC; LCP^.NEXT:=FWPROCS; FWPROCS:=LCP;
(*2440*)       INSYMBOL;
(*2441*)       IF SY = SEMICOLON THEN INSYMBOL ELSE ERROR(14);
(*2442*)       TEST2(FSYS,6,(. .));
(*2443*)     END
(*2444*)   ELSE
(*2445*)IF (SY=IDENT) AND ((ID='FORTRAN ') OR (ID = 'PASCAL  ') OR
(*2446*)       (ID='EXTERN  ')) THEN
(*2447*)BEGIN
(*2448*)  IF FORW THEN ERROR(162);                                                      
(*2449*)  IF EXTRNL AND (LEVEL =2) THEN ERROR(383);                                     
(*2450*) IF PCNT < MAXPROCFUNC THEN
(*2451*) BEGIN
(*2452*)   WITH EXTARRAY(.EXTPROCS.) DO                                                 
(*2453*)   BEGIN                                                                        
(*2454*)      ENAME := LCP^.NAME;                                                       
(*2455*)      ECNT := PCNT;                                                             
(*2456*)   END; EXTPROCS:=EXTPROCS+1;                                                   
(*2457*)   PROCADDRESS(.PCNT.) := 1; (* DEFAULT IS EXTERNAL PASCAL *)                   
(*2458*)  PCNT:=PCNT+1; PROCADDRESS(.PCNT.):=0;
(*2459*)    LCP1:=LCP^.PARAMS;
(*2460*)    WHILE LCP1 <> NIL DO
(*2461*)    BEGIN
(*2462*)      IF LCP1^.KLASS IN (.PROC,FUNC.) THEN ERROR(380);
(*2463*)      LCP1:=LCP1^.NEXT;
(*2464*)    END;
(*2465*) IF ID = 'FORTRAN ' THEN                                                        
(*2466*) BEGIN                                                                          
(*2467*)    WITH LCP^ DO
(*2468*)    BEGIN
(*2469*)      IF KLASS = PROC THEN TP:=2 ELSE
(*2470*)       IF IDTYPE=REALPTR THEN TP:=4 ELSE
(*2471*)        IF COMPTYPES(IDTYPE,INTPTR) OR
(*2472*)          COMPTYPES(IDTYPE,BOOLPTR) THEN TP:=3
(*2473*)         ELSE ERROR(381);
(*2474*)    END;
(*2475*)    PROCADDRESS(.PCNT-1.):=TP;
(*2476*)  END;
(*2477*)  END ELSE BEGIN ERROR(261); PCNT:=1 END;
(*2478*)  INSYMBOL; TEST1(SEMICOLON,14); TEST2(FSYS,6,(..));
(*2479*)END ELSE
(*2480*) 
//(*2481*)     BEGIN MARK(LP);
(*2481*)     BEGIN
(*2482*)       REPEAT BLOCK(FSYS,SEMICOLON,LCP);
(*2483*)         IF SY = SEMICOLON THEN
(*2484*)      BEGIN
(*2485*)     IF (NOT EXTRNL) OR(EXTRNL AND(LEVEL>2)) THEN                               
(*2486*)           BEGIN INSYMBOL;
(*2487*)             IF NOT (SY IN (.BEGINSY,PROCSY,FUNCTSY.)) THEN
(*2488*)               BEGIN ERROR(6); SKIP(FSYS) END
(*2489*)           END
(*2490*)        END
(*2491*)         ELSE ERROR(14)
(*2492*)  UNTIL (SY IN (.BEGINSY,PROCSY,FUNCTSY.)) OR                                   
(*2493*)     (EXTRNL AND (LEVEL=2));                                                    
(*2494*)       // RELEASE(LP);
(*2495*)     END;
(*2496*)   LEVEL := OLDLEV; TOP := OLDTOP; LC := LLC;
(*2497*)  END (*PROCDECLARATION*) ;
(*2498*) 
(*2499*) 
(*2500*) 
(*2501*) 
// $TITLE  BODY - (HEADING)
(*2502*)  PROCEDURE BODY(FSYS: SETOFSYS);
(*2503*)    CONST
(*2504*)      ZA=90;     ZAD=106;   ZADR=42;   ZAR=26;
(*2505*)      ZAW=110;   ZBAL=69;   ZBALR=5;   ZBC=71;
(*2506*)      ZBCR=7;    ZBCTR=6;   ZC=89;     ZCD=105;
(*2507*)      ZCDR=41;   ZCL=85;    ZCLC=213;  ZCLR=21;
(*2508*)      ZCR=25;    ZD=93;     ZDD=109;   ZIC=67;
(*2509*)      ZL=88;     ZLA=65;               ZLCDR=35;
(*2510*)      ZLCR=19;   ZLD=104;   ZLM=152;   ZLNDR=33;
(*2511*)      ZLNR=17;   ZLPDR=32;  ZLPR=16;   ZLR=24;
(*2512*)      ZLTDR=34;  ZLTR=18;   ZM=92;     ZMD=108;
(*2513*)      ZMDR=44;   ZMR=28;    ZMVC=210;  ZN=84;
(*2514*)      ZNR=20;    ZO=86;     ZS=91;     ZSD=107;
(*2515*)      ZSDR=43;   ZSLA=139;  ZSLDA=143; ZSLDL=141;
(*2516*)      ZSLL=137;  ZSR=27;    ZSRDA=142; ZSRDL=140;
(*2517*)      ZSRL=136;  ZST=80;    ZSTC=66;   ZSTD=96;
(*2518*)      ZSTM=144;  ZTM=145;   ZX=87;     ZXR=23;
(*2519*)      ZBCT=70; ZEX = 68;
(*2520*)      ZMVI = 146;
(*2521*) 
(*2522*)      PBASE1=14; R0=0; BASEWORK=9;
(*2523*)      NEWPOINTER=7; STACKPOINTER=8;
(*2524*)      SAVEAREA=64;             (*LENGTH OF SAVEAREA*)
(*2525*)      JUMPERR1=376; JUMPERR2=384; JUMPERR3=392; JUMPERR4=400;
(*2526*)PROCBASE = 0; NPINIT = 140;
(*2527*)      IRCONVWORK = 152;
(*2528*)      ENTRYSIN=160; ENTRYAL=408; ENTRYCL=416;
(*2529*)      ENTRYWB=256; ENTRYWC=288; ENTRYWI=264; ENTRYWS=296; ENTRYWR1=272; ENTRYWR2=280;
(*2530*)      ENTRYRC=320; ENTRYRI=304; ENTRYRR=312; ENTRYRL=328;
(*2531*)      ENTRYRS = 496;  (* READ STRING ENTRY POINT *)
(*2532*)      ENTRYRET = 80; (* PROCEDURE RETURN DISPLACEMENT *)
(*2533*)      ENTRYVARPROC = 8 ; (* PASSED PROCEDURE CALL *)
(*2534*)      OPENINPUT=336; ENTGETCH=344; ENTWRITLN=472;
(*2535*)      ENTRYCLOCK=424; ENTRYTIME=432;
(*2536*)      ENTRYGET=224; ENTOPEXT=208; ENTCLEXT=216;
(*2537*)      ENTOPLOC=448; ENTCLLOC=456;
(*2538*)      ENTPAGE=464;
(*2539*)      ENTRYHALT = 456; ENTRYEXPON = 448;
(*2540*)      ENTRYMESSAGE = 480; (* MESSAGE ENTRY POINT *)
(*2541*)      ENTRYLONGJUMP = 488; (* LONG JUMP LANDING ENTRY *)
(*2542*)      CONDZ=8; CONDP=2; CONDM=4; CONDNZ=7; CONDNP=13; CONDNM=11;
(*2543*)   TYPE
(*2544*) 
(*2545*)               (*TO DESCRIBE EXPRESSION CURRENTLY COMPILED*)
(*2546*)               (*******************************************)
(*2547*) 
(*2548*)      ATTRP = ^ ATTR;
(*2549*)      ATTRKIND = (CST,VARBL,EXPR);
(*2550*)      CMP=^TEMPREC;
(*2551*)      TEMPREC=RECORD TEMPADRS:ADDRRANGE; TEMPLNGTH:INTEGER; (* 4 OR 8 *)
(*2552*)                     NEXTTEMP:CMP; TEMPCONT:ATTRP END;
(*2553*)      REGKIND=(SINGLE,DOUBLE,FLOAT);
(*2554*)      EXPRKIND=(REGIST,TEMPORARY);
(*2555*)      ACCESSKIND=(DIRECT,INDIRECT);           (*INDIRECT: INDEXED OR POINTED VARIABLE*)
(*2556*)      REGORTEMP=RECORD CASE REGTEMP:EXPRKIND OF
(*2557*)                             REGIST:(RNO:REGNO);
(*2558*)                             TEMPORARY:(ATEMP:CMP)
(*2559*)                END;
(*2560*)      REGRECORD=RECORD USED:BOOLEAN; REGCONT:ATTRP END;
(*2561*) 
(*2562*)      ATTR = RECORD TYPTR: STP;
(*2563*)               FOLLOW: ATTRP;
(*2564*)               CASE KIND: ATTRKIND OF
(*2565*)                CST:   (CVAL: VALU);
(*2566*)                VARBL: (VADRS:ADDRRANGE;
(*2567*)                        ACCESS:ACCESSKIND; INDEXREG:REGORTEMP;
(*2568*)                        CASE VARKIND:DRCTINDRCT OF
(*2569*)                              DRCT: (VLEVEL:LEVRANGE);
(*2570*)                              INDRCT: (BASELEV:LEVRANGE; BASEADD:ADDRRANGE));
(*2571*)                EXPR:(REXPR:REGORTEMP)
(*2572*)          END;
(*2573*) 
(*2574*)     CONSTCHAIN=^CONSTCREC;
(*2575*)     CONSTCREC=RECORD SAVECONST:VALU; CCHAIN:LOCOFREF;
(*2576*)                      NEXTCONST:CONSTCHAIN
(*2577*)               END;
(*2578*) 
(*2579*) 
(*2580*)          (*  CODE  BUFFERS  *)
(*2581*)          (*******************)
(*2582*) 
(*2583*) 
(*2584*)   CODESPTR = ^CODESEG;         (* POINTER TO CODE SEGMENT *)
(*2585*)   CODESEG  = RECORD            (* CODE SEGMENT DESCRIPTOR *)
(*2586*)                     CASE BOOLEAN OF
(*2587*)                  TRUE : (FULLWORDS:ARRAY(.0..CODEBLCK.) OF INTEGER);
(*2588*)                  FALSE: (BYTES    :PACKED ARRAY
(*2589*)                                       (. 0.. 255 .) OF CHAR )
(*2590*)              END; (* OF CODE SEGMENT *)
(*2591*)   VAR
(*2592*)     REGISTER:ARRAY(.REGNO.) OF REGRECORD;
(*2593*)     DISPLEVEL: LEVRANGE;                 (*NUMBER OF USED DISPLAY REGISTERS,  C.F. WITHSTATEMENT*)
(*2594*)     GATTRP,ATTRHEAD: ATTRP;
(*2595*)     RINDEX,RBASE:INTEGER;              (*INDEX AND BASE REGISTER NUMBER *)
(*2596*)     EFFADRS:INTEGER;                   (*EFFECTIVE ADDRESS*)
(*2597*)     RMAIN:INTEGER;                     (* WORKING REGISTER NUMBER *)
(*2598*)     RWORK:REGNO;
(*2599*)     FREETEMP:CMP;
(*2600*)     STACKTOP:INTEGER;
(*2601*)     BOOLFLAG: BOOLEAN;
(*2602*) 
(*2603*)     CONSTTOP:CONSTCHAIN;
(*2604*)     STACKSIZE:LOCOFREF;
(*2605*)     CODEPTR : ARRAY (.0..95.) OF CODESPTR;
(*2606*)     EXTENDEDADDRESS : BOOLEAN; (* FLAG FOR EXTENDED ADDRESSING *)
(*2607*)     REG6USED,REG5USED:BOOLEAN;
(*2608*)     PROCPASS : BOOLEAN;
(*2609*) 
(*2610*) 
// $TITLE  CODE GEN - ATTRNEW,ATTRDISP,COPYATTR,COPYREG
(*2611*)    PROCEDURE ATTRNEW(VAR FATTRP: ATTRP);
(*2612*)      BEGIN
(*2613*)        IF ATTRHEAD = NIL THEN NEW(FATTRP)
(*2614*)        ELSE BEGIN FATTRP:=ATTRHEAD; ATTRHEAD:=ATTRHEAD^.FOLLOW
(*2615*)             END;
(*2616*)      END;
(*2617*) 
(*2618*)  PROCEDURE ATTRDISP(FATTRP:ATTRP);                                             
(*2619*)                                                                                
(*2620*)    PROCEDURE TEMPDISP(ATP:CMP);                                                
(*2621*)      BEGIN                                                                     
(*2622*)        IF ATP^.TEMPCONT=FATTRP THEN                                            
(*2623*)         BEGIN ATP^.NEXTTEMP := FREETEMP; FREETEMP := ATP END;                  
(*2624*)      END;                                                                      
(*2625*)                                                                                
(*2626*)  BEGIN                                                                         
(*2627*)    WITH FATTRP^ DO                                                             
(*2628*)      BEGIN                                                                     
(*2629*)        FOLLOW := ATTRHEAD; ATTRHEAD := FATTRP;                                 
(*2630*)        IF KIND = EXPR THEN                                                     
(*2631*)         WITH REXPR DO                                                          
(*2632*)          BEGIN                                                                 
(*2633*)           IF REGTEMP = REGIST THEN                                             
(*2634*)             BEGIN                                                              
(*2635*)               IF REGISTER(.RNO.).REGCONT=FATTRP THEN                           
(*2636*)                BEGIN REGISTER(.RNO.).USED := FALSE;                            
(*2637*)                  IF FATTRP^.TYPTR^.FORM=POWER THEN                             
(*2638*)                    REGISTER(.SUCC(RNO).).USED := FALSE;                        
(*2639*)                END                                                             
(*2640*)             END                                                                
(*2641*)            ELSE TEMPDISP(ATEMP);                                               
(*2642*)   END;                                                                         
(*2643*)    END;                                                                        
(*2644*)  END;                                                                          
(*2645*) 
(*2646*)   PROCEDURE COPYATTR(SOURCEATTRP,DESTATTRP : ATTRP);
(*2647*) 
(*2648*)    PROCEDURE COPYREG(R: REGORTEMP);
(*2649*)       BEGIN IF R.REGTEMP = REGIST THEN REGISTER(.R.RNO.).REGCONT:= DESTATTRP
(*2650*)              ELSE R.ATEMP^.TEMPCONT:=DESTATTRP;
(*2651*)     END;
(*2652*) 
(*2653*)     BEGIN DESTATTRP^ := SOURCEATTRP^;
(*2654*)       IF SOURCEATTRP^.KIND=VARBL THEN
(*2655*)         BEGIN IF SOURCEATTRP^.ACCESS=INDIRECT THEN
(*2656*)         COPYREG(SOURCEATTRP^.INDEXREG)
(*2657*)        END
(*2658*)       ELSE IF SOURCEATTRP^.KIND=EXPR THEN
(*2659*)        BEGIN COPYREG(SOURCEATTRP^.REXPR);
(*2660*)         IF (SOURCEATTRP^.TYPTR^.FORM=POWER) AND (SOURCEATTRP^.REXPR.REGTEMP=REGIST)
(*2661*)           THEN REGISTER(.SUCC(SOURCEATTRP^.REXPR.RNO).).REGCONT:=DESTATTRP;
(*2662*)        END
(*2663*)     END;
(*2664*) 
// $TITLE   CODE HANDLING-MAKECODE,GETCODE
(*2665*)PROCEDURE MAKECODE( LOC,HALF : INTEGER );
(*2666*)  VAR
(*2667*)    LOCSEG : CODESPTR;
(*2668*)    N      : 0..CODEPERSEG;
(*2669*)    DUMMY  : RECORD
(*2670*)               CASE BOOLEAN OF
(*2671*)              TRUE:(A: PACKED ARRAY (.1..4.) OF CHAR);
(*2672*)              FALSE:(X:INTEGER)
(*2673*)             END;
(*2674*) 
(*2675*)BEGIN (* MAKECODE *)
(*2676*)  LOCSEG := CODEPTR(. LOC DIV CODEPERSEG .);   (* PICK UP SEGMENT *)
(*2677*)  IF LOCSEG = NIL THEN   (* PERHAPS NOT CREATED YET *)
(*2678*)  BEGIN
(*2679*)    NEW(LOCSEG{,TRUE});
(*2680*)    CODEPTR(. LOC DIV CODEPERSEG .) := LOCSEG;
(*2681*)  END;   (* NEW SEGMENT NOW CREATED *)
(*2682*)  N := LOC MOD CODEPERSEG;
(*2683*)  DUMMY.X:=HALF;      (* NOW PICK UP HALF WORD *)
(*2684*)  LOCSEG^.BYTES(. N .) := DUMMY.A(. 3 .);
(*2685*)  LOCSEG^.BYTES(. N + 1 .) := DUMMY.A(. 4 .);
(*2686*)END; (*  MAKECODE *)
(*2687*) 
(*2688*) 
(*2689*)FUNCTION GETCODE(LOC : INTEGER):INTEGER;
(*2690*)  VAR
(*2691*)    DUMMY : RECORD
(*2692*)              CASE BOOLEAN OF
(*2693*)                TRUE : (INT:INTEGER);
(*2694*)                FALSE: (CH : PACKED ARRAY(.1..4.) OF CHAR)
(*2695*)              END;
(*2696*)    LOCPTR : CODESPTR;
(*2697*)    X      : INTEGER;
(*2698*)BEGIN (*GETCODE*)
(*2699*)  LOCPTR := CODEPTR(. LOC DIV CODEPERSEG .);
(*2700*)  DUMMY.INT := 0;
(*2701*)  X := LOC MOD CODEPERSEG;
(*2702*)  DUMMY.CH(. 3 .) := LOCPTR^.BYTES(.X.);
(*2703*)  DUMMY.CH(. 4 .) := LOCPTR^.BYTES(.X+1.);
(*2704*)  GETCODE := DUMMY.INT;
(*2705*)END; (* GETCODE *)
(*2706*) 
// $TITLE  CODE GEN-EXCATTR,RESETG,ERRORSET
(*2707*) 
(*2708*) 
(*2709*)    PROCEDURE EXCATTR(F1ATTRP,F2ATTRP:ATTRP);
(*2710*)      VAR ATTRWORK:ATTRP;
(*2711*)      BEGIN ATTRNEW(ATTRWORK); COPYATTR(F1ATTRP,ATTRWORK);
(*2712*)        COPYATTR(F2ATTRP,F1ATTRP); COPYATTR(ATTRWORK,F2ATTRP); ATTRDISP(ATTRWORK)
(*2713*)      END;
(*2714*) 
(*2715*)    PROCEDURE RESETG;
(*2716*)      BEGIN ATTRDISP(GATTRP); ATTRNEW(GATTRP);
(*2717*)        WITH GATTRP^ DO
(*2718*)          BEGIN TYPTR:=NIL; KIND:=CST; END;
(*2719*)      END;
(*2720*) 
(*2721*)    PROCEDURE ERRORRESET(N:INTEGER);
(*2722*)      BEGIN ERROR(N);
(*2723*)            GATTRP^.TYPTR:=NIL;
(*2724*)      END;
(*2725*) 
// $TITLE  CODE GEN - GENRX,GENRXP,GENRR,GENRRP1
(*2726*)    PROCEDURE GENRX(OP,REG,INDEX,BASE,ADDR:INTEGER);
(*2727*)      BEGIN
(*2728*)         IF IC >= 4096*(7-LEVEL)-2 THEN
(*2729*)         BEGIN ERROR(253); IC:=0 END;
(*2730*)         IF (BASE=14) AND (ADDR >= 4096) THEN
(*2731*)         BEGIN EXTENDEDADDRESS:=TRUE; BASE:=LEVEL END;
(*2732*)         MAKECODE(IC,256*OP+16*REG+INDEX);
(*2733*)         MAKECODE(IC+2,4096*BASE+ADDR);
(*2734*)            IC:=IC+4; BOOLFLAG:=FALSE;
(*2735*)      END;
(*2736*) 
(*2737*)    PROCEDURE GENRXP(OP:INTEGER; R:REGNO; INDEX,BASE,ADDR:INTEGER);
(*2738*)      BEGIN GENRX(OP,REALREG(.R.),INDEX,BASE,ADDR);
(*2739*)      END;
(*2740*) 
(*2741*)    PROCEDURE GENRR(OP,R1,R2:INTEGER);
(*2742*)        BEGIN
(*2743*)          IF IC >= 4096*(7-LEVEL) THEN
(*2744*)          BEGIN
(*2745*)            ERROR(253); IC:=0
(*2746*)          END;
(*2747*)          MAKECODE(IC,256*OP+16*R1+R2);
(*2748*)            IC:=IC+2; BOOLFLAG:=FALSE;
(*2749*)      END;
(*2750*) 
(*2751*)    PROCEDURE GENRRP1(OP:INTEGER; R:REGNO);
(*2752*)      BEGIN GENRR(OP,REALREG(.R.),REALREG(.R.));
(*2753*)      END;
(*2754*) 
// $TITLE CODE GEN - GENRRP,GENSS,INSERTIC
(*2755*)    PROCEDURE GENRRP(OP:INTEGER; R1,R2:REGNO);
(*2756*)      BEGIN GENRR(OP,REALREG(.R1.),REALREG(.R2.));
(*2757*)      END;
(*2758*) 
(*2759*)    PROCEDURE GENSS(OP,L,R1,D1,R2,D2:INTEGER);
(*2760*)     BEGIN
(*2761*)        IF IC >= 4096*(7-LEVEL)-4 THEN
(*2762*)        BEGIN ERROR(253); IC:=0 END;
(*2763*)        IF (R2=14) AND (D2 >=4096) THEN
(*2764*)        BEGIN EXTENDEDADDRESS:=TRUE; R2:=LEVEL END;
(*2765*)        MAKECODE(IC,256*OP+L);
(*2766*)        MAKECODE(IC+2,4096*R1+D1);
(*2767*)        MAKECODE(IC+4,4096*R2+D2);
(*2768*)            IC:=IC+6; BOOLFLAG:=FALSE;
(*2769*)      END;
(*2770*) 
(*2771*)    PROCEDURE INSERTIC(FCIX:ADDRRANGE);
(*2772*)     VAR BASE : INTEGER;
(*2773*)      BEGIN
(*2774*)        IF IC >= 4096 THEN
(*2775*)        BEGIN BASE:=LEVEL; EXTENDEDADDRESS:=TRUE
(*2776*)        END
(*2777*)       ELSE BASE:=PBASE1;
(*2778*)        MAKECODE(FCIX+2,4096*BASE+IC);
(*2779*)      END;
(*2780*) 
// $TITLE CODE GEN - LINKOCC,MAKECONST,MKEINTCNST
(*2781*)    PROCEDURE INSERTCHAIN(CHAIN:LOCOFREF);
(*2782*)      BEGIN
(*2783*)        WHILE CHAIN<>NIL DO
(*2784*)          WITH CHAIN^ DO
(*2785*)            BEGIN INSERTIC(LOC); CHAIN:=NXTREF; END;
(*2786*)      END;
(*2787*) 
(*2788*)    PROCEDURE LINKOCC(VAR FPTR: LOCOFREF; FCIX: ADDRRANGE);
(*2789*)      VAR LOCP: LOCOFREF;
(*2790*)      BEGIN NEW(LOCP);
(*2791*)        WITH LOCP^ DO
(*2792*)          BEGIN NXTREF:=FPTR; LOC:=FCIX; END;
(*2793*)        FPTR:=LOCP;
(*2794*)      END;
(*2795*) 
(*2796*)    PROCEDURE MAKECONSTANT(X:VALU);
(*2797*)      LABEL 1;
(*2798*)      VAR EQUAL:BOOLEAN; P,Q:CTAILP; C:CONSTCHAIN;
(*2799*)      BEGIN C:=CONSTTOP;
(*2800*)        WHILE C<>NIL DO
(*2801*)          WITH C^ DO
(*2802*)            BEGIN
(*2803*)              IF SAVECONST.CKIND=X.CKIND THEN
(*2804*)                BEGIN CASE X.CKIND OF
(*2805*)                  INT: EQUAL:=(X.IVAL=SAVECONST.IVAL);
(*2806*)                  REEL:EQUAL:=(X.RVAL=SAVECONST.RVAL);
(*2807*)                  PSET:EQUAL:=(X.PVAL=SAVECONST.PVAL);
(*2808*)                  STRG:BEGIN EQUAL:=TRUE;
(*2809*)                         P:=X.VALP; Q:=SAVECONST.VALP;
(*2810*)                         WHILE EQUAL AND (P<>NIL) AND (Q<>NIL) DO
(*2811*)                           BEGIN EQUAL:=(P^.STFR=Q^.STFR);
(*2812*)                             P:=P^.NXTCSP; Q:=Q^.NXTCSP;
(*2813*)                           END;
(*2814*)                         EQUAL:=EQUAL AND (P=Q);
(*2815*)                       END
(*2816*)                  END;
(*2817*)                  IF EQUAL THEN
(*2818*)                    BEGIN LINKOCC(CCHAIN,IC); GOTO 1; END;
(*2819*)                END;
(*2820*)              C:=C^.NEXTCONST;
(*2821*)            END;
(*2822*)        NEW(C);
(*2823*)        WITH C^ DO
(*2824*)          BEGIN SAVECONST:=X; CCHAIN:=NIL;
(*2825*)                NEXTCONST:=CONSTTOP; LINKOCC(CCHAIN,IC);
(*2826*)          END;
(*2827*)        CONSTTOP:=C;
(*2828*)   1: END;
(*2829*) 
(*2830*)    PROCEDURE MAKEINTCONST(N:INTEGER);
(*2831*)      VAR X:VALU;
(*2832*)      BEGIN X.CKIND:=INT; X.IVAL:=N;
(*2833*)            MAKECONSTANT(X);
(*2834*)      END;
(*2835*) 
// $TITLE CODE GEN - GETTEMP,DELTEMP,USING
(*2836*)    PROCEDURE GETTEMP(LENGTH:INTEGER; VAR X:CMP);
(*2837*)      LABEL 1,2;
(*2838*)      VAR P,Q:CMP;
(*2839*)      BEGIN Q:=NIL; P:=FREETEMP;
(*2840*)        WHILE P<>NIL DO
(*2841*)          IF P^.TEMPLNGTH=LENGTH THEN GOTO 1
(*2842*)          ELSE BEGIN Q:=P; P:=P^.NEXTTEMP END;
(*2843*)        NEW(P);
(*2844*)        ALIGNMENT(LC,LENGTH);
(*2845*)        P^.TEMPADRS:=LC; LC:=LC+LENGTH;
(*2846*)        P^.TEMPLNGTH:=LENGTH; GOTO 2;
(*2847*)     1: IF Q=NIL THEN FREETEMP:=P^.NEXTTEMP
(*2848*)                 ELSE Q^.NEXTTEMP:=P^.NEXTTEMP;
(*2849*)     2: X:=P;
(*2850*)      END;
(*2851*) 
(*2852*)    PROCEDURE DELETETEMP(X:CMP);
(*2853*)      BEGIN X^.NEXTTEMP:=FREETEMP; FREETEMP:=X;
(*2854*)      END;
(*2855*) 
(*2856*)    FUNCTION USING(R:REGNO; FATTRP:ATTRP):BOOLEAN;      (*CHECK IF R IS OCCUPIED BY FATTRP*)
(*2857*)      BEGIN IF FATTRP=NIL THEN USING:=FALSE
(*2858*)        ELSE
(*2859*)          BEGIN WITH FATTRP^ DO CASE KIND OF
(*2860*)            CST:   USING:=FALSE;
(*2861*)            VARBL: IF ACCESS=INDIRECT THEN IF INDEXREG.REGTEMP=REGIST
(*2862*)                       THEN USING:=(R=INDEXREG.RNO)
(*2863*)                      ELSE USING:=FALSE
(*2864*)                     ELSE USING:=FALSE;
(*2865*)            EXPR:  IF REXPR.REGTEMP=REGIST
(*2866*)                      THEN USING:=(REGISTER(.R.).REGCONT=FATTRP)
(*2867*)                      ELSE USING:=FALSE
(*2868*)            END;
(*2869*)          END;
(*2870*)      END;
(*2871*) 
// $TITLE CODE GEN - DISPLCMNT,BASEREG,SAVE
(*2872*)    PROCEDURE DISPLACEMENT(ADRS:INTEGER; VAR REM:INTEGER);
(*2873*)      VAR I:INTEGER;
(*2874*)      BEGIN
(*2875*)        IF ADRS>=0 THEN I:=ADRS DIV 4096*4096
(*2876*)                   ELSE I:=((ADRS+1) DIV 4096-1)*4096;
(*2877*)        MAKEINTCONST(I); REM:=ADRS-I;
(*2878*)      END;
(*2879*) 
(*2880*)    PROCEDURE BASEREGISTER(LEVEL:LEVRANGE; ADRS:ADDRRANGE);
(*2881*)      BEGIN
(*2882*)        IF (ADRS>=4096) OR (ADRS<0) THEN
(*2883*)          BEGIN DISPLACEMENT(ADRS,EFFADRS); GENRX(ZL,BASEWORK,0,0,0);
(*2884*)                IF LEVEL<>0 THEN GENRR(ZAR,BASEWORK,LEVEL);
(*2885*)                RBASE:=BASEWORK;
(*2886*)          END
(*2887*)        ELSE BEGIN EFFADRS:=ADRS; RBASE:=LEVEL; END;
(*2888*)      END;
(*2889*) 
(*2890*)    PROCEDURE SAVE(R:REGNO);
(*2891*)      VAR TEMP:CMP;
(*2892*)      BEGIN IF REGISTER(.R.).USED THEN
(*2893*)        BEGIN
(*2894*)          IF R>=F0 THEN
(*2895*)            BEGIN GETTEMP(8,TEMP);
(*2896*)                  BASEREGISTER(LEVEL,TEMP^.TEMPADRS);
(*2897*)                  GENRXP(ZSTD,R,0,RBASE,EFFADRS)
(*2898*)            END
(*2899*)          ELSE IF (REGISTER(.R.).REGCONT^.TYPTR^.FORM=POWER)
(*2900*)               AND (REGISTER(.R.).REGCONT^.KIND=EXPR) THEN
(*2901*)            BEGIN GETTEMP(8,TEMP);
(*2902*)                  BASEREGISTER(LEVEL,TEMP^.TEMPADRS);
(*2903*)                  GENRXP(ZSTM,R,REALREG(.SUCC(R).),RBASE,EFFADRS);
(*2904*)                  REGISTER(.SUCC(R).).USED:=FALSE
(*2905*)            END
(*2906*)          ELSE BEGIN GETTEMP(4,TEMP);
(*2907*)                     BASEREGISTER(LEVEL,TEMP^.TEMPADRS);
(*2908*)                     GENRXP(ZST,R,0,RBASE,EFFADRS)
(*2909*)               END;
(*2910*)          REGISTER(.R.).USED:=FALSE;
(*2911*)          TEMP^.TEMPCONT:=REGISTER(.R.).REGCONT;
(*2912*)          WITH REGISTER(.R.).REGCONT^ DO
(*2913*)            IF KIND=EXPR THEN
(*2914*)              BEGIN REXPR.REGTEMP:=TEMPORARY;
(*2915*)                    REXPR.ATEMP:=TEMP
(*2916*)              END
(*2917*)            ELSE BEGIN INDEXREG.REGTEMP:=TEMPORARY;
(*2918*)                       INDEXREG.ATEMP:=TEMP
(*2919*)                 END
(*2920*)        END;
(*2921*)      END;
(*2922*) 
// $TITLE CODE GEN - REGSEARCH,LOADINDX,LDBASE
(*2923*)    PROCEDURE REGSEARCH(FATTRP:ATTRP; T:REGKIND);
              Var Reg: regno;
(*2924*)      LABEL 1;
(*2925*)      BEGIN CASE T OF
(*2926*)        SINGLE: BEGIN FOR Reg :=R10 TO R13 DO begin
                                      rwork :=reg;
(*2927*)                         IF NOT REGISTER(.RWORK.).USED THEN GOTO 1;
                                    end;
(*2928*)                      FOR Reg:=R10 TO R13 DO begin
                                  rwork :=reg;
(*2929*)                         IF NOT USING(RWORK,FATTRP) THEN BEGIN SAVE(RWORK); GOTO 1 END;
                                    end;
(*2930*)                      ERROR(400);
(*2931*)                 END;
(*2932*)        FLOAT: BEGIN FOR Reg:=F0 TO F6 DO begin
                                      rwork :=reg;
(*2933*)                         IF NOT REGISTER(.RWORK.).USED THEN GOTO 1;
                                 end;
(*2934*)                     FOR Reg:=F0 TO F6 DO  begin
                                      rwork :=reg;
(*2935*)                         IF NOT USING(RWORK,FATTRP) THEN BEGIN SAVE(RWORK); GOTO 1 END;
                                 end;
(*2936*)                     ERROR(400);
(*2937*)               END;
(*2938*)        DOUBLE: IF NOT(REGISTER(.R10.).USED OR REGISTER(.R11.).USED) THEN RWORK:=R10
(*2939*)                  ELSE IF NOT(REGISTER(.R12.).USED OR REGISTER(.R13.).USED) THEN RWORK:=R12
(*2940*)                  ELSE IF NOT(USING(R10,FATTRP) OR USING(R11,FATTRP)) THEN
(*2941*)                           BEGIN SAVE(R10); SAVE(R11); RWORK:=R10 END
(*2942*)                  ELSE BEGIN SAVE(R12); SAVE(R13); RWORK:=R12 END
(*2943*)           END;
(*2944*)     1: RMAIN:=REALREG(.RWORK.);
(*2945*)      END;
(*2946*) 
(*2947*)    PROCEDURE LOADINDEX(F1ATTRP,F2ATTRP:ATTRP);
(*2948*)      BEGIN
(*2949*)        WITH F1ATTRP^ DO
(*2950*)          BEGIN
(*2951*)            IF ACCESS=DIRECT THEN RINDEX:=0
(*2952*)              ELSE IF INDEXREG.REGTEMP=REGIST THEN
(*2953*)                BEGIN REGISTER(.INDEXREG.RNO.).USED:=FALSE;
(*2954*)                      RINDEX:=REALREG(.INDEXREG.RNO.); RWORK:=INDEXREG.RNO;
(*2955*)                END
(*2956*)              ELSE WITH INDEXREG.ATEMP^ DO
(*2957*)                BEGIN REGSEARCH(F2ATTRP,SINGLE); BASEREGISTER(LEVEL,TEMPADRS);
(*2958*)                      GENRX(ZL,RMAIN,0,RBASE,EFFADRS);
(*2959*)                      RINDEX:=RMAIN; DELETETEMP(INDEXREG.ATEMP);
(*2960*)                END;
(*2961*)          END;
(*2962*)      END;
(*2963*) 
(*2964*)    PROCEDURE LOADBASE(FATTRP:ATTRP);
(*2965*)      BEGIN
(*2966*)        WITH FATTRP^ DO
(*2967*)          BEGIN
(*2968*)            IF VARKIND=DRCT
(*2969*)              THEN BASEREGISTER(VLEVEL,VADRS)
(*2970*)              ELSE
(*2971*)                BEGIN
(*2972*)                  BASEREGISTER(BASELEV,BASEADD);
(*2973*)                  GENRX(ZL,BASEWORK,0,RBASE,EFFADRS);
(*2974*)                  RBASE:=BASEWORK;
(*2975*)                  IF (VADRS>=4096) OR (VADRS<0) THEN
(*2976*)                    BEGIN DISPLACEMENT(VADRS,EFFADRS); GENRX(ZA,BASEWORK,0,0,0); END
(*2977*)                  ELSE EFFADRS:=VADRS;
(*2978*)                END;
(*2979*)          END;
(*2980*)      END;
// $TITLE   LOADINTCONST
type RegType = 0..15;
(*2981*)    PROCEDURE LOADINTCONST(REG:RegType; VAL:INTEGER);
(*2982*)      BEGIN
(*2983*)        IF VAL=0
(*2984*)          THEN GENRR(ZXR,REG,REG)
(*2985*)          ELSE IF (VAL>0) AND (VAL<4096)
(*2986*)            THEN GENRX(ZLA,REG,0,0,VAL)
(*2987*)            ELSE BEGIN MAKEINTCONST(VAL);
(*2988*)                       GENRX(ZL,REG,0,0,0);
(*2989*)                 END;
(*2990*)      END;
(*2991*) 
// $TITLE  LOAD
(*2992*)    PROCEDURE LOAD(F1ATTRP,F2ATTRP:ATTRP);
(*2993*)      VAR RKIND:REGKIND; LOADOP:INTEGER;
(*2994*)      BEGIN WITH F1ATTRP^ DO
(*2995*)        BEGIN
(*2996*)          IF (KIND<>EXPR) OR (REXPR.REGTEMP<>REGIST) THEN
(*2997*)            BEGIN
(*2998*)              IF TYPTR^.FORM=POWER THEN RKIND:=DOUBLE
(*2999*)                ELSE IF COMPTYPES(TYPTR,REALPTR)
(*3000*)                  THEN BEGIN RKIND:=FLOAT; LOADOP:=ZLD; END
(*3001*)                  ELSE BEGIN RKIND:=SINGLE; LOADOP:=ZL; END;
(*3002*)              CASE KIND OF
(*3003*)          CST:   BEGIN REGSEARCH(F2ATTRP,RKIND);
(*3004*)                   IF RKIND=SINGLE
(*3005*)                     THEN LOADINTCONST(RMAIN,CVAL.IVAL)
(*3006*)                     ELSE BEGIN MAKECONSTANT(CVAL);
(*3007*)                            IF RKIND=DOUBLE THEN GENRX(ZLM,RMAIN,RMAIN+1,0,0)
(*3008*)                                            ELSE GENRX(LOADOP,RMAIN,0,0,0);
(*3009*)                          END;
(*3010*)                 END;
(*3011*)          VARBL: BEGIN LOADINDEX(F1ATTRP,F2ATTRP); REGSEARCH(F2ATTRP,RKIND);
(*3012*)                   LOADBASE(F1ATTRP);
(*3013*)                   IF RKIND=DOUBLE THEN
(*3014*)         BEGIN
(*3015*)           IF RINDEX=0 THEN
(*3016*)              GENRX(ZLM,RMAIN,RMAIN+1,RBASE,EFFADRS) ELSE
(*3017*)                     IF RINDEX=RMAIN
(*3018*)                       THEN BEGIN GENRX(ZL,RMAIN+1,RINDEX,RBASE,EFFADRS+4);
(*3019*)                                  GENRX(ZL,RMAIN,RINDEX,RBASE,EFFADRS);
(*3020*)                            END
(*3021*)                       ELSE BEGIN GENRX(ZL,RMAIN,RINDEX,RBASE,EFFADRS);
(*3022*)                                  GENRX(ZL,RMAIN+1,RINDEX,RBASE,EFFADRS+4);
(*3023*)                            END
(*3024*)         END
(*3025*)                   ELSE IF TYPTR^.SIZE.WBLENGTH=1
(*3026*)                     THEN BEGIN GENRX(ZIC,RMAIN,RINDEX,RBASE,EFFADRS);
(*3027*)                                MAKEINTCONST(255); GENRX(ZN,RMAIN,0,0,0);
(*3028*)                          END
(*3029*)                     ELSE GENRX(LOADOP,RMAIN,RINDEX,RBASE,EFFADRS);
(*3030*)                 END;
(*3031*)          EXPR:  BEGIN REGSEARCH(F2ATTRP,RKIND);
(*3032*)                   BASEREGISTER(LEVEL,REXPR.ATEMP^.TEMPADRS);
(*3033*)                   IF RKIND=DOUBLE
(*3034*)                     THEN GENRX(ZLM,RMAIN,RMAIN+1,RBASE,EFFADRS)
(*3035*)                     ELSE GENRX(LOADOP,RMAIN,0,RBASE,EFFADRS);
(*3036*)                   DELETETEMP(REXPR.ATEMP);
(*3037*)                 END
(*3038*)              END; (*CASE*)
(*3039*)              KIND:=EXPR; REXPR.REGTEMP:=REGIST; REXPR.RNO:=RWORK;
(*3040*)              REGISTER(.RWORK.).USED:=TRUE; REGISTER(.RWORK.).REGCONT:=F1ATTRP;
(*3041*)              IF RKIND=DOUBLE THEN
(*3042*)                BEGIN REGISTER(.SUCC(RWORK).).USED:=TRUE;
(*3043*)                      REGISTER(.SUCC(RWORK).).REGCONT:=F1ATTRP;
(*3044*)                END;
(*3045*)            END;
(*3046*)        END;
(*3047*)      END;
(*3048*) 
// $TITLE  LOADEVENODD,LOADADDRESS
(*3049*)    PROCEDURE LOADEVENODD(F1ATTRP,F2ATTRP:ATTRP; SWITCH:INTEGER);    (*SWITCH=0: EVEN, 1: ODD*)
(*3050*)      BEGIN WITH F1ATTRP^ DO
(*3051*)        BEGIN CASE KIND OF
(*3052*)          CST:  BEGIN REGSEARCH(F2ATTRP,DOUBLE);
(*3053*)                      LOADINTCONST(RMAIN+SWITCH,CVAL.IVAL);
(*3054*)                END;
(*3055*)          VARBL:BEGIN LOADINDEX(F1ATTRP,F2ATTRP); REGSEARCH(F2ATTRP,DOUBLE);
(*3056*)                  LOADBASE(F1ATTRP);
(*3057*)                  IF TYPTR^.SIZE.WBLENGTH=1
(*3058*)                    THEN BEGIN GENRX(ZIC,RMAIN+SWITCH,RINDEX,RBASE,EFFADRS);
(*3059*)                               MAKEINTCONST(255); GENRX(ZN,RMAIN+SWITCH,0,0,0);
(*3060*)                         END
(*3061*)                    ELSE GENRX(ZL,RMAIN+SWITCH,RINDEX,RBASE,EFFADRS);
(*3062*)                END;
(*3063*)          EXPR: IF REXPR.REGTEMP=REGIST
(*3064*)                  THEN BEGIN REGISTER(.REXPR.RNO.).USED:=FALSE;
(*3065*)                             REGSEARCH(F2ATTRP,DOUBLE);
(*3066*)                             IF RMAIN+SWITCH<>REALREG(.REXPR.RNO.) THEN
(*3067*)                               GENRR(ZLR,RMAIN+SWITCH,REALREG(.REXPR.RNO.));
(*3068*)                       END
(*3069*)                  ELSE BEGIN REGSEARCH(F2ATTRP,DOUBLE);
(*3070*)                             BASEREGISTER(LEVEL,REXPR.ATEMP^.TEMPADRS);
(*3071*)                             GENRX(ZL,RMAIN+SWITCH,0,RBASE,EFFADRS);
(*3072*)                             DELETETEMP(REXPR.ATEMP);
(*3073*)                       END
(*3074*)          END;
(*3075*)          IF SWITCH=1 THEN RWORK:=SUCC(RWORK);
(*3076*)          KIND:=EXPR; REXPR.REGTEMP:=REGIST; REXPR.RNO:=RWORK;
(*3077*)          REGISTER(.RWORK.).USED:=TRUE; REGISTER(.RWORK.).REGCONT:=F1ATTRP;
(*3078*)        END;
(*3079*)      END;
(*3080*) 
(*3081*)    PROCEDURE LOADADDRESS(F1ATTRP,F2ATTRP:ATTRP);
(*3082*)     VAR SWITCH:BOOLEAN;
(*3083*)     BEGIN
(*3084*)       SWITCH:=FALSE;
(*3085*)       WITH F1ATTRP^ DO
(*3086*)        CASE KIND OF
(*3087*)          EXPR: ERROR(400);
(*3088*)          CST:  BEGIN REGSEARCH(F2ATTRP,SINGLE); MAKECONSTANT(CVAL);
(*3089*)                      GENRX(ZLA,RMAIN,0,0,0);
(*3090*)                END;
(*3091*)          VARBL:BEGIN LOADINDEX(F1ATTRP,F2ATTRP);
(*3092*)                  IF RINDEX=0
(*3093*)                    THEN
(*3094*)                      BEGIN REGSEARCH(F2ATTRP,SINGLE);
(*3095*)                            IF VARKIND=DRCT
(*3096*)                              THEN BEGIN IF VLEVEL=0 THEN ERROR(400)
(*3097*)                             ELSE
(*3098*)                            IF (VADRS<4096) AND (VADRS>0) THEN
(*3099*)                            BEGIN SWITCH:=TRUE;
(*3100*)                              GENRX(ZLA,RMAIN,0,VLEVEL,VADRS)
(*3101*)                            END ELSE GENRR(ZLR,RMAIN,VLEVEL);
(*3102*)                                   END
(*3103*)                              ELSE
(*3104*)                                BEGIN BASEREGISTER(BASELEV,BASEADD);
(*3105*)                                      GENRX(ZL,RMAIN,0,RBASE,EFFADRS);
(*3106*)                                END;
(*3107*)                      END
(*3108*)                    ELSE
(*3109*)                      BEGIN RMAIN:=RINDEX;
(*3110*)                        IF VARKIND=DRCT THEN
(*3111*)                          BEGIN IF VLEVEL<>0 THEN GENRR(ZAR,RMAIN,VLEVEL); END
(*3112*)                        ELSE BEGIN BASEREGISTER(BASELEV,BASEADD);
(*3113*)                                   GENRX(ZA,RMAIN,0,RBASE,EFFADRS);
(*3114*)                             END;
(*3115*)                      END;
(*3116*)                 IF (VADRS<>0) AND (NOT SWITCH) THEN
(*3117*)                     BEGIN MAKEINTCONST(VADRS); GENRX(ZA,RMAIN,0,0,0); END;
(*3118*)                END
(*3119*)            END;
(*3120*)        WITH F1ATTRP^ DO
(*3121*)          BEGIN TYPTR:=INTPTR; KIND:=EXPR;
(*3122*)                REXPR.REGTEMP:=REGIST; REXPR.RNO:=RWORK;
(*3123*)          END;
(*3124*)        REGISTER(.RWORK.).USED:=TRUE; REGISTER(.RWORK.).REGCONT:=F1ATTRP;
(*3125*)      END;
(*3126*) 
// $TITLE  SETOPERATION,SETOP1
(*3127*)    PROCEDURE SETOPERATION(F1ATTRP,F2ATTRP:ATTRP; OPRX1,OPRR1,OPRX2,OPRR2:INTEGER);
(*3128*) 
(*3129*)     PROCEDURE SETOP1(OPRX,OPRR:INTEGER);
(*3130*)       VAR A1,A2:INTEGER;
(*3131*)       BEGIN
(*3132*)         WITH F2ATTRP^ DO CASE KIND OF
(*3133*)           CST:   BEGIN SETVALUE(CVAL.PVAL,A1,A2); MAKEINTCONST(A1);
(*3134*)                    GENRXP(OPRX,F1ATTRP^.REXPR.RNO,0,0,0);
(*3135*)                    IF OPRX=ZCL THEN GENRX(ZBC,CONDNZ,0,PBASE1,IC+8);
(*3136*)                    MAKEINTCONST(A2); GENRXP(OPRX,SUCC(F1ATTRP^.REXPR.RNO),0,0,0);
(*3137*)                  END;
(*3138*)           VARBL: BEGIN GENRXP(OPRX,F1ATTRP^.REXPR.RNO,RINDEX,RBASE,EFFADRS);
(*3139*)                    IF OPRX=ZCL THEN GENRX(ZBC,CONDNZ,0,PBASE1,IC+8);
(*3140*)                    GENRXP(OPRX,SUCC(F1ATTRP^.REXPR.RNO),RINDEX,RBASE,EFFADRS+4);
(*3141*)                  END;
(*3142*)           EXPR:  IF REXPR.REGTEMP=REGIST
(*3143*)                    THEN BEGIN GENRRP(OPRR,F1ATTRP^.REXPR.RNO,REXPR.RNO);
(*3144*)                           IF OPRX=ZCL THEN GENRX(ZBC,CONDNZ,0,PBASE1,IC+6);
(*3145*)                           GENRRP(OPRR,SUCC(F1ATTRP^.REXPR.RNO),SUCC(REXPR.RNO));
(*3146*)                         END
(*3147*)                    ELSE BEGIN GENRXP(OPRX,F1ATTRP^.REXPR.RNO,0,RBASE,EFFADRS);
(*3148*)                           IF OPRX=ZCL THEN GENRX(ZBC,CONDNZ,0,PBASE1,IC+8);
(*3149*)                           GENRXP(OPRX,SUCC(F1ATTRP^.REXPR.RNO),0,RBASE,EFFADRS+4);
(*3150*)                         END
(*3151*)         END;
(*3152*)       END;
(*3153*) 
(*3154*)      BEGIN
(*3155*)        WITH F2ATTRP^ DO
(*3156*)          IF KIND=VARBL THEN
(*3157*)            BEGIN LOADINDEX(F2ATTRP,F1ATTRP); LOADBASE(F2ATTRP); END
(*3158*)          ELSE IF KIND=EXPR THEN
(*3159*)            IF REXPR.REGTEMP<>REGIST THEN
(*3160*)              BASEREGISTER(LEVEL,REXPR.ATEMP^.TEMPADRS);
(*3161*)        SETOP1(OPRX1,OPRR1);
(*3162*)        IF OPRX2<>0 THEN SETOP1(OPRX2,OPRR2);
(*3163*)      END;
(*3164*) 
// $TITLE  OPERATION
(*3165*)    PROCEDURE OPERATION(F1ATTRP,F2ATTRP:ATTRP; OPRX,OPRR:INTEGER);
(*3166*)      BEGIN
(*3167*)        WITH F2ATTRP^ DO
(*3168*)          BEGIN
(*3169*)            IF KIND=VARBL THEN IF TYPTR^.SIZE.WBLENGTH=1
(*3170*)              THEN LOAD(F2ATTRP,F1ATTRP);
(*3171*)            CASE KIND OF
(*3172*)             CST:   BEGIN MAKECONSTANT(CVAL);
(*3173*)                      GENRXP(OPRX,F1ATTRP^.REXPR.RNO,0,0,0)
(*3174*)                    END;
(*3175*)             VARBL: BEGIN LOADINDEX(F2ATTRP,F1ATTRP); LOADBASE(F2ATTRP);
(*3176*)                      GENRXP(OPRX,F1ATTRP^.REXPR.RNO,RINDEX,RBASE,EFFADRS);
(*3177*)                    END;
(*3178*)             EXPR: IF REXPR.REGTEMP=REGIST
(*3179*)                     THEN GENRRP(OPRR,F1ATTRP^.REXPR.RNO,REXPR.RNO)
(*3180*)                     ELSE BEGIN BASEREGISTER(LEVEL,REXPR.ATEMP^.TEMPADRS);
(*3181*)                                GENRXP(OPRX,F1ATTRP^.REXPR.RNO,0,RBASE,EFFADRS)
(*3182*)                          END
(*3183*)          END;
(*3184*)        END;
(*3185*)      END;
(*3186*) 
// $TITLE  INTTOREAL,INTARITH
(*3187*)    PROCEDURE INTTOREAL(FATTRP : ATTRP);
(*3188*)      BEGIN
(*3189*)        LOAD(FATTRP,NIL);
(*3190*)        GENRR(ZLPR,R0,REALREG(.FATTRP^.REXPR.RNO.));
(*3191*)        GENRX(ZST,R0,0,1,IRCONVWORK+4); REGSEARCH(NIL,FLOAT);
(*3192*)        GENRR(ZSDR,RMAIN,RMAIN); GENRX(ZAD,RMAIN,0,1,IRCONVWORK);
(*3193*)        GENRRP1(ZLTR,FATTRP^.REXPR.RNO); GENRX(ZBC,CONDNM,0,PBASE1,IC+6);
(*3194*)        GENRR(ZLNDR,RMAIN,RMAIN);
(*3195*)        WITH FATTRP^ DO
(*3196*)          BEGIN REGISTER(.REXPR.RNO.).USED:=FALSE;
(*3197*)                TYPTR:=REALPTR; REXPR.RNO:=RWORK;
(*3198*)          END;
(*3199*)        REGISTER(.RWORK.).USED:=TRUE; REGISTER(.RWORK.).REGCONT:=FATTRP;
(*3200*)      END;
(*3201*) 
(*3202*)     (*IN THE FOLLOWING ROUTINES CONSIDER THE RULES:
  3203         - IF A ROUTINE HAS TWO ARGUMENTS 'F1ATTRP' AND 'F2ATTRP', THE ARGUMENTS
  3204           HAVE TO BE TAKEN IN THIS ORDER. THE DESCRIPTION OF THE RESULT IS
  3205           ALWAYS TO BE PUT IN 'F2ATTRP^'.
  3206         - IF A ROUTINE HAS ONE ARGUMENT 'FATTRP' THE DESCRIPTION OF THE RESULT
  3207           HAS TO REPLACE THE DESCRIPTION OF THE ARGUMENT IN 'FATTRP^' *)
(*3208*) 
(*3209*)    PROCEDURE INTARITH(F1ATTRP,F2ATTRP:ATTRP; FOP:&OPERATOR);
(*3210*)      VAR X:INTEGER;
(*3211*)      BEGIN
(*3212*)        IF FOP IN (.PLUS,MUL.) THEN
(*3213*)          IF F2ATTRP^.KIND=EXPR THEN
(*3214*)            IF F2ATTRP^.REXPR.REGTEMP=REGIST THEN EXCATTR(F1ATTRP,F2ATTRP);
(*3215*)        CASE FOP OF
(*3216*)          PLUS:  BEGIN X:=0; LOAD(F1ATTRP,F2ATTRP); END;
(*3217*)          MINUS: BEGIN X:=ZS-ZA; LOAD(F1ATTRP,F2ATTRP); END;
(*3218*)          MUL:   BEGIN X:=ZM-ZA; LOADEVENODD(F1ATTRP,F2ATTRP,1); END;
(*3219*)          IDIV,IMOD: BEGIN X:=ZD-ZA; LOADEVENODD(F1ATTRP,F2ATTRP,0);
(*3220*)                           GENRXP(ZSRDA,F1ATTRP^.REXPR.RNO,0,0,32);
(*3221*)                     END
(*3222*)        END;
(*3223*)        OPERATION(F1ATTRP,F2ATTRP,ZA+X,ZAR+X);
(*3224*)        IF FOP=MUL THEN
(*3225*)          IF (F2ATTRP^.KIND=EXPR) AND (F2ATTRP^.REXPR.REGTEMP=REGIST)
(*3226*)          THEN MAKECODE(IC-2,GETCODE(IC-2)-16)
(*3227*)          ELSE MAKECODE(IC-4,GETCODE(IC-4)-16);
(*3228*)        IF FOP=IDIV THEN
(*3229*)          WITH F1ATTRP^.REXPR DO
(*3230*)            BEGIN REGISTER(.SUCC(RNO).):=REGISTER(.RNO.);
(*3231*)                  REGISTER(.RNO.).USED:=FALSE; RNO:=SUCC(RNO);
(*3232*)            END;
(*3233*)        EXCATTR(F1ATTRP,F2ATTRP);
(*3234*)      END;
(*3235*) 
// $TITLE  REALARITH,SETARITH,NEGATE,NOTFACTOR
(*3236*)    PROCEDURE REALARITH(F1ATTRP,F2ATTRP: ATTRP; FOP: &OPERATOR);
(*3237*)      VAR X:INTEGER;
(*3238*)      BEGIN
(*3239*)        IF COMPTYPES(F1ATTRP^.TYPTR,INTPTR) THEN INTTOREAL(F1ATTRP);
(*3240*)        IF COMPTYPES(F2ATTRP^.TYPTR,INTPTR) THEN INTTOREAL(F2ATTRP);
(*3241*)        IF FOP IN (.PLUS,MUL.) THEN
(*3242*)          IF F2ATTRP^.KIND=EXPR THEN
(*3243*)            IF F2ATTRP^.REXPR.REGTEMP=REGIST THEN EXCATTR(F1ATTRP,F2ATTRP);
(*3244*)        LOAD(F1ATTRP,F2ATTRP);
(*3245*)        CASE FOP OF
(*3246*)          PLUS:  X:=0;
(*3247*)          MINUS: X:=ZSD-ZAD;
(*3248*)          MUL:   X:=ZMD-ZAD;
(*3249*)          RDIV:  X:=ZDD-ZAD
(*3250*)        END;
(*3251*)        OPERATION(F1ATTRP,F2ATTRP,ZAD+X,ZADR+X);
(*3252*)        EXCATTR(F1ATTRP,F2ATTRP);
(*3253*)      END;
(*3254*) 
(*3255*)    PROCEDURE SETARITH(F1ATTRP,F2ATTRP: ATTRP; FOP: &OPERATOR);
(*3256*)      VAR X:INTEGER;
(*3257*)      BEGIN
(*3258*)        IF FOP<>MINUS THEN
(*3259*)          BEGIN IF F2ATTRP^.KIND=EXPR THEN
(*3260*)              IF F2ATTRP^.REXPR.REGTEMP=REGIST THEN EXCATTR(F1ATTRP,F2ATTRP);
(*3261*)            IF FOP=MUL THEN X:=0 ELSE X:=ZO-ZN;
(*3262*)            LOAD(F1ATTRP,F2ATTRP);
(*3263*)            SETOPERATION(F1ATTRP,F2ATTRP,ZN+X,ZNR+X,0,0);
(*3264*)            EXCATTR(F1ATTRP,F2ATTRP);
(*3265*)          END
(*3266*)        ELSE (*FOP=MINUS*)
(*3267*)          BEGIN LOAD(F2ATTRP,F1ATTRP);
(*3268*)            SETOPERATION(F2ATTRP,F1ATTRP,ZN,ZNR,ZX,ZXR);
(*3269*)          END;
(*3270*)      END;
(*3271*) 
(*3272*)    PROCEDURE NEGATE(FATTRP: ATTRP);
(*3273*)      BEGIN
(*3274*)        WITH FATTRP^ DO
(*3275*)          IF KIND=CST
(*3276*)            THEN IF COMPTYPES(TYPTR,INTPTR)
(*3277*)              THEN CVAL.IVAL:=-CVAL.IVAL
(*3278*)              ELSE CVAL.RVAL:=-CVAL.RVAL
(*3279*)            ELSE
(*3280*)              BEGIN LOAD(FATTRP,NIL);
(*3281*)                IF COMPTYPES(TYPTR,INTPTR)
(*3282*)                  THEN GENRRP1(ZLCR,REXPR.RNO)
(*3283*)                  ELSE GENRRP1(ZLCDR,REXPR.RNO);
(*3284*)              END;
(*3285*)      END;
(*3286*) 
(*3287*)    PROCEDURE NOTFACTOR(FATTRP: ATTRP);
(*3288*)      BEGIN
(*3289*)        LOAD(FATTRP,NIL);
(*3290*)        IF BOOLFLAG THEN
(*3291*)          MAKECODE(IC-6,256*ZBC+240 -(GETCODE(IC-6) MOD 256))
(*3292*)        ELSE BEGIN MAKEINTCONST(1); GENRXP(ZX,FATTRP^.REXPR.RNO,0,0,0); END;
(*3293*)      END;
// $TITLE BOOLARITH,BOOLVALUE,RELINT,RELREAL,INPWR
(*3294*)    PROCEDURE BOOLARITH(F1ATTRP,F2ATTRP: ATTRP; FOP: &OPERATOR);
(*3295*)      VAR X:INTEGER;
(*3296*)      BEGIN
(*3297*)        IF F2ATTRP^.KIND=EXPR THEN EXCATTR(F1ATTRP,F2ATTRP);
(*3298*)        LOAD(F1ATTRP,F2ATTRP);
(*3299*)        IF FOP=ANDOP THEN X:=0 ELSE X:=ZO-ZN;
(*3300*)        OPERATION(F1ATTRP,F2ATTRP,ZN+X,ZNR+X);
(*3301*)        EXCATTR(F1ATTRP,F2ATTRP);
(*3302*)      END;
(*3303*) 
(*3304*)    PROCEDURE BOOLVALUE(REG,TRUECOND: INTEGER);
(*3305*)      BEGIN GENRX(ZLA,REG,0,0,1);
(*3306*)            GENRX(ZBC,TRUECOND,0,PBASE1,IC+6);
(*3307*)            GENRR(ZXR,REG,REG); BOOLFLAG:=TRUE;
(*3308*)      END;
(*3309*) 
(*3310*)    PROCEDURE RELINT(F1ATTRP,F2ATTRP: ATTRP; FOP: &OPERATOR);
(*3311*)      BEGIN
(*3312*)        IF F2ATTRP^.KIND=EXPR THEN
(*3313*)          BEGIN FOP:=DUALOP(.FOP.); EXCATTR(F1ATTRP,F2ATTRP);
(*3314*)          END;
(*3315*)        LOAD(F1ATTRP,F2ATTRP);
(*3316*)        OPERATION(F1ATTRP,F2ATTRP,ZC,ZCR);
(*3317*)        BOOLVALUE(REALREG(.F1ATTRP^.REXPR.RNO.),BMASK(.FOP.));
(*3318*)        F1ATTRP^.TYPTR := BOOLPTR;
(*3319*)        EXCATTR(F1ATTRP,F2ATTRP);
(*3320*)      END;
(*3321*) 
(*3322*)    PROCEDURE RELREAL(F1ATTRP,F2ATTRP: ATTRP; FOP: &OPERATOR);
(*3323*)      BEGIN
(*3324*)        IF F2ATTRP^.KIND=EXPR THEN
(*3325*)          BEGIN FOP:=DUALOP(.FOP.); EXCATTR(F1ATTRP,F2ATTRP); END;
(*3326*)        LOAD(F1ATTRP,F2ATTRP);
(*3327*)        OPERATION(F1ATTRP,F2ATTRP,ZCD,ZCDR);
(*3328*)        REGISTER(.F1ATTRP^.REXPR.RNO.).USED:=FALSE;
(*3329*)        REGSEARCH(NIL,SINGLE);
(*3330*)        BOOLVALUE(RMAIN,BMASK(.FOP.));
(*3331*)        WITH F1ATTRP^ DO
(*3332*)          BEGIN TYPTR:=BOOLPTR; REXPR.RNO:=RWORK; END;
(*3333*)        REGISTER(.RWORK.).USED:=TRUE; REGISTER(.RWORK.).REGCONT:=F1ATTRP;
(*3334*)        EXCATTR(F1ATTRP,F2ATTRP);
(*3335*)      END;
(*3336*) 
(*3337*)    PROCEDURE INPOWER(F1ATTRP,F2ATTRP: ATTRP);
(*3338*)      BEGIN
(*3339*)        LOAD(F2ATTRP,F1ATTRP); LOAD(F1ATTRP,F2ATTRP);
(*3340*)        GENRXP(ZSLDL,F2ATTRP^.REXPR.RNO,0,REALREG(.F1ATTRP^.REXPR.RNO.),0);
(*3341*)        GENRRP1(ZLTR,F2ATTRP^.REXPR.RNO);
(*3342*)        EXCATTR(F1ATTRP,F2ATTRP);
(*3343*)        BOOLVALUE(REALREG(.F2ATTRP^.REXPR.RNO.),CONDM);
(*3344*)        F2ATTRP^.TYPTR:=BOOLPTR;
(*3345*)      END;
(*3346*) 
(*3347*)    PROCEDURE RELPOWER(F1ATTRP,F2ATTRP: ATTRP; FOP: &OPERATOR);
(*3348*)      BEGIN
(*3349*)        IF FOP=LEOP THEN BEGIN EXCATTR(F1ATTRP,F2ATTRP); FOP:=GEOP END
(*3350*)          ELSE IF FOP<>GEOP THEN
(*3351*)            IF F2ATTRP^.KIND=EXPR THEN
(*3352*)              IF F2ATTRP^.REXPR.REGTEMP=REGIST THEN EXCATTR(F1ATTRP,F2ATTRP);
(*3353*)        LOAD(F1ATTRP,F2ATTRP);
(*3354*)        IF FOP=GEOP THEN SETOPERATION(F1ATTRP,F2ATTRP,ZN,ZNR,ZCL,ZCLR)
(*3355*)                    ELSE SETOPERATION(F1ATTRP,F2ATTRP,ZCL,ZCLR,0,0);
(*3356*)        IF FOP=NEOP
(*3357*)          THEN BOOLVALUE(REALREG(.F1ATTRP^.REXPR.RNO.),CONDNZ)
(*3358*)          ELSE BOOLVALUE(REALREG(.F1ATTRP^.REXPR.RNO.),CONDZ);
(*3359*)        REGISTER(.SUCC(F1ATTRP^.REXPR.RNO).).USED:=FALSE;
(*3360*)        F1ATTRP^.TYPTR:=BOOLPTR; EXCATTR(F1ATTRP,F2ATTRP);
(*3361*)      END;
(*3362*) 
// $TITLE LONGOPERATION,SSOPERAND
(*3363*)    PROCEDURE LONGOPERATION(F1ATTRP,F2ATTRP:ATTRP; OPSS,ENTRY:INTEGER);
(*3364*)      VAR LENGTH,BR1,BR2,DISPL1,DISPL2: INTEGER;
(*3365*) 
(*3366*)     PROCEDURE SSOPERAND(F1ATTRP,F2ATTRP:ATTRP; VAR BR,DISPL:INTEGER);
(*3367*)       VAR VARFLAG:BOOLEAN;
(*3368*)       BEGIN WITH F1ATTRP^ DO
(*3369*)         BEGIN VARFLAG:=TRUE;
(*3370*)           LOADINDEX(F1ATTRP,F2ATTRP);
(*3371*)           IF RINDEX=0 THEN
(*3372*)             BEGIN IF VARKIND<>DRCT THEN
(*3373*)               BEGIN REGSEARCH(F2ATTRP,SINGLE); BASEREGISTER(BASELEV,BASEADD);
(*3374*)                     GENRX(ZL,RMAIN,0,RBASE,EFFADRS); VARFLAG:=FALSE;
(*3375*)               END;
(*3376*)              END
(*3377*)           ELSE BEGIN RMAIN:=RINDEX; VARFLAG:=FALSE;
(*3378*)                  IF VARKIND<>DRCT THEN
(*3379*)                    BEGIN BASEREGISTER(BASELEV,BASEADD); GENRX(ZA,RMAIN,0,RBASE,EFFADRS); END
(*3380*)                  ELSE IF VLEVEL<>0 THEN GENRR(ZAR,RMAIN,VLEVEL);
(*3381*)                END;
(*3382*)           IF (VADRS>=4096) OR (VADRS<0) THEN
(*3383*)             BEGIN IF VARFLAG THEN
(*3384*)                 BEGIN REGSEARCH(F2ATTRP,SINGLE); VARFLAG:=FALSE; GENRR(ZLR,RMAIN,VLEVEL); END;
(*3385*)               DISPLACEMENT(VADRS,VADRS); GENRX(ZA,RMAIN,0,0,0);
(*3386*)             END;
(*3387*)           DISPL:=VADRS;
(*3388*)           IF VARFLAG THEN BR:=VLEVEL
(*3389*)             ELSE BEGIN TYPTR:=INTPTR; KIND:=EXPR; REXPR.REGTEMP:=REGIST;
(*3390*)                        REXPR.RNO:=RWORK; REGISTER(.RWORK.).USED:=TRUE;
(*3391*)                        REGISTER(.RWORK.).REGCONT:=F1ATTRP; BR:=RMAIN;
(*3392*)                  END;
(*3393*)         END;
(*3394*)       END;
(*3395*) 
(*3396*)      BEGIN LENGTH:=F1ATTRP^.TYPTR^.SIZE.WBLENGTH;
(*3397*)        IF LENGTH<=256 THEN
(*3398*)          BEGIN
(*3399*)            WITH F1ATTRP^ DO CASE KIND OF
(*3400*)              EXPR:  ERROR(400);
(*3401*)              CST:   BEGIN MAKECONSTANT(CVAL); BR1:=0; DISPL1:=0; END;
(*3402*)              VARBL: SSOPERAND(F1ATTRP,F2ATTRP,BR1,DISPL1)
(*3403*)            END;
(*3404*)            WITH F2ATTRP^ DO CASE KIND OF
(*3405*)              EXPR:  ERROR(400);
(*3406*)              CST:   BEGIN IC:=IC+2; MAKECONSTANT(CVAL); IC:=IC-2;
(*3407*)                       BR2:=0; DISPL2:=0;
(*3408*)                     END;
(*3409*)              VARBL: SSOPERAND(F2ATTRP,F1ATTRP,BR2,DISPL2);
(*3410*)            END;
(*3411*)            GENSS(OPSS,LENGTH-1,BR1,DISPL1,BR2,DISPL2);
(*3412*)          END
(*3413*)        ELSE
(*3414*)          BEGIN LOADADDRESS(F1ATTRP,F2ATTRP); LOADADDRESS(F2ATTRP,F1ATTRP);
(*3415*)            LOADINTCONST(R0,256*LENGTH+16*REALREG(.F1ATTRP^.REXPR.RNO.)+REALREG(.F2ATTRP^.REXPR.RNO.));
(*3416*)            GENRX(ZBAL,BASEWORK,0,1,ENTRY);
(*3417*)          END;
(*3418*)      END;
(*3419*) 
// $TITLE ASSIGNLONG,RELLONG
(*3420*)    PROCEDURE ASSIGNLONG(F1ATTRP,F2ATTRP:ATTRP);
(*3421*)      BEGIN
(*3422*)        LONGOPERATION(F1ATTRP,F2ATTRP,ZMVC,ENTRYAL);
(*3423*)      END;
(*3424*) 
(*3425*)    PROCEDURE RELLONG(F1ATTRP,F2ATTRP:ATTRP; FOP:&OPERATOR);
(*3426*)      BEGIN
(*3427*)        LONGOPERATION(F1ATTRP,F2ATTRP,ZCLC,ENTRYCL);
(*3428*)        IF F2ATTRP^.KIND<>EXPR THEN
(*3429*)          IF F1ATTRP^.KIND=EXPR THEN EXCATTR(F1ATTRP,F2ATTRP)
(*3430*)            ELSE BEGIN REGSEARCH(NIL,SINGLE);
(*3431*)                   WITH F2ATTRP^ DO
(*3432*)                     BEGIN KIND:=EXPR; REXPR.REGTEMP:=REGIST;
(*3433*)                           REXPR.RNO:=RWORK;
(*3434*)                     END;
(*3435*)                   REGISTER(.RWORK.).USED:=TRUE; REGISTER(.RWORK.).REGCONT:=F2ATTRP;
(*3436*)                 END;
(*3437*)        BOOLVALUE(REALREG(.F2ATTRP^.REXPR.RNO.),BMASK(.FOP.));
(*3438*)        F2ATTRP^.TYPTR:=BOOLPTR;
(*3439*)      END;
(*3440*) 
// $TITLE CHKREG,CHKRANGE,CHKPOINTER,OVFLOW
(*3441*)    PROCEDURE CHECKREGISTER(R,FMIN,FMAX:INTEGER);
(*3442*)    BEGIN
(*3443*)            GENRR(ZBALR,9,0);
(*3444*)            MAKEINTCONST(FMIN); GENRX(ZC,R,0,0,0);
(*3445*)            GENRX(ZBC,CONDM,0,1,JUMPERR2);
(*3446*)            MAKEINTCONST(FMAX); GENRX(ZC,R,0,0,0);
(*3447*)            GENRX(ZBC,CONDP,0,1,JUMPERR2);
(*3448*)      END;
(*3449*) 
(*3450*)    PROCEDURE CHECKRANGE(FATTRP:ATTRP; FMIN,FMAX,ERRORNO:INTEGER);
(*3451*)      BEGIN
(*3452*)        IF FATTRP^.KIND=CST THEN
(*3453*)          BEGIN IF (FATTRP^.CVAL.IVAL<FMIN) OR (FATTRP^.CVAL.IVAL>FMAX) THEN ERROR(ERRORNO)
(*3454*)          END
(*3455*)        ELSE IF DEBUG THEN
(*3456*)          BEGIN LOAD(FATTRP,NIL);
(*3457*)            CHECKREGISTER(REALREG(.FATTRP^.REXPR.RNO.),FMIN,FMAX);
(*3458*)          END;
(*3459*)      END;
(*3460*) 
(*3461*)    PROCEDURE CHECKPOINTER(FATTRP: ATTRP; NILALLOWED: BOOLEAN);
(*3462*)      BEGIN
(*3463*)        IF FATTRP^.KIND=CST THEN
(*3464*)          BEGIN IF NOT NILALLOWED THEN ERROR(305); END
(*3465*)        ELSE
(*3466*)          IF DEBUG THEN
(*3467*)            BEGIN LOAD(FATTRP,NIL);
(*3468*)              GENRR(ZBALR,9,0);
(*3469*)              IF NILALLOWED THEN
(*3470*)                BEGIN GENRRP1(ZLTR,FATTRP^.REXPR.RNO);
(*3471*)                      GENRX(ZBC,CONDZ,0,PBASE1,IC+18);
(*3472*)                END;
(*3473*)              GENRR(ZCLR,REALREG(.FATTRP^.REXPR.RNO.),NEWPOINTER);
(*3474*)              GENRX(ZBC,CONDM,0,1,JUMPERR3);
(*3475*)              GENRXP(ZCL,FATTRP^.REXPR.RNO,0,1,NPINIT);
(*3476*)                 GENRX(ZBC,CONDP,0,1,JUMPERR3);                                 
(*3477*)            END;
(*3478*)      END;
(*3479*) 
(*3480*)    PROCEDURE OVERFLOWTEST;
(*3481*)      BEGIN
(*3482*)        IF DEBUG THEN GENRR(ZBALR,9,0);
(*3483*)        GENRR(ZCLR,NEWPOINTER,STACKPOINTER);
(*3484*)        GENRX(ZBC,CONDM,0,1,JUMPERR4);
(*3485*)      END;
(*3486*) 
// $TITLE  STORE,ASSIGN
(*3487*)    PROCEDURE STORE(F1ATTRP,F2ATTRP:ATTRP; ERRORNO:INTEGER);
(*3488*)      VAR LMIN,LMAX:INTEGER;
(*3489*) 
(*3490*)      PROCEDURE ASSIGN(X:INTEGER);
(*3491*)        BEGIN LOAD(F2ATTRP,F1ATTRP); LOADINDEX(F1ATTRP,F2ATTRP);
(*3492*)          LOADBASE(F1ATTRP);
(*3493*)          GENRXP(X,F2ATTRP^.REXPR.RNO,RINDEX,RBASE,EFFADRS);
(*3494*)        END;
(*3495*) 
(*3496*)      BEGIN
(*3497*)        IF F1ATTRP^.TYPTR=REALPTR THEN
(*3498*)          BEGIN
(*3499*)            IF COMPTYPES(F2ATTRP^.TYPTR,INTPTR) THEN
(*3500*)              INTTOREAL(F2ATTRP);
(*3501*)            IF F2ATTRP^.TYPTR=REALPTR THEN ASSIGN(ZSTD)
(*3502*)              ELSE ERROR(ERRORNO)
(*3503*)          END
(*3504*)        ELSE
(*3505*)          IF COMPTYPES(F2ATTRP^.TYPTR,F1ATTRP^.TYPTR) THEN
(*3506*)            CASE F1ATTRP^.TYPTR^.FORM OF
(*3507*)              SCALAR,
(*3508*)              SUBRANGE:
(*3509*)                BEGIN
(*3510*)                  IF F1ATTRP^.TYPTR <> INTPTR THEN
(*3511*)                    BEGIN
(*3512*)                      GETBOUNDS(F1ATTRP^.TYPTR,LMIN,LMAX);
(*3513*)                      CHECKRANGE(F2ATTRP,LMIN,LMAX,303);
(*3514*)                    END;
(*3515*)                  ASSIGN(ZST)
(*3516*)                END;
(*3517*)              PACKDTYPE: ASSIGN(ZSTC);
(*3518*)              POINTER:
(*3519*)                BEGIN CHECKPOINTER(F2ATTRP,TRUE); ASSIGN(ZST) END;
(*3520*)              POWER: BEGIN LOAD(F2ATTRP,F1ATTRP); LOADINDEX(F1ATTRP,F2ATTRP);
(*3521*)                       LOADBASE(F1ATTRP);
(*3522*)                       GENRXP(ZST,F2ATTRP^.REXPR.RNO,RINDEX,RBASE,EFFADRS);
(*3523*)                       GENRXP(ZST,SUCC(F2ATTRP^.REXPR.RNO),RINDEX,RBASE,EFFADRS+4);
(*3524*)                     END;
(*3525*)              ARRAYS,
(*3526*)              RECORDS: IF F1ATTRP^.TYPTR^.FTYPE THEN ERROR(146)
(*3527*)                         ELSE ASSIGNLONG(F1ATTRP,F2ATTRP);
(*3528*)              FILES: ERROR(146)
(*3529*)            END
(*3530*)          ELSE ERROR(ERRORNO)
(*3531*)      END;
(*3532*) 
(*3533*) 
// $TITLE  SELECTOR,IDADDRESS
(*3534*)    PROCEDURE EXPRESSION(FSYS: SETOFSYS); FORWARD;
(*3535*) 
(*3536*)    PROCEDURE SELECTOR(FSYS: SETOFSYS; FCP: CTP);
(*3537*)     VAR LATTRP: ATTRP; LCP: CTP;
(*3538*) 
(*3539*)     PROCEDURE IDADDRESS;
(*3540*)      (* PUT IN 'LATTRP^' THE DESCRIPTION OF THE IDENTIFIER POINTED AT BY 'FCP'.
  3541         USEFUL GLOBAL VARIABLES:
  3542          LATTRP: ATTRP (POINTS TO THE ATTRIBUTE TO BE BUILT UP)
  3543          FCP: CTP; (POINTS TO THE IDENTIFIER WE ARE WORKING ON)
  3544          DISX: DISPRANGE (IN THE CASE WHERE THE IDENTIFIER 'ID' IS A FIELD
  3545                           IDENTIFIER: 'DISX' IS THE LEVEL ON WHICH 'ID'
  3546                           WAS DEFINED)  *)
(*3547*)     BEGIN
(*3548*)      WITH FCP^, LATTRP^ DO
(*3549*)       BEGIN TYPTR := IDTYPE; KIND := VARBL;
(*3550*)        IF TYPTR=NIL THEN
(*3551*)          BEGIN VADRS:=0; ACCESS:=DIRECT;
(*3552*)                VARKIND:=DRCT; VLEVEL:=0;
(*3553*)          END
(*3554*)        ELSE
(*3555*)         CASE KLASS OF
(*3556*)          VARS:
(*3557*)           BEGIN
(*3558*)             IF VKIND=DRCT THEN BEGIN VLEVEL:=VLEV; VADRS:=VADDR END
(*3559*)                 ELSE BEGIN BASELEV:=VLEV; BASEADD:=PARADDR; VADRS:=0 END;
(*3560*)               ACCESS:=DIRECT; VARKIND:=VKIND;
(*3561*)           END;
(*3562*)          FIELD:
(*3563*)           WITH DISPLAY(.DISX.) DO
(*3564*)            BEGIN
(*3565*)              VADRS:=DADRS+FLDADDR; ACCESS:=DIRECT; VARKIND:=DISPKIND;
(*3566*)              IF VARKIND=DRCT THEN VLEVEL:=DLEVEL
(*3567*)                 ELSE BEGIN BASELEV:=DBASEL; BASEADD:=DBASEA END;
(*3568*)            END;
(*3569*)          FUNC:
(*3570*)           IF PFDECKIND = STANDARD THEN ERROR(150)
(*3571*)             ELSE IF PFKIND = FORMAL THEN ERROR(151)
(*3572*)               ELSE IF PFLEV = LEVEL THEN ERROR(182)
(*3573*)                 ELSE
(*3574*)                  BEGIN VLEVEL:=PFLEV+1; VARKIND:=DRCT;
(*3575*)                        VADRS:=SAVEAREA; ACCESS:=DIRECT;
(*3576*)                  END
(*3577*)         END (*CASE*)
(*3578*)       END (*WITH*)
(*3579*)     END (*IDADDRESS*) ;
(*3580*) 
// $TITLE INDEXCODE
(*3581*)    PROCEDURE INDEXCODE;
(*3582*)      LABEL 1;
(*3583*)      VAR ATTRWORK:ATTRP; LMIN,LMAX,LENGTH,SHIFT,N:INTEGER;
(*3584*)      BEGIN
(*3585*)        LENGTH:=LATTRP^.TYPTR^.AELLENG;
(*3586*)        GETBOUNDS(LATTRP^.TYPTR^.INXTYPE,LMIN,LMAX);
(*3587*)        CHECKRANGE(GATTRP,LMIN,LMAX,302);
(*3588*)        IF GATTRP^.KIND=CST
(*3589*)          THEN LATTRP^.VADRS:=LATTRP^.VADRS+(GATTRP^.CVAL.IVAL-LMIN)*LENGTH
(*3590*)        ELSE
(*3591*)          BEGIN
(*3592*)            LATTRP^.VADRS:=LATTRP^.VADRS-LMIN*LENGTH;
(*3593*)            LOAD(GATTRP,NIL);
(*3594*)            IF LENGTH<>1 THEN
(*3595*)              BEGIN N:=2;
(*3596*)                FOR SHIFT:=1 TO 12 DO
(*3597*)                  BEGIN
(*3598*)                    IF LENGTH=N THEN
(*3599*)                      BEGIN GENRXP(ZSLA,GATTRP^.REXPR.RNO,0,0,SHIFT);
(*3600*)                            GOTO 1;
(*3601*)                      END;
(*3602*)                    N:=N*2;
(*3603*)                  END;
(*3604*)                ATTRNEW(ATTRWORK);
(*3605*)                WITH ATTRWORK^ DO
(*3606*)                  BEGIN TYPTR:=INTPTR; KIND:=CST; CVAL.CKIND:=INT;
(*3607*)                        CVAL.IVAL:=LENGTH;
(*3608*)                  END;
(*3609*)                INTARITH(ATTRWORK,GATTRP,MUL);
(*3610*)                ATTRDISP(ATTRWORK);
(*3611*)              END;
(*3612*)        1:  IF LATTRP^.ACCESS=INDIRECT THEN
(*3613*)              BEGIN ATTRNEW(ATTRWORK);
(*3614*)                WITH ATTRWORK^ DO
(*3615*)                  BEGIN TYPTR:=INTPTR; KIND:=EXPR;
(*3616*)                        REXPR:=LATTRP^.INDEXREG;
(*3617*)                        IF REXPR.REGTEMP=REGIST
(*3618*)                          THEN REGISTER(.REXPR.RNO.).REGCONT:=ATTRWORK
(*3619*)                          ELSE REXPR.ATEMP^.TEMPCONT:=ATTRWORK;
(*3620*)                  END;
(*3621*)                INTARITH(ATTRWORK,GATTRP,PLUS); ATTRDISP(ATTRWORK);
(*3622*)              END;
(*3623*)            WITH LATTRP^ DO
(*3624*)              BEGIN INDEXREG:=GATTRP^.REXPR;
(*3625*)                    ACCESS:=INDIRECT; KIND:=VARBL;
(*3626*)                    REGISTER(.INDEXREG.RNO.).USED:=TRUE;
(*3627*)                    REGISTER(.INDEXREG.RNO.).REGCONT:=LATTRP;
(*3628*)              END;
(*3629*)          END;
(*3630*)     END (*INDEXCODE*);
(*3631*) 
// $TITLE  RECFIELD,FILEBUFFER,POINTDELEMENT
(*3632*)    PROCEDURE RECFIELD;
(*3633*)      BEGIN WITH LCP^, LATTRP^ DO
(*3634*)        BEGIN
(*3635*)          VADRS:=VADRS+FLDADDR;
(*3636*)          TYPTR:=IDTYPE;
(*3637*)          KIND := VARBL;
(*3638*)        END
(*3639*)      END;
(*3640*) 
(*3641*)    PROCEDURE FILEBUFFER;
(*3642*)      VAR R:REGNO;
(*3643*)      BEGIN
(*3644*)        WITH LATTRP^ DO
(*3645*)          BEGIN
(*3646*)            IF TYPTR^.TEXTFILE
(*3647*)              THEN BEGIN LOADADDRESS(LATTRP,NIL); R:=REXPR.RNO;
(*3648*)                     GENRXP(ZL,R,0,REALREG(.R.),8);
(*3649*)                     ACCESS:=INDIRECT; INDEXREG.REGTEMP:=REGIST;
(*3650*)                     INDEXREG.RNO:=R;VARKIND:=DRCT;
(*3651*)                     VADRS:=0; VLEVEL:=0;
(*3652*)                     TYPTR:=PACKDCHARPTR;
(*3653*)                   END
(*3654*)              ELSE BEGIN VADRS:=VADRS+8; TYPTR:=TYPTR^.FILTYPE; END;
(*3655*)            KIND:=VARBL;
(*3656*)          END;
(*3657*)      END;
(*3658*) 
(*3659*)    PROCEDURE POINTEDELEMENT;
(*3660*)      VAR WORK:REGORTEMP;
(*3661*)      BEGIN
(*3662*)        WITH LATTRP^ DO
(*3663*)          BEGIN
(*3664*)            CHECKPOINTER(LATTRP,FALSE);
(*3665*)            LOAD(LATTRP,NIL); WORK:=REXPR;
(*3666*)            INDEXREG:=WORK; ACCESS:=INDIRECT;
(*3667*)            VADRS:=0; VARKIND:=DRCT; VLEVEL:=0;
(*3668*)            TYPTR := TYPTR^.ELTYPE; KIND := VARBL;
(*3669*)          END
(*3670*)      END;
(*3671*) 
// $TITLE SELECTOR - (BODY)
(*3672*)    BEGIN (*SELECTOR*)
(*3673*)     ATTRNEW(LATTRP);
(*3674*)     IDADDRESS;
(*3675*)     IF NOT (SY IN SELECTSYS+FSYS) THEN
(*3676*)      BEGIN ERROR(59); SKIP(SELECTSYS+FSYS) END;
(*3677*)     WHILE SY IN SELECTSYS DO
(*3678*)      BEGIN
(*3679*)(*(.*)   IF SY = LBRACK THEN
(*3680*)        BEGIN
(*3681*)         REPEAT
(*3682*)          WITH LATTRP^ DO
(*3683*)           IF TYPTR <> NIL THEN
(*3684*)            IF TYPTR^.FORM <> ARRAYS THEN
(*3685*)             BEGIN ERROR(138); TYPTR := NIL END;
(*3686*)          INSYMBOL; EXPRESSION(FSYS+(.COMMA,RBRACK.));
(*3687*)          IF GATTRP^.TYPTR <> NIL THEN
(*3688*)           IF GATTRP^.TYPTR^.FORM > SUBRANGE THEN ERROR(113);
(*3689*)          IF LATTRP^.TYPTR <> NIL THEN
(*3690*)           WITH LATTRP^.TYPTR^ DO
(*3691*)            BEGIN
(*3692*)             IF COMPTYPES(INXTYPE,GATTRP^.TYPTR) THEN
(*3693*)              BEGIN
(*3694*)               IF (INXTYPE <> NIL)AND (AELTYPE <> NIL) THEN INDEXCODE
(*3695*)              END
(*3696*)             ELSE ERROR(139);
(*3697*)             LATTRP^.TYPTR := AELTYPE
(*3698*)            END
(*3699*)         UNTIL SY <> COMMA;
(*3700*)         TEST1(RBRACK,12);
(*3701*)        END (*IF SY = LBRACK*)
(*3702*)       ELSE
(*3703*)(*.*)    IF SY = PERIOD THEN
(*3704*)         BEGIN
(*3705*)          WITH LATTRP^ DO
(*3706*)           BEGIN
(*3707*)            IF TYPTR <> NIL THEN
(*3708*)             IF TYPTR^.FORM <> RECORDS THEN
(*3709*)              BEGIN ERROR(140); TYPTR := NIL END;
(*3710*)            INSYMBOL;
(*3711*)            IF SY = IDENT THEN
(*3712*)             BEGIN
(*3713*)              IF TYPTR <> NIL THEN
(*3714*)               BEGIN SEARCHSECTION(TYPTR^.FIELDS,LCP);
(*3715*)                IF LCP = NIL THEN
(*3716*)                 BEGIN ERROR(152); TYPTR := NIL END
(*3717*)                ELSE
(*3718*)                 RECFIELD;
(*3719*)               END;
(*3720*)              INSYMBOL
(*3721*)             END (*SY = IDENT*)
(*3722*)            ELSE ERROR(2)
(*3723*)           END (*WITH LATTRP^*)
(*3724*)         END (*IF SY = PERIOD*)
(*3725*)        ELSE
(*3726*)(*@*)    BEGIN
(*3727*)          IF LATTRP^.TYPTR <> NIL THEN
(*3728*)           BEGIN
(*3729*)            WITH LATTRP^.TYPTR^ DO
(*3730*)             IF FORM = FILES THEN FILEBUFFER
(*3731*)             ELSE
(*3732*)              IF FORM = POINTER THEN POINTEDELEMENT
(*3733*)              ELSE ERROR(141);
(*3734*)           END;
(*3735*)          INSYMBOL
(*3736*)         END;
(*3737*)         TEST2(FSYS+SELECTSYS,6,(. .));
(*3738*)      END (*WHILE*) ;
(*3739*)     COPYATTR(LATTRP,GATTRP);
(*3740*)     ATTRDISP(LATTRP);
(*3741*)    END (*SELECTOR*) ;
(*3742*) 
(*3743*) 
(*3744*)    PROCEDURE CALL(FSYS: SETOFSYS; FCP: CTP);
(*3745*)     VAR LKEY: 1..NRSTDNAMES;
(*3746*) 
(*3747*)     PROCEDURE VARIABLE(FSYS: SETOFSYS);
(*3748*)       VAR LCP: CTP;
(*3749*)       BEGIN
(*3750*)         IF SY = IDENT
(*3751*)           THEN BEGIN SEARCHID((.VARS,FIELD.),LCP); INSYMBOL END
(*3752*)           ELSE BEGIN ERROR(2); LCP := UVARPTR END;
(*3753*)         SELECTOR(FSYS,LCP)
(*3754*)       END;
(*3755*) 
// $TITLE STDFLPROCS,SETSFILATTR
(*3756*)     PROCEDURE STDFLPROCS;
(*3757*)       VAR ENTRY:INTEGER;
(*3758*)     BEGIN
(*3759*)        TEST1(LPARENT,9);
(*3760*)         VARIABLE(FSYS+(.COMMA,RPARENT.));
(*3761*)         WITH GATTRP^ DO
(*3762*)           IF TYPTR <> NIL THEN
(*3763*)             IF TYPTR^.FORM = FILES THEN
(*3764*)               BEGIN
(*3765*)                 IF TYPTR^.TEXTFILE
(*3766*)                   THEN ENTRY:=ENTGETCH+8*(LKEY-1)
(*3767*)                   ELSE ENTRY:=ENTRYGET+8*(LKEY-1);
(*3768*)                 LOADADDRESS(GATTRP,NIL); GENRR(ZLR,15,REALREG(.REXPR.RNO.));
(*3769*)                 GENRX(ZBAL,BASEWORK,0,1,ENTRY);
(*3770*)                 RESETG;
(*3771*)               END
(*3772*)             ELSE ERROR(116);
(*3773*)         TEST1(RPARENT,4);
(*3774*)       END;
(*3775*) 
(*3776*)      PROCEDURE SETSTFILATTR(FATTRP:ATTRP; FCP:CTP);
(*3777*)        BEGIN
(*3778*)          WITH FATTRP^ DO
(*3779*)            BEGIN TYPTR:=TEXTPTR; KIND:=VARBL; ACCESS:=DIRECT;
(*3780*)              IF FCP=OUTPUTPTR
(*3781*)                THEN BEGIN VARKIND:=INDRCT; BASELEV:=1;
(*3782*)                       BASEADD:=PTROUTBLCK; VADRS:=0;
(*3783*)                     END
(*3784*)                ELSE BEGIN VARKIND:=DRCT; VLEVEL:=1; VADRS:=LCSTART; END;
(*3785*)            END;
(*3786*)        END;
(*3787*) 
// $TITLE  STDWIDTH,STRINGIO
(*3788*)PROCEDURE STDWIDTH(VAR FORMP:ATTRP; WIDTH : INTEGER);
(*3789*) VAR SW:BOOLEAN;
(*3790*)BEGIN (* STDWIDTH *)
(*3791*)  IF FORMP = NIL THEN SW:=TRUE
(*3792*)    ELSE IF FORMP^.TYPTR = NIL THEN SW := TRUE
(*3793*)      ELSE SW := FALSE;
(*3794*)  IF SW THEN
(*3795*)  BEGIN
(*3796*)    ATTRNEW(FORMP);
(*3797*)    WITH FORMP^ DO
(*3798*)    BEGIN
(*3799*)      TYPTR := INTPTR; KIND := CST;
(*3800*)      CVAL.CKIND := INT; CVAL.IVAL := WIDTH;
(*3801*)    END;
(*3802*)  END;
(*3803*)END; (* STDWIDTH *)
(*3804*) 
(*3805*) 
(*3806*)PROCEDURE STRINGIO(VAR LATTRP,FORM1P:ATTRP; ENTRY:INTEGER);
(*3807*) VAR LENGTH : INTEGER;
(*3808*) BEGIN (* STRINGIO *)
(*3809*)   LENGTH := LATTRP^.TYPTR^.SIZE.WBLENGTH;
(*3810*)   LOADADDRESS(LATTRP,FORM1P);
(*3811*)   STDWIDTH(FORM1P,LENGTH); LOAD(FORM1P,LATTRP);
(*3812*)   LOADINTCONST(R0, 256*LENGTH +
(*3813*)            16*REALREG(.LATTRP^.REXPR.RNO.)+
(*3814*)            REALREG(.FORM1P^.REXPR.RNO.));
(*3815*)   GENRX(ZBAL,BASEWORK,0,1,ENTRY);
(*3816*) END; (* STRINGIO *)
(*3817*) 
(*3818*) 
// $TITLE  READWRITE;
(*3819*)     PROCEDURE READWRITE;
(*3820*)       VAR GETIN,DEFAULT:BOOLEAN; ENTRY:INTEGER; FILATTRP:ATTRP;
(*3821*)          FORM1P,FORM2P,LATTRP,FIL1ATTRP:ATTRP;
(*3822*)          LSP:STP;
(*3823*)          FCP:CTP;
(*3824*) 
(*3825*) 
(*3826*)PROCEDURE WRITEINT(ENTRY,STDLENG:INTEGER);
(*3827*) BEGIN
(*3828*)   LOAD(LATTRP,FORM1P);
(*3829*)   STDWIDTH(FORM1P,STDLENG); LOAD(FORM1P,LATTRP);
(*3830*)   LOADINTCONST(R0,16*REALREG(.LATTRP^.REXPR.RNO.)+
(*3831*)                      REALREG(.FORM1P^.REXPR.RNO.));
(*3832*)   GENRX(ZBAL,BASEWORK,0,1,ENTRY);
(*3833*) END;
(*3834*) 
(*3835*) 
(*3836*)PROCEDURE WRITEREAL;
(*3837*) VAR ENTRY:INTEGER;
(*3838*) BEGIN (* WRITEREAL *)
(*3839*)   LOAD(LATTRP,NIL);
(*3840*)   STDWIDTH(FORM1P,24);
(*3841*)   IF FORM2P = NIL THEN ENTRY:=ENTRYWR1 ELSE ENTRY:=ENTRYWR2;
(*3842*)   STDWIDTH(FORM2P,0);
(*3843*)   LOAD(FORM1P,NIL);  LOAD(FORM2P,FORM1P);
(*3844*)   LOADINTCONST(R0,256*REALREG(.LATTRP^.REXPR.RNO.)
(*3845*)             +16*REALREG(.FORM1P^.REXPR.RNO.)+
(*3846*)             REALREG(.FORM2P^.REXPR.RNO.));
(*3847*)   GENRX(ZBAL,BASEWORK,0,1,ENTRY);
(*3848*) END; (* WRITEREAL *)
(*3849*) 
(*3850*) 
(*3851*)       BEGIN
(*3852*)         ATTRNEW(FILATTRP);ATTRNEW(FIL1ATTRP);
(*3853*)         IF (LKEY<=7) THEN FCP:=INPUTPTR ELSE
(*3854*)          FCP:=OUTPUTPTR;
(*3855*)         SETSTFILATTR(FILATTRP,FCP);
(*3856*)         GETIN:=FALSE; DEFAULT:=TRUE;
(*3857*)         IF SY=LPARENT THEN
(*3858*)            BEGIN
(*3859*)             GETIN:=TRUE; INSYMBOL;
(*3860*)             IF LKEY<=7 THEN
(*3861*)             VARIABLE(FSYS+(.COMMA,RPARENT,COLON,IDENT.))
(*3862*)             ELSE
(*3863*)             EXPRESSION(FSYS+(.COMMA,COLON,RPARENT,IDENT.));
(*3864*)             IF GATTRP^.TYPTR<>NIL THEN
(*3865*)               IF GATTRP^.TYPTR^.FORM=FILES THEN
(*3866*)                 BEGIN
(*3867*)                     IF NOT GATTRP^.TYPTR^.TEXTFILE THEN
(*3868*)                        IF EXTWARN THEN ERROR(291);
(*3869*)                   COPYATTR(GATTRP,FILATTRP); DEFAULT:=FALSE;
(*3870*)                   IF SY=RPARENT
(*3871*)                     THEN BEGIN INSYMBOL; GETIN:=FALSE; END
(*3872*)                     ELSE IF SY=COMMA THEN
(*3873*)                        BEGIN INSYMBOL;
(*3874*)                       IF LKEY <=7 THEN VARIABLE(FSYS+
(*3875*)                            (.COMMA,RPARENT.))
(*3876*)                         ELSE EXPRESSION(FSYS+(.COMMA,COLON,
(*3877*)                                 RPARENT,IDENT.))
(*3878*)                         END;
(*3879*)                 END;
(*3880*)           END;
(*3881*)         IF DEFAULT THEN
(*3882*)          IF FCP=NIL THEN
(*3883*)            IF LKEY<=7 THEN ERROR(175) ELSE ERROR(176);
(*3884*)          COPYATTR(FILATTRP,FIL1ATTRP);
(*3885*)          PROCPASS:=FALSE;
(*3886*)         LOADADDRESS(FILATTRP,NIL); GENRR(ZLR,15,REALREG(.FILATTRP^.REXPR.RNO.));
(*3887*)         ATTRDISP(FILATTRP);
(*3888*)         IF GETIN THEN
(*3889*)           BEGIN
//(*3890*)             LOOP ENTRY:=0;
(*3890*)             while true do begin   // loop 10
                    ENTRY:=0;

(*3891*)            LSP:=GATTRP^.TYPTR;ATTRNEW(LATTRP);
(*3892*)              IF LKEY <= 7 THEN
(*3893*)            IF xSTRING(LSP) THEN IF EXTWARN THEN ERROR(291);
(*3894*)            COPYATTR(GATTRP,LATTRP);
(*3895*)            FORM1P:=NIL;FORM2P:=NIL;
(*3896*)            IF FIL1ATTRP^.TYPTR^.TEXTFILE THEN
(*3897*)            IF SY=COLON THEN
(*3898*)            BEGIN
(*3899*)              INSYMBOL;
(*3900*)              EXPRESSION(FSYS+(.COMMA,COLON,RPARENT,IDENT.));
(*3901*)              IF COMPTYPES(GATTRP^.TYPTR,INTPTR) THEN
(*3902*)              BEGIN
(*3903*)                ATTRNEW(FORM1P); COPYATTR(GATTRP,FORM1P);
(*3904*)              END ELSE ERROR(116);
(*3905*)              (* FOR FUTURE IMPLEMENTATION *)
(*3906*)              (*****************************)
(*3907*)              IF SY = COLON THEN
(*3908*)              BEGIN
(*3909*)                 INSYMBOL; EXPRESSION(FSYS+(.COMMA,RPARENT.));
(*3910*)                 IF COMPTYPES(GATTRP^.TYPTR,INTPTR) THEN
(*3911*)                 BEGIN
(*3912*)                   ATTRNEW(FORM2P);COPYATTR(GATTRP,FORM2P);
(*3913*)                 END ELSE ERROR(116);
(*3914*)                IF LSP<>REALPTR THEN ERROR(124);
(*3915*)               END;
(*3916*)             END;
(*3917*)             IF PROCPASS THEN
(*3918*)             BEGIN
(*3919*)               ATTRNEW(FILATTRP);COPYATTR(FIL1ATTRP,FILATTRP);
(*3920*)               LOADADDRESS(FILATTRP,NIL);
(*3921*)               GENRR(ZLR,15,REALREG(.FILATTRP^.REXPR.RNO.));
(*3922*)               ATTRDISP(FILATTRP);
(*3923*)             END;
(*3924*)            IF LKEY <= 7 THEN
(*3925*)            BEGIN
(*3926*)               WITH GATTRP^ DO
(*3927*)                 IF TYPTR<>NIL THEN
(*3928*)                   BEGIN
(*3929*)                     IF NOT FIL1ATTRP^.TYPTR^.TEXTFILE THEN
(*3930*)                     BEGIN
(*3931*)                       ATTRNEW(FILATTRP);
(*3932*)                       COPYATTR(FIL1ATTRP,FILATTRP);
(*3933*)                 WITH FILATTRP^ DO
(*3934*)                 BEGIN TYPTR:=TYPTR^.FILTYPE; VADRS:=VADRS+8 END;
(*3935*)                       STORE(GATTRP,FILATTRP,118);
(*3936*)                       ATTRDISP(FILATTRP);
(*3937*)            GENRX(ZBAL,BASEWORK,0,1,ENTRYGET);
(*3938*)                    END ELSE
(*3939*)                      IF xSTRING(LSP) OR(LSP^.SIZE.WBLENGTH=1) THEN
(*3940*)                         STRINGIO(LATTRP,FORM1P,ENTRYRS) ELSE
(*3941*)                       IF COMPTYPES(TYPTR,CHARPTR) THEN ENTRY:=ENTRYRC
(*3942*)                         ELSE IF COMPTYPES(TYPTR,INTPTR) THEN ENTRY:=ENTRYRI
(*3943*)                           ELSE IF TYPTR=REALPTR THEN ENTRY:=ENTRYRR
(*3944*)                               ELSE ERROR(153);
(*3945*)                     IF ENTRY<>0 THEN
(*3946*)                       BEGIN LOADADDRESS(GATTRP,NIL);
(*3947*)                         GENRR(ZLR,R0,REALREG(.REXPR.RNO.));
(*3948*)                         GENRX(ZBAL,BASEWORK,0,1,ENTRY);
(*3949*)                       END;
(*3950*)                   END;
(*3951*)             END ELSE
(*3952*)             BEGIN
(*3953*)              IF LSP <> NIL THEN
(*3954*)                  IF NOT FIL1ATTRP^.TYPTR^.TEXTFILE THEN
(*3955*)                  BEGIN
(*3956*)                    ATTRNEW(FILATTRP);
// misspelled as coppyattrp in original. OS360 object modules only allow
// 8 chars for proc names so rest is ignored
(*3957*)                    COPYATTR(FIL1ATTRP,FILATTRP);
(*3958*)                WITH FILATTRP^ DO
(*3959*)                BEGIN VADRS:=VADRS+8; TYPTR:=TYPTR^.FILTYPE END;
(*3960*)                    STORE(FILATTRP,GATTRP,116);
(*3961*)                    GENRX(ZBAL,BASEWORK,0,1,ENTRYGET+8);
(*3962*)                    ATTRDISP(FILATTRP);
(*3963*)                  END ELSE
(*3964*)               IF COMPTYPES(LSP,CHARPTR) THEN WRITEINT(ENTRYWC,1)
(*3965*)                ELSE IF COMPTYPES(LSP,INTPTR) THEN WRITEINT(ENTRYWI,12)
(*3966*)                 ELSE IF LSP=REALPTR THEN WRITEREAL
(*3967*)                  ELSE IF COMPTYPES(LSP,BOOLPTR) THEN
(*3968*)                        WRITEINT(ENTRYWB,5)
(*3969*)                   ELSE IF xSTRING(LSP) THEN
(*3970*)                        STRINGIO(LATTRP,FORM1P,ENTRYWS)
(*3971*)                    ELSE ERROR(116)
(*3972*)            END;
(*3973*)               ATTRDISP(LATTRP);
(*3974*)               IF FORM1P<>NIL THEN ATTRDISP(FORM1P);
(*3975*)               IF FORM2P <> NIL THEN ATTRDISP(FORM2P);
(*3976*)               RESETG;
//(*3977*)               IF SY<>COMMA THEN EXIT;
(*3977*)               IF SY<>COMMA THEN
                         break;
(*3978*)            INSYMBOL; IF LKEY <= 7 THEN
(*3979*)               VARIABLE(FSYS+(.COMMA,RPARENT.)) ELSE
(*3980*)               EXPRESSION(FSYS+(.COMMA,COLON,RPARENT,IDENT.));
(*3981*)             END;  // end loop 10
(*3982*)             TEST1(RPARENT,4);
(*3983*)           END
(*3984*)          ELSE IF (LKEY=6) OR (LKEY=8) THEN ERROR(116);
(*3985*)          IF ( LKEY IN (.7,9.) ) AND
(*3986*)             (NOT FIL1ATTRP^.TYPTR^.TEXTFILE) THEN ERROR(116);
(*3987*)          IF LKEY = 7 THEN GENRX(ZBAL,BASEWORK,0,1,ENTRYRL) ELSE
(*3988*)            IF LKEY = 9 THEN GENRX(ZBAL,BASEWORK,0,1,ENTWRITLN);
(*3989*)       END;
(*3990*) 
(*3991*) 
// $TITLE  PAGE
(*3992*)     PROCEDURE PAGE;
(*3993*)       BEGIN
(*3994*)         IF SY<>LPARENT
(*3995*)        THEN BEGIN IF OUTPUTPTR= NIL THEN ERROR(176) ELSE
(*3996*)                SETSTFILATTR(GATTRP,OUTPUTPTR)
(*3997*)             END
(*3998*)           ELSE BEGIN INSYMBOL; VARIABLE(FSYS+(.RPARENT.));
(*3999*)                  IF SY=RPARENT THEN INSYMBOL ELSE ERROR(9);
(*4000*)                END;
(*4001*)         IF GATTRP^.TYPTR <> NIL THEN
(*4002*)           BEGIN
(*4003*)             WITH GATTRP^.TYPTR^ DO
(*4004*)               IF FORM = FILES THEN
(*4005*)                 BEGIN IF NOT TEXTFILE THEN ERROR(116);
(*4006*)                   LOADADDRESS(GATTRP,NIL);
(*4007*)                   GENRR(ZLR,15,REALREG(.GATTRP^.REXPR.RNO.));
(*4008*)                   GENRX(ZBAL,BASEWORK,0,1,ENTPAGE);
(*4009*)                   RESETG;
(*4010*)                 END
(*4011*)               ELSE ERROR(116)
(*4012*)           END;
(*4013*)       END (*PAGE*) ;
(*4014*) 
// $TITLE   PACK
(*4015*)PROCEDURE PACK;
(*4016*)  VAR
(*4017*)    LATTRP,CATTRP : ATTRP;
(*4018*)    LOW,HIGH,LMIN,LMAX:INTEGER;
(*4019*)    LSP,LSP1:STP;
(*4020*) 
(*4021*)BEGIN (* PACK *)
(*4022*)  TEST1(LPARENT,9);
(*4023*)  VARIABLE(FSYS+(.COMMA,RPARENT.));
(*4024*)  ATTRNEW(LATTRP); COPYATTR(GATTRP,LATTRP);
(*4025*)  LOW:=0; HIGH:=0; LSP:=NIL; LSP1:=NIL;
(*4026*)  IF GATTRP^.TYPTR <> NIL THEN
(*4027*)   WITH GATTRP^.TYPTR^ DO
(*4028*)     IF FORM = ARRAYS THEN
(*4029*)      IF AELTYPE^.FORM <> PACKDTYPE THEN
(*4030*)      BEGIN
(*4031*)        LSP:=INXTYPE; LSP1:=AELTYPE;
(*4032*)       IF LSP <> NIL THEN GETBOUNDS(LSP,LOW,HIGH);
(*4033*)     END
(*4034*)   ELSE ERROR(116)
(*4035*)  ELSE ERROR(116);
(*4036*)  TEST1(COMMA,20);
(*4037*)  EXPRESSION(FSYS+(.COMMA,RPARENT.));
(*4038*)  IF NOT COMPTYPES(GATTRP^.TYPTR,LSP) THEN ERROR(116);
(*4039*)  TEST1(COMMA,20);
(*4040*)  ATTRNEW(CATTRP);  COPYATTR(GATTRP,CATTRP);
(*4041*)  VARIABLE(FSYS+(.RPARENT.));
(*4042*)  IF GATTRP^.TYPTR <> NIL THEN
(*4043*)  WITH GATTRP^.TYPTR^ DO
(*4044*)  BEGIN
(*4045*)    IF FORM = ARRAYS THEN
(*4046*)           IF (AELTYPE^.FORM = PACKDTYPE) THEN
(*4047*)             IF COMPTYPES(AELTYPE,LSP1) AND
(*4048*)         COMPTYPES(INXTYPE,LSP) THEN
(*4049*)     BEGIN
(*4050*)       LMIN:=0; LMAX:=0;
(*4051*)       IF INXTYPE <> NIL THEN GETBOUNDS(INXTYPE,LMIN,LMAX);
(*4052*)       IF LMAX-LMIN>HIGH-LOW THEN ERROR(116);
(*4053*)            CHECKRANGE(CATTRP,LOW,LMIN-LMAX+HIGH,116);
(*4054*)  IF (LATTRP^.TYPTR<>NIL) AND (CATTRP^.TYPTR<>NIL) THEN
(*4055*)  BEGIN
(*4056*)    LATTRP^.VADRS:=LATTRP^.VADRS-4*LOW;
(*4057*)    IF CATTRP^.KIND=CST THEN
(*4058*)    BEGIN
(*4059*)      LATTRP^.VADRS:=LATTRP^.VADRS+4*CATTRP^.CVAL.IVAL;
(*4060*)      LOADADDRESS(LATTRP,NIL);
(*4061*)    END ELSE
(*4062*)    BEGIN
(*4063*)      LOADADDRESS(LATTRP,NIL);
(*4064*)     LOAD(CATTRP,NIL);
(*4065*)     GENRX(ZSLL,REALREG(.CATTRP^.REXPR.RNO.),0,0,2);
(*4066*)     GENRR(ZAR,REALREG(.LATTRP^.REXPR.RNO.),
(*4067*)     REALREG(.CATTRP^.REXPR.RNO.));
(*4068*)   END
(*4069*)   END;
(*4070*)        LOAD(CATTRP,NIL);
(*4071*)        IF GATTRP^.TYPTR <> NIL THEN
(*4072*)        LOADADDRESS(GATTRP,CATTRP);
(*4073*)        LOADINTCONST(REALREG(.CATTRP^.REXPR.RNO.),ABS(LMAX-LMIN)+1);
(*4074*)            GENRX(ZL,0,0,REALREG(.LATTRP^.REXPR.RNO.),0);
(*4075*)            GENRX(ZSTC,0,0,REALREG(.GATTRP^.REXPR.RNO.),0);
(*4076*)            GENRX(ZLA,REALREG(.LATTRP^.REXPR.RNO.),0,
(*4077*)                      REALREG(.LATTRP^.REXPR.RNO.),4);
(*4078*)            GENRX(ZLA,REALREG(.GATTRP^.REXPR.RNO.),0,
(*4079*)                      REALREG(.GATTRP^.REXPR.RNO.),1);
(*4080*)            GENRX(ZBCT,REALREG(.CATTRP^.REXPR.RNO.),0,PBASE1,IC-16);
(*4081*)         END ELSE ERROR(116)
(*4082*)            ELSE ERROR(118)
(*4083*)       ELSE ERROR(116);
(*4084*) 
(*4085*)     END;
(*4086*)     ATTRDISP(CATTRP); ATTRDISP(LATTRP);
(*4087*)     RESETG;
(*4088*)     TEST1(RPARENT,4);
(*4089*)END; (* PACK *)
(*4090*) 
(*4091*) 
// $TITLE   UNPACK
(*4092*)PROCEDURE UNPACK;
(*4093*)VAR
(*4094*)  SOURCE,DEST:ATTRP;
(*4095*)  LOW,HIGH,LMIN,LMAX : INTEGER;
(*4096*)  LSP,LSP1 : STP;
(*4097*) 
(*4098*)BEGIN (* UNPACK *)
(*4099*)  TEST1(LPARENT,9);
(*4100*)  EXPRESSION(FSYS+(.COMMA,RPARENT.));
(*4101*)  LSP:=NIL; LSP1:=NIL; LMIN:=0; LMAX:=0;
(*4102*)  IF GATTRP^.TYPTR <> NIL THEN
(*4103*)    WITH GATTRP^.TYPTR^ DO
(*4104*)      IF FORM = ARRAYS THEN
(*4105*)        IF AELTYPE^.FORM = PACKDTYPE THEN
(*4106*)        BEGIN
(*4107*)          LSP:=INXTYPE; LSP1:=AELTYPE;
(*4108*)          IF LSP <> NIL THEN GETBOUNDS(LSP,LMIN,LMAX);
(*4109*)        END
(*4110*)        ELSE ERROR(118)
(*4111*)      ELSE ERROR(116);
(*4112*)  ATTRNEW(SOURCE); COPYATTR(GATTRP,SOURCE);
(*4113*)  TEST1(COMMA,20);
(*4114*)  VARIABLE(FSYS+(.COMMA,RPARENT.));
(*4115*)  ATTRNEW(DEST); COPYATTR(GATTRP,DEST);
(*4116*)  IF DEST^.TYPTR <> NIL THEN
(*4117*)    WITH DEST^.TYPTR^ DO
(*4118*)      IF FORM = ARRAYS THEN
(*4119*)        IF ( AELTYPE^.FORM <> PACKDTYPE) THEN
(*4120*)          IF COMPTYPES(INXTYPE,LSP) AND COMPTYPES(AELTYPE,LSP1) THEN
(*4121*)          BEGIN
(*4122*)            LOW:=0; HIGH :=0;
(*4123*)            IF INXTYPE <> NIL THEN GETBOUNDS(INXTYPE,LOW,HIGH);
(*4124*)            IF LMAX-LMIN > HIGH - LOW THEN ERROR(116);
(*4125*)          END
(*4126*)          ELSE ERROR(116)
(*4127*)        ELSE ERROR(116)
(*4128*)      ELSE ERROR(116);
(*4129*)  TEST1(COMMA,20);
(*4130*)  EXPRESSION(FSYS+(.RPARENT.));
(*4131*)  CHECKRANGE(GATTRP,LOW,LMIN-LMAX+HIGH,116);
(*4132*)  IF (DEST^.TYPTR <> NIL) AND (GATTRP^.TYPTR<>NIL) THEN
(*4133*)  BEGIN
(*4134*)    DEST^.VADRS := DEST^.VADRS - 4*LOW;
(*4135*)    IF GATTRP^.KIND = CST THEN
(*4136*)    BEGIN
(*4137*)      DEST^.VADRS := DEST^.VADRS + 4*GATTRP^.CVAL.IVAL;
(*4138*)      LOADADDRESS(DEST,NIL);
(*4139*)    END ELSE
(*4140*)    BEGIN
(*4141*)      LOADADDRESS(DEST,NIL);
(*4142*)      LOAD(GATTRP,NIL);
(*4143*)      GENRX(ZSLL,REALREG(.GATTRP^.REXPR.RNO.),0,0,2);
(*4144*)      GENRR(ZAR,REALREG(.DEST^.REXPR.RNO.),REALREG(.GATTRP^.REXPR.RNO.));
(*4145*)    END;
(*4146*)  END;
(*4147*)  LOAD(GATTRP,NIL);
(*4148*)  IF SOURCE^.TYPTR <> NIL THEN
(*4149*)  LOADADDRESS(SOURCE,NIL);
(*4150*)  LOADINTCONST(REALREG(.GATTRP^.REXPR.RNO.),ABS(LMAX-LMIN)+1);
(*4151*)  LOADINTCONST(R0,0);
(*4152*)  GENRX(ZIC,0,0,REALREG(.SOURCE^.REXPR.RNO.),0);
(*4153*)  GENRX(ZST,0,0,REALREG(.DEST^.REXPR.RNO.),0);
(*4154*)  GENRX(ZLA,REALREG(.SOURCE^.REXPR.RNO.),0,
(*4155*)            REALREG(.SOURCE^.REXPR.RNO.),1);
(*4156*)  GENRX(ZLA,REALREG(.DEST^.REXPR.RNO.),0,
(*4157*)            REALREG(.DEST^.REXPR.RNO.),4);
(*4158*)  GENRX(ZBCT,REALREG(.GATTRP^.REXPR.RNO.),0,PBASE1,IC-16);
(*4159*)  ATTRDISP(SOURCE); ATTRDISP(DEST); RESETG;
(*4160*)  TEST1(RPARENT,4);
(*4161*)END; (* UNPACK *)
// $TITLE  TIME AND DATE FUNCTIONS
(*4162*)    PROCEDURE TIMEDATE;
(*4163*)      VAR LMIN,LMAX:INTEGER;
(*4164*)      BEGIN
(*4165*)        TEST1(LPARENT,9);
(*4166*)        VARIABLE(FSYS+(.RPARENT.));
(*4167*)        WITH GATTRP^ DO
(*4168*)          IF TYPTR<>NIL THEN
(*4169*)            IF TYPTR^.FORM<>ARRAYS THEN ERRORRESET(116)
(*4170*)              ELSE IF TYPTR^.AELTYPE<>NIL THEN
(*4171*)                IF (TYPTR^.AELTYPE^.FORM<>PACKDTYPE) OR
(*4172*)                    (TYPTR^.AELTYPE^.BASETYPE<>CHARPTR)
(*4173*)                  THEN ERRORRESET(116)
(*4174*)                  ELSE BEGIN GETBOUNDS(TYPTR^.INXTYPE,LMIN,LMAX);
(*4175*)                          IF LMAX-LMIN<>7 THEN ERRORRESET(116);
(*4176*)                       END;
(*4177*)        IF GATTRP^.TYPTR<>NIL THEN
(*4178*)          BEGIN LOADADDRESS(GATTRP,NIL);
(*4179*)            GENRR(ZLR,R0,REALREG(.GATTRP^.REXPR.RNO.));
(*4180*)            GENRX(ZBAL,BASEWORK,0,1,ENTRYTIME+8*(LKEY-10));
(*4181*)          END;
(*4182*)        RESETG;
(*4183*)        TEST1(RPARENT,4);
(*4184*)      END;
(*4185*) 
// $TITLE  NEW - PROCEDURE
(*4186*)    PROCEDURE NEWPROC;
(*4187*)      LABEL 1;
(*4188*)      TYPE TAGPTR = ^TAGSTORE;
(*4189*)       TAGSTORE = RECORD OP,VAL,OFFST:INTEGER;NXT:TAGPTR END;
(*4190*)      VAR LSP,LSP1: STP; LVAL: VALU; LSIZE: WBSIZE; LMIN,LMAX: INTEGER;
(*4191*)          STOREOP:INTEGER;
(*4192*)          STMARK : ^BOOLEAN;
(*4193*)          SAVEDISP,FIRSTDISP : TAGPTR;
(*4194*)      BEGIN IF SY = LPARENT THEN INSYMBOL ELSE ERROR(9);
(*4195*)        VARIABLE(FSYS+(.COMMA,RPARENT.));
(*4196*)        FIRSTDISP :=NIL;
(*4197*)        LSP := NIL; INITSIZE(LSIZE);
(*4198*)        IF GATTRP^.TYPTR <> NIL THEN
(*4199*)          WITH GATTRP^.TYPTR^ DO
(*4200*)            IF FORM = POINTER THEN
(*4201*)              BEGIN
(*4202*)                IF ELTYPE <> NIL THEN
(*4203*)                  BEGIN LSIZE := ELTYPE^.SIZE;
(*4204*)                    IF ELTYPE^.FORM = RECORDS THEN LSP := ELTYPE^.RECVAR
(*4205*)                  END
(*4206*)              END
(*4207*)            ELSE ERROR(116);
(*4208*)        WHILE SY = COMMA DO
(*4209*)          BEGIN INSYMBOL; CONSTANT(FSYS+(.COMMA,RPARENT.),LSP1,LVAL);
(*4210*)            IF LSP = NIL THEN ERROR(158)
(*4211*)            ELSE
(*4212*)              IF LSP^.TGFLDP <> NIL THEN
(*4213*)                IF (LSP1 = REALPTR) OR xSTRING(LSP1) THEN ERROR(159)
(*4214*)                ELSE
(*4215*)                  IF COMPTYPES(LSP^.TGFLDP^.IDTYPE,LSP1) THEN
(*4216*)                    BEGIN
(*4217*)                      GETBOUNDS(LSP^.TGFLDP^.IDTYPE,LMIN,LMAX);
(*4218*)                      IF (LVAL.IVAL > LMAX) OR (LVAL.IVAL < LMIN) THEN ERROR(181);
(*4219*)                      IF LSP^.TGFLDP^.NAME<>'        ' THEN
(*4220*)                        BEGIN
(*4221*)                          IF LSP^.TGFLDP^.IDTYPE<>NIL THEN
(*4222*)                            BEGIN IF LSP^.TGFLDP^.IDTYPE^.FORM=PACKDTYPE
(*4223*)                                    THEN STOREOP:=ZSTC
(*4224*)                                    ELSE STOREOP:=ZST;
(*4225*)                              NEW(SAVEDISP); SAVEDISP^.NXT:=NIL;
(*4226*)                               IF FIRSTDISP=NIL THEN
(*4227*)                                FIRSTDISP:=SAVEDISP
(*4228*)                               ELSE
(*4229*)                               BEGIN
(*4230*)                                SAVEDISP^.NXT:=FIRSTDISP;
(*4231*)                                FIRSTDISP:=SAVEDISP
(*4232*)                               END;
(*4233*)                               WITH SAVEDISP^ DO
(*4234*)                               BEGIN
(*4235*)                                OP:=STOREOP;
(*4236*)                                VAL := LVAL.IVAL;
(*4237*)                                OFFST :=LSP^.TGFLDP^.FLDADDR
(*4238*)                               END;
(*4239*)                            END;
(*4240*)                        END;
(*4241*)                      LSP1 := LSP^.FSTVAR;
(*4242*)                      WHILE LSP1 <> NIL DO
(*4243*)                        WITH LSP1^ DO
(*4244*)                          IF VARVAL=LVAL.IVAL THEN
(*4245*)                            BEGIN LSP := SUBVAR;
(*4246*)                              LSIZE:=SIZE;
(*4247*)                              GOTO 1
(*4248*)                            END
(*4249*)                          ELSE LSP1 := NXTVAR;
(*4250*)                      LSIZE:=LSP^.SIZE;
(*4251*)                      LSP:=NIL;
(*4252*)                    END
(*4253*)                  ELSE ERROR(116);
(*4254*)      1:  END (*WHILE*) ;
(*4255*)  ALIGNMENT(LSIZE.WBLENGTH,4); MAKEINTCONST(LSIZE.WBLENGTH);
(*4256*)  GENRX(ZS,NEWPOINTER,0,0,0);
(*4257*)  IF LSIZE.BOUNDARY=8 THEN
(*4258*)    BEGIN MAKEINTCONST(-8); GENRX(ZN,NEWPOINTER,0,0,0); END;
(*4259*)  OVERFLOWTEST;
(*4260*)  LOADINDEX(GATTRP,NIL); LOADBASE(GATTRP);
(*4261*)  GENRX(ZST,NEWPOINTER,RINDEX,RBASE,EFFADRS);
(*4262*)  WHILE FIRSTDISP <> NIL DO
(*4263*)  BEGIN
(*4264*)    WITH FIRSTDISP^ DO
(*4265*)    BEGIN
(*4266*)      LOADINTCONST(R0,VAL);
(*4267*)      GENRX(OP,R0,0,NEWPOINTER,OFFST)
(*4268*)    END;
(*4269*)    FIRSTDISP:=FIRSTDISP^.NXT
(*4270*)  END;
(*4271*)     TEST1(RPARENT,4);
(*4272*)      END (*NEWPROC*) ;
(*4273*) 
// $TITLE  MARK AND RELEASE
(*4274*)    PROCEDURE MARKRELEASE;
(*4275*)    BEGIN
(*4276*)        TEST1(LPARENT,9);
(*4277*)        VARIABLE(FSYS+(.COMMA,RPARENT.));
(*4278*)        IF GATTRP^.TYPTR <> NIL THEN
(*4279*)          IF GATTRP^.TYPTR^.FORM = POINTER THEN
(*4280*)            BEGIN
(*4281*)              IF LKEY = 13 THEN
(*4282*)                BEGIN LOADINDEX(GATTRP,NIL); LOADBASE(GATTRP);
(*4283*)                      GENRX(ZST,NEWPOINTER,RINDEX,RBASE,EFFADRS);
(*4284*)                END
(*4285*)              ELSE
(*4286*)                BEGIN
(*4287*)                  CHECKPOINTER(GATTRP,FALSE);
(*4288*)                  LOAD(GATTRP,NIL); GENRR(ZLR,NEWPOINTER,REALREG(.GATTRP^.REXPR.RNO.))
(*4289*)                END
(*4290*)            END
(*4291*)          ELSE ERROR(116);
(*4292*)        RESETG;
(*4293*)        TEST1(RPARENT,4);
(*4294*)      END;
(*4295*) 
// $TITLE  STANDARD PROCEDURES AND FUNCTS
(*4296*) 
(*4297*)PROCEDURE LEFTXPRS;
(*4298*)  BEGIN (* LEFTXPRS *)
(*4299*)    IF SY = LPARENT THEN INSYMBOL ELSE ERROR(9);                                
(*4300*)    EXPRESSION(FSYS+(.RPARENT.));
(*4301*)  END; (* LEFTXPRS *)
(*4302*) 
(*4303*) 
(*4304*) 
(*4305*)    PROCEDURE ROUNDTRUNCF;
(*4306*)      VAR TEMP:CMP; ZERO,HALF,ONE:VALU;
(*4307*)      BEGIN
(*4308*)         LEFTXPRS;
(*4309*)        IF GATTRP^.TYPTR<>REALPTR THEN ERROR(125);
(*4310*)        LOAD(GATTRP,NIL); ZERO.CKIND:=PSET; ZERO.PVAL:=(.1,4,5,6.);
(*4311*)        IF LKEY=4 THEN
(*4312*)          BEGIN HALF.CKIND:=REEL; HALF.RVAL:=0.5;
(*4313*)                ONE.CKIND:=REEL; ONE.RVAL:=1.0;
(*4314*)                MAKECONSTANT(HALF); GENRXP(ZAD,GATTRP^.REXPR.RNO,0,0,0);
(*4315*)                GENRX(ZBC,CONDP,0,PBASE1,IC+8);
(*4316*)                MAKECONSTANT(ONE); GENRXP(ZSD,GATTRP^.REXPR.RNO,0,0,0);
(*4317*)          END;
(*4318*)        MAKECONSTANT(ZERO); GENRXP(ZAW,GATTRP^.REXPR.RNO,0,0,0);
(*4319*)        GETTEMP(8,TEMP); REGSEARCH(NIL,DOUBLE);
(*4320*)        BASEREGISTER(LEVEL,TEMP^.TEMPADRS);
(*4321*)        GENRXP(ZSTD,GATTRP^.REXPR.RNO,0,RBASE,EFFADRS);
(*4322*)        GENRX(ZLM,RMAIN,RMAIN+1,RBASE,EFFADRS);
(*4323*)        GENRX(ZLA,RMAIN,0,RMAIN,0); GENRX(ZSLDA,RMAIN,0,0,32);
(*4324*)        GENRX(ZTM,8,0,RBASE,EFFADRS); GENRX(ZBC,14,0,PBASE1,IC+6);
(*4325*)        GENRR(ZLNR,RMAIN,RMAIN);
(*4326*)        WITH GATTRP^ DO
(*4327*)          BEGIN REGISTER(.REXPR.RNO.).USED:=FALSE;
(*4328*)                TYPTR:=INTPTR; REXPR.RNO:=RWORK;
(*4329*)          END;
(*4330*)        REGISTER(.RWORK.).USED:=TRUE; REGISTER(.RWORK.).REGCONT:=GATTRP;
(*4331*)        DELETETEMP(TEMP);
(*4332*)        TEST1(RPARENT,4);
(*4333*)      END;
(*4334*) 
(*4335*)    PROCEDURE ABSF;
(*4336*)     BEGIN
(*4337*)        LEFTXPRS;
(*4338*)        LOAD(GATTRP,NIL);
(*4339*)        WITH GATTRP^ DO
(*4340*)          IF COMPTYPES(TYPTR,INTPTR)
(*4341*)            THEN GENRRP1(ZLPR,REXPR.RNO)
(*4342*)            ELSE IF TYPTR=REALPTR
(*4343*)              THEN GENRRP1(ZLPDR,REXPR.RNO)
(*4344*)              ELSE ERROR(125);
(*4345*)        TEST1(RPARENT,4);
(*4346*)      END;
(*4347*) 
(*4348*)    PROCEDURE SQRF;
(*4349*)      BEGIN
(*4350*)        LEFTXPRS;
(*4351*)        WITH GATTRP^ DO
(*4352*)          BEGIN
(*4353*)            IF TYPTR=REALPTR THEN
(*4354*)              BEGIN LOAD(GATTRP,NIL); GENRRP1(ZMDR,REXPR.RNO); END
(*4355*)            ELSE IF COMPTYPES(TYPTR,INTPTR) THEN
(*4356*)              BEGIN LOADEVENODD(GATTRP,NIL,1); GENRRP(ZMR,PRED(REXPR.RNO),REXPR.RNO);
(*4357*)              END
(*4358*)            ELSE ERROR(125);
(*4359*)          END;
(*4360*)       TEST1(RPARENT,4);
(*4361*)      END;
(*4362*) 
(*4363*)    PROCEDURE ODDP;
(*4364*)     BEGIN
(*4365*)        LEFTXPRS;
(*4366*)        IF NOT COMPTYPES(GATTRP^.TYPTR,INTPTR) THEN ERROR(125);
(*4367*)        LOAD(GATTRP,NIL); MAKEINTCONST(1);
(*4368*)        GENRXP(ZN,GATTRP^.REXPR.RNO,0,0,0);
(*4369*)        GATTRP^.TYPTR := BOOLPTR;
(*4370*)        TEST1(RPARENT,4);
(*4371*)      END;
(*4372*) 
(*4373*)    PROCEDURE ORDF;
(*4374*)     BEGIN
(*4375*)        LEFTXPRS;
(*4376*)        WITH GATTRP^ DO
(*4377*)          IF TYPTR <> NIL THEN
(*4378*)            IF TYPTR^.FORM>=POWER
(*4379*)              THEN ERROR(125)
(*4380*)              ELSE IF TYPTR=REALPTR
(*4381*)                THEN ERROR(125)
(*4382*)                ELSE IF TYPTR^.SIZE.WBLENGTH<>1
(*4383*)                  THEN TYPTR:=INTPTR
(*4384*)                  ELSE TYPTR:=PACKDINTPTR;
(*4385*)        TEST1(RPARENT,4);
(*4386*)      END;
(*4387*) 
(*4388*)    PROCEDURE CHRF;
(*4389*)    BEGIN
(*4390*)        LEFTXPRS;
(*4391*)        WITH GATTRP^ DO
(*4392*)          IF COMPTYPES(TYPTR,INTPTR)
(*4393*)            THEN IF TYPTR^.SIZE.WBLENGTH<>1
(*4394*)              THEN TYPTR:=CHARPTR
(*4395*)              ELSE TYPTR:=PACKDCHARPTR
(*4396*)            ELSE ERROR(125);
(*4397*)        TEST1(RPARENT,4);
(*4398*)      END;
(*4399*) 
(*4400*)    PROCEDURE PREDSUCCF;
(*4401*)    BEGIN
(*4402*)        LEFTXPRS;
(*4403*)        IF GATTRP^.TYPTR <> NIL THEN
(*4404*)          WITH GATTRP^ DO
(*4405*)            IF TYPTR^.FORM > SUBRANGE THEN ERROR(125)
(*4406*)            ELSE IF TYPTR=REALPTR THEN ERROR(125);
(*4407*)        LOAD(GATTRP,NIL);
(*4408*)        IF LKEY = 11
(*4409*)          THEN BEGIN MAKEINTCONST(1);
(*4410*)                 GENRXP(ZA,GATTRP^.REXPR.RNO,0,0,0);
(*4411*)               END
(*4412*)          ELSE GENRR(ZBCTR,REALREG(.GATTRP^.REXPR.RNO.),0);
(*4413*)          TEST1(RPARENT,4);
(*4414*)      END;
(*4415*) 
(*4416*)PROCEDURE HALT;
(*4417*)BEGIN
(*4418*)  GENRX(ZBAL,BASEWORK,0,1,ENTRYHALT);
(*4419*)END;
(*4420*) 
(*4421*) 
(*4422*) 
(*4423*)PROCEDURE MESSAGE;
(*4424*)   VAR LSP:STP;
(*4425*)BEGIN(*MESSAGE*)
(*4426*)  LEFTXPRS;
(*4427*)  IF GATTRP^.TYPTR <> NIL THEN
(*4428*)         IF xSTRING(GATTRP^.TYPTR) OR
(*4429*)            COMPTYPES(GATTRP^.TYPTR,CHARPTR) THEN
(*4430*)    BEGIN
(*4431*)     LSP:=GATTRP^.TYPTR;
(*4432*)     LOADADDRESS(GATTRP,NIL);
(*4433*)      LOADINTCONST(R0,16*LSP^.SIZE.WBLENGTH+
(*4434*)                         REALREG(.GATTRP^.REXPR.RNO.));
(*4435*)      GENRX(ZBAL,BASEWORK,0,1,ENTRYMESSAGE);
(*4436*)      RESETG;
(*4437*)    END ELSE ERROR(116);
(*4438*)   TEST1(RPARENT,4);
(*4439*)END; (* MESSAGE *)
(*4440*) 
(*4441*) 
(*4442*) 
(*4443*)PROCEDURE CARD;
(*4444*)BEGIN (* CARD *)
(*4445*)  LEFTXPRS;
(*4446*)  LOAD(GATTRP,NIL);
(*4447*)  IF GATTRP^.TYPTR <> NIL THEN
(*4448*)    IF GATTRP^.TYPTR^.FORM = POWER THEN
(*4449*)    BEGIN
(*4450*)      REGSEARCH(NIL,SINGLE);
(*4451*)      GENRR(ZSR,RMAIN,RMAIN);
(*4452*)      GENRXP(ZSLDA,GATTRP^.REXPR.RNO,0,0,0);
(*4453*)      GENRX(ZBC,CONDZ,0,PBASE1,IC+20);
(*4454*)      GENRX(ZBC,CONDP,0,PBASE1,IC+8);
(*4455*)      GENRX(ZLA,RMAIN,0,RMAIN,1);
(*4456*)      GENRXP(ZSLDL,GATTRP^.REXPR.RNO,0,0,1);
(*4457*)      GENRX(ZBC,15,0,PBASE1,IC-20);
(*4458*)      WITH GATTRP^ DO
(*4459*)      BEGIN
(*4460*)        TYPTR := INTPTR;
(*4461*)        REGISTER(.REXPR.RNO.).USED := FALSE;
(*4462*)        REXPR.RNO:=RWORK;
(*4463*)      END;
(*4464*)      REGISTER(.RWORK.).USED := TRUE;
(*4465*)      REGISTER(.RWORK.).REGCONT := GATTRP;
(*4466*)    END ELSE ERROR(116);
(*4467*)  TEST1(RPARENT,4);
(*4468*)END; (* CARD *)
(*4469*)     PROCEDURE STDFLFUNCS;
(*4470*)       BEGIN
(*4471*)         IF SY<>LPARENT
(*4472*)           THEN BEGIN IF INPUTPTR=NIL THEN ERROR(175);
(*4473*)                  SETSTFILATTR(GATTRP,INPUTPTR);
(*4474*)                END
(*4475*)           ELSE BEGIN INSYMBOL; VARIABLE(FSYS+(.RPARENT.));
(*4476*)                  IF SY=RPARENT THEN INSYMBOL ELSE ERROR(9);
(*4477*)                END;
(*4478*)         IF GATTRP^.TYPTR <> NIL THEN
(*4479*)           WITH GATTRP^, TYPTR^ DO
(*4480*)             IF FORM = FILES THEN
(*4481*)               BEGIN
(*4482*)                 VADRS:=VADRS+4; LOAD(GATTRP,NIL);
(*4483*)                 IF LKEY=2 THEN
(*4484*)                   BEGIN IF NOT TEXTFILE THEN ERROR(125);
(*4485*)                     GENRXP(ZSRL,GATTRP^.REXPR.RNO,0,0,1);
(*4486*)                   END;
(*4487*)                 MAKEINTCONST(1); GENRXP(ZN,GATTRP^.REXPR.RNO,0,0,0);
(*4488*)                 TYPTR := BOOLPTR; KIND := EXPR;
(*4489*)               END
(*4490*)             ELSE ERROR(125);
(*4491*)     END;
(*4492*) 
(*4493*)    PROCEDURE STDARITHFUNCS;
(*4494*)  VAR TP : INTEGER; NAME : ALFA;                                                
(*4495*)  (* NOTE : STANDARD PROCS/FUNCTS USED LIKE THIS                                
  4496              MUST HAVE A LENGTH OF <= ALFALENG-2  *)                             
(*4497*)    BEGIN
(*4498*)        NAME := FCP^.NAME;                                                      
(*4499*)        TP := ALFALENG;                                                         
(*4500*)        WHILE NAME(.TP.) =' ' DO TP:=TP-1;                                      
(*4501*)        NAME (.TP+1.) := '@';
(*4502*)        NAME (.TP+2.) := 'P';                                                   
(*4503*)        TP := 1;                                                                
(*4504*)          WHILE (TP<>NRSTARITH) AND (STDPRCS(.TP.) <> NAME)                     
(*4505*)           AND (STDPRCS(.TP.) <> '        ') DO TP:=TP+1;                       
(*4506*)        IF STDPRCS(.TP.) = '        ' THEN                                      
(*4507*)         STDPRCS(.TP.) := NAME ELSE                                             
(*4508*)          IF STDPRCS(.TP.) <> NAME THEN ERROR(400);                             
(*4509*)        LEFTXPRS;
(*4510*)        IF COMPTYPES(GATTRP^.TYPTR,INTPTR)
(*4511*)          THEN INTTOREAL(GATTRP);
(*4512*)        IF GATTRP^.TYPTR<>REALPTR THEN ERROR(125);
(*4513*)        LOAD(GATTRP,NIL);
(*4514*)        LOADINTCONST(R0,REALREG(.GATTRP^.REXPR.RNO.));
(*4515*)        GENRX(ZBAL,BASEWORK,0,1,ENTRYSIN+(LKEY-12)*8);
(*4516*)        TEST1(RPARENT,4);
(*4517*)      END;
(*4518*) 
(*4519*)    PROCEDURE CLOCKF;
(*4520*)      BEGIN
(*4521*)        REGSEARCH(NIL,SINGLE);
(*4522*)        GENRX(ZBAL,BASEWORK,0,1,ENTRYCLOCK);
(*4523*)        GENRR(ZLR,RMAIN,R0);
(*4524*)        WITH GATTRP^ DO
(*4525*)          BEGIN TYPTR:=INTPTR; KIND:=EXPR;
(*4526*)                REXPR.REGTEMP:=REGIST; REXPR.RNO:=RWORK;
(*4527*)          END;
(*4528*)        REGISTER(.RWORK.).USED:=TRUE; REGISTER(.RWORK.).REGCONT:=GATTRP;
(*4529*)      END;
(*4530*) 
// $TITLE   CALL OF NON STANDARD PROCEDURES
(*4531*)  PROCEDURE CALLNONSTANDARD;
(*4532*)    VAR NXT,LCP,NXT1,NXT2:CTP; PASSPROC:BOOLEAN;
(*4533*)        OLDSTACK:INTEGER; FORMAL:ATTRP;
(*4534*)        FSP:STP; KIND:REGKIND; X:INTEGER;
(*4535*)        FLOATREG:REGNO;
(*4536*)        FORT : BOOLEAN;
(*4537*)        L : INTEGER;
(*4538*)      FORTSTACK : INTEGER;                                                      
(*4539*)    BEGIN
(*4540*)      NXT:=FCP^.PARAMS;
(*4541*)      OLDSTACK:=STACKTOP;
(*4542*)      IF OLDSTACK<>0 THEN
(*4543*)        BEGIN ALIGNMENT(OLDSTACK,8); MAKEINTCONST(OLDSTACK);
(*4544*)              GENRX(ZA,STACKPOINTER,0,0,0); STACKTOP:=0;
(*4545*)        END;
(*4546*)     L:=FCP^.PFCNT;                                                             
(*4547*)     FORT := (PROCADDRESS(.L+1.)=0) AND                                         
(*4548*)             (PROCADDRESS(.L.) >1 ) AND                                         
(*4549*)             (PROCADDRESS(.L.) <= 4);                                           
(*4550*)      IF SY=LPARENT THEN
(*4551*)        BEGIN
(*4552*)          REPEAT PASSPROC:=FALSE;
(*4553*)            IF NXT=NIL THEN ERROR(126)
(*4554*)              ELSE IF NXT^.KLASS IN (.PROC,FUNC.) THEN PASSPROC:=TRUE;
(*4555*)            INSYMBOL;
(*4556*)            IF PASSPROC THEN
(*4557*)              BEGIN
(*4558*)                IF SY<>IDENT
(*4559*)                  THEN BEGIN ERROR(2); SKIP(FSYS+(.COMMA,RPARENT.)); END
(*4560*)                ELSE
(*4561*)                  BEGIN
(*4562*)                    IF NXT^.KLASS=PROC THEN SEARCHID((.PROC.),LCP)
(*4563*)                    ELSE BEGIN SEARCHID((.FUNC.),LCP);
(*4564*)                           IF NOT COMPTYPES(NXT^.IDTYPE,LCP^.IDTYPE) THEN ERROR(128);
(*4565*)                         END;
(*4566*)                    IF LCP^.PFDECKIND=STANDARD THEN ERROR(164)
(*4567*)                    ELSE
(*4568*)                      BEGIN NXT1:=NXT^.PARAMS;
(*4569*)                        NXT2:=LCP^.PARAMS;
(*4570*)                        WHILE (NXT1<>NIL) AND (NXT2<>NIL) DO
(*4571*)                          BEGIN
(*4572*)                            IF NXT2^.KLASS<>VARS THEN ERROR(170)
(*4573*)                            ELSE IF NXT2^.VKIND=INDRCT THEN ERROR(170)
(*4574*)                            ELSE IF NOT COMPTYPES(NXT1^.IDTYPE,NXT2^.IDTYPE) THEN ERROR(186);
(*4575*)                            NXT1:=NXT1^.NEXT; NXT2:=NXT2^.NEXT;
(*4576*)                          END;
(*4577*)                        IF NXT1<>NXT2 THEN ERROR(186);
(*4578*)                        WITH LCP^ DO
(*4579*)                          IF PFKIND=ACTUAL
(*4580*)                        THEN
(*4581*)                        BEGIN
(*4582*)                       IF NOT EXTRNL THEN L := 4 ELSE L:=8;                     
(*4583*)                       GENRX(ZLA,R0,0,0,PROCBASE+4*PFCNT-L);                    
(*4584*)                          BASEREGISTER(STACKPOINTER,NXT^.PFADDR);
(*4585*)                          GENRX(ZST,R0,0,RBASE,EFFADRS);
(*4586*)                          BASEREGISTER(STACKPOINTER,NXT^.PFADDR+4);
(*4587*)                          GENRX(ZST,STACKPOINTER,0,RBASE,EFFADRS);
(*4588*)                        END ELSE
(*4589*)                        BEGIN
(*4590*)                          BASEREGISTER(PFLEV,PFADDR);
(*4591*)                          GENSS(ZMVC,7,8,NXT^.PFADDR,RBASE,EFFADRS);
(*4592*)                        END;
(*4593*)                       STACKTOP:=NXT^.PFADDR+8;
(*4594*)                      END;
(*4595*)                    INSYMBOL;
(*4596*)                  END;
(*4597*)              END (*PROC/FUNC PARAMETER*)
(*4598*)            ELSE
(*4599*)              BEGIN EXPRESSION(FSYS+(.COMMA,RPARENT.));
(*4600*)                IF (NXT<>NIL) AND (GATTRP^.TYPTR<>NIL) THEN
(*4601*)          IF NXT^.VKIND=DRCT                                                    
(*4602*)                    THEN
(*4603*)                      BEGIN ATTRNEW(FORMAL);
(*4604*)                        WITH FORMAL^,NXT^ DO
(*4605*)                          BEGIN TYPTR:=IDTYPE; KIND:=VARBL; VADRS:=VADDR;
(*4606*)                                ACCESS:=DIRECT; VARKIND:=DRCT; VLEVEL:=STACKPOINTER;
(*4607*)                          END;
(*4608*)                        IF NXT^.IDTYPE<>NIL THEN
(*4609*)                          BEGIN STORE(FORMAL,GATTRP,142);
(*4610*)                            STACKTOP:=NXT^.VADDR+NXT^.IDTYPE^.SIZE.WBLENGTH;
(*4611*)                          END;
(*4612*)                        ATTRDISP(FORMAL);
(*4613*)                      END
(*4614*)                    ELSE
(*4615*)                      BEGIN
(*4616*)       IF GATTRP^.KIND <> VARBL THEN                                            
(*4617*)                          BEGIN ERROR(154); GATTRP^.TYPTR:=NIL; END
(*4618*)                        ELSE IF COMPTYPES(NXT^.IDTYPE,GATTRP^.TYPTR) THEN
(*4619*)                          IF GATTRP^.TYPTR^.SIZE.WBLENGTH=1 THEN ERROR(187)
(*4620*)                          ELSE
(*4621*)                            BEGIN LOADADDRESS(GATTRP,NIL);
(*4622*)                             BASEREGISTER(STACKPOINTER,NXT^.PARADDR);
(*4623*)                          GENRXP(ZST,GATTRP^.REXPR.RNO,0,RBASE,EFFADRS);
(*4624*)                         STACKTOP:=NXT^.PARADDR+4;                              
(*4625*)                            END
(*4626*)                        ELSE ERROR(142);
(*4627*)                      END;
(*4628*)                RESETG;
(*4629*)              END;
(*4630*)          IF NXT<>NIL THEN NXT:=NXT^.NEXT;
(*4631*)        UNTIL SY<>COMMA;
(*4632*)        TEST1(RPARENT,4);
(*4633*)      END;
(*4634*)        IF FORT THEN
(*4635*)        BEGIN
(*4636*)          NXT := FCP^.PARAMS;                                                   
(*4637*)    ALIGNMENT(STACKTOP,4);                                                      
(*4638*)          FORTSTACK := STACKTOP;                                                
(*4639*)          IF NXT <> NIL THEN                                                    
(*4640*)          REPEAT                                                                
(*4641*)            IF NXT^.VKIND=DRCT THEN                                             
(*4642*)            BEGIN                                                               
(*4643*)              BASEREGISTER(STACKPOINTER,NXT^.VADDR);                            
(*4644*)              GENRX(ZLA,0,0,RBASE,EFFADRS)                                      
(*4645*)             END ELSE                                                           
(*4646*)             BEGIN                                                              
(*4647*)               BASEREGISTER(STACKPOINTER,NXT^.PARADDR);                         
(*4648*)               GENRX(ZL,0,0,RBASE,EFFADRS);                                     
(*4649*)             END;                                                               
(*4650*)             BASEREGISTER(STACKPOINTER,STACKTOP);                               
(*4651*)             GENRX(ZST,0,0,RBASE,EFFADRS);                                      
(*4652*)             STACKTOP := STACKTOP+4;                                            
(*4653*)             IF NXT<> NIL THEN NXT:=NXT^.NEXT;                                  
(*4654*)          UNTIL NXT=NIL;                                                        
(*4655*)          IF FCP^.PARAMS <> NIL THEN
(*4656*)          BEGIN
(*4657*)            GENRX(ZMVI,8,0,RBASE,EFFADRS);
(*4658*)         BASEREGISTER(STACKPOINTER,FORTSTACK);                                  
(*4659*)          GENRX(ZLA,0,0,RBASE,EFFADRS);                                         
(*4660*)          END ELSE
(*4661*)          GENRR(ZSR,0,0);
(*4662*)          GENRX(ZST,0,0,8,60);
(*4663*)        END;
(*4664*)    IF NXT<>NIL THEN ERROR(126);
(*4665*)    FOR FLOATREG:=F0 TO F6 DO
(*4666*)      IF REGISTER(.FLOATREG.).USED THEN SAVE(FLOATREG);
(*4667*)     PROCPASS:=TRUE;
(*4668*)    WITH FCP^ DO
(*4669*)     IF PFKIND <> ACTUAL THEN
(*4670*)     BEGIN
(*4671*)       BASEREGISTER(PFLEV,PFADDR);
(*4672*)       GENRX(ZL,15,0,RBASE,EFFADRS);
(*4673*)      GENRX(ZL,0,0,1,8);                                                        
(*4674*)      IF (PFADDR+4)>=4096 THEN GENRX(ZLA,9,0,PBASE1,IC+24) ELSE
(*4675*)        GENRX(ZLA,9,0,PBASE1,IC+18);
(*4676*)       GENRX(ZSTM,8,6,STACKPOINTER,0);
(*4677*)       BASEREGISTER(PFLEV,PFADDR+4);
(*4678*)       GENRX(ZL,2,0,RBASE,EFFADRS);
(*4679*)       GENRX(ZLM,2,6,2,40);
(*4680*)        GENRX(ZBC,15,0,1,ENTRYVARPROC);
(*4681*)     END
(*4682*)     ELSE
(*4683*)     BEGIN
(*4684*)        IF NOT EXTRNL THEN L:=4 ELSE L:=8;                                      
(*4685*)        L := PROCBASE+4*FCP^.PFCNT-L;                                           
(*4686*)        IF L = 0 THEN GENRX(ZL,0,0,1,8);                                        
(*4687*)          GENRR(ZBALR,9,1);
(*4688*)        MAKECODE(IC,L);                                                         
(*4689*)          IC := IC +2;
(*4690*)     END;
(*4691*)    IF FCP^.KLASS=FUNC THEN
(*4692*)      BEGIN
(*4693*)        FSP:=FCP^.IDTYPE;
(*4694*)        IF FSP=REALPTR
(*4695*)          THEN BEGIN KIND:=FLOAT; X:=ZLD END
(*4696*)          ELSE BEGIN KIND:=SINGLE; X:=ZL END;
(*4697*)        REGSEARCH(NIL,KIND);
(*4698*)        GENRX(X,RMAIN,0,STACKPOINTER,SAVEAREA);
(*4699*)        WITH GATTRP^ DO
(*4700*)          BEGIN TYPTR := FSP; KIND := EXPR;
(*4701*)                REXPR.REGTEMP:=REGIST; REXPR.RNO:=RWORK;
(*4702*)          END;
(*4703*)        REGISTER(.RWORK.).USED:=TRUE; REGISTER(.RWORK.).REGCONT:=GATTRP;
(*4704*)      END ;
(*4705*)    IF OLDSTACK<>0 THEN
(*4706*)      BEGIN MAKEINTCONST(OLDSTACK); GENRX(ZS,STACKPOINTER,0,0,0); END;
(*4707*)    STACKTOP:=OLDSTACK;
(*4708*)  END;
(*4709*) 
(*4710*)    BEGIN (*CALL*)
(*4711*)      IF FCP^.PFDECKIND=DECLARED THEN CALLNONSTANDARD
(*4712*)      ELSE
(*4713*)        BEGIN
(*4714*)          LKEY := FCP^.KEY;
(*4715*)          IF FCP^.KLASS = PROC THEN
(*4716*)            CASE LKEY OF
(*4717*)              1,2,
(*4718*)              3,4:   STDFLPROCS;      (*GET,PUT,RESET,REWRITE*)
(*4719*)              5:     PAGE;
(*4720*)        6,7,8,9:READWRITE;
(*4721*)              10,11: TIMEDATE;
(*4722*)              12:    NEWPROC;
(*4723*)              13,14:MARKRELEASE;
(*4724*)              15:PACK;
(*4725*)              16:UNPACK;
(*4726*)              17:MESSAGE;
(*4727*)              18:HALT;
(*4728*)            END
(*4729*)          ELSE
(*4730*)            CASE LKEY OF
(*4731*)              1,2:   STDFLFUNCS;      (*EOF,EOLN*)
(*4732*)              3:     ODDP;
(*4733*)              4,5:   ROUNDTRUNCF;
(*4734*)              6:     ABSF;
(*4735*)              7:     SQRF;
(*4736*)              8:     ORDF;
(*4737*)              9:     CHRF;
(*4738*)              10,11: PREDSUCCF;
(*4739*)              12,13,
(*4740*)              14,15,
(*4741*)              16,17: STDARITHFUNCS;   (*SIN,COS,EXP,SQRT,LN,ARCTAN*)
(*4742*)              18:CLOCKF;
(*4743*)              19:CARD;
(*4744*)            END
(*4745*)        END;
(*4746*)    END (*CALL*) ;
(*4747*) 
// $TITLE EXPRSSN - REGULAROP,SETTYPCHK
(*4748*)    PROCEDURE EXPRESSION(FSYS: SETOFSYS);
(*4749*)     VAR LATTRP: ATTRP; LOP: &OPERATOR;
(*4750*) 
(*4751*)    PROCEDURE REGULAROPERATION(FATTRP:ATTRP; FOP:&OPERATOR);
(*4752*)      BEGIN
(*4753*)        IF COMPTYPES(FATTRP^.TYPTR,INTPTR) THEN
(*4754*)          IF COMPTYPES(GATTRP^.TYPTR,INTPTR)
(*4755*)            THEN INTARITH(FATTRP,GATTRP,FOP)
(*4756*)            ELSE
(*4757*)              IF GATTRP^.TYPTR=REALPTR
(*4758*)                THEN REALARITH(FATTRP,GATTRP,FOP)
(*4759*)                ELSE ERRORRESET(134)
(*4760*)        ELSE
(*4761*)          IF (FATTRP^.TYPTR=REALPTR) THEN
(*4762*)            IF (GATTRP^.TYPTR=REALPTR) OR
(*4763*)                COMPTYPES(GATTRP^.TYPTR,INTPTR)
(*4764*)              THEN REALARITH(FATTRP,GATTRP,FOP)
(*4765*)              ELSE ERRORRESET(134)
(*4766*)          ELSE
(*4767*)            IF (FATTRP^.TYPTR^.FORM = POWER) AND
(*4768*)                COMPTYPES(FATTRP^.TYPTR,GATTRP^.TYPTR)
(*4769*)              THEN SETARITH(FATTRP,GATTRP,FOP)
(*4770*)              ELSE ERRORRESET(134);
(*4771*)      END;
(*4772*) 
(*4773*)    PROCEDURE SETTYPECHECK(FSP:STP);
(*4774*)      BEGIN
(*4775*)        IF GATTRP^.TYPTR=REALPTR THEN ERRORRESET(109);
(*4776*)        IF GATTRP^.TYPTR <> NIL THEN
(*4777*)          IF GATTRP^.TYPTR^.FORM > SUBRANGE THEN ERRORRESET(136)
(*4778*)          ELSE
(*4779*)            IF NOT COMPTYPES(FSP,GATTRP^.TYPTR) THEN ERRORRESET(137);
(*4780*)        IF GATTRP^.TYPTR<>NIL THEN CHECKRANGE(GATTRP,SETMIN,SETMAX,304);
(*4781*)      END;
(*4782*) 
// $TITLE  POWERSET OPERATIONS
(*4783*)    PROCEDURE POWERSET;
(*4784*)      VAR LSP:STP; LCSTATTRP,LVARATTRP,LATTRP,ATTRWORK:ATTRP;
(*4785*)          VARPART:BOOLEAN; N:INTEGER;
(*4786*)      BEGIN INSYMBOL;
(*4787*)       NEW(LSP{,POWER});
(*4788*)       WITH LSP^ DO
(*4789*)        BEGIN ELSET := NIL; PCKDSET := FALSE;
(*4790*)         FTYPE := FALSE;
(*4791*)         SIZE.WBLENGTH:=8; SIZE.BOUNDARY:=8;
(*4792*)        END;
(*4793*)       VARPART := FALSE;
(*4794*)       ATTRNEW(LCSTATTRP);
(*4795*)       WITH LCSTATTRP^ DO
(*4796*)        BEGIN TYPTR:=LSP; KIND:=CST; CVAL.CKIND:=PSET; CVAL.PVAL:=(. .);
(*4797*)        END;
(*4798*)       IF SY = RBRACK THEN INSYMBOL
(*4799*)       ELSE
(*4800*)        BEGIN
(*4801*)         (*LOOP UNTIL SY <> COMMA:*)
//(*4802*)         LOOP
(*4802*)         while true do  begin

(*4803*)          EXPRESSION(FSYS+(.COMMA,COLON,RBRACK.));
(*4804*)          SETTYPECHECK(LSP^.ELSET);
(*4805*)          IF GATTRP^.TYPTR<>NIL THEN LSP^.ELSET:=GATTRP^.TYPTR;
(*4806*)          IF SY = COLON THEN
(*4807*)           BEGIN ATTRNEW(LATTRP); COPYATTR(GATTRP,LATTRP);
(*4808*)            INSYMBOL;
(*4809*)            EXPRESSION(FSYS+(.COMMA,RBRACK.));
(*4810*)            SETTYPECHECK(LATTRP^.TYPTR);
(*4811*)            IF (LATTRP^.TYPTR <> NIL)AND (GATTRP^.TYPTR <> NIL)
(*4812*)             THEN
(*4813*)             BEGIN
(*4814*)              IF (LATTRP^.KIND = CST)AND (GATTRP^.KIND = CST)
(*4815*)               THEN
(*4816*)               BEGIN
(*4817*)                 FOR N := LATTRP^.CVAL.IVAL TO GATTRP^.CVAL.IVAL DO
(*4818*)                   IF (N>=SETMIN) AND (N<=SETMAX) THEN
(*4819*)                    LCSTATTRP^.CVAL.PVAL := LCSTATTRP^.CVAL.PVAL+(.N.);
(*4820*)                  ATTRDISP(LATTRP);
(*4821*)               END
(*4822*)              ELSE
(*4823*)      BEGIN
(*4824*)        LOAD(GATTRP,LATTRP); OPERATION(GATTRP,LATTRP,ZS,ZSR);
(*4825*)        ATTRNEW(ATTRWORK);
(*4826*)        WITH ATTRWORK^ DO
(*4827*)          BEGIN TYPTR:=LSP; KIND:=CST; CVAL.CKIND:=PSET;
(*4828*)                CVAL.PVAL:=(.0.);
(*4829*)          END;
(*4830*)        LOAD(ATTRWORK,GATTRP);
(*4831*)        GENRXP(ZSRDA,ATTRWORK^.REXPR.RNO,0,REALREG(.GATTRP^.REXPR.RNO.),0);
(*4832*)        EXCATTR(LATTRP,GATTRP); ATTRDISP(LATTRP);
(*4833*)        LOAD(GATTRP,ATTRWORK);
(*4834*)        GENRXP(ZSRDL,ATTRWORK^.REXPR.RNO,0,REALREG(.GATTRP^.REXPR.RNO.),0);
(*4835*)        EXCATTR(ATTRWORK,GATTRP); ATTRDISP(ATTRWORK);
(*4836*)        IF VARPART THEN SETARITH(GATTRP,LVARATTRP,PLUS)
(*4837*)        ELSE BEGIN VARPART:=TRUE; ATTRNEW(LVARATTRP); COPYATTR(GATTRP,LVARATTRP) END;
(*4838*)      END;
(*4839*)             END;
(*4840*)           END (*COLON*)
(*4841*)         ELSE
(*4842*)           IF GATTRP^.TYPTR <> NIL THEN
(*4843*)             BEGIN
(*4844*)               IF GATTRP^.KIND = CST THEN
(*4845*)                 LCSTATTRP^.CVAL.PVAL := LCSTATTRP^.CVAL.PVAL
(*4846*)                 +(.GATTRP^.CVAL.IVAL.)
(*4847*)               ELSE
(*4848*)      BEGIN
(*4849*)        ATTRNEW(ATTRWORK);
(*4850*)        WITH ATTRWORK^ DO
(*4851*)          BEGIN TYPTR:=LSP; KIND:=CST; CVAL.CKIND:=PSET;
(*4852*)                CVAL.PVAL:=(.0.);
(*4853*)          END;
(*4854*)        LOAD(ATTRWORK,GATTRP); LOAD(GATTRP,ATTRWORK);
(*4855*)        GENRXP(ZSRDL,ATTRWORK^.REXPR.RNO,0,REALREG(.GATTRP^.REXPR.RNO.),0);
(*4856*)        EXCATTR(ATTRWORK,GATTRP); ATTRDISP(ATTRWORK);
(*4857*)        IF VARPART THEN SETARITH(GATTRP,LVARATTRP,PLUS)
(*4858*)        ELSE BEGIN VARPART:=TRUE; ATTRNEW(LVARATTRP); COPYATTR(GATTRP,LVARATTRP) END;
(*4859*)      END;
(*4860*)             END;
(*4861*)         IF SY<>COMMA THEN break; INSYMBOL;
(*4862*)       END;
(*4863*)       TEST1(RBRACK,12);
(*4864*)        END;
(*4865*)       IF VARPART THEN
(*4866*)        BEGIN
(*4867*)         IF LCSTATTRP^.CVAL.PVAL <> (. .) THEN
(*4868*)          SETARITH(LCSTATTRP,LVARATTRP,PLUS);
(*4869*)         COPYATTR(LVARATTRP,GATTRP); ATTRDISP(LVARATTRP);
(*4870*)        END
(*4871*)       ELSE COPYATTR(LCSTATTRP,GATTRP);
(*4872*)       ATTRDISP(LCSTATTRP);
(*4873*)      END;
(*4874*) 
// $TITLE  FACTOR PROCEDURE
(*4875*)     PROCEDURE FACTOR(FSYS: SETOFSYS);
(*4876*)       VAR LCP: CTP;
(*4877*)       LATTRP:ATTRP;
(*4878*)         TP,L:INTEGER; NAMEX : ALFA;                                            
(*4879*)       FLT   :0..1;
(*4880*)       BEGIN
(*4881*)         IF NOT (SY IN FACBEGSYS) THEN
(*4882*)           BEGIN ERROR(58); SKIP(FSYS+FACBEGSYS);
(*4883*)             GATTRP^.TYPTR := NIL
(*4884*)           END;
(*4885*)         REPEAT
(*4886*)           IF SY IN FACBEGSYS THEN
(*4887*)             BEGIN
(*4888*)               CASE SY OF
(*4889*)       (*ID*)    IDENT:
(*4890*)                     BEGIN
(*4891*)                       SEARCHID((.KONST,VARS,FIELD,FUNC,TYPES.),LCP);
(*4892*)                     INSYMBOL;
(*4893*)                     CASE LCP^.KLASS OF
(*4894*)                       KONST: WITH LCP^,GATTRP^ DO
(*4895*)                                BEGIN TYPTR:=IDTYPE; KIND:=CST;
(*4896*)                                      CVAL:=VALUES;
(*4897*)                                END;
(*4898*)       (* TYPES *)      TYPES:BEGIN
(*4899*)                                IF EXTWARN THEN ERROR(291);
(*4900*)                                TEST1(LPARENT,9);
(*4901*)                                EXPRESSION(FSYS+(.RPARENT.));
(*4902*)                                WITH GATTRP^ DO
(*4903*)                                  IF TYPTR<>NIL THEN
(*4904*)                                  BEGIN
(*4905*)                                    IF KIND=CST THEN ERROR(292);
(*4906*)                                    TYPTR:=LCP^.IDTYPE
(*4907*)                                 END;
(*4908*)                                 TEST1(RPARENT,4);
(*4909*)                            END;
(*4910*)                       VARS,
(*4911*)                       FIELD: SELECTOR(FSYS,LCP);
(*4912*)                       FUNC:  CALL(FSYS,LCP)
(*4913*)                     END
(*4914*)                   END;
(*4915*)       (*CST*)   INTCONST:
(*4916*)                   BEGIN
(*4917*)                     WITH GATTRP^ DO
(*4918*)                       BEGIN TYPTR := INTPTR; KIND := CST;
(*4919*)                             CVAL.CKIND:=INT; CVAL.IVAL:=IVAL;
(*4920*)                       END;
(*4921*)                     INSYMBOL
(*4922*)                   END;
(*4923*)                 REALCONST:
(*4924*)                   BEGIN
(*4925*)                     WITH GATTRP^ DO
(*4926*)                       BEGIN TYPTR := REALPTR; KIND := CST;
(*4927*)                             CVAL.CKIND:=REEL; CVAL.RVAL:=RVAL;
(*4928*)                       END;
(*4929*)                     INSYMBOL
(*4930*)                   END;
(*4931*)                 CHARCONST:
(*4932*)                   BEGIN
(*4933*)                     WITH GATTRP^ DO
(*4934*)                       BEGIN TYPTR := CHARPTR; KIND := CST;
(*4935*)                             CVAL.CKIND:=INT; CVAL.IVAL:=IVAL;
(*4936*)                       END;
(*4937*)                     INSYMBOL
(*4938*)                   END;
(*4939*)                 STRINGCONST:
(*4940*)                   BEGIN
(*4941*)                     WITH GATTRP^ DO
(*4942*)                       BEGIN STRINGTYPE(TYPTR); KIND := CST;
(*4943*)                             CVAL.CKIND:=STRG; CVAL.VALP:=CONSTP;
(*4944*)                       END;
(*4945*)                     INSYMBOL
(*4946*)                   END;
(*4947*)       (*( *)   LPARENT:
(*4948*)                   BEGIN INSYMBOL; EXPRESSION(FSYS+(.RPARENT.));
(*4949*)                     IF SY = RPARENT THEN INSYMBOL ELSE ERROR(4)
(*4950*)                   END;
(*4951*)       (*NOT*)   NOTSY:
(*4952*)                   BEGIN INSYMBOL; FACTOR(FSYS);
(*4953*)                     IF GATTRP^.TYPTR<>NIL THEN
(*4954*)                       IF COMPTYPES(GATTRP^.TYPTR,BOOLPTR) THEN NOTFACTOR(GATTRP)
(*4955*)                         ELSE BEGIN ERROR(135); GATTRP^.TYPTR:=NIL; END;
(*4956*)                   END;
(*4957*)       (*(.*)    LBRACK:  POWERSET;
(*4958*)               END (*CASE*) ;
(*4959*)               TEST2(FSYS,6,FACBEGSYS)
(*4960*)             END (*IF*)
(*4961*)         UNTIL SY IN FSYS;
(*4962*) 
(*4963*) 
(*4964*)(*  EXPONENTIATION *)
(*4965*)(*******************)
(*4966*) 
(*4967*) 
(*4968*)IF SY = EXPONOP THEN
(*4969*)BEGIN
(*4970*)  IF EXTWARN THEN ERROR(291);
(*4971*)  IF (NOT COMPTYPES(GATTRP^.TYPTR,INTPTR)) AND
(*4972*)     (GATTRP^.TYPTR <> REALPTR) THEN ERROR(399);
(*4973*)  INSYMBOL;
(*4974*)  ATTRNEW(LATTRP); COPYATTR(GATTRP,LATTRP);
(*4975*)  FACTOR(FSYS);
(*4976*)  IF COMPTYPES(GATTRP^.TYPTR,INTPTR) THEN
(*4977*)  BEGIN
(*4978*)        LOAD(LATTRP,GATTRP);
(*4979*)        LOAD(GATTRP,LATTRP);
(*4980*)        IF LATTRP^.TYPTR=INTPTR THEN FLT:=0 ELSE FLT:=1;
(*4981*)        LOADINTCONST(R0,FLT*256+16*REALREG(.LATTRP^.REXPR.RNO.)
(*4982*)                         +REALREG(.GATTRP^.REXPR.RNO.));
(*4983*)        GENRX(ZBAL,BASEWORK,0,1,ENTRYEXPON);
(*4984*)        EXCATTR(LATTRP,GATTRP);
(*4985*) 
(*4986*)  END
(*4987*)  ELSE
(*4988*)  IF GATTRP^.TYPTR=REALPTR THEN
(*4989*)  BEGIN
(*4990*)         FOR L := 1 TO 2 DO                                                     
(*4991*)         BEGIN                                                                  
(*4992*)           TP:=1; IF L=1 THEN NAMEX:='LN@P    ' ELSE NAMEX:='EXP@P   ';
(*4993*)           WHILE (TP<>NRSTARITH) AND(STDPRCS(.TP.)<>NAMEX)                      
(*4994*)            AND (STDPRCS(.TP.) <> '        ') DO TP:=TP+1;                      
(*4995*)           IF STDPRCS(.TP.)='        ' THEN STDPRCS(.TP.):=NAMEX ELSE           
(*4996*)            IF STDPRCS(.TP.) <> NAMEX THEN ERROR(400)                           
(*4997*)         END;                                                                   
(*4998*)         IF COMPTYPES(LATTRP^.TYPTR,INTPTR) THEN
(*4999*)            INTTOREAL(LATTRP);
(*5000*)    LOAD(LATTRP,GATTRP);
(*5001*)    LOADINTCONST(R0,REALREG(.LATTRP^.REXPR.RNO.));
(*5002*)    GENRX(ZBAL,BASEWORK,0,1,ENTRYSIN+(16-12)*8);
(*5003*)    REGULAROPERATION(LATTRP,MUL);
(*5004*)    LOAD(GATTRP,LATTRP);
(*5005*)    LOADINTCONST(R0,REALREG(.GATTRP^.REXPR.RNO.));
(*5006*)    GENRX(ZBAL,BASEWORK,0,1,ENTRYSIN+(14-12)*8);
(*5007*)  END;
(*5008*)  ATTRDISP(LATTRP);
(*5009*)END;
(*5010*)       END (*FACTOR*) ;
(*5011*) 
// $TITLE PROCEDURE TERM
(*5012*)    PROCEDURE TERM(FSYS: SETOFSYS);
(*5013*)      VAR LATTRP: ATTRP; LOP: &OPERATOR;
(*5014*)      BEGIN
(*5015*)        FACTOR(FSYS+(.MULOP,EXPONOP.));
(*5016*)        WHILE SY = MULOP DO
(*5017*)          BEGIN
(*5018*)            ATTRNEW(LATTRP); COPYATTR(GATTRP,LATTRP);
(*5019*)            LOP := OP;
(*5020*)              INSYMBOL; FACTOR(FSYS+(.MULOP,EXPONOP.));
(*5021*)            IF (LATTRP^.TYPTR <> NIL)AND (GATTRP^.TYPTR <> NIL) THEN
(*5022*)              CASE LOP OF
(*5023*)(***)           MUL : REGULAROPERATION(LATTRP,MUL);
(*5024*)(*/*)           RDIV: IF COMPTYPES(LATTRP^.TYPTR,INTPTR) OR
(*5025*)                           (LATTRP^.TYPTR=REALPTR) THEN
(*5026*)                        IF COMPTYPES(GATTRP^.TYPTR,INTPTR) OR
(*5027*)                             (GATTRP^.TYPTR=REALPTR) THEN
(*5028*)                          REALARITH(LATTRP,GATTRP,RDIV)
(*5029*)                        ELSE ERRORRESET(134)
(*5030*)                      ELSE ERRORRESET(134);
(*5031*)(*DIV,MOD*)     IDIV,IMOD: IF COMPTYPES(LATTRP^.TYPTR,INTPTR) AND
(*5032*)                                COMPTYPES(GATTRP^.TYPTR,INTPTR) THEN
(*5033*)                             INTARITH(LATTRP,GATTRP,LOP)
(*5034*)                           ELSE ERRORRESET(134);
(*5035*)(*AND*)         ANDOP:IF COMPTYPES(LATTRP^.TYPTR,BOOLPTR)AND
(*5036*)                          COMPTYPES(GATTRP^.TYPTR,BOOLPTR) THEN
(*5037*)                        BOOLARITH(LATTRP,GATTRP,ANDOP)
(*5038*)                      ELSE ERRORRESET(134)
(*5039*)              END (*CASE*)
(*5040*)            ELSE GATTRP^.TYPTR := NIL;
(*5041*)            ATTRDISP(LATTRP)
(*5042*)          END (*WHILE*);
(*5043*)      END;
(*5044*) 
// $TITLE  SIMPLE EXPRESSION
(*5045*)    PROCEDURE SIMPLEEXPRESSION(FSYS: SETOFSYS);
(*5046*)      VAR LATTRP: ATTRP; LOP: &OPERATOR;
(*5047*)      BEGIN
(*5048*)        LOP:=NOOP;
(*5049*)        IF OP IN (.PLUS,MINUS.) THEN
(*5050*)          BEGIN LOP:=OP; INSYMBOL; END;
(*5051*)        TERM(FSYS+(.ADDOP.));
(*5052*)        IF LOP<>NOOP THEN
(*5053*)          BEGIN
(*5054*)            IF NOT ((GATTRP^.TYPTR=REALPTR) OR COMPTYPES(GATTRP^.TYPTR,INTPTR))
(*5055*)              THEN ERRORRESET(134)
(*5056*)              ELSE IF LOP=MINUS THEN NEGATE(GATTRP);
(*5057*)          END;
(*5058*)        WHILE SY = ADDOP DO
(*5059*)          BEGIN ATTRNEW(LATTRP); COPYATTR(GATTRP,LATTRP); LOP := OP;
(*5060*)            INSYMBOL; TERM(FSYS+(.ADDOP.));
(*5061*)            IF (LATTRP^.TYPTR <> NIL)AND (GATTRP^.TYPTR <> NIL) THEN
(*5062*)              CASE LOP OF
(*5063*)(*+,-*)         PLUS,MINUS:
(*5064*)                  REGULAROPERATION(LATTRP,LOP);
(*5065*)(*OR*)          OROP:
(*5066*)                  IF COMPTYPES(LATTRP^.TYPTR,BOOLPTR) AND COMPTYPES(GATTRP^.TYPTR,BOOLPTR)
(*5067*)                    THEN BOOLARITH(LATTRP,GATTRP,OROP)
(*5068*)                    ELSE ERRORRESET(134)
(*5069*)              END (*CASE*)
(*5070*)            ELSE GATTRP^.TYPTR := NIL;
(*5071*)            ATTRDISP(LATTRP);
(*5072*)          END (*WHILE*);
(*5073*)      END;
(*5074*) 
// $TITLE  EXPRESSION - (BODY)
(*5075*)    BEGIN (*EXPRESSION*)
(*5076*)      SIMPLEEXPRESSION(FSYS+(.RELOP.));
(*5077*)      IF SY = RELOP THEN
(*5078*)        BEGIN
(*5079*)          LOP:=OP;
(*5080*)          ATTRNEW(LATTRP); COPYATTR(GATTRP,LATTRP);
(*5081*)          INSYMBOL; SIMPLEEXPRESSION(FSYS);
(*5082*)          IF (LATTRP^.TYPTR <> NIL)AND (GATTRP^.TYPTR <> NIL) THEN
(*5083*)            IF LOP = INOP THEN
(*5084*)              IF GATTRP^.TYPTR^.FORM = POWER THEN
(*5085*)                IF COMPTYPES(LATTRP^.TYPTR,GATTRP^.TYPTR^.ELSET) THEN
(*5086*)                  INPOWER(LATTRP,GATTRP)
(*5087*)                ELSE ERRORRESET(129)
(*5088*)              ELSE ERRORRESET(130)
(*5089*)            ELSE
(*5090*)             BEGIN
(*5091*)              IF COMPTYPES(LATTRP^.TYPTR,INTPTR) AND
(*5092*)                 (GATTRP^.TYPTR=REALPTR) THEN INTTOREAL(LATTRP);
(*5093*)              IF COMPTYPES(GATTRP^.TYPTR,INTPTR) AND
(*5094*)                 (LATTRP^.TYPTR=REALPTR) THEN INTTOREAL(GATTRP);
(*5095*)              IF NOT COMPTYPES(LATTRP^.TYPTR,GATTRP^.TYPTR) THEN
(*5096*)                ERRORRESET(129)
(*5097*)              ELSE
(*5098*)               CASE LATTRP^.TYPTR^.FORM OF
(*5099*)                 SCALAR,SUBRANGE,PACKDTYPE:
(*5100*)                   IF (LATTRP^.TYPTR=REALPTR)
(*5101*)                     THEN RELREAL(LATTRP,GATTRP,LOP)
(*5102*)                     ELSE RELINT(LATTRP,GATTRP,LOP);
(*5103*)                 POINTER:
(*5104*)                   IF LOP IN (.EQOP,NEOP.)
(*5105*)                     THEN RELINT(LATTRP,GATTRP,LOP)
(*5106*)                     ELSE ERRORRESET(131);
(*5107*)                 POWER:
(*5108*)                   IF LOP IN (.LTOP,GTOP.)
(*5109*)                     THEN ERRORRESET(132)
(*5110*)                     ELSE RELPOWER(LATTRP,GATTRP,LOP);
(*5111*)                 ARRAYS,RECORDS:
(*5112*)                   IF xSTRING(LATTRP^.TYPTR) THEN RELLONG(LATTRP,GATTRP,LOP)
(*5113*)                     ELSE IF LOP IN (.LTOP,GTOP,LEOP,GEOP.)
(*5114*)                       THEN ERROR(131)
(*5115*)                       ELSE ERROR(399);
(*5116*)                 FILES:
(*5117*)                   ERRORRESET(133)
(*5118*)               END (*CASE*);
(*5119*)             END (*SY <> INOP*)
(*5120*)            ELSE GATTRP^.TYPTR := NIL;
(*5121*)          ATTRDISP(LATTRP);
(*5122*)        END (*SY = RELOP*) ;
(*5123*)    END (*EXPRESSION*) ;
(*5124*) 
// $TITLE  STATEMENT AND JMPS
(*5125*)    PROCEDURE COMPOUNDSTATEMENT(FSYS:SETOFSYS); FORWARD;
(*5126*) 
(*5127*)   PROCEDURE STATEMENT(FSYS: SETOFSYS);
(*5128*)      LABEL 1;
(*5129*)    VAR LCP:CTP; LLP:LBP; LCIX:ADDRRANGE;
(*5130*) 
(*5131*)    PROCEDURE GENFJMP(FADDR:ADDRRANGE);
(*5132*)      VAR X: INTEGER;
(*5133*)      BEGIN
(*5134*)        IF NOT COMPTYPES(GATTRP^.TYPTR,BOOLPTR) THEN ERROR(145);
(*5135*)        LOAD(GATTRP,NIL);
(*5136*)        IF BOOLFLAG THEN
(*5137*)          BEGIN IC:=IC-10;
(*5138*)                X:=15-GETCODE(IC+4) MOD 256 DIV 16;
(*5139*)                GENRX(ZBC,X,0,PBASE1,FADDR);
(*5140*)          END
(*5141*)        ELSE
(*5142*)          BEGIN GENRRP1(ZLTR,GATTRP^.REXPR.RNO);
(*5143*)                GENRX(ZBC,CONDZ,0,PBASE1,FADDR);
(*5144*)          END;
(*5145*)      END;
(*5146*) 
(*5147*)    PROCEDURE GENJMP(FADDR:ADDRRANGE);
(*5148*)      BEGIN
(*5149*)        GENRX(ZBC,15,0,PBASE1,FADDR);
(*5150*)      END;
(*5151*) 
(*5152*)    PROCEDURE PREPFJMP(VAR FIX: ADDRRANGE);
(*5153*)      BEGIN
(*5154*)        GENFJMP(-4096*PBASE1); FIX:=IC-4;
(*5155*)      END;
(*5156*) 
(*5157*)    PROCEDURE PREPJMP(VAR FIX: ADDRRANGE);
(*5158*)      BEGIN
(*5159*)        FIX:=IC; GENRX(ZBC,15,0,0,0);
(*5160*)      END;
(*5161*) 
// $TITLE ASSIGNMENT
(*5162*)    PROCEDURE ASSIGNMENT(FCP: CTP);
(*5163*)      VAR LATTRP:ATTRP;
(*5164*)      BEGIN
(*5165*)        SELECTOR(FSYS+(.BECOMES.),FCP);
(*5166*)        IF SY = BECOMES THEN
(*5167*)          BEGIN
(*5168*)            ATTRNEW(LATTRP); COPYATTR(GATTRP,LATTRP);
(*5169*)            INSYMBOL; EXPRESSION(FSYS);
(*5170*)            IF (LATTRP^.TYPTR <> NIL)AND (GATTRP^.TYPTR <> NIL) THEN
(*5171*)              STORE(LATTRP,GATTRP,129);
(*5172*)            ATTRDISP(LATTRP); RESETG;
(*5173*)          END
(*5174*)        ELSE ERROR(51);
(*5175*)      END;
(*5176*) 
// $TITLE  GOTO STATEMENT
(*5177*)    PROCEDURE GOTOSTATEMENT;
(*5178*)     LABEL 1;
(*5179*)     VAR LLP:LBP; LCIX:ADDRRANGE;
(*5180*)      BEGIN
(*5181*)        IF SY = INTCONST THEN
(*5182*)          BEGIN LLP := FSTLABP;
(*5183*)            WHILE LLP <> FLABP DO (*DECIDE WHETHER LOCALLY DECLARED*)
(*5184*)              WITH LLP^ DO
(*5185*)                IF LABVAL = IVAL THEN
(*5186*)                  BEGIN
(*5187*)                    IF DEFINED THEN GENJMP(LABADDR)
(*5188*)                    ELSE
(*5189*)                      BEGIN PREPJMP(LCIX); LINKOCC(FSTOCC,LCIX); END;
(*5190*)                    GOTO 1
(*5191*)                  END
(*5192*)                ELSE LLP := NEXTLAB;
(*5193*)            WHILE LLP<>NIL DO
(*5194*)              WITH LLP^ DO
(*5195*)                IF LABVAL<>IVAL THEN LLP:=NEXTLAB
(*5196*)                ELSE
(*5197*)                  BEGIN
(*5198*)                    IF LCNT=0 THEN
(*5199*)                      IF PCNT>=MAXPROCFUNC THEN ERROR(261)
(*5200*)                         ELSE BEGIN
(*5201*)                                PCNT:=PCNT+1;LCNT:=PCNT;
(*5202*)                              END;
(*5203*)                     GENRX(ZLA,15,0,0,PROCBASE+4*LCNT-4);
(*5204*)                     GENRX(ZLA,9,0,0,240);
(*5205*)                     GENRX(ZEX,9,0,1,8);
(*5206*)                     GENRR(ZBCR,15,15);
(*5207*)                     GOTO 1;
(*5208*)                  END;
(*5209*)            ERROR(167);
(*5210*)      1:    INSYMBOL
(*5211*)          END
(*5212*)        ELSE ERROR(15);
(*5213*)      END (*GOTOSTATEMENT*) ;
(*5214*) 
// $TITLE IFSTATEMENT
(*5215*)    PROCEDURE IFSTATEMENT;
(*5216*)      VAR LCIX1,LCIX2: ADDRRANGE;
(*5217*)      BEGIN EXPRESSION(FSYS+(.THENSY.));
(*5218*)        PREPFJMP(LCIX1); RESETG;
(*5219*)        TEST1(THENSY,52);
(*5220*)        STATEMENT(FSYS+(.ELSESY.));
(*5221*)        IF SY = ELSESY THEN
(*5222*)          BEGIN PREPJMP(LCIX2); INSERTIC(LCIX1); INSYMBOL;
(*5223*)                STATEMENT(FSYS); INSERTIC(LCIX2);
(*5224*)          END
(*5225*)        ELSE INSERTIC(LCIX1);
(*5226*)      END;
(*5227*) 
// $TITLE CASE STATEMENT
(*5228*)    PROCEDURE CASESTATEMENT;
(*5229*)   LABEL 1,2;
(*5230*)     TYPE CIP = ^CASEREC;
(*5231*)        CASEREC=
(*5232*)             RECORD NEXT: CIP;
(*5233*)              CSLAB: INTEGER;
(*5234*)              CSADDR: ADDRRANGE;
(*5235*)             END;
(*5236*)     VAR LSP,LSP1: STP; FSTPTR,LPT1,LPT2,LPT3: CIP; LVAL: VALU;
(*5237*)         SWITCHIX,LCIX:ADDRRANGE;
(*5238*)         LMIN,LMAX: INTEGER;
(*5239*)         JUMPREG: REGNO; CHAIN:LOCOFREF;
(*5240*)         INDEXJUMP:BOOLEAN;
(*5241*)         COUNT : INTEGER;
(*5242*)         DEFAULT:INTEGER;
(*5243*)         LSP2:STP;
(*5244*) 
(*5245*)    PROCEDURE GENSWITCH(FCIP: CIP);
(*5246*)      VAR LVAL,JUMPBASE: INTEGER;
(*5247*)      BEGIN
(*5248*)        IF ((LMAX-LMIN)<CIXMAX) AND INDEXJUMP
(*5249*)          THEN
(*5250*)            BEGIN
(*5251*)         IF DEFAULT = 0 THEN
(*5252*)         BEGIN
(*5253*)           IF DEBUG THEN CHECKREGISTER(REALREG(.JUMPREG.),LMIN,LMAX)
(*5254*)         END ELSE
(*5255*)       BEGIN
(*5256*)         GENRR(ZBALR,9,0);
(*5257*)         MAKEINTCONST(LMIN); GENRX(ZC,REALREG(.JUMPREG.),0,0,0);
(*5258*)         GENRX(ZBC,CONDM,0,14,DEFAULT);
(*5259*)         MAKEINTCONST(LMAX); GENRX(ZC,REALREG(.JUMPREG.),0,0,0);
(*5260*)         GENRX(ZBC,CONDP,0,14,DEFAULT);
(*5261*)      END;
(*5262*)              GENRXP(ZSLL,JUMPREG,0,0,2);
(*5263*)              JUMPBASE:=IC+4-4*LMIN;
(*5264*)              IF (JUMPBASE<0) OR (JUMPBASE>=4096)
(*5265*)                THEN
(*5266*)                  BEGIN GENRR(ZLR,BASEWORK,PBASE1); MAKEINTCONST(JUMPBASE+6);
(*5267*)                        GENRX(ZA,BASEWORK,0,0,0);
(*5268*)                        GENRX(ZBC,15,REALREG(.JUMPREG.),BASEWORK,0);
(*5269*)                  END
(*5270*)                ELSE GENRX(ZBC,15,REALREG(.JUMPREG.),PBASE1,JUMPBASE);
(*5271*)              LVAL:=LMIN;
(*5272*)              REPEAT
(*5273*)                WITH FCIP^ DO
(*5274*)                  BEGIN
(*5275*)                    WHILE CSLAB > LVAL DO
(*5276*)                   BEGIN IF DEFAULT = 0 THEN
(*5277*)                         GENRX(ZBAL,9,0,1,JUMPERR1) ELSE
(*5278*)                         GENJMP(DEFAULT);
(*5279*)                            LVAL:=LVAL+1;
(*5280*)                      END;
(*5281*)                    GENJMP(CSADDR);
(*5282*)                    LVAL := LVAL + 1; FCIP := NEXT
(*5283*)                  END
(*5284*)              UNTIL FCIP = NIL
(*5285*)            END
(*5286*)          ELSE
(*5287*)            BEGIN
(*5288*)              REPEAT
(*5289*)                WITH FCIP^ DO
(*5290*)                  BEGIN MAKEINTCONST(CSLAB); GENRXP(ZC,JUMPREG,0,0,0);
(*5291*)                    GENRX(ZBC,CONDZ,0,PBASE1,CSADDR); FCIP:=NEXT;
(*5292*)                  END;
(*5293*)              UNTIL FCIP=NIL;
(*5294*)         IF DEFAULT = 0 THEN GENRX(ZBAL,9,0,1,JUMPERR1)
(*5295*)                 ELSE GENJMP(DEFAULT);
(*5296*)            END;
(*5297*)      END;
(*5298*) 
(*5299*)    BEGIN
(*5300*)      EXPRESSION(FSYS+(.OFSY,COMMA,COLON.));
(*5301*)      LSP := GATTRP^.TYPTR;
(*5302*)      IF LSP <> NIL THEN
(*5303*)        IF (LSP^.FORM>SUBRANGE) OR (LSP=REALPTR) THEN
(*5304*)          BEGIN ERROR(144); LSP := NIL END;
(*5305*)      LOAD(GATTRP,NIL); JUMPREG:=GATTRP^.REXPR.RNO; RESETG;
(*5306*)      PREPJMP(SWITCHIX);
(*5307*)      TEST1(OFSY,8);
(*5308*)      DEFAULT := 0;
(*5309*)      FSTPTR:=NIL; LPT3:=NIL; CHAIN:=NIL;
(*5310*)      (*LOOP UNTIL SY <> SEMICOLON*)
(*5311*)      {LOOP} while true do begin
(*5312*)        IF NOT (SY IN (.SEMICOLON,ENDSY.)) THEN
(*5313*)          BEGIN
(*5314*)            (*LOOP UNTIL SY <> COMMA:*)
(*5315*)       IF SY = ELSESY THEN
(*5316*)       BEGIN
(*5317*)         IF EXTWARN THEN ERROR(291);
(*5318*)         INSYMBOL; IF DEFAULT = 0 THEN DEFAULT:=IC ELSE ERROR(156)
(*5319*)       END ELSE
(*5320*)       {LOOP} while true do begin
(*5321*)         CONSTANT(FSYS+(.COMMA,COLON.),LSP1,LVAL);
(*5322*)         IF LSP1 <> NIL THEN
(*5323*)          IF COMPTYPES(LSP,LSP1) THEN LMIN:=LVAL.IVAL
(*5324*)          ELSE BEGIN ERROR(147); LSP1:=NIL END;
(*5325*)          IF (SY=COLON) AND DOTDOT THEN
(*5326*)          BEGIN
(*5327*)            IF EXTWARN THEN ERROR(291);
(*5328*)            INSYMBOL; CONSTANT(FSYS+(.COMMA,COLON.),LSP2,LVAL);
(*5329*)            IF LSP2 <> NIL THEN
(*5330*)              IF COMPTYPES(LSP,LSP2) THEN
(*5331*)                IF LMIN<=LVAL.IVAL THEN LMAX:=LVAL.IVAL
(*5332*)                 ELSE
(*5333*)                   BEGIN ERROR(102);LMAX:=LMIN;LMIN:=LVAL.IVAL;END
(*5334*)              ELSE
(*5335*)              BEGIN ERROR(147); LSP2:=NIL END
(*5336*)         END ELSE BEGIN LSP2:=LSP1;LMAX:=LMIN END;
(*5337*)       IF (LSP1 <> NIL) AND (LSP2<> NIL) THEN
(*5338*)       BEGIN LPT1:=FSTPTR; LPT2:=NIL;
(*5339*)         WHILE LPT1 <> NIL DO
(*5340*)           WITH LPT1^ DO
(*5341*)            IF LMIN <= CSLAB THEN
(*5342*)                IF (CSLAB=LMIN) OR (LMAX >= CSLAB) THEN
(*5343*)                BEGIN ERROR(156); GOTO 2 END
(*5344*)                 ELSE GOTO 1
(*5345*)            ELSE BEGIN LPT2:=LPT1; LPT1:=NEXT END;
(*5346*)   1:    FOR COUNT:=LMIN TO LMAX DO
(*5347*)         BEGIN
(*5348*)           NEW(LPT3);
(*5349*)           WITH LPT3^ DO
(*5350*)           BEGIN
(*5351*)             CSLAB:=COUNT;
(*5352*)             CSADDR:=IC
(*5353*)           END;
(*5354*)           IF LPT2 = NIL THEN FSTPTR:=LPT3 ELSE
(*5355*)              LPT2^.NEXT:=LPT3;
(*5356*)           LPT2:=LPT3;
(*5357*)         END;
(*5358*)           LPT2^.NEXT:=LPT1
(*5359*)         END;
(*5360*)2:
(*5361*)              IF SY<>COMMA THEN break; INSYMBOL;
(*5362*)            END;
(*5363*)            TEST1(COLON,5);
(*5364*)            REPEAT STATEMENT(FSYS+(.SEMICOLON.));
(*5365*)              IF SY IN STATBEGSYS THEN ERROR(14);
(*5366*)            UNTIL NOT (SY IN STATBEGSYS);
(*5367*)            PREPJMP(LCIX); LINKOCC(CHAIN,LCIX);
(*5368*)          END (*SY <> ENDSY*) ;
(*5369*)        IF SY<>SEMICOLON THEN break; INSYMBOL;
(*5370*)      END;
(*5371*)      IF FSTPTR <> NIL THEN
(*5372*)        BEGIN
(*5373*)          LPT1:=FSTPTR;
(*5374*)          WHILE LPT1<>NIL DO
(*5375*)            BEGIN LPT2:=LPT1; LPT1:=LPT1^.NEXT; END;
(*5376*)          LMAX:=LPT2^.CSLAB; LMIN:=FSTPTR^.CSLAB;
(*5377*)          IF (LMAX>MXINT DIV 4-4100) OR (LMIN<-MXINT DIV 4)
(*5378*)            THEN INDEXJUMP:=FALSE
(*5379*)            ELSE INDEXJUMP:=TRUE;
(*5380*)          INSERTIC(SWITCHIX);
(*5381*)          GENSWITCH(FSTPTR);
(*5382*)          INSERTCHAIN(CHAIN);
(*5383*)        END
(*5384*)      ELSE ERROR(6);
(*5385*)  IF SY = ENDSY THEN
(*5386*)  BEGIN
(*5387*)    RIGHTCHECK; INSYMBOL
(*5388*)  END ELSE ERROR(13);
(*5389*)    END (*CASESTATEMENT*) ;
(*5390*) 
// $TITLE REPEAT,WHILE STATEMENT
(*5391*)    PROCEDURE REPEATSTATEMENT;
(*5392*)      VAR LADDR: ADDRRANGE;
(*5393*)      BEGIN
(*5394*)        LADDR := IC;
(*5395*)        REPEAT
(*5396*)          STATEMENT(FSYS+(.SEMICOLON,UNTILSY.));
(*5397*)          IF SY IN STATBEGSYS THEN ERROR(14)
(*5398*)        UNTIL NOT (SY IN STATBEGSYS);
(*5399*)        WHILE SY = SEMICOLON DO
(*5400*)          BEGIN INSYMBOL;
(*5401*)            REPEAT STATEMENT(FSYS+(.SEMICOLON,UNTILSY.))
(*5402*)            UNTIL NOT (SY IN STATBEGSYS);
(*5403*)          END;
(*5404*)        IF SY = UNTILSY THEN
(*5405*)           BEGIN
(*5406*)             RIGHTCHECK; INSYMBOL; EXPRESSION(FSYS);
(*5407*)            GENFJMP(LADDR); RESETG;
(*5408*)          END
(*5409*)        ELSE ERROR(53);
(*5410*)      END;
(*5411*) 
(*5412*)    PROCEDURE WHILESTATEMENT;
(*5413*)      VAR LADDR,LCIX:ADDRRANGE;
(*5414*)      BEGIN
(*5415*)        LADDR:=IC;
(*5416*)        EXPRESSION(FSYS+(.DOSY.));
(*5417*)        PREPFJMP(LCIX); RESETG;
(*5418*)        TEST1(DOSY,54);
(*5419*)        STATEMENT(FSYS);
(*5420*)        GENJMP(LADDR);  INSERTIC(LCIX);
(*5421*)      END;
(*5422*) 
// $TITLE LOOP STATEMENT
(*5423*)    PROCEDURE LOOPSTATEMENT;
(*5424*)      VAR OLDTOP:DISPRANGE; CHAIN:LOCOFREF; LCIX,LADDR:ADDRRANGE;
(*5425*)          LCP,LCP1:CTP;
(*5426*)     BEGIN
(*5427*)       IF EXTWARN THEN ERROR(291);
(*5428*)       CHAIN:=NIL; OLDTOP:=TOP;
(*5429*)        IF TOP<DISPLIMIT THEN
(*5430*)          BEGIN TOP:=TOP+1; DISPLAY(.TOP.).FNAME:=NIL;
(*5431*)                DISPLAY(.TOP.).OCCUR:=REC;
(*5432*)          END
(*5433*)        ELSE ERROR(250);
(*5434*)        LCP1:=NIL; NEW(LCP{,EVENT});
(*5435*)        WITH LCP^ DO
(*5436*)          BEGIN NAME:='EXIT    '; IDTYPE:=NIL; NEXT:=LCP1;
(*5437*)                EVENTJUMP:=NIL; EVENTDEF:=FALSE;
(*5438*)          END;
(*5439*)        ENTERID(LCP); LCP1:=LCP;
(*5440*)        IF SY=UNTILSY THEN
(*5441*)          BEGIN
(*5442*)            REPEAT INSYMBOL;
(*5443*)              IF SY<>IDENT THEN
(*5444*)                BEGIN ERROR(2); SKIP(FSYS+(.COMMA,COLON.)); END
(*5445*)              ELSE
(*5446*)                BEGIN NEW(LCP{,EVENT});
(*5447*)                  WITH LCP^ DO
(*5448*)                    BEGIN NAME:=ID; NEXT:=LCP1; IDTYPE:=NIL;
(*5449*)                          EVENTJUMP:=NIL; EVENTDEF:=FALSE;
(*5450*)                    END;
(*5451*)                  ENTERID(LCP); LCP1:=LCP; INSYMBOL;
(*5452*)                END;
(*5453*)            UNTIL SY<>COMMA;
(*5454*)            IF SY=COLON THEN INSYMBOL ELSE ERROR(5);
(*5455*)          END;
(*5456*)        LADDR:=IC;
(*5457*)        REPEAT STATEMENT(FSYS+(.SEMICOLON,ENDSY,POSTSY.));
(*5458*)          IF SY IN STATBEGSYS THEN ERROR(14);
(*5459*)        UNTIL NOT (SY IN STATBEGSYS);
(*5460*)        WHILE SY=SEMICOLON DO
(*5461*)          BEGIN INSYMBOL;
(*5462*)            REPEAT STATEMENT(FSYS+(.SEMICOLON,ENDSY,POSTSY.));
(*5463*)            UNTIL NOT (SY IN STATBEGSYS);
(*5464*)          END;
(*5465*)        GENJMP(LADDR);
(*5466*)        IF SY=POSTSY THEN
(*5467*)          BEGIN
(*5468*)            REPEAT INSYMBOL;
(*5469*)              IF SY<>IDENT THEN
(*5470*)                BEGIN ERROR(2); SKIP(FSYS+(.COLON.)); END
(*5471*)              ELSE
(*5472*)                BEGIN SEARCHID((.EVENT.),LCP);
(*5473*)                  WITH LCP^ DO
(*5474*)                    IF DISX<>TOP THEN ERROR(280)
(*5475*)                      ELSE IF NAME='EXIT    ' THEN ERROR(281)
(*5476*)                        ELSE IF EVENTDEF THEN ERROR(282)
(*5477*)                          ELSE
(*5478*)                            BEGIN INSERTCHAIN(EVENTJUMP);
(*5479*)                              EVENTJUMP:=NIL; EVENTDEF:=TRUE;
(*5480*)                            END;
(*5481*)                  INSYMBOL;
(*5482*)                  TEST1(COLON,5);
(*5483*)                END;
(*5484*)              REPEAT STATEMENT(FSYS+(.SEMICOLON.));
(*5485*)                IF SY IN STATBEGSYS THEN ERROR(14);
(*5486*)              UNTIL NOT (SY IN STATBEGSYS);
(*5487*)              PREPJMP(LCIX); LINKOCC(CHAIN,LCIX);
(*5488*)            UNTIL SY<>SEMICOLON;
(*5489*)          END;
(*5490*)        IF SY=ENDSY THEN
(*5491*)        BEGIN RIGHTCHECK; INSYMBOL END ELSE ERROR(13);
(*5492*)        WHILE LCP1<>NIL DO
(*5493*)          BEGIN INSERTCHAIN(LCP1^.EVENTJUMP);
(*5494*)            LCP1:=LCP1^.NEXT;
(*5495*)          END;
(*5496*)        INSERTCHAIN(CHAIN);
(*5497*)        TOP:=OLDTOP;
(*5498*)      END;
(*5499*) 
// $TITLE FOR STATEMENT
(*5500*)    PROCEDURE CONTROLVARIABLE(VAR LCP:CTP; VAR CVAR1,CVAR2:INTEGER);
(*5501*)      BEGIN INSYMBOL;
(*5502*)        IF SY = IDENT THEN
(*5503*)          BEGIN SEARCHID((.VARS.),LCP);
(*5504*)            WITH LCP^ DO
(*5505*)              IF IDTYPE <> NIL THEN
(*5506*)                IF (IDTYPE^.FORM>SUBRANGE) OR (IDTYPE=REALPTR)
(*5507*)                    OR (IDTYPE^.FORM=PACKDTYPE)
(*5508*)                  THEN ERROR(143)
(*5509*)                  ELSE IF VKIND=DRCT
(*5510*)                    THEN BEGIN CVAR1:=VLEV; CVAR2:=VADDR END
(*5511*)                    ELSE ERROR(155);
(*5512*)            INSYMBOL
(*5513*)          END
(*5514*)        ELSE
(*5515*)          BEGIN ERROR(2); SKIP(FSYS+(.BECOMES,TOSY,DOWNTOSY,DOSY.));
(*5516*)                LCP:=UVARPTR;
(*5517*)          END;
(*5518*)      END;
(*5519*) 
(*5520*)    PROCEDURE FORSTATEMENT;
(*5521*)      VAR LIMITP: ATTRP; LSP: STP; LSY: SYMBOL;
(*5522*)         LCIX: ADDRRANGE;  LCP: CTP;
(*5523*)         LMIN,LMAX: INTEGER; LADDR: ADDRRANGE;
(*5524*)         CVAR1,CVAR2:INTEGER;(*ADDRESS OF CONTROL VARIABLE*)
(*5525*)         COND:INTEGER;
(*5526*)      BEGIN
(*5527*)        CONTROLVARIABLE(LCP,CVAR1,CVAR2);
(*5528*)        IF SY = BECOMES THEN
(*5529*)          BEGIN INSYMBOL; EXPRESSION(FSYS+(.TOSY,DOWNTOSY,DOSY.));
(*5530*)            IF GATTRP^.TYPTR <> NIL THEN
(*5531*)              IF COMPTYPES(LCP^.IDTYPE,GATTRP^.TYPTR) THEN
(*5532*)                BEGIN LOAD(GATTRP,NIL); BASEREGISTER(CVAR1,CVAR2);
(*5533*)                      GENRXP(ZST,GATTRP^.REXPR.RNO,0,RBASE,EFFADRS);
(*5534*)                END
(*5535*)              ELSE ERROR(145);
(*5536*)            RESETG;
(*5537*)          END
(*5538*)        ELSE
(*5539*)          BEGIN ERROR(51); SKIP(FSYS+(.TOSY,DOWNTOSY,DOSY.)) END;
(*5540*)        LSY := SY; ATTRNEW(LIMITP); LIMITP^.TYPTR := NIL;
(*5541*)        IF SY IN (.TOSY,DOWNTOSY.) THEN
(*5542*)          BEGIN
(*5543*)            INSYMBOL; EXPRESSION(FSYS+(.DOSY.));
(*5544*)            IF GATTRP^.TYPTR <> NIL THEN
(*5545*)              IF COMPTYPES(LCP^.IDTYPE,GATTRP^.TYPTR) THEN
(*5546*)                BEGIN COPYATTR(GATTRP,LIMITP);
(*5547*)                  IF LIMITP^.KIND<>CST THEN
(*5548*)                    BEGIN LOAD(LIMITP,NIL); SAVE(LIMITP^.REXPR.RNO); END;
(*5549*)                END
(*5550*)              ELSE ERROR(145)
(*5551*)          END
(*5552*)        ELSE BEGIN ERROR(55); SKIP(FSYS+(.DOSY.)) END;
(*5553*)        TEST1(DOSY,54);
(*5554*)        BASEREGISTER(CVAR1,CVAR2); GENRX(ZL,R0,0,RBASE,EFFADRS);
(*5555*)        LADDR:=IC;
(*5556*)        IF LIMITP^.TYPTR <> NIL THEN
(*5557*)        IF LIMITP^.KIND=CST
(*5558*)          THEN BEGIN MAKECONSTANT(LIMITP^.CVAL);
(*5559*)                     GENRX(ZC,R0,0,0,0);
(*5560*)               END
(*5561*)          ELSE BEGIN BASEREGISTER(LEVEL,LIMITP^.REXPR.ATEMP^.TEMPADRS);
(*5562*)                     GENRX(ZC,R0,0,RBASE,EFFADRS);
(*5563*)               END;
(*5564*)        IF LSY=TOSY THEN COND:=CONDP ELSE COND:=CONDM;
(*5565*)        LCIX:=IC; GENRX(ZBC,COND,0,0,0);
(*5566*)        IF LCP^.IDTYPE <> NIL THEN
(*5567*)          BEGIN
(*5568*)            IF DEBUG THEN     IF LCP^.IDTYPE<>INTPTR THEN
(*5569*)              BEGIN GETBOUNDS(LCP^.IDTYPE,LMIN,LMAX);
(*5570*)                CHECKREGISTER(R0,LMIN,LMAX);
(*5571*)              END;
(*5572*)          END;
(*5573*)        STATEMENT(FSYS);
(*5574*)        BASEREGISTER(CVAR1,CVAR2);
(*5575*)        GENRX(ZL,R0,0,RBASE,EFFADRS);
(*5576*)        IF LSY=TOSY
(*5577*)          THEN BEGIN MAKEINTCONST(1); GENRX(ZA,R0,0,0,0) END
(*5578*)          ELSE GENRR(ZBCTR,R0,0);
(*5579*)        GENRX(ZST,R0,0,RBASE,EFFADRS);
(*5580*)        GENJMP(LADDR); INSERTIC(LCIX);
(*5581*)        ATTRDISP(LIMITP);
(*5582*)      END (*FORSTATEMENT*) ;
(*5583*) 
// $TITLE FOR ALL STATEMENT
(*5584*)    PROCEDURE FORALLSTATEMENT;
(*5585*)      VAR LCP:CTP; CVAR1,CVAR2:INTEGER; SETREG:INTEGER;
(*5586*)          TEMP:CMP; LADDR,LCIX:ADDRRANGE;
(*5587*)        BEGIN
(*5588*)          IF EXTWARN THEN ERROR(291);
(*5589*)          CONTROLVARIABLE(LCP,CVAR1,CVAR2);
(*5590*)        IF OP=INOP THEN
(*5591*)          BEGIN INSYMBOL; EXPRESSION(FSYS+(.DOSY.));
(*5592*)            IF GATTRP^.TYPTR<>NIL THEN
(*5593*)              IF GATTRP^.TYPTR^.FORM<>POWER THEN ERROR(130)
(*5594*)              ELSE IF COMPTYPES(LCP^.IDTYPE,GATTRP^.TYPTR^.ELSET)
(*5595*)                THEN BEGIN LOAD(GATTRP,NIL); SETREG:=REALREG(.GATTRP^.REXPR.RNO.); END
(*5596*)                ELSE BEGIN ERROR(129); SETREG:=10; END;
(*5597*)          END
(*5598*)        ELSE BEGIN ERROR(60); SKIP(FSYS+(.DOSY.)); SETREG:=10; END;
(*5599*)        IF SY=DOSY THEN INSYMBOL ELSE ERROR(54);
(*5600*)        RESETG; GENRR(ZXR,R0,R0); LADDR:=IC; GENRR(ZLTR,SETREG,SETREG);
(*5601*)        LCIX:=IC; GENRX(ZBC,CONDNM,0,0,0); BASEREGISTER(CVAR1,CVAR2);
(*5602*)        GENRX(ZST,R0,0,RBASE,EFFADRS); GETTEMP(8,TEMP);
(*5603*)        BASEREGISTER(LEVEL,TEMP^.TEMPADRS);
(*5604*)        GENRX(ZSTM,SETREG,SETREG+1,RBASE,EFFADRS);
(*5605*)        STATEMENT(FSYS);
(*5606*)        BASEREGISTER(CVAR1,CVAR2); GENRX(ZL,R0,0,RBASE,EFFADRS);
(*5607*)        BASEREGISTER(LEVEL,TEMP^.TEMPADRS); GENRX(ZLM,SETREG,SETREG+1,RBASE,EFFADRS);
(*5608*)        INSERTIC(LCIX); MAKEINTCONST(1);
(*5609*)        GENRX(ZA,R0,0,0,0); MAKEINTCONST(64); GENRX(ZCL,R0,0,0,0);
(*5610*)        GENRX(ZBC,CONDNM,0,PBASE1,IC+12); GENRX(ZSLDL,SETREG,0,0,1);
(*5611*)        GENJMP(LADDR); DELETETEMP(TEMP);
(*5612*)      END;
(*5613*) 
// $TITLE  WITH - STATEMENT
(*5614*)    PROCEDURE WITHSTATEMENT;
(*5615*)      VAR LCP:CTP; OLDTOP:DISPRANGE; OLDLEVEL:LEVRANGE;
(*5616*)          TEMP:CMP;
(*5617*)      BEGIN OLDTOP:=TOP; OLDLEVEL:=DISPLEVEL;
(*5618*)        REPEAT INSYMBOL;
(*5619*)          IF SY = IDENT THEN
(*5620*)            BEGIN SEARCHID((.VARS,FIELD.),LCP); INSYMBOL END
(*5621*)          ELSE BEGIN ERROR(2); LCP := UVARPTR END;
(*5622*)          SELECTOR(FSYS+(.COMMA,DOSY.),LCP);
(*5623*)          IF GATTRP^.TYPTR <> NIL THEN
(*5624*)            IF GATTRP^.TYPTR^.FORM = RECORDS THEN
(*5625*)              IF TOP < DISPLIMIT THEN
(*5626*)                BEGIN TOP := TOP + 1;
(*5627*)                  WITH DISPLAY(.TOP.), GATTRP^ DO
(*5628*)                    BEGIN FNAME:=TYPTR^.FIELDS; OCCUR:=REC;
(*5629*)                      IF ((ACCESS=INDIRECT)
(*5630*)                      OR (VARKIND = INDRCT)) AND
(*5631*)                      (DISPLEVEL>=5) AND
(*5632*)                      ((LEVEL<5) OR ((LEVEL=5)AND(DISPLEVEL=6)))
(*5633*)                      THEN
(*5634*)                      BEGIN
(*5635*)                       DADRS:=0; DISPKIND:=DRCT;
(*5636*)                       IF DISPLEVEL=6 THEN
(*5637*)                       BEGIN
(*5638*)                        REG6USED:=TRUE
(*5639*)                       END ELSE
(*5640*)                      BEGIN  REG5USED:=TRUE; END;
(*5641*)                          DLEVEL:=DISPLEVEL; LOADADDRESS(GATTRP,NIL);
(*5642*)                          GENRR(ZLR,DISPLEVEL,REALREG(.GATTRP^.REXPR.RNO.));
(*5643*)                        DISPLEVEL:=DISPLEVEL-1;
(*5644*)                        END
(*5645*)                      ELSE IF ACCESS=DIRECT THEN
(*5646*)                        BEGIN DADRS:=VADRS; DISPKIND:=VARKIND;
(*5647*)                          IF VARKIND=DRCT THEN DLEVEL:=VLEVEL
(*5648*)                                          ELSE BEGIN DBASEL:=BASELEV; DBASEA:=BASEADD; END;
(*5649*)                        END
(*5650*)                      ELSE BEGIN DADRS:=0; DISPKIND:=INDRCT;
(*5651*)                             LOADADDRESS(GATTRP,NIL);
(*5652*)                             GETTEMP(4,TEMP);
(*5653*)                             BASEREGISTER(LEVEL,TEMP^.TEMPADRS); GENRXP(ZST,REXPR.RNO,0,RBASE,EFFADRS);
(*5654*)                             DBASEL:=LEVEL; DBASEA:=TEMP^.TEMPADRS;
(*5655*)                           END;
(*5656*)                      RESETG;
(*5657*)                    END
(*5658*)                END
(*5659*)              ELSE ERROR(250)
(*5660*)            ELSE ERROR(140);
(*5661*)        UNTIL SY<>COMMA;
(*5662*)        TEST1(DOSY,54);
(*5663*)        STATEMENT(FSYS);
(*5664*)        TOP:=OLDTOP; DISPLEVEL:=OLDLEVEL;
(*5665*)      END (*WITHSTATEMENT*) ;
(*5666*) 
// $TITLE  STATEMENT - (BODY)
(*5667*)   BEGIN (*STATEMENT*)
(*5668*)    IF SY = INTCONST THEN (*LABEL*)
(*5669*)     BEGIN
(*5670*)      LLP := FSTLABP;
(*5671*)      WHILE LLP <> FLABP DO
(*5672*)       WITH LLP^ DO
(*5673*)        IF LABVAL = IVAL THEN
(*5674*)         BEGIN
(*5675*)          IF DEFINED THEN ERROR(165)
(*5676*)          ELSE
(*5677*)           BEGIN INSERTCHAIN(FSTOCC);
(*5678*)            DEFINED := TRUE; LABADDR := IC;
(*5679*)            IF LCNT<>0 THEN       (*LONG JUMP*)
(*5680*)              BEGIN
(*5681*)                GENRX(ZBC,15,0,PBASE1,IC+10);
(*5682*)                PROCADDRESS(.LCNT.):=PROGCOUNT+IC;
(*5683*)                GENRR(ZLR,0,LEVEL);
(*5684*)                GENRX(ZBAL,BASEWORK,0,1,ENTRYLONGJUMP);
(*5685*)              END;
(*5686*)           END;
(*5687*)          GOTO 1
(*5688*)         END
(*5689*)        ELSE LLP := NEXTLAB;
(*5690*)      ERROR(167);
(*5691*)   1: INSYMBOL;
(*5692*)      TEST1(COLON,5);
(*5693*)     END;
(*5694*)    IF NOT (SY IN FSYS+(.IDENT.)) THEN
(*5695*)     BEGIN ERROR(6); SKIP(FSYS) END;
(*5696*)    IF SY IN STATBEGSYS+(.IDENT.) THEN
(*5697*)     BEGIN
(*5698*)      CASE SY OF
(*5699*)       IDENT:    BEGIN SEARCHID((.VARS,FIELD,FUNC,PROC,EVENT.),LCP); INSYMBOL;
(*5700*)                   CASE LCP^.KLASS OF
(*5701*)                     PROC:  CALL(FSYS,LCP);
(*5702*)                     VARS,FIELD,FUNC:  ASSIGNMENT(LCP);
(*5703*)                     EVENT: BEGIN PREPJMP(LCIX); LINKOCC(LCP^.EVENTJUMP,LCIX); END
(*5704*)                   END;
(*5705*)                 END;
(*5706*)       BEGINSY:  BEGIN
(*5707*)                  LEFTCHECK;INSYMBOL;
(*5708*)                  COMPOUNDSTATEMENT(FSYS+(.SEMICOLON,ENDSY.));
(*5709*)                  INSYMBOL;
(*5710*)                 END;
(*5711*)       GOTOSY:   BEGIN INSYMBOL; GOTOSTATEMENT END;
(*5712*)       IFSY:     BEGIN INSYMBOL; IFSTATEMENT END;
(*5713*)       CASESY:   BEGIN LEFTCHECK;INSYMBOL;CASESTATEMENT END;
(*5714*)       WHILESY:  BEGIN INSYMBOL; WHILESTATEMENT END;
(*5715*)       REPEATSY: BEGIN LEFTCHECK;INSYMBOL;REPEATSTATEMENT END;
(*5716*)       LOOPSY:   BEGIN LEFTCHECK;INSYMBOL; LOOPSTATEMENT END;
(*5717*)       FORSY:    FORSTATEMENT;
(*5718*)       FORALLSY: FORALLSTATEMENT;
(*5719*)       WITHSY:   WITHSTATEMENT
(*5720*)      END;
(*5721*)      TEST2(FSYS,6,(..));
(*5722*)     END;
(*5723*)   END (*STATEMENT*) ;
(*5724*) 
// $TITLE  COMPOUNDSTATEMENT
(*5725*)    PROCEDURE COMPOUNDSTATEMENT(FSYS:SETOFSYS);
(*5726*)      BEGIN
(*5727*)        REPEAT
(*5728*)          STATEMENT(FSYS);
(*5729*)          IF SY IN STATBEGSYS THEN ERROR(14);
(*5730*)        UNTIL NOT (SY IN STATBEGSYS);
(*5731*)        WHILE SY = SEMICOLON DO
(*5732*)          BEGIN INSYMBOL;
(*5733*)            REPEAT
(*5734*)              STATEMENT(FSYS);
(*5735*)              IF SY IN STATBEGSYS THEN ERROR(14);
(*5736*)            UNTIL NOT (SY IN STATBEGSYS)
(*5737*)          END;
(*5738*)       IF SY=ENDSY THEN
(*5739*)       RIGHTCHECK ELSE ERROR(13);
(*5740*)      END;
(*5741*) 
// $TITLE  BODYINIT,CLOSECODEGEN
(*5742*)    PROCEDURE BODYINITIALIZE;
(*5743*)      VAR R:REGNO;
(*5744*)      I : INTEGER;
(*5745*)      BEGIN
(*5746*)        DP := FALSE;
(*5747*)        REG5USED:=FALSE; REG6USED:=FALSE;
(*5748*)        DISPLEVEL:=6;
(*5749*)        FOR I:=0 TO NCODESEGS-1 DO CODEPTR(.I.):=NIL;
(*5750*)        FOR R:=R10 TO F6 DO
(*5751*)          REGISTER(.R.).USED:=FALSE;
(*5752*)        ATTRHEAD:=NIL;
(*5753*)        ATTRNEW(GATTRP);
(*5754*)        WITH GATTRP^ DO
(*5755*)          BEGIN TYPTR:=NIL; KIND:=CST; END;
(*5756*)        FREETEMP:=NIL;
(*5757*)        IC:=4; STACKTOP:=0;
(*5758*)        IF INITFLAG THEN
(*5759*)        BEGIN
(*5760*)          DATA1(0); DATA1(INITNUMBER);
(*5761*)          INITFLAG := FALSE;
(*5762*)        END;
(*5763*)        EXTENDEDADDRESS:=FALSE;
(*5764*)        CONSTTOP:=NIL; STACKSIZE:=NIL;
(*5765*)        GENRR(ZLR,LEVEL,0);
(*5766*)        IF PMD OR PROCNAMES THEN GENRX(ZBC,0,0,0,0);
(*5767*)      END;
(*5768*) 
(*5769*)    PROCEDURE CLOSECODEGEN;
(*5770*)      VAR I,A,B,X,CODEEND,OP,A1,A2,Y,Z:INTEGER; P:CTAILP;
(*5771*)         LOCODEPTR : CODESPTR;
(*5772*)         STCNVRT : RECORD
(*5773*)               CASE X : BOOLEAN OF
(*5774*)                TRUE:(NME:ALFA);
(*5775*)               FALSE:(A1,A2:INTEGER);
(*5776*)            END;
(*5777*) 
(*5778*) 
(*5779*)      PROCEDURE ALIGNCONST(X:INTEGER);
(*5780*)        VAR X1,X2:INTEGER;
(*5781*)        BEGIN HALFWORD(X,X1,X2);
(*5782*)              IF IC >= 4096*(7-LEVEL)-2 THEN
(*5783*)              BEGIN ERROR(253); IC:=0 END;
(*5784*)              MAKECODE(IC,X1); MAKECODE(IC+2,X2);
(*5785*)              IC:=IC+4;
(*5786*)        END;
(*5787*) 
// $TITLE POST MORTEM DUMP (PMDINFO)
(*5788*)PROCEDURE PMDINFO ( FCP : CTP);
(*5789*)   VAR I ,DISPT : INTEGER;
(*5790*)BEGIN (* PMDINFO *)
(*5791*)  IF FCP <> NIL THEN
(*5792*)   WITH FCP^ DO
(*5793*)   BEGIN
(*5794*)     PMDINFO(LLINK);
(*5795*)     IF KLASS = VARS THEN
(*5796*)       IF IDTYPE <> NIL THEN
(*5797*)         IF ((IDTYPE^.FORM <= POINTER) AND
(*5798*)            (IDTYPE^.FORM <> PACKDTYPE))  OR
(*5799*)             COMPTYPES(IDTYPE,ALFAPTR) THEN
(*5800*)         BEGIN
(*5801*)           IF IDTYPE^.FORM = POINTER THEN I := 0
(*5802*)            ELSE
(*5803*)             IF COMPTYPES(IDTYPE,INTPTR) THEN I:= 2
(*5804*)              ELSE
(*5805*)               IF COMPTYPES(IDTYPE,REALPTR) THEN I:=4
(*5806*)               ELSE
(*5807*)                IF COMPTYPES(IDTYPE,CHARPTR) THEN I:=6
(*5808*)                ELSE
(*5809*)                 IF COMPTYPES(IDTYPE,BOOLPTR) THEN I:=8
(*5810*)                  ELSE IF COMPTYPES(IDTYPE,ALFAPTR) THEN I:=10
(*5811*)                     ELSE I:=12;
(*5812*)           STCNVRT.NME := NAME;
(*5813*)           IF VKIND = INDRCT THEN
(*5814*)           BEGIN
(*5815*)             I := I + 1;  DISPT := PARADDR;
(*5816*)           END ELSE DISPT := VADDR;
(*5817*)     DATA1(16777216*I+DISPT);                                                   
(*5818*)           DATA1(STCNVRT.A1);
(*5819*)           DATA1(STCNVRT.A2);
(*5820*)           A:=A+12;
(*5821*)        END;
(*5822*)        PMDINFO(RLINK);
(*5823*)      END
(*5824*)END; (* PMDINFO *)
(*5825*) 
(*5826*)      BEGIN
(*5827*)        ALIGNMENT(LC,8);
(*5828*)              IF EXTRNL AND (LEVEL=2) THEN                                      
(*5829*)        BEGIN
(*5830*)          GENRX(ZLM,8,6,2,0);
(*5831*)          GENRX(ZST,0,0,1,8);
(*5832*)          GENRX(ZBC,15,0,9,2);
(*5833*)       END ELSE
(*5834*)        GENRX(ZBC,15,0,1,ENTRYRET+LEVEL*8);
(*5835*)        CODEEND:=IC;
(*5836*)        IF IC MOD 4<>0 THEN
(*5837*)        BEGIN MAKECODE(IC,0); IC:=IC+2 END;
(*5838*)        WHILE CONSTTOP<>NIL DO
(*5839*)          WITH CONSTTOP^.SAVECONST DO
(*5840*)            BEGIN CASE CKIND OF
(*5841*)              INT: BEGIN INSERTCHAIN(CONSTTOP^.CCHAIN); ALIGNCONST(IVAL); END;
(*5842*)              REEL,PSET:
(*5843*)                   BEGIN
(*5844*)                     IF IC MOD 8<>0 THEN ALIGNCONST(0);
(*5845*)                     INSERTCHAIN(CONSTTOP^.CCHAIN);
(*5846*)                     SETVALUE(PVAL,A1,A2);
(*5847*)                     ALIGNCONST(A1); ALIGNCONST(A2);
(*5848*)                   END;
(*5849*)              STRG:BEGIN P:=VALP; INSERTCHAIN(CONSTTOP^.CCHAIN);   (*BOUNDARY CHECK (8*N) IS*)
(*5850*)                     WHILE P<>NIL DO WITH P^ DO                    (*NOT NECESSARY. THE ONLY USE*)
(*5851*)                       BEGIN ALIGNCONST(STFR); P:=NXTCSP; END;     (*OF STRUCTURED CONSTANT IS*)
(*5852*)                   END                                             (*ASSIGNMENT AS A WHOLE*)
(*5853*)              END;
(*5854*)              CONSTTOP:=CONSTTOP^.NEXTCONST;
(*5855*)            END;
(*5856*)        IF PMD OR PROCNAMES THEN
(*5857*)        MAKECODE(8,IC)  ELSE
(*5858*)        IF IC MOD 8<>0 THEN ALIGNCONST(0);
(*5859*)           HALFWORD(LC,A1,A2);
(*5860*)           IF EXTENDEDADDRESS THEN A1:=A1+4*256*LEVEL;
(*5861*)           MAKECODE(0,A1); MAKECODE(2,A2);
(*5862*)           IF REG6USED THEN
(*5863*)           BEGIN
(*5864*)              IF IC>4096*(6-LEVEL) THEN ERROR(253)
(*5865*)           END ELSE
(*5866*)           IF REG5USED THEN
(*5867*)           BEGIN
(*5868*)             IF IC >4096*(5-LEVEL) THEN ERROR(253)
(*5869*)           END;
(*5870*)           X:=0; Y:=0; LOCODEPTR:= CODEPTR(.0.);
(*5871*)          FOR A := 0 TO (IC DIV 4) -1 DO
(*5872*)          BEGIN
(*5873*)          DATA1(LOCODEPTR^.FULLWORDS(.X.));
(*5874*)          X:=X+1;
(*5875*)           IF X = CODEBLCK+1 THEN
(*5876*)           BEGIN
(*5877*)             X:=0; Y := Y + 1;
(*5878*)             LOCODEPTR:=CODEPTR(.Y.);
(*5879*)           END;
(*5880*)         END;
(*5881*)         IF PMD OR PROCNAMES THEN
(*5882*)         BEGIN
(*5883*)           A:=0;
(*5884*)           STCNVRT.NME:=FPROCP^.NAME;
(*5885*)           DATA1(STCNVRT.A1); DATA1(STCNVRT.A2);
(*5886*)           A:=A+8;
(*5887*)           IF PMD THEN
(*5888*)           PMDINFO(DISPLAY(.LEVEL.).FNAME);
(*5889*)           DATA1(0);
(*5890*)           IF ( A+IC + 4) MOD 8 <> 0 THEN
(*5891*)           BEGIN A:=A+4; DATA1(0) END;
(*5892*)           PROGCOUNT:=PROGCOUNT+IC+A+4
(*5893*)           END ELSE PROGCOUNT:=PROGCOUNT+IC;
(*5894*)        IF PRINTCODE THEN
(*5895*)          BEGIN I:=0;
(*5896*)               ENDOFLINE;
(*5897*)            WHILE I<CODEEND DO
(*5898*)                 BEGIN WRITE(' ');WRITEHEX(I); WRITE(' ');                      
(*5899*)                  X:=GETCODE(I); WRITEHEX(X);                                   
(*5900*)                OP:=X DIV 256; X:=X MOD 256;
(*5901*)                IF OP<64 THEN
(*5902*)                   BEGIN WRITE(' ':14,MNEMONIC(.OP.),'  ',X DIV 16:1,           
(*5903*)                              ',', X MOD 16:1); I:=I+2;
(*5904*)                  END
(*5905*)                ELSE IF OP<192 THEN
(*5906*)                    BEGIN  Y:=GETCODE(I+2);WRITEHEX(Y);
(*5907*)                 WRITE(' ':8,MNEMONIC(.OP.),'  ',X DIV 16:1,',',                
(*5908*)                          Y MOD 4096:1, '(', X MOD 16:1,
(*5909*)                          ',', Y DIV 4096:1, ')'); I:=I+4;
(*5910*)                  END
(*5911*)                ELSE
(*5912*)                  BEGIN
(*5913*)                Y := GETCODE(I+2); WRITEHEX(Y);                                 
(*5914*)                     Z:=GETCODE(I+4); WRITEHEX(Z);
(*5915*)                    WRITE('  ', MNEMONIC(.OP.), '  ',
(*5916*)                          Y MOD 4096:1, '(', X+1:1, ',', Y DIV 4096:1,
(*5917*)                          '),', Z MOD 4096:1, '(', Z DIV 4096:1, ')'); I:=I+6;
(*5918*)                  END;
(*5919*)                WRITELN;
(*5920*)               ENDOFLINE;
(*5921*)              END;
(*5922*)            IF I MOD 4<>0 THEN
(*5923*)              BEGIN WRITE(' '); WRITEHEX(I);
(*5924*)             WRITE('  '); WRITEHEX(GETCODE(I));
(*5925*)                    WRITELN; I:=I+2;
(*5926*)             ENDOFLINE;
(*5927*)              END;
(*5928*)            WHILE I<IC DO
(*5929*)                 BEGIN WRITE(' ');  WRITEHEX(I); WRITE('  ');
(*5930*)                   WRITEHEX(GETCODE(I)); WRITE(' ');
(*5931*)                   WRITEHEX(GETCODE(I+2));
(*5932*)                    WRITELN; I:=I+4;
(*5933*)                  ENDOFLINE;
(*5934*)              END;
(*5935*)         END;
(*5936*)      END;
(*5937*) 
// $TITLE OPENFILES,OPENEXT,OPENLOC,OPEN1
(*5938*)    PROCEDURE OPENFILES(FCP:CTP);
(*5939*)      VAR EXTFILE:BOOLEAN; CLSP,EXFILP:FILEP;
(*5940*) 
(*5941*)      PROCEDURE OPENEXT(FSIZE,FADDR:ADDRRANGE);
(*5942*)        VAR WNAME: RECORD CASE FLAG:BOOLEAN OF
(*5943*)                     FALSE: (STR: PACKED ARRAY(.1..8.) OF CHAR);
(*5944*)                     TRUE:  (INT: ARRAY(.1..2.) OF INTEGER)
(*5945*)                   END;
(*5946*)        BEGIN
(*5947*)          GENRR(ZLR,BASEWORK,LEVEL); MAKEINTCONST(FADDR);
(*5948*)          GENRX(ZA,BASEWORK,0,0,0);
(*5949*)          LOADINTCONST(R0,FSIZE);
(*5950*)          GENRX(ZST,R0,0,BASEWORK,0);
(*5951*)          WNAME.STR:=FCP^.NAME; MAKEINTCONST(WNAME.INT(.1.));
(*5952*)          GENRX(ZL,R0,0,0,0); GENRX(ZST,R0,0,BASEWORK,4);
(*5953*)          MAKEINTCONST(WNAME.INT(.2.));
(*5954*)          GENRX(ZL,R0,0,0,0); GENRX(ZST,R0,0,BASEWORK,8);
(*5955*)          GENRR(ZLR,R0,BASEWORK); GENRX(ZBAL,BASEWORK,0,1,ENTOPEXT);
(*5956*)        END;
(*5957*) 
(*5958*)     PROCEDURE OPENLOC(FSIZE,FADDR:ADDRRANGE);
(*5959*)        BEGIN
(*5960*)          GENRR(ZLR,BASEWORK,LEVEL); MAKEINTCONST(FADDR);
(*5961*)          GENRX(ZA,BASEWORK,0,0,0);
(*5962*)          LOADINTCONST(R0,FSIZE);
(*5963*)          GENRX(ZST,R0,0,BASEWORK,0);
(*5964*)          GENRX(ZMVI,R0,0,BASEWORK,4);
(*5965*)          GENRR(ZLR,R0,BASEWORK);
(*5966*)          GENRX(ZBAL,BASEWORK,0,1,ENTOPEXT);
(*5967*)        END;
(*5968*) 
(*5969*)      PROCEDURE OPEN1(FSP:STP; FADDR:ADDRRANGE);
(*5970*)        VAR I,LMIN,LMAX,S:INTEGER; LCP:CTP;
(*5971*)        BEGIN
(*5972*)          IF FSP<>NIL THEN
(*5973*)            WITH FSP^ DO
(*5974*)              IF FORM IN (.RECORDS,ARRAYS,FILES.) THEN
(*5975*)                CASE FORM OF
(*5976*)         RECORDS: BEGIN LCP:=FSTFLD;
(*5977*)                    WHILE LCP<>NIL DO
(*5978*)                      WITH LCP^ DO
(*5979*)                        BEGIN OPEN1(IDTYPE,FADDR+FLDADDR);
(*5980*)                              LCP:=NEXT;
(*5981*)                        END;
(*5982*)                  END;
(*5983*)         ARRAYS:  IF INXTYPE<>NIL THEN
(*5984*)                    BEGIN GETBOUNDS(INXTYPE,LMIN,LMAX);
(*5985*)                      FOR I:=0 TO LMAX-LMIN DO
(*5986*)                        OPEN1(AELTYPE,FADDR+AELLENG*I);
(*5987*)                    END;
(*5988*)         FILES:   BEGIN
(*5989*)                    IF TEXTFILE
(*5990*)                      THEN S:=-1
(*5991*)                      ELSE S:=SIZE.WBLENGTH-8;
(*5992*)                        IF EXTFILE THEN
(*5993*)                             OPENEXT(S,FADDR)
(*5994*)                        ELSE
(*5995*)                       OPENLOC(S,FADDR);
(*5996*)                            NEW(CLSP);
(*5997*)                            WITH CLSP^ DO
(*5998*)                              BEGIN NXTP:=LOCFILP; ADDR:=FADDR; END;
(*5999*)                            LOCFILP:=CLSP;
(*6000*)                  END
(*6001*)                END;
(*6002*)        END;
(*6003*) 
(*6004*)      BEGIN
(*6005*)        IF FCP<>NIL THEN
(*6006*)          WITH FCP^ DO
(*6007*)            BEGIN OPENFILES(LLINK); OPENFILES(RLINK);
(*6008*)              IF (KLASS=VARS) AND (VKIND=DRCT) THEN
(*6009*)                IF IDTYPE<>NIL THEN
(*6010*)                  IF IDTYPE^.FTYPE THEN
(*6011*)                    BEGIN EXTFILE:=FALSE;
(*6012*)                      IF IDTYPE^.FORM=FILES THEN
(*6013*)                        BEGIN EXFILP:=FEXFILP;
(*6014*)                          {LOOP} while true do begin IF EXFILP=NIL THEN break;
(*6015*)                            WITH EXFILP^ DO
(*6016*)                              BEGIN
(*6017*)                                IF FILENAME=NAME THEN
(*6018*)                                  BEGIN EXTFILE:=TRUE; DECLARED:=TRUE; ADDR:=VADDR; break; END;
(*6019*)                                EXFILP:=NXTP;
(*6020*)                              END;
(*6021*)                          END;
(*6022*)                        END;
(*6023*)                      IF (FCP<>INPUTPTR) AND (FCP<>OUTPUTPTR) THEN
(*6024*)                        OPEN1(IDTYPE,VADDR);
(*6025*)                    END;
(*6026*)            END;
(*6027*)      END;
(*6028*) 
// $TITLE  FILECHECK,LABELCHECK
(*6029*)    PROCEDURE FILECHECK;
(*6030*)      VAR FP:FILEP; FIRST:BOOLEAN; LCHCNT:INTEGER;
(*6031*)      BEGIN FP:=FEXFILP; FIRST:=TRUE;
(*6032*)        WHILE FP<>NIL DO
(*6033*)          WITH FP^ DO
(*6034*)            BEGIN
(*6035*)              IF NOT DECLARED THEN
(*6036*)                BEGIN
(*6037*)                  IF FIRST THEN
(*6038*)                    BEGIN ERROR(172); LCHCNT:=CHCNT; ENDOFLINE;
(*6039*)                                             FIRST:=FALSE;
(*6040*)                    END;
(*6041*)                            ENDOFLINE;
(*6042*)                  WRITELN('  FILE-ID ', FILENAME);
(*6043*)                END;
(*6044*)              FP:=NXTP;
(*6045*)            END;
(*6046*)      END;
(*6047*) 
(*6048*)    PROCEDURE LABELCHECK;
(*6049*)      VAR FIRST:BOOLEAN; LCHCNT:INTEGER;
(*6050*)      BEGIN
(*6051*)        FIRST := TRUE;
(*6052*)        WHILE FSTLABP <> FLABP DO
(*6053*)          WITH FSTLABP^ DO
(*6054*)            BEGIN
(*6055*)              IF NOT DEFINED THEN
(*6056*)                IF (LCNT<>0) OR (FSTOCC<>NIL) THEN
(*6057*)                  BEGIN
(*6058*)                    IF FIRST THEN
(*6059*)                      BEGIN ERROR(168); LCHCNT := CHCNT; ENDOFLINE;
(*6060*)                                FIRST := FALSE;
(*6061*)                      END;
(*6062*)                          ENDOFLINE;
(*6063*)                    WRITELN('  LABEL ',LABVAL)
(*6064*)                  END;
(*6065*)              FSTLABP := NEXTLAB
(*6066*)            END;
(*6067*)      END;
(*6068*) 
// $TITLE  CLOSE FILES
(*6069*)    PROCEDURE CLOSEFILES;
(*6070*) 
(*6071*)   PROCEDURE CLOSEALLFILES(FP:FILEP);
(*6072*)   BEGIN
(*6073*)     GENRR(ZLR,R0,LEVEL);  MAKEINTCONST(FP^.ADDR);
(*6074*)     GENRX(ZA,R0,0,0,0); GENRX(ZBAL,BASEWORK,0,1,ENTCLEXT);
(*6075*)   END;
(*6076*) 
(*6077*) 
(*6078*)      BEGIN
(*6079*)    IF LEVEL = 1 THEN
(*6080*)        IF INPUTPTR<>NIL THEN
(*6081*)          BEGIN GENRX(ZLA,R0,0,1,LCSTART);
(*6082*)            GENRX(ZBAL,BASEWORK,0,1,ENTCLEXT);
(*6083*)          END;
(*6084*)        WHILE LOCFILP<>NIL DO
(*6085*)       BEGIN CLOSEALLFILES(LOCFILP);LOCFILP:=LOCFILP^.NXTP END;
(*6086*)      END;
(*6087*) 
// $TITLE  BLOCK,BODY - (BODY)
(*6088*)  BEGIN (*BODY*)
(*6089*)    BODYINITIALIZE;
(*6090*)    OPENFILES(DISPLAY(.TOP.).FNAME);
(*6091*)    IF LEVEL=1 THEN
(*6092*)     BEGIN FILECHECK;
(*6093*)        IF INPUTPTR<>NIL THEN
(*6094*)          BEGIN GENRX(ZLA,15,0,1,LCSTART);
(*6095*)            GENRX(ZBAL,BASEWORK,0,1,OPENINPUT);
(*6096*)          END;
(*6097*)      END;
(*6098*)    IF SY =  BEGINSY THEN  INSYMBOL;
(*6099*)    COMPOUNDSTATEMENT(FSYS+(.SEMICOLON,ENDSY.));
(*6100*)    IF LEVEL > 1 THEN PROCLEV:=CHR(ORD('A')+LEVEL-2);
(*6101*)    INSYMBOL;
(*6102*)    LABELCHECK;
(*6103*)    CLOSEFILES;
(*6104*)    CLOSECODEGEN;
(*6105*)  END (*BODY*);
(*6106*) 
(*6107*) BEGIN (*BLOCK*)
(*6108*)   FLABP:=FSTLABP; FWPROCS:=NIL;
(*6109*)   REPEAT
(*6110*)     IF SY=LABELSY THEN LABELDECLARATION;
(*6111*)     IF SY = CONSTSY THEN
(*6112*)       BEGIN INSYMBOL; CONSTDECLARATION END;
(*6113*)     IF SY = TYPESY THEN
(*6114*)       BEGIN INSYMBOL; TYPEDECLARATION END;
(*6115*)     IF SY = VARSY THEN
(*6116*)       BEGIN INSYMBOL; VARDECLARATION END;
(*6117*)     IF SY=VALUESY THEN
(*6118*)       BEGIN
(*6119*)         IF EXTWARN THEN ERROR(291);
(*6120*)         IF EXTRNL THEN ERROR(300);
(*6121*)         INSYMBOL;
(*6122*)            VARINITIALIZATION END;
(*6123*)     IF (LEVEL =1) AND ( NOT EXTRNL ) THEN
(*6124*)     BEGIN
(*6125*)        PUTESD('P@MAIN  ',SD,FALSE); PUTESD('P@MAIN^ ',ER,TRUE);
(*6126*)     END;
(*6127*)     WHILE SY IN (.PROCSY,FUNCTSY.) DO
(*6128*)      BEGIN
(*6129*)       PROCLEV:=CHR(ORD('A')+LEVEL-1);
(*6130*)       LSY :=SY; INSYMBOL;
(*6131*)       PROCDECLARATION(LSY)
(*6132*)      END;
(*6133*)     IF SY <> BEGINSY THEN
(*6134*)      IF NOT EXTRNL THEN
(*6135*)       BEGIN ERROR(18); SKIP(FSYS) END
(*6136*)   UNTIL EXTRNL OR ( SY IN STATBEGSYS);
(*6137*)   UNDEFINED(FWPROCS,'PROC/FUNC');
(*6138*)   IF (NOT EXTRNL) OR (EXTRNL AND (LEVEL <> 1)) THEN
(*6139*)BEGIN
(*6140*)IF SY=BEGINSY THEN
(*6141*)BEGIN
(*6142*)  IF LEVEL > 1 THEN PROCLEV:=CHR(ORD('A')+LEVEL-2);
(*6143*)  LEFTCHECK;
(*6144*)  IC := 0; DP := FALSE;
(*6145*)   LOCATION := 0;
(*6146*)END ELSE ERROR(17);
(*6147*)   PROCADDRESS(.FPROCP^.PFCNT.):=PROGCOUNT;
(*6148*)   BODY(FSYS+(.CASESY.));
(*6149*)   IF SY <> FSY THEN
(*6150*)     BEGIN ERROR(6); SKIP(FSYS) END;
(*6151*)END;
(*6152*) END (*BLOCK*) ;
(*6153*) 
(*6154*) 
// $TITLE  PROGRAMME
(*6155*) PROCEDURE PROGRAMME(FSYS: SETOFSYS);
(*6156*)   VAR EXFILP:FILEP; LCP:CTP;
(*6157*)   BEGIN
(*6158*)     WITH DISPLAY(.1.) DO BEGIN FNAME:=NIL; OCCUR:=BLCK; END;
(*6159*)    NEW(LCP{,VARS});
(*6160*)    WITH LCP^ DO
(*6161*)      BEGIN NAME := 'OUTPUT  '; IDTYPE := TEXTPTR;
(*6162*)        VKIND := INDRCT; NEXT := NIL;
(*6163*)        VLEV:=1;  PARADDR := PTROUTBLCK;
(*6164*)      END;
(*6165*)    ENTERID(LCP);
(*6166*)    IF SY = PROGRAMSY THEN
(*6167*)     BEGIN INSYMBOL;
(*6168*)      IF SY = IDENT THEN
(*6169*)       BEGIN INSYMBOL;
(*6170*)        IF NOT (SY IN (.SEMICOLON,LPARENT.)) THEN
(*6171*)         BEGIN ERROR(7); SKIP(FSYS+(.SEMICOLON,LPARENT.)) END;
(*6172*)        IF SY = LPARENT THEN
(*6173*)         BEGIN
(*6174*)          REPEAT INSYMBOL;
(*6175*)           IF SY = IDENT THEN
(*6176*)            BEGIN
(*6177*)             IF ID = 'INPUT   ' THEN
(*6178*)               BEGIN NEW(INPUTPTR{,VARS});
(*6179*)                 WITH INPUTPTR^ DO
(*6180*)                   BEGIN NAME := 'INPUT   '; IDTYPE := TEXTPTR;
(*6181*)                     VKIND := DRCT; NEXT := NIL;
(*6182*)                     VLEV:=1; VADDR:=LC; LC:=LC+TEXTSIZE;
(*6183*)                   END;
(*6184*)                 ENTERID(INPUTPTR);
(*6185*)               END
(*6186*)             ELSE
(*6187*)              IF ID = 'OUTPUT  ' THEN OUTPUTPTR := LCP;
(*6188*)             EXFILP := FEXFILP;
(*6189*)             WHILE EXFILP <> NIL DO
(*6190*)               WITH EXFILP^ DO
(*6191*)                 BEGIN
(*6192*)                   IF FILENAME=ID THEN ERROR(101);
(*6193*)                   EXFILP := NXTP
(*6194*)                 END;
(*6195*)             IF (ID<>'INPUT   ') AND (ID<>'OUTPUT  ') THEN
(*6196*)               BEGIN NEW(EXFILP);
(*6197*)                 WITH EXFILP^ DO
(*6198*)                   BEGIN FILENAME := ID; NXTP := FEXFILP;
(*6199*)                    DECLARED := FALSE;
(*6200*)                   END;
(*6201*)                 FEXFILP := EXFILP
(*6202*)               END;
(*6203*)             INSYMBOL;
(*6204*)            END
(*6205*)           ELSE ERROR(2);
(*6206*)           IF NOT (SY IN (.COMMA,RPARENT.)) THEN
(*6207*)            BEGIN ERROR(6); SKIP(FSYS+(.COMMA,RPARENT.)) END
(*6208*)          UNTIL SY <> COMMA;
(*6209*)          IF SY = RPARENT THEN INSYMBOL ELSE ERROR(4)
(*6210*)         END;
(*6211*)   IF OUTPUTPTR = NIL THEN IF EXTWARN THEN ERROR(291);
(*6212*)        IF SY = SEMICOLON THEN INSYMBOL ELSE ERROR(14)
(*6213*)       END
(*6214*)      ELSE BEGIN ERROR(2); SKIP(FSYS) END
(*6215*)     END
(*6216*)    ELSE BEGIN ERROR(3); SKIP(FSYS) END;
(*6217*)    BLOCK(FSYS,PERIOD,UPRCPTR);
(*6218*)   END (*PROGRAMME*) ;
(*6219*) 
// $TITLE STDTYPENTRIES.
(*6220*) PROCEDURE STDTYPENTRIES;
(*6221*)   VAR SP:STP;
(*6222*) BEGIN
(*6223*)  NEW(INTPTR{,SCALAR,STANDARD});
(*6224*)  WITH INTPTR^ DO
(*6225*)    BEGIN FTYPE:=FALSE; SIZE.WBLENGTH:=4; SIZE.BOUNDARY:=4; END;
(*6226*)  NEW(REALPTR{,SCALAR,STANDARD});
(*6227*)  WITH REALPTR^ DO
(*6228*)    BEGIN FTYPE:=FALSE; SIZE.WBLENGTH:=8; SIZE.BOUNDARY:=8; END;
(*6229*)  NEW(CHARPTR{,SCALAR,STANDARD});
(*6230*)  WITH CHARPTR^ DO
(*6231*)    BEGIN FTYPE:=FALSE; SIZE.WBLENGTH:=4; SIZE.BOUNDARY:=4; END;
(*6232*)  NEW(BOOLPTR{,SCALAR,DECLARED});
(*6233*)  WITH BOOLPTR^ DO
(*6234*)    BEGIN FTYPE:=FALSE; SIZE.WBLENGTH:=4; SIZE.BOUNDARY:=4; END;
(*6235*)  NEW(NILPTR{,POINTER});
(*6236*)  WITH NILPTR^ DO
(*6237*)   BEGIN ELTYPE := NIL; FTYPE := FALSE;
(*6238*)         SIZE.WBLENGTH:=4; SIZE.BOUNDARY:=4;
(*6239*)   END;
(*6240*)  NEW(PACKDINTPTR{,PACKDTYPE});
(*6241*)  WITH PACKDINTPTR^ DO
(*6242*)    BEGIN SIZE.WBLENGTH:=1; SIZE.BOUNDARY:=1;
(*6243*)          FTYPE:=FALSE; BASETYPE:=INTPTR;
(*6244*)    END;
(*6245*)  NEW(PACKDCHARPTR{,PACKDTYPE});
(*6246*)  WITH PACKDCHARPTR^ DO
(*6247*)    BEGIN SIZE.WBLENGTH:=1; SIZE.BOUNDARY:=1;
(*6248*)          FTYPE:=FALSE; BASETYPE:=CHARPTR;
(*6249*)    END;
(*6250*)  NEW(TEXTPTR{,FILES});
(*6251*)  WITH TEXTPTR^ DO
(*6252*)    BEGIN FILTYPE:=PACKDCHARPTR;
(*6253*)          TEXTFILE  :=  TRUE;  FTYPE  :=  TRUE;
(*6254*)          SIZE.WBLENGTH:=TEXTSIZE;  SIZE.BOUNDARY:=4;
(*6255*)    END;
(*6256*)NEW(SP{,SUBRANGE});
(*6257*)WITH SP^ DO
(*6258*)BEGIN
(*6259*)  RANGETYPE:=INTPTR;
(*6260*)  FTYPE:=FALSE;
(*6261*)  MIN:=1; MAX:=ALFALENG;
(*6262*)  SIZE.WBLENGTH:=4;
(*6263*)  SIZE.BOUNDARY:=4;
(*6264*)END;
(*6265*) 
(*6266*)NEW(ALFAPTR{,ARRAYS});
(*6267*)WITH ALFAPTR^ DO
(*6268*)BEGIN
(*6269*)  AELTYPE:=PACKDCHARPTR;
(*6270*)  INXTYPE:=SP;
(*6271*)  FTYPE:=FALSE; AELLENG:=1;
(*6272*)  SIZE.WBLENGTH:=ALFALENG;
(*6273*)  SIZE.BOUNDARY:=1
(*6274*)END
(*6275*) END (*STDTYPENTRIES*);
(*6276*) 
// $TITLE STDNAMENTRIES,TYPENAME,CONSTNAME
(*6277*) PROCEDURE STDNAMENTRIES;
(*6278*)  VAR CP,CP1:CTP; I:INTEGER;
(*6279*) 
(*6280*)  PROCEDURE TYPENAME(S:ALFA; P:STP);
(*6281*)    BEGIN NEW(CP{,TYPES});
(*6282*)      WITH CP^ DO
(*6283*)        BEGIN NAME:=S; IDTYPE:=P; END;
(*6284*)      ENTERID(CP);
(*6285*)    END;
(*6286*) 
(*6287*)  PROCEDURE CONSTNAME(S:ALFA; P:STP; V:INTEGER);
(*6288*)    BEGIN NEW(CP{,KONST});
(*6289*)      WITH CP^ DO
(*6290*)        BEGIN NAME:=S; IDTYPE:=P; NEXT:=NIL;
(*6291*)              VALUES.CKIND:=INT; VALUES.IVAL:=V;
(*6292*)        END;
(*6293*)      ENTERID(CP);
(*6294*)    END;
(*6295*) 
(*6296*)  BEGIN
(*6297*)    TYPENAME('INTEGER ',INTPTR);  TYPENAME('REAL    ',REALPTR);
(*6298*)    TYPENAME('CHAR    ',CHARPTR); TYPENAME('BOOLEAN ',BOOLPTR);
(*6299*)    TYPENAME('TEXT    ',TEXTPTR);
(*6300*)    TYPENAME('ALFA    ',ALFAPTR);
(*6301*)    CONSTNAME('NIL     ',NILPTR,NILVAL);
(*6302*)    CONSTNAME('MAXINT  ',INTPTR,MXINT);
(*6303*)    CONSTNAME('FALSE   ',BOOLPTR,0); CP1:=CP;
(*6304*)    CONSTNAME('TRUE    ',BOOLPTR,1); CP^.NEXT:=CP1; BOOLPTR^.FCONST:=CP;
(*6305*)    FOR I := 1 TO NRSTDPROC DO
(*6306*)      BEGIN NEW(CP{,PROC,STANDARD});                  (*STANDARD PROCEDURES*)
(*6307*)        WITH CP^ DO
(*6308*)          BEGIN NAME := NA(.I.); IDTYPE := NIL;
(*6309*)            NEXT := NIL; KEY := I;
(*6310*)          END;
(*6311*)        ENTERID(CP)
(*6312*)      END;
(*6313*)    FOR I := 1 TO NRSTDFUNC DO                     (*STANDARD FUNCTIONS*)
(*6314*)      BEGIN NEW(CP{,FUNC,STANDARD});
(*6315*)        WITH CP^ DO
(*6316*)          BEGIN NAME := NA(.NRSTDPROC+I.); IDTYPE := NIL;
(*6317*)            NEXT := NIL; KEY := I;
(*6318*)          END;
(*6319*)        ENTERID(CP)
(*6320*)      END;
(*6321*)          FOR I := 1 TO NRSTARITH DO STDPRCS(.I.) := '        ';                
(*6322*)  END (*STDNAMENTRIES*);
(*6323*) 
// $TITLE ENTERUNDECL,INITSCALARS
(*6324*) PROCEDURE ENTERUNDECL;
(*6325*)   BEGIN
(*6326*)     NEW(UTYPPTR{,TYPES});
(*6327*)     WITH UTYPPTR^ DO
(*6328*)       BEGIN NAME:='        '; IDTYPE:=NIL; END;
(*6329*)       NEW(UCSTPTR{,KONST});
(*6330*)     WITH UCSTPTR^ DO
(*6331*)        BEGIN NAME := '        '; IDTYPE := NIL; NEXT := NIL;
(*6332*)            VALUES.IVAL:=0; VALUES.CKIND := INT;
(*6333*)        END;
(*6334*)     NEW(UVARPTR{,VARS});
(*6335*)     WITH UVARPTR^ DO
(*6336*)        BEGIN NAME := '        '; IDTYPE := NIL; VKIND := DRCT;
(*6337*)           NEXT := NIL; VLEV := 0; VADDR := 0
(*6338*)        END;
(*6339*)     NEW(UFLDPTR{,FIELD});
(*6340*)     WITH UFLDPTR^ DO
(*6341*)        BEGIN NAME := '        '; IDTYPE := NIL; NEXT := NIL;
(*6342*)           FLDADDR := 0
(*6343*)        END;
(*6344*)     NEW(UPRCPTR{,PROC,DECLARED,ACTUAL});
(*6345*)     WITH UPRCPTR^ DO
(*6346*)        BEGIN NAME := 'P@MAIN  '; IDTYPE := NIL;
(*6347*)          NEXT:=NIL; PFLEV:=0; PFCNT:=1; PARAMS:=NIL;
(*6348*)        END;
(*6349*)     NEW(UFCTPTR{,FUNC,DECLARED,ACTUAL});
(*6350*)     WITH UFCTPTR^ DO
(*6351*)        BEGIN NAME := '        '; IDTYPE := NIL; NEXT := NIL;
(*6352*)              PFLEV := 0; PFCNT:=1; PARAMS:=NIL;
(*6353*)        END;
(*6354*)     NEW(UEVENTPTR{,EVENT});
(*6355*)     WITH UEVENTPTR^ DO
(*6356*)        BEGIN NAME:='        '; IDTYPE:=NIL; NEXT:=NIL;
(*6357*)              EVENTJUMP:=NIL; EVENTDEF:=FALSE;
(*6358*)        END;
(*6359*)   END (*ENTERUNDECL*) ;
(*6360*)

PROCEDURE DateTime(VAR D,T:Alfa);
var datime: tdatetime;
begin
    datime := now;
    D :=  FormatDateTime('dd mmm yy ' ,datime);
    T :=  FormatDateTime('hh:nn:ss  ' ,datime) ;

  end;


(*6361*) PROCEDURE INITSCALARS;
(*6362*)   BEGIN
(*6363*)     CH:=' '; SWEOL:=FALSE; CHCNT:=0; PROGCOUNT:=0;
(*6364*)     LC:=LCSTART;
(*6365*)     PCNT:=1;
(*6366*)     DOTFLG := FALSE;
(*6367*)     EXTWARN:=FALSE;
(*6368*)     PRTERR:=TRUE;
(*6369*)     DEBUG:=TRUE; LISTON:=TRUE; PMD:=TRUE; PRINTCODE:=FALSE;
(*6370*)     INPUTPTR:=NIL; OUTPUTPTR:=NIL;
(*6371*)     FWPTR:=NIL; FSTLABP:=NIL; FEXFILP:=NIL; LOCFILP:=NIL;
(*6372*)     FSTPCRP:=NIL;
(*6373*)     ERRINX:=0; ERRORS:=FALSE;
(*6374*)     INITNUMBER:=0; OBPOINTER:=0;
(*6375*)     SWEOL:=TRUE; LEFT:='-';RIGHT :='-';
(*6376*)     MAXLN := FALSE;
(*6377*)     PAGEE:=1; FOR ZLEV:=1 TO 40 DO TTL(.ZLEV.):=' ';
(*6378*)     ZLEV:=-1;
             //DATE(DDATE); TIME(TTIME);
             DateTime(DDATE,TTIME);
             PRINTED:=0;
(*6379*)     PROCLEV:=' '; MAXLINE :=MAXCHCNT; LINEE:=LINESPERPAGE-1;
(*6380*)     ERRORTOT := 0;
(*6381*)     DP:=TRUE;
(*6382*)      OBPOINTER := 1;
(*6383*)      CURRADDRESS:= 0;
(*6384*)      PROCNAMES := FALSE;
(*6385*)      INITFLAG := TRUE;
(*6386*)      EXTRNL := FALSE; PROCREF:='NOPROC@@';
(*6387*)      ESDID := 1;
(*6388*)      TXT.PRELUDE(.1.):=CHR(2);
(*6389*)      ESD.PRELUDE(.1.):=CHR(2);
(*6390*)      RLD.PRELUDE(.1.):=CHR(2);
(*6391*)      ENDC.PRELUDE(.1.):=CHR(2);
(*6392*)      ESDCNT := 0;
(*6393*)      RLDPOS:=1;
(*6394*)EXTPROCS := 0;                                                                  
(*6395*)   END;
(*6396*) 
// $TITLE INITSETS,SYMBOLS
(*6397*) PROCEDURE INITSETS;
(*6398*)     VAR I : 0..MAXMSGSDIV64;
(*6399*) BEGIN
(*6400*)  CONSTBEGSYS := (.ADDOP,INTCONST,REALCONST,CHARCONST,STRINGCONST,IDENT,LBRACK.);
(*6401*)  SIMPTYPEBEGSYS := (.LPARENT.)+CONSTBEGSYS-(.LBRACK,STRINGCONST.);
(*6402*)  TYPEBEGSYS := (.ARROW,PACKEDSY,ARRAYSY,RECORDSY,SETSY,
(*6403*)         FILESY.)+SIMPTYPEBEGSYS;
(*6404*)  TYPEDELS := (.ARRAYSY,RECORDSY,SETSY,FILESY.);
(*6405*)  BLOCKBEGSYS := (.LABELSY,CONSTSY,TYPESY,VARSY,VALUESY,PROCSY,FUNCTSY,
(*6406*)          BEGINSY.);
(*6407*)  SELECTSYS := (.ARROW,PERIOD,LBRACK.);
(*6408*)  FACBEGSYS := (.INTCONST,REALCONST,CHARCONST,STRINGCONST,IDENT,LPARENT,
(*6409*)         LBRACK,NOTSY.);
(*6410*)  STATBEGSYS := (.BEGINSY,GOTOSY,IFSY,WHILESY,REPEATSY,LOOPSY,FORSY,FORALLSY,WITHSY,
(*6411*)          CASESY.);
(*6412*)     BMASK(.EQOP.):=8; BMASK(.NEOP.):=7; BMASK(.GTOP.):=2;
(*6413*)     BMASK(.LTOP.):=4; BMASK(.GEOP.):=11; BMASK(.LEOP.):=13;
(*6414*)     DUALOP(.EQOP.):=EQOP; DUALOP(.NEOP.):=NEOP; DUALOP(.GTOP.):=LTOP;
(*6415*)     DUALOP(.GEOP.):=LEOP; DUALOP(.LTOP.):=GTOP; DUALOP(.LEOP.):=GEOP;
(*6416*)     FOR I := 0 TO MAXMSGSDIV64 DO
(*6417*)        ERRMSGS(. I .) := (. .);
(*6418*) END (*INITSETS*) ;
(*6419*) 
(*6420*)  PROCEDURE SYMBOLS;
(*6421*)    VAR I:INTEGER; C:CHAR;
(*6422*)    BEGIN
(*6423*)      SSY(.'+'.) := ADDOP; SSY(.'-'.) := ADDOP; SSY(.'*'.) := MULOP;
(*6424*)      SSY(.'/'.):=MULOP; SSY(.')'.):=RPARENT;
(*6425*)      SSY(.'='.):=RELOP; SSY(.','.):=COMMA;
(*6426*)      SSY(.'%'.):=RBRACK; SSY(.'@'.):=ARROW; SSY(.';'.):=SEMICOLON;
(*6427*)      FOR I := 1 TO RESWORDS DO ROP(.I.) := NOOP;
(*6428*)      ROP(.5.) := INOP; ROP(.10.) := IDIV; ROP(.11.) := IMOD;
(*6429*)      ROP(.6.) := OROP; ROP(.13.) := ANDOP;
(*6430*)      FOR I:=64 TO 127 DO SOP(.CHR(I).):=NOOP;
(*6431*)     SSY(.CHR(173).):=LBRACK;   // $AD Y OVERBAF
(*6432*)     SSY(.CHR(189).):=RBRACK;   // $BD
(*6433*)     SSY(.'&'.):=MULOP;                                                         
(*6434*)     SSY(.CHR(79).):=ADDOP;     // $4F | HORIZZ. BAR
(*6435*)     SOP(.'&'.):=ANDOP;                                                         
(*6436*)     SOP(.CHR(79).):=OROP;
(*6437*)      SOP(.'+'.) := PLUS;SOP(.'-'.) := MINUS;SOP(.'*'.) := MUL;SOP(.'/'.) := RDIV;
(*6438*)      SOP(.'='.):=EQOP;
(*6439*)      FOR I:=0 TO 255 DO CHTYPE(.CHR(I).):=SPCHAR;
(*6440*)      FOR C:='A' TO 'I' DO CHTYPE(.C.):=LETTER;
(*6441*)      FOR C:='J' TO 'R' DO CHTYPE(.C.):=LETTER;
(*6442*)      FOR C:='S' TO 'Z' DO CHTYPE(.C.):=LETTER;
(*6443*)      CHTYPE(.'$'.):=LETTER;
(*6444*)      FOR C:='0' TO '9' DO CHTYPE(.C.):=DIGIT;
(*6445*)      CHTYPE(.'_'.) := LETTER;                                                  
(*6446*)    END;
(*6447*) 
// $TITLE  ENDING PROCEDURES (FINAL)
(*6448*)  PROCEDURE FINAL;
(*6449*)    VAR I:INTEGER;
(*6450*)   TP,K : INTEGER;                                                              
(*6451*)     L : INTEGER;
(*6452*)        CONV:RECORD CASE BOOLEAN OF
(*6453*)               TRUE:  (I1,I2:INTEGER);
(*6454*)               FALSE: (STR:ALFA)
(*6455*)             END;
(*6456*)      BEGIN
(*6457*)        OBCLEAR;
(*6458*)        PUTRLD(2,1,0,TRUE);
(*6459*)        ENDC.LENGTH := CURRADDRESS;
(*6460*)       { SYSGO^:=CARD(ENDC);  }
               FIXEND;
                 Write(sysgo,card(endc)); //  PR fix
(*6461*)      //  PUT(SYSGO);
                CURRADDRESS:=0; ESDID:=1; ESDCNT:=0;
(*6462*)        IF NOT EXTRNL THEN PROCREF := 'P@MAIN@ ';
(*6463*)        PUTESD(PROCREF,SD,TRUE);
(*6464*)        TP:=EXTPROCS;
(*6465*)   IF TP <> 0 THEN                                                              
(*6466*)   BEGIN                                                                        
(*6467*)     FOR L := 0 TO TP-2 DO                                                      
(*6468*)      PUTESD(EXTARRAY(.L.).ENAME,ER,FALSE);                                     
(*6469*)     PUTESD(EXTARRAY(.TP-1.).ENAME,ER,TRUE);                                    
(*6470*)   END;                                                                         
(*6471*)       L := 1;                                                                  
(*6472*)       //LOOP
               repeat
(*6473*)         IF STDPRCS(.L.) <> '        ' THEN                                     
(*6474*)          PUTESD(STDPRCS(.L.),ER,TRUE) ELSE break;                               
(*6475*)         L := L+1;                                                              
(*6476*)       // END;
               until false;
(*6477*)       IF NOT ERRORS THEN
(*6478*)         IF EXTRNL THEN DATA1((PCNT-1)+Z7FE) ELSE DATA1(PCNT+Z7FE)
(*6479*)       ELSE DATA1(Z7FE);
(*6480*)      IF EXTRNL THEN L:=2 ELSE L:=1;
(*6481*)      FOR I := L TO PCNT DO DATA1(PROCADDRESS(.I.));
(*6482*)      OBCLEAR;
(*6483*)      TP:=EXTPROCS; I:=2;
(*6484*)  IF EXTRNL THEN L:=0 ELSE L:=1;                                                
(*6485*)  IF TP <> 0 THEN                                                               
(*6486*)  BEGIN                                                                         
(*6487*)    FOR K := 0 TO TP-2 DO                                                       
(*6488*)    BEGIN                                                                       
(*6489*)      PUTRLD(I,1,4*(EXTARRAY(.K.).ECNT+L),FALSE);                               
(*6490*)      I := I +1;                                                                
(*6491*)    END;                                                                        
(*6492*)    PUTRLD(I,1,4*(EXTARRAY(.TP-1.).ECNT+L),TRUE);                               
(*6493*)  END;                                                                          
(*6494*)     ENDC.LENGTH := CURRADDRESS;
(*6495*)   //  SYSGO^:=CARD(ENDC);
         Write(SYSGO,CARD(ENDC));
(*6496*)    // PUT(SYSGO);
(*6497*)    END;
(*6498*)



// $TITLE  PASCAL COMPILER - (BODY)
(*6499*)BEGIN
(*6500*)
(*6501*)    INITSCALARS; INITSETS; SYMBOLS;
(*6502*) 
(*6503*) 
(*6504*)    LEVEL := 0; TOP := 0;
(*6505*)    WITH DISPLAY(.0.) DO
(*6506*)       BEGIN FNAME := NIL; OCCUR := BLCK END;
(*6507*)    STDTYPENTRIES; STDNAMENTRIES; ENTERUNDECL;
(*6508*)    TOP := 1; LEVEL := 1;
(*6509*) 
(*6510*) 
(*6511*)    REWRITE(SYSGO);
(*6512*)    INSYMBOL;                 // get the first symbol

               // LABEL,CONST,TYPE,VAR,VALUE,PROC,FUNCT, BEGIN
               // BEGIN,GOTO,IF,WHILE,REPEAT,LOOP,FOR,FORALL,WITH
(*6411*)
(*6513*)    PROGRAMME(BLOCKBEGSYS+STATBEGSYS-(.CASESY.));
(*6514*) 
(*6515*)9999:
             ENDOFLINE; FINAL;
(*6516*)     WRITELN; ENDOFLINE;
(*6517*)     WRITELN(' *AAEC PASCAL COMPILATION CONCLUDED *');
(*6518*)     ENDOFLINE;
(*6519*)     WRITELN; ENDOFLINE;
(*6520*)     IF NOT ERRORS THEN WRITE('0*NO')ELSE WRITE('0*');
(*6521*)     WRITELN(' ERRORS DETECTED IN PASCAL PROGRAM *');
(*6522*)     WRITELN; WRITELN;
(*6523*)     SWEOL:=FALSE;
(*6524*)     IF ERRORTOT  <> 0 THEN
(*6525*)     BEGIN
     //  end
     //  end .


(*6526*)         WRITELN(' *',ERRORTOT:4,' LINE(S) FLAGGED IN PASCAL PROGRAM*');
(*6527*)         WRITELN;
(*6528*)         IF ERRORS THEN
(*6529*)         BEGIN
(*6530*)             WRITELN; WRITELN(' ERROR LOG : ');
(*6531*)             WRITELN(' *********** ');
(*6532*)          END;
(*6533*)          WRITELN; RESET(_PASMSGS);
(*6534*)          LOCATION := -1;
    //      end end .
(*6535*)          FOR ERRORTOT := 0 TO MAXMSGSDIV64 DO
(*6536*)              FOR ZLEV := 0 TO SETMAX DO
(*6537*)                  IF ZLEV IN ERRMSGS(.ERRORTOT.) THEN
(*6538*)                  BEGIN
(*6539*)                      PRINTED := 64*ERRORTOT + ZLEV;
(*6540*)                      ERRORS := LOCATION >= PRINTED;
(*6541*)                      WHILE (  NOT ERRORS) AND (NOT EOF(_PASMSGS)) DO
(*6542*)                      BEGIN
(*6543*)                      IF SWEOL THEN
                              BEGIN
                                  SWEOL:=FALSE; READLN(_PASMSGS);
                              END;
           //    end   end  end end .
(*6544*)                      READ(_PASMSGS,LOCATION);
(*6545*)                      IF LOCATION >= PRINTED THEN
                                 ERRORS := TRUE
                              ELSE
(*6546*)                         READLN(_PASMSGS);
(*6547*)                  END;
//           end end end .

(*6548*)                  IF LOCATION = PRINTED THEN
(*6549*)                  BEGIN
(*6550*)                      WRITE(LOCATION:4);
(*6551*)                      WHILE NOT EOLN(_PASMSGS) DO
(*6552*)                      BEGIN
(*6553*)                          READ(_PASMSGS,CH);
(*6554*)                          WRITE(CH);
(*6555*)                      END;
(*6556*)                      READLN(_PASMSGS); WRITELN;
(*6557*)                  END

    //        end end end .
                          ELSE
(*6558*)                  BEGIN
(*6559*)                      SWEOL :=TRUE;
(*6560*)                      WRITELN(PRINTED:4,': MESSAGE NOT IMPLEMENTED');
(*6561*)                  END
(*6562*)             END
         //          end; end .
(*6563*)     END;

// (*6564*){* $L+*)

(*6565*) END .

