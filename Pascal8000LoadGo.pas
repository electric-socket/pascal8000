(*0001*)(***********************************************************************
  0002  *                                                                      *
  0003  *                                                                      *
  0004  *                                                                      *
  0005  *                                                                      *
  0006  *                                                                      *
  0007  *                   PASCAL 8000 - IBM 360/370 VERSION                  *
  0008  *                   ---------------------------------                  *
  0009  *                                                                      *
  0010  *                        INTERNAL LOADER VERSION.                      *
  0011  *                        -----------------------                       *
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
  0044  *           THE CODE PRODUCED BY THIS VERSION OF THE COMPILER CANNOT   *
  0045  *           BE PASSED THROUGH THE STANDARD IBM LINKAGE EDITOR.         *
  0046  *                                                                      *
  0047  *                                                                      *
  0048  *        DATASETS:             "INPUT" - PROGRAM TO COMPILE            *
  0049  *        --------             "OUTPUT" - PROGRAM LISTING               *
  0050  *                           "$PASMSGS" - ERROR MESSAGE DATASET         *
  0051  *                           "$PASOBJ1" - CODE FILE 1 PRODUCED          *
  0052  *                           "$PASOBJ2" - CODE FILE 2 PRODUCED          *
  0053  *                                                                      *
  0054  ************************************************************************)
$TITLE PASCAL COMPILER
(*0055*)      PROGRAM COMPILER(INPUT,OUTPUT,$PASOBJ1,$PASOBJ2,$PASMSGS);
(*0056*)(*$T-,L+,U-,P-*)                                                                
(*0057*) 
(*0058*) 
(*0059*)LABEL 9999;
(*0060*)CONST
(*0061*)     DISPLIMIT = 20;       (*MAX NUMBER OF NESTED SCOPES OF IDENTIFIERS*)
(*0062*)     MAXLEVEL  = 6;        (*MAX NUMBER OF NESTED PROC/FUNCT*)
(*0063*)     MAXCHCNT = 121; (* MAX NO OF CHARS ON AN INPUT LINE + 1 *)
(*0064*) 
(*0065*) 
(*0066*)     RESWORDS  = 38;       (*NUMBER OF RESERVED WORDS*)
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
(*0081*)     CIXMAX    = 256;     (*MAXIMUM NUMBER OF CASE LABEL*)
(*0082*)     TEXTSIZE = 20; (*   NEW RUN TIME SYSTEM FILE STRUCTURE *)
(*0083*) 
(*0084*)     LINESPERPAGE = 60;
(*0085*)     INDENT = 14;                                                               
(*0086*)     MAXMSGSDIV64 = 7;
(*0087*)     VERSION = 'AAEC (1ST FEB 78) ';                                            
(*0088*)     CODEBLCK= 63;            (* 1024 DIV 16 -1          *)
(*0089*)     NCODESEGS=96;            (* NUMBER OF CODE SEGMENTS *)
(*0090*)     CODEPERSEG = 256;        (* BYTES PER SEGMENT       *)
(*0091*)     PTROUTBLCK = 144;        (* POINTER TO OUTPUT BLOCK *)
(*0092*)     OBJLENGTH = 256;         (* OBJECT FILE RECORD SIZE *)
(*0093*)     OBJLENMIN1 = 255;        (* OBJLENGTH-1 *)
(*0094*) 
(*0095*) 
$TITLE  GLOBAL TYPES
(*0096*)TYPE
(*0097*) 
(*0098*)   ALFA = PACKED ARRAY(.1..ALFALENG.) OF CHAR;
(*0099*)   LEVRANGE = 0..8; ADDRRANGE = INTEGER;
(*0100*) 
(*0101*) 
(*0102*)                              (*BASIC SYMBOLS*)
(*0103*)                              (***************)
(*0104*) 
(*0105*)   SYMBOL = (IDENT,INTCONST,REALCONST,CHARCONST,STRINGCONST,NOTSY,MULOP,
(*0106*)        ADDOP,RELOP,LPARENT,RPARENT,LBRACK,RBRACK,LCBRACK,RCBRACK,COMMA,SEMICOLON,
(*0107*)        PERIOD,ARROW,COLON,BECOMES,LABELSY,CONSTSY,TYPESY,VARSY,VALUESY,
(*0108*)        FUNCTSY,PROCSY,SETSY,PACKEDSY,ARRAYSY,RECORDSY,FILESY,
(*0109*)        BEGINSY,IFSY,CASESY,REPEATSY,WHILESY,LOOPSY,FORSY,FORALLSY,WITHSY,
(*0110*)        GOTOSY,ENDSY,ELSESY,POSTSY,UNTILSY,OFSY,DOSY,TOSY,DOWNTOSY,
(*0111*)        THENSY,PROGRAMSY,EXPONOP,OTHERSY);
(*0112*)   OPERATOR = (MUL,RDIV,ANDOP,IDIV,IMOD,PLUS,MINUS,OROP,LTOP,LEOP,GEOP,
(*0113*)         GTOP,NEOP,EQOP,INOP,NOOP);
(*0114*)   SETOFSYS = SET OF SYMBOL;
(*0115*) 
(*0116*)                              (*CONSTANTS*)
(*0117*)                              (***********)
(*0118*) 
(*0119*)   LOCOFREF = @LOCREC;
(*0120*)   LOCREC=RECORD NXTREF:LOCOFREF;
(*0121*)              LOC: ADDRRANGE
(*0122*)            END;
(*0123*) 
(*0124*)   CSTCLASS = (INT,REEL,PSET,STRG);
(*0125*)   CTAILP = @ CSTTAILREC;
(*0126*)   STRGFRAG=PACKED ARRAY(.1..4.) OF 0..255;
(*0127*)   CSTTAILREC = RECORD NXTCSP: CTAILP; STFR : INTEGER END;
(*0128*) 
(*0129*)   BASICSET=SET OF SETMIN..SETMAX;
(*0130*)   CELLUNIT=1..8;
(*0131*)   VALU=RECORD CASE CKIND:CSTCLASS OF
(*0132*)           INT:  (IVAL: INTEGER);
(*0133*)           REEL: (RVAL: REAL);
(*0134*)           PSET: (PVAL: BASICSET);
(*0135*)           STRG: (VALP: CTAILP)
(*0136*)          END;
(*0137*) 
(*0138*)                              (*DATA STRUCTURES*)
(*0139*)                              (*****************)
(*0140*)   STRUCTFORM = (SCALAR,PACKDTYPE,SUBRANGE,POINTER,POWER,ARRAYS,RECORDS,FILES,
(*0141*)          TAGFIELD,VARIANT);
(*0142*)   DECLKIND = (STANDARD,DECLARED);
(*0143*)   WBSIZE=RECORD WBLENGTH:INTEGER; BOUNDARY:CELLUNIT END;
(*0144*)   STP = @ STRUCTREC; CTP = @ IDENTREC;
(*0145*) 
(*0146*)   STRUCTREC=RECORD
(*0147*)          FTYPE: BOOLEAN; (* TRUE IFF THE STRUCTURE CONTAINS OR IS A FILE *)
(*0148*)          SIZE: WBSIZE;
(*0149*)          CASE FORM: STRUCTFORM OF
(*0150*)           SCALAR:   (CASE SCALKIND: DECLKIND OF
(*0151*)                      DECLARED: (FCONST: CTP));
(*0152*)           PACKDTYPE:(BASETYPE:STP);
(*0153*)           SUBRANGE: (RANGETYPE: STP; MIN,MAX: INTEGER);
(*0154*)           POINTER:  (ELTYPE: STP);
(*0155*)           POWER:    (PCKDSET: BOOLEAN; ELSET: STP);
(*0156*)           ARRAYS:   (AELTYPE,INXTYPE: STP; AELLENG:INTEGER);
(*0157*)           RECORDS:  (FIELDS,FSTFLD: CTP;
(*0158*)                      RECVAR: STP);
(*0159*)           FILES:    (TEXTFILE:BOOLEAN; FILTYPE:STP);
(*0160*)           TAGFIELD: (TGFLDP: CTP; FSTVAR: STP);
(*0161*)           VARIANT:  (FSTVARFLD: CTP; NXTVAR,SUBVAR: STP;
(*0162*)                      VARVAL: INTEGER)
(*0163*)          END;
(*0164*) 
(*0165*) 
(*0166*)                              (*NAMES*)
(*0167*)                              (*******)
(*0168*) 
(*0169*)   IDCLASS = (TYPES,KONST,VARS,FIELD,EVENT,PROC,FUNC);
(*0170*)   SETOFIDS = SET OF IDCLASS;
(*0171*)   IDKIND = (ACTUAL,FORMAL);
(*0172*)   DRCTINDRCT = (DRCT,INDRCT);        (*INDRCT: VARIABLE PARAMETER, WITH STATEMENT*)
(*0173*) 
(*0174*)   IDENTREC=RECORD
(*0175*)          NAME: ALFA; LLINK,RLINK: CTP;
(*0176*)          IDTYPE: STP; NEXT: CTP;
(*0177*)          CASE KLASS: IDCLASS OF
(*0178*)           KONST: (VALUES: VALU);
(*0179*)           VARS:  (VKIND: DRCTINDRCT; VLEV: LEVRANGE;
(*0180*)                   VADDR,PARADDR: ADDRRANGE);
(*0181*)           FIELD: (FLDADDR: ADDRRANGE);
(*0182*)           EVENT: (EVENTJUMP:LOCOFREF; EVENTDEF:BOOLEAN);
(*0183*)           PROC,
(*0184*)           FUNC:  (CASE PFDECKIND: DECLKIND OF
(*0185*)                   STANDARD: (KEY: 1..NRSTDNAMES);
(*0186*)                   DECLARED: (PFLEV: LEVRANGE; PARAMS:CTP;
(*0187*)                              CASE PFKIND: IDKIND OF
(*0188*)                               ACTUAL: (PFCNT:INTEGER; LCSAVE:ADDRRANGE);
(*0189*)                               FORMAL: (PFADDR:ADDRRANGE)))
(*0190*)          END;
(*0191*) 
(*0192*) 
(*0193*)   CEP=@CSTEXPREC;
(*0194*)   CSTEXPREC=RECORD ELEMTYPE:STP; ELEMVALUE:VALU;
(*0195*)                    NEXTELEM:CEP
(*0196*)             END;
(*0197*)   FILEP = @ FILEREC;
(*0198*)   FILEREC = RECORD
(*0199*)               FILENAME: ALFA; ADDR:ADDRRANGE;
(*0200*)               NXTP: FILEP;
(*0201*)               DECLARED: BOOLEAN
(*0202*)             END;
(*0203*) 
(*0204*) 
(*0205*)   DISPRANGE = 0..DISPLIMIT;
(*0206*)   WHERE = (BLCK,REC);
(*0207*) 
(*0208*) 
(*0209*)                              (*LABELS*)
(*0210*)                              (********)
(*0211*)   LBP = @LABREC;
(*0212*)   LABREC=RECORD
(*0213*)              LABVAL: INTEGER; NEXTLAB: LBP;
(*0214*)              LCNT: 0..MAXPROCFUNC;
(*0215*)              CASE DEFINED: BOOLEAN OF
(*0216*)               TRUE:  (LABADDR: ADDRRANGE);
(*0217*)               FALSE: (FSTOCC: LOCOFREF)
(*0218*)            END;
(*0219*)                              (*MISCELLANEOUS*)
(*0220*)                              (***************)
(*0221*)   PCRP = @ PTRCOMPREC;     (*POINTER COMPARISON*)
(*0222*)   PTRCOMPREC = RECORD NEXT : PCRP;  (*TO AVOID INFINITE RECURSION
  0223                                         IN 'COMPTYPES'*)
(*0224*)                PTR1,PTR2 : STP
(*0225*)               END;
(*0226*)  MARKP = @BOOLEAN;        (*MARK AND RELEASE*)
(*0227*)   REGNO=(R10,R11,R12,R13,F0,F2,F4,F6);
(*0228*) 
(*0229*) 
(*0230*)(*--------------------------------------------------------------------*)
(*0231*) 
$TITLE  GLOBAL VARIABLES
(*0232*) 
(*0233*)VAR
(*0234*)                  (*RETURNED BY SOURCE PROGRAM SCANNER
  0235                     INSYMBOL:
  0236                     **********)
(*0237*) 
(*0238*)  SY: SYMBOL;                     (*LAST SYMBOL*)
(*0239*)  OP: OPERATOR;                   (*CLASSIFICATION OF LAST SYMBOL*)
(*0240*)  IVAL: INTEGER;                  (*VALUE OF LAST INTEGER CONSTANT*)
(*0241*)  RVAL: REAL;                     (*VALUE OF LAST REAL CONSTANT*)
(*0242*)  CONSTP: CTAILP;                 (*POINTER TO LAST STRING*)
(*0243*)  LGTH: INTEGER;                  (*LENGTH OF LAST STRING CONSTANT*)
(*0244*)  ID: ALFA;                       (*LAST IDENT (POSSIBLY TRUNCATED)*)
(*0245*)  CH: CHAR;                       (*LAST CHARACTER*)
(*0246*)  SWEOL: BOOLEAN;                   (*END OF LINE CONDITION*)
(*0247*)  DOTDOT : BOOLEAN;
(*0248*)  DOTFLG : BOOLEAN;
(*0249*) 
(*0250*) 
(*0251*)                  (*COUNTERS:*)
(*0252*)                  (***********)
(*0253*) 
(*0254*)  CHCNT: INTEGER;                 (*CHARACTER COUNTER*)
(*0255*)  LC,IC: ADDRRANGE;               (*DATA LOCATION AND INSTR COUNTER*)
(*0256*)  PCNT: INTEGER;                  (*NUMBER OF PROCSY/FUNCTIONS*)
(*0257*)  PROGCOUNT:INTEGER;              (*GLOBAL INSTRUCTION COUNTER*)
(*0258*) 
(*0259*) 
(*0260*)                  (*SWITCHES:*)
(*0261*)                  (***********)
(*0262*) 
(*0263*)  PRTERR: BOOLEAN;                (*TO ALLOW FORWARD REFERENCES
  0264                                    BY SUPPRESSING ERROR MESSAGE*)
(*0265*)  DEBUG,LISTON,PMD,PRINTCODE,EXTWARN : BOOLEAN; (* $ SWITCHES *)
(*0266*) 
(*0267*) 
(*0268*)                  (*POINTERS:*)
(*0269*)                  (***********)
(*0270*)  INTPTR,REALPTR,CHARPTR,ALFAPTR,
(*0271*)  BOOLPTR,NILPTR,TEXTPTR: STP;    (*POINTERS TO ENTRIES OF STD IDS*)
(*0272*)  PACKDINTPTR,PACKDCHARPTR:STP;
(*0273*)  UTYPPTR,UCSTPTR,UVARPTR,
(*0274*)  UFLDPTR,UPRCPTR,UFCTPTR,        (*POINTERS TO ENTRIES FOR UNDECL IDS*)
(*0275*)  UEVENTPTR,
(*0276*)  INPUTPTR,OUTPUTPTR,             (*ENTRIES FOR INPUT AND OUTPUT*)
(*0277*)  FWPTR: CTP;                     (*HEAD OF CHAIN OF FORW  TYPE IDS*)
(*0278*)  FSTLABP : LBP;                  (*HEAD OF LABEL CHAIN*)
(*0279*)  FEXFILP,LOCFILP: FILEP;         (*HEAD OF LIST OF EXTERNAL/LOCAL FILES*)
(*0280*)  FSTPCRP : PCRP;                 (*HEAD OF LIST OF POINTER COMPARISON*)
(*0281*) 
(*0282*) 
(*0283*)                  (*BOOKKEEPING OF DECLARATION LEVELS:*)
(*0284*)                  (************************************)
(*0285*) 
(*0286*)  LEVEL: LEVRANGE;                (*CURRENT STATIC LEVEL*)
(*0287*)  DISX,                           (*LEVEL OF LAST ID SRCHD BY SEARCHID*)
(*0288*)  TOP: DISPRANGE;                 (*TOP OF DISPLAY*)
(*0289*) 
(*0290*)  DISPLAY:                        (*WHERE:   MEANS:*)
(*0291*)   ARRAY (.DISPRANGE.) OF
(*0292*)           RECORD               (*=BLCK:   ID IS VARIABLE ID*)
(*0293*)     FNAME: CTP;               (*=REC:   ID IS FIELD ID IN RECORD*)
(*0294*)     CASE OCCUR: WHERE OF
(*0295*)       REC: (DADRS:ADDRRANGE;
(*0296*)             CASE DISPKIND:DRCTINDRCT OF
(*0297*)                   DRCT: (DLEVEL:LEVRANGE);
(*0298*)                  INDRCT: (DBASEL:LEVRANGE; DBASEA:ADDRRANGE))
(*0299*)     END;
(*0300*) 
(*0301*)         (* LISTING CONTROLS *)
(*0302*)         (********************)
(*0303*) 
(*0304*)LEFT,RIGHT,PROCLEV : CHAR;             (*NESTING LEVEL INDICATORS *)
(*0305*)LOCATION           : INTEGER;          (*OFFSET AT EOL            *)
(*0306*)DDATE,TTIME        : ALFA;             (*DATE AND TIME FOR NEWPAGE*)
(*0307*)PAGEE              : INTEGER;          (*PAGE COUNTER             *)
(*0308*)ZLEV               : INTEGER;          (*NESTING LEVEL COUNTER    *)
(*0309*)TTL                : PACKED ARRAY(.1..40.)
(*0310*)                        OF CHAR;       (*TITLE BUFFER             *)
(*0311*)PRINTED            : INTEGER;          (*INTEGER                  *)
(*0312*)MAXLINE            : 0..MAXCHCNT;      (* MAX NO OF INPUT CHARACTERS *)
(*0313*)LINEE              : INTEGER;           (* NO OF LINES PRINTED      *)
(*0314*)DP                 : BOOLEAN;
(*0315*)MAXLN              : BOOLEAN;
(*0316*) 
(*0317*)                  (*ERROR MESSAGES:*)
(*0318*)                  (*****************)
(*0319*) 
(*0320*)  ERRINX: 0..10;                  (*NR OF ERRORS IN CURR SOURCE LINE*)
(*0321*)  ERRORS: BOOLEAN;                (*TRUE IFF THE PROGRAM CONTAINS AN ERROR*)
(*0322*)  ERRLIST:
(*0323*)   ARRAY (.1..10.) OF
(*0324*)     RECORD POS: 1..MAXCHCNT;
(*0325*)            NMR: 1..400
(*0326*)        END;
(*0327*) 
(*0328*)ERRORTOT : INTEGER;     (* TOTAL NUMBER OF LINES IN ERROR *)
(*0329*) $PASMSGS : TEXT;        (* PASCAL ERROR MESSAGE FILE      *)
(*0330*)ERRMSGS  : ARRAY (. 0 .. MAXMSGSDIV64 .)
(*0331*)                   OF SET OF SETMIN..SETMAX;
(*0332*) 
(*0333*) 
(*0334*) 
(*0335*)                  (*STRUCTURED CONSTANTS:*)
(*0336*)                  (***********************)
(*0337*) 
(*0338*)  CONSTBEGSYS,SIMPTYPEBEGSYS,TYPEBEGSYS,BLOCKBEGSYS,SELECTSYS,FACBEGSYS,
(*0339*)  STATBEGSYS,TYPEDELS: SETOFSYS;
(*0340*)  LRW: ARRAY (.0..ALFALENG.) OF 0..RESWORDS;
(*0341*)  RW:  ARRAY (.1..RESWORDS.) OF ALFA;
(*0342*)  RSY: ARRAY (.1..RESWORDS.) OF SYMBOL;
(*0343*)  ROP: ARRAY (.1..RESWORDS.) OF OPERATOR;
(*0344*)  SSY : ARRAY (.' '..'A'.) OF SYMBOL;                                           
(*0345*)  SOP: ARRAY (.' '..'"'.) OF OPERATOR;
(*0346*)  REALREG: ARRAY (. REGNO .) OF INTEGER;
(*0347*)  NA: ARRAY (.1..NRSTDNAMES.) OF ALFA;
(*0348*)  BMASK: ARRAY(.LTOP..EQOP.) OF INTEGER;
(*0349*)  DUALOP: ARRAY(.LTOP..EQOP.) OF LTOP..EQOP;
(*0350*)  MNEMONIC: PACKED ARRAY(.0..255,1..4.) OF CHAR;
(*0351*)  CHTYPE: PACKED ARRAY(.CHAR.) OF (SPCHAR,LETTER,DIGIT);
(*0352*)                    (*OUTPUT BUFFER:*)
(*0353*)                    (****************)
(*0354*)   LINE  : PACKED ARRAY(.1..MAXCHCNT.) OF CHAR;
(*0355*) 
(*0356*)   PROCADDRESS: ARRAY(.1..MAXPROCFUNC.) OF INTEGER;
(*0357*)   $PASOBJ1 : FILE OF  ARRAY (. 0..OBJLENMIN1 .) OF INTEGER;
(*0358*)   $PASOBJ2 : FILE OF ALFA;
(*0359*)   INITNUMBER,OBPOINTER: INTEGER;
(*0360*) 
(*0361*) 
$TITLE  GLOBAL VALUES
(*0362*)VALUE
(*0363*)  MNEMONIC:=
(*0364*)    (#'    ', '    ', '    ', 'TRSK', 'SPM ', 'BALR', 'BCTR', 'BCR ',
(*0365*)      'SSK ', 'ISK ', 'SVC ', 'SKC ', '    ', 'BASR', 'SCFR', 'ICFR',
(*0366*)      'LPR ', 'LNR ', 'LTR ', 'LCR ', 'NR  ', 'CLR ', 'OR  ', 'XR  ',
(*0367*)      'LR  ', 'CR  ', 'AR  ', 'SR  ', 'MR  ', 'DR  ', 'ALR ', 'SLR ',
(*0368*)      'LPDR', 'LNDR', 'LTDR', 'LCDR', 'HDR ', 'LRDR', 'MXR ', 'MXDR',
(*0369*)      'LDR ', 'CDR ', 'ADR ', 'SDR ', 'MDR ', 'DDR ', 'AWR ', 'SWR ',
(*0370*)      'LPER', 'LNER', 'LTER', 'LCER', 'HER ', 'LRER', 'AXR ', 'SXR ',
(*0371*)      'LER ', 'CER ', 'AER ', 'SER ', 'MER ', 'DER ', 'AUR ', 'SUR ',
(*0372*)      'STH ', 'LA  ', 'STC ', 'IC  ', 'EX  ', 'BAL ', 'BCT ', 'BC  ',
(*0373*)      'LH  ', 'CH  ', 'AH  ', 'SH  ', 'MH  ', 'BAS ', 'CVD ', 'CVB ',
(*0374*)      'ST  ', 'LAE ', 'LS  ', 'ICE ', 'N   ', 'CL  ', 'O   ', 'X   ',
(*0375*)      'L   ', 'C   ', 'A   ', 'S   ', 'M   ', 'D   ', 'AL  ', 'SL  ',
(*0376*)      'STD ', '    ', '    ', '    ', '    ', '    ', '    ', 'MXD ',
(*0377*)      'LD  ', 'CD  ', 'AD  ', 'SD  ', 'MD  ', 'DD  ', 'AW  ', 'SW  ',
(*0378*)      'STE ', '    ', '    ', '    ', '    ', '    ', '    ', '    ',
(*0379*)      'LE  ', 'CE  ', 'AE  ', 'SE  ', 'ME  ', 'DE  ', 'AU  ', 'SU  ',
(*0380*)      'IDL ', 'FGP ', 'PC  ', 'DIG ', 'WRD ', 'RDD ', 'BXH ', 'BXLE',
(*0381*)      'SRL ', 'SLL ', 'SRA ', 'SLA ', 'SRDL', 'SLDL', 'SRDA', 'SLDA',
(*0382*)      'STM ', 'TM  ', 'MVI ', 'TS  ', 'NI  ', 'CLI ', 'OI  ', 'XI  ',
(*0383*)      'LM  ', '    ', '    ', '    ', 'SDV ', 'TDV ', 'HDV ', 'CKC ',
(*0384*)      'STMA', 'SKB ', 'PCAS', 'GSK ', '    ', '    ', '    ', '    ',
(*0385*)      'LMA ', 'RTN ', 'TRC ', '    ', '    ', '    ', '    ', '    ',
(*0386*)      'STMC', 'LRA ', '    ', '    ', '    ', '    ', '    ', '    ',
(*0387*)      'LMC ', 'FSK ', '    ', '    ', '    ', '    ', '    ', '    ',
(*0388*)      '    ', '    ', '    ', '    ', '    ', '    ', '    ', '    ',
(*0389*)      '    ', '    ', '    ', '    ', '    ', '    ', '    ', '    ',
(*0390*)      '    ', 'MVN ', 'MVC ', 'MVZ ', 'NC  ', 'CLC ', 'OC  ', 'XC  ',
(*0391*)      '    ', '    ', '    ', '    ', 'TR  ', 'TRT ', 'ED  ', 'EDMK',
(*0392*)      '    ', '    ', '    ', '    ', '    ', '    ', '    ', '    ',
(*0393*)      '    ', '    ', '    ', '    ', '    ', '    ', '    ', '    ',
(*0394*)      '    ', 'MVO ', 'PACK', 'UNPK', '    ', '    ', '    ', '    ',
(*0395*)      'ZAP ', 'CP  ', 'AP  ', 'SP  ', 'MP  ', 'DP  ', '    ', '    '#);
(*0396*)  LRW:=(# 0, 0, 6, 14, 22, 29, 34, 35, 38 #);
(*0397*)  REALREG:=(# 10, 11, 12, 13, 0, 2, 4, 6 #);
(*0398*)  RW:=(#'IF      ', 'DO      ', 'OF      ', 'TO      ', 'IN      ', 'OR      ',
(*0399*)        'END     ', 'FOR     ', 'VAR     ', 'DIV     ', 'MOD     ', 'SET     ',
(*0400*)        'AND     ', 'NOT     ', 'THEN    ', 'ELSE    ', 'WITH    ', 'GOTO    ',
(*0401*)        'CASE    ', 'TYPE    ', 'FILE    ', 'LOOP    ', 'BEGIN   ', 'UNTIL   ',
(*0402*)        'WHILE   ', 'ARRAY   ', 'CONST   ', 'LABEL   ', 'VALUE   ', 'REPEAT  ',
(*0403*)        'RECORD  ', 'DOWNTO  ', 'PACKED  ', 'FORALL  ', 'PROGRAM ', 'FUNCTION',
(*0404*)        'POSTLUDE', 'PROCEDUR' #);
(*0405*)  RSY:=(#IFSY,     DOSY,     OFSY,     TOSY,     RELOP,    ADDOP,
(*0406*)         ENDSY,    FORSY,    VARSY,    MULOP,    MULOP,    SETSY,
(*0407*)         MULOP,    NOTSY,    THENSY,   ELSESY,   WITHSY,   GOTOSY,
(*0408*)         CASESY,   TYPESY,   FILESY,   LOOPSY,   BEGINSY,  UNTILSY,
(*0409*)         WHILESY,  ARRAYSY,  CONSTSY,  LABELSY,  VALUESY,  REPEATSY,
(*0410*)         RECORDSY, DOWNTOSY, PACKEDSY, FORALLSY, PROGRAMSY,FUNCTSY,
(*0411*)         POSTSY,   PROCSY #);
(*0412*)  NA:=(#'GET     ', 'PUT     ', 'RESET   ', 'REWRITE ',
(*0413*)        'PAGE    ', 'READ    ', 'READLN  ', 'WRITE   ',
(*0414*)        'WRITELN ', 'TIME    ', 'DATE    ', 'NEW     ',
(*0415*)        'MARK    ', 'RELEASE ', 'PACK    ', 'UNPACK  ',
(*0416*)        'MESSAGE ', 'HALT    ', 'EOF     ', 'EOLN    ',
(*0417*)        'ODD     ', 'ROUND   ', 'TRUNC   ', 'ABS     ',
(*0418*)        'SQR     ', 'ORD     ', 'CHR     ', 'PRED    ',
(*0419*)        'SUCC    ', 'SIN     ', 'COS     ', 'EXP     ',
(*0420*)        'SQRT    ', 'LN      ', 'ARCTAN  ', 'CLOCK   ',
(*0421*)        'CARD    '#);
(*0422*) 
(*0423*) 
(*0424*)(*--------------------------------------------------------------------*)
(*0425*) 
(*0426*) 
$TITLE  LEXICAL ANALYSER
(*0427*)PROCEDURE NEWPAGE;
(*0428*) VAR I,J : INTEGER;
(*0429*)BEGIN
(*0430*)  PAGE(OUTPUT);
(*0431*) WRITELN('   PASCAL 8000/1.2',                                                  
(*0432*)    VERSION :22,' ':3,TTL:40,' ':11,DDATE,' AT ',                               
(*0433*)          TTIME,'     PAGE ',PAGEE:4);
(*0434*)  WRITELN;
(*0435*)  PAGEE := PAGEE+1;
(*0436*)  LINEE := 2;
(*0437*)END; (* NEWPAGE *)
(*0438*) 
(*0439*) 
(*0440*)PROCEDURE  ENDOFLINE; FORWARD;
(*0441*) 
(*0442*) 
(*0443*) 
(*0444*)PROCEDURE RIGHTCHECK;
(*0445*)BEGIN
(*0446*)  RIGHT := CHR(ORD('0') + ZLEV MOD 10);
(*0447*)  ZLEV := ZLEV - 1
(*0448*) END;
(*0449*)PROCEDURE LEFTCHECK;
(*0450*)BEGIN
(*0451*)  ZLEV:=ZLEV+1;
(*0452*)  IF LEFT =  '-' THEN LEFT:=CHR(ORD('0')+ZLEV MOD 10)
(*0453*)END;
(*0454*) 
(*0455*) 
(*0456*)PROCEDURE WRITEHEX(X:INTEGER);
(*0457*) VAR I,N,C : INTEGER;
(*0458*)  L:INTEGER;                                                                    
(*0459*)BEGIN                                                                           
(*0460*)   IF X < 65636 THEN BEGIN L:=4;N:=4096 END ELSE                                
(*0461*)    IF X < 1050176 THEN BEGIN L := 5; N:=65636 END ELSE                         
(*0462*)     BEGIN L := 6; N := 1050176 END;                                            
(*0463*)   WRITE(' ':6-L);                                                              
(*0464*)   FOR I :=  1 TO L DO                                                          
(*0465*)    BEGIN C:=X DIV N;
(*0466*)      IF C>= 10 THEN WRITE(CHR(C-10+ORD('A')))
(*0467*)                ELSE WRITE(CHR(C+ORD('0')));
(*0468*)      X:=X MOD N; N:=N DIV 16
(*0469*)    END
(*0470*)END; (*WRITEHEX *)
(*0471*) 
(*0472*) 
(*0473*)PROCEDURE OPTCARD;
(*0474*) VAR I:INTEGER;
(*0475*) C : CHAR;
(*0476*)BEGIN (*OPTCARD*)
(*0477*)  READ(CH);
(*0478*)  IF NOT EOLN(INPUT) THEN
(*0479*)  BEGIN
(*0480*)   READ(CH);
(*0481*)   IF CH = 'E' THEN LINEE := LINESPERPAGE (*EJECT *)
(*0482*)  ELSE
(*0483*)   IF CH = 'S' THEN
(*0484*)   BEGIN IF LISTON THEN BEGIN C:=' ';WHILE NOT EOLN(INPUT) AND (C=' ') DO
(*0485*)     BEGIN READ(CH); IF (CH>='0') AND (CH<='9') THEN
(*0486*)       FOR C:='1' TO CH DO BEGIN ENDOFLINE;WRITELN END
(*0487*)     END
(*0488*)     END
(*0489*)    END
(*0490*)  ELSE
(*0491*)  IF CH = 'T' THEN
(*0492*)  BEGIN
(*0493*)    REPEAT READ(CH) UNTIL (CH = ' ') OR EOLN(INPUT);
(*0494*)    WHILE (NOT EOLN(INPUT)) AND (CH=' ') DO READ(CH);
(*0495*)    FOR I:=1 TO 40 DO BEGIN TTL(.I.):=CH;
(*0496*)    IF EOLN(INPUT) THEN CH:=' ' ELSE READ(CH);
(*0497*)     END; LINEE := LINESPERPAGE;
(*0498*)  END ELSE IF CH ='U' THEN LINEE:=LINESPERPAGE+1
(*0499*)  END; READLN
(*0500*)END; (*OPTCARD *)
(*0501*) 
(*0502*) 
(*0503*)PROCEDURE WRITERRORS;
(*0504*)   VAR I : INTEGER;
(*0505*)       LASTPOS,FREEPOS,CURRPOS,CURRNMR,F,K: INTEGER;
(*0506*)         FLAG:BOOLEAN;
(*0507*)BEGIN
(*0508*)            FLAG:=FALSE;
(*0509*)            ENDOFLINE;
(*0510*)            FOR K:=1 TO ERRINX DO
(*0511*)             IF ERRLIST(. K .).NMR <> 291 THEN FLAG:=TRUE;
(*0512*)            IF FLAG THEN WRITE(' ***ERROR***':INDENT) ELSE
(*0513*)             WRITE(' **WARNING**':INDENT);
(*0514*)             LASTPOS := 0; FREEPOS := 1;
(*0515*)             FOR K := 1 TO ERRINX DO
(*0516*)               BEGIN
(*0517*)                 WITH ERRLIST(.K.) DO
(*0518*)                   BEGIN CURRPOS := POS; CURRNMR := NMR END;
(*0519*)                 IF CURRPOS = LASTPOS THEN WRITE(',')
(*0520*)                 ELSE
(*0521*)                   BEGIN
(*0522*)                      IF FREEPOS > CURRPOS THEN
(*0523*)                      BEGIN
(*0524*)                        WRITELN;
(*0525*)                        ENDOFLINE; WRITE(' ':INDENT);
(*0526*)                        FREEPOS:=1
(*0527*)                     END;
(*0528*)                     WHILE FREEPOS < CURRPOS DO
(*0529*)                       BEGIN WRITE(' '); FREEPOS := FREEPOS + 1 END;
(*0530*)                     WRITE('@'); LASTPOS:=CURRPOS;
(*0531*)                   END;
(*0532*)                 IF CURRNMR < 10 THEN F := 1
(*0533*)                   ELSE IF CURRNMR < 100 THEN F := 2
(*0534*)                     ELSE F := 3;
(*0535*)                 WRITE(CURRNMR:F);
(*0536*)                 FREEPOS := FREEPOS + F + 1
(*0537*)               END;
(*0538*)             WRITELN; ERRINX := 0
(*0539*)   END;
(*0540*) 
(*0541*) PROCEDURE ERROR(FERRNR: INTEGER);
(*0542*)   VAR
(*0543*)      ERRCNT : INTEGER;
(*0544*)    BEGIN
(*0545*)      IF FERRNR <> 291 THEN
(*0546*)             ERRORS:=TRUE;
(*0547*)     IF FERRNR = 400 THEN
(*0548*)     BEGIN
(*0549*)       ENDOFLINE; WRITELN(' ***** COMPILER ERROR *****'); HALT
(*0550*)     END;
(*0551*)     IF ERRINX = 0 THEN ERRORTOT := ERRORTOT + 1;
(*0552*)     IF ERRINX >= 9 THEN
(*0553*)       BEGIN ERRLIST(.10.).NMR := 255; ERRINX := 10 END
(*0554*)     ELSE
(*0555*)       BEGIN ERRINX := ERRINX + 1;
(*0556*)         ERRLIST(.ERRINX.).NMR := FERRNR
(*0557*)       END;
(*0558*)      ERRCNT := ERRLIST(. ERRINX .).NMR;
(*0559*)      ERRMSGS(. ERRCNT DIV  64 .) :=
(*0560*)            ERRMSGS(. ERRCNT DIV 64 .) + (.ERRCNT MOD 64 .);
(*0561*)     ERRLIST(.ERRINX.).POS := CHCNT
(*0562*)   END;
(*0563*) 
(*0564*)    FUNCTION BYTEPACK(X:STRGFRAG):INTEGER;
(*0565*)      BEGIN BYTEPACK:=256*(256*(256*X(.1.)+X(.2.))+X(.3.))+X(.4.);
(*0566*)      END;
(*0567*) 
(*0568*)    PROCEDURE BYTEUNPACK(VAR X:STRGFRAG; V:INTEGER);
(*0569*)      VAR W: RECORD CASE FLAG:BOOLEAN OF
(*0570*)               TRUE:  (STR: STRGFRAG);
(*0571*)               FALSE: (INT: INTEGER)
(*0572*)             END;
(*0573*)      BEGIN W.INT:=V; X:=W.STR; END;
(*0574*) 
(*0575*)    PROCEDURE SETVALUE(X:BASICSET; VAR I1,I2:INTEGER);
(*0576*)      VAR W: RECORD DUMMY:INTEGER;
(*0577*)               CASE FLAG:BOOLEAN OF
(*0578*)                 FALSE: (S: BASICSET);
(*0579*)                 TRUE:  (A1,A2: INTEGER)
(*0580*)             END;
(*0581*)      BEGIN W.S:=X; I1:=W.A1; I2:=W.A2; END;
(*0582*) 
(*0583*)    PROCEDURE HALFWORD(X:INTEGER; VAR X1,X2:INTEGER);
(*0584*)      BEGIN
(*0585*)        IF X>=0 THEN
(*0586*)          BEGIN X1:=X DIV 65536;
(*0587*)                X2:=X MOD 65536;
(*0588*)          END
(*0589*)        ELSE IF X MOD 65536=0 THEN
(*0590*)          BEGIN X1:=X DIV 65536+65536;
(*0591*)                X2:=0;
(*0592*)          END
(*0593*)        ELSE
(*0594*)          BEGIN X1:=X DIV 65536+65535;
(*0595*)                X2:=X MOD 65536+65536;
(*0596*)          END;
(*0597*)      END;
(*0598*) 
(*0599*)PROCEDURE ENDOFLINE;
(*0600*) VAR I : INTEGER;
(*0601*)BEGIN
(*0602*)  IF CHCNT > PRINTED THEN
(*0603*)  BEGIN
(*0604*)    IF LISTON OR (ERRINX>0) THEN
(*0605*)    BEGIN
(*0606*)      IF LINEE = LINESPERPAGE THEN NEWPAGE;
(*0607*)      LINEE := LINEE + 1;
(*0608*)      WRITE(' ');
(*0609*)      WRITEHEX(LOCATION);
(*0610*)      WRITE(' ',LEFT,RIGHT,' ',PROCLEV,' ':PRINTED+2);
(*0611*)      FOR I:=PRINTED+1 TO CHCNT DO
(*0612*)         IF I <= MAXCHCNT THEN WRITE(LINE(.I.));
(*0613*)      WRITELN
(*0614*)   END;
(*0615*)   LEFT:='-'; RIGHT:='-';PROCLEV:=' ';PRINTED:=CHCNT;
(*0616*)   IF ERRINX>0 THEN WRITERRORS
(*0617*)  END ELSE
(*0618*)  BEGIN
(*0619*)    IF LINEE = LINESPERPAGE THEN NEWPAGE;
(*0620*)    LINEE:=LINEE+1;
(*0621*)  END;
(*0622*)  IF DP THEN LOCATION:= LC ELSE LOCATION := IC;
(*0623*)END;(*ENDOFLINE*)
(*0624*) PROCEDURE INSYMBOL;
(*0625*)  LABEL 1,2;
(*0626*)  (*READ NEXT BASIC SYMBOL OF SOURCE PROGRAM AND RETURN ITS DESCRIPTION
  0627    IN THE GLOBAL VARIABLES SY, OP, ID, IVAL, RVAL, SVAL AND LGTH*)
(*0628*)  CONST ONE=1E0; TEN=10E0;
(*0629*)  VAR I,K,SCALE,EXP: INTEGER;
(*0630*)      R,FAC: REAL; SIGN: BOOLEAN; SF:STRGFRAG;
(*0631*)      NXTP,TAILP:CTAILP;
(*0632*) 
(*0633*)PROCEDURE NEXTCH;
(*0634*)BEGIN
(*0635*)  REPEAT
(*0636*)  IF SWEOL THEN
(*0637*)  BEGIN
(*0638*)    ENDOFLINE;
(*0639*)    REPEAT
(*0640*)     IF EOF(INPUT) THEN
(*0641*)     BEGIN
(*0642*)       ENDOFLINE; WRITELN;
(*0643*)       ENDOFLINE; WRITELN(' *** WARNING - PREMATURE PROGRAM EOF ***');
(*0644*)         ERRORS:=TRUE;
(*0645*)         GOTO 9999
(*0646*)     END ELSE
(*0647*)     BEGIN
(*0648*)       IF INPUT@ = '$' THEN OPTCARD ELSE
(*0649*)       BEGIN CHCNT:=0; PRINTED:=0; SWEOL:=FALSE; END;
(*0650*)     END
(*0651*)   UNTIL NOT SWEOL
(*0652*)  END;
(*0653*)  SWEOL:=EOLN(INPUT); READ(CH); IF CHCNT=MAXCHCNT THEN ERROR(180);
(*0654*)  CHCNT:=CHCNT+1; IF CHCNT <= MAXCHCNT THEN LINE(.CHCNT.):=CH;
(*0655*)  UNTIL SWEOL OR (CHCNT<= MAXLINE);
(*0656*)END; (*NEXTCH *)
(*0657*) 
(*0658*)    PROCEDURE OPTIONS;
(*0659*)      VAR CH1:CHAR;
(*0660*) 
(*0661*)      PROCEDURE SETOPTION(VAR F:BOOLEAN; C:CHAR);
(*0662*)        BEGIN IF CH1=C THEN
(*0663*)                IF (CH='+') OR (CH='-') THEN F:=(CH='+');
(*0664*)        END;
(*0665*) 
(*0666*)      BEGIN
(*0667*)        REPEAT NEXTCH; CH1:=CH; NEXTCH;
(*0668*)          SETOPTION(PRINTCODE,'C'); SETOPTION(LISTON,'L');
(*0669*)          SETOPTION(PMD,'P'); SETOPTION(DEBUG,'T');
(*0670*)          SETOPTION(MAXLN,'U');
(*0671*)          SETOPTION(EXTWARN,'S');
(*0672*)            IF MAXLN THEN MAXLINE:=72 ELSE MAXLINE:=MAXCHCNT;
(*0673*)          NEXTCH;
(*0674*)        UNTIL CH<>',';
(*0675*)      END;
(*0676*) 
(*0677*) BEGIN (*INSYMBOL*)
(*0678*) 1:
(*0679*)  WHILE CH=' ' DO NEXTCH;
(*0680*)  IF CHTYPE(.CH.)=LETTER THEN
(*0681*)    BEGIN K:=0; ID:='        ';
(*0682*)      REPEAT
(*0683*)        IF K < ALFALENG THEN
(*0684*)          BEGIN K:=K+1; ID(.K.):=CH; END;
(*0685*)        NEXTCH;
(*0686*)      UNTIL CHTYPE(.CH.)=SPCHAR;
(*0687*)      FOR I := LRW(.K-1.) + 1 TO LRW(.K.) DO
(*0688*)        IF RW(.I.) = ID THEN
(*0689*)          BEGIN SY := RSY(.I.); OP := ROP(.I.); GOTO 2 END;
(*0690*)      SY := IDENT; OP := NOOP;
(*0691*) 2: END
(*0692*)  ELSE IF (CH>='0') AND (CH<='9') THEN
(*0693*)    BEGIN SY := INTCONST; OP := NOOP;
(*0694*)      IVAL:=0;
(*0695*)      REPEAT
(*0696*)        IF IVAL<MAX10
(*0697*)          THEN IVAL:=IVAL*10+(ORD(CH)-ORD('0'))
(*0698*)          ELSE IF (IVAL>MAX10) OR (CH>='8')
(*0699*)                 THEN BEGIN ERROR(203); IVAL:=0; END
(*0700*)                 ELSE IVAL:=IVAL*10+(ORD(CH)-ORD('0'));
(*0701*)        NEXTCH;
(*0702*)      UNTIL (CH<'0') OR (CH>'9');
(*0703*)       SCALE := 0;
(*0704*)       IF CH = '.' THEN
(*0705*)        BEGIN NEXTCH;
(*0706*)         IF CH = '.' THEN BEGIN DOTFLG:=TRUE; CH:=':' END
(*0707*)         ELSE IF CH=')' THEN CH:='%'
(*0708*)         ELSE
(*0709*)          BEGIN RVAL := IVAL; SY := REALCONST;
(*0710*)           IF (CH<'0') OR (CH>'9') THEN ERROR(201)
(*0711*)           ELSE
(*0712*)            REPEAT RVAL := TEN*RVAL + (ORD(CH)-ORD('0'));
(*0713*)             SCALE := SCALE - 1; NEXTCH
(*0714*)            UNTIL (CH<'0') OR (CH>'9')
(*0715*)          END
(*0716*)        END;
(*0717*)       IF CH = 'E' THEN
(*0718*)         BEGIN
(*0719*)           IF SCALE = 0 THEN
(*0720*)             BEGIN RVAL := IVAL; SY := REALCONST END;
(*0721*)           SIGN := FALSE; NEXTCH;
(*0722*)           IF CH = '+' THEN NEXTCH
(*0723*)           ELSE
(*0724*)             IF CH = '-' THEN
(*0725*)               BEGIN SIGN := TRUE; NEXTCH END;
(*0726*)           EXP := 0;
(*0727*)           IF (CH<'0') OR (CH>'9') THEN ERROR(201)
(*0728*)           ELSE
(*0729*)             REPEAT EXP := 10*EXP + (ORD(CH)-ORD('0'));
(*0730*)               NEXTCH
(*0731*)             UNTIL (CH<'0') OR (CH>'9');
(*0732*)           IF SIGN THEN SCALE := SCALE - EXP
(*0733*)                   ELSE SCALE := SCALE + EXP
(*0734*)         END;
(*0735*)       IF SCALE<>0 THEN
(*0736*)         BEGIN R:=ONE; SIGN:=FALSE;
(*0737*)           IF SCALE<0 THEN BEGIN SIGN:=TRUE; SCALE:=-SCALE; END;
(*0738*)           FAC:=TEN;
(*0739*)           REPEAT IF ODD(SCALE) THEN R:=R*FAC;
(*0740*)                  FAC:=SQR(FAC); SCALE:=SCALE DIV 2;
(*0741*)           UNTIL SCALE=0;
(*0742*)           IF SIGN THEN RVAL:=RVAL/R ELSE RVAL:=RVAL*R;
(*0743*)         END;
(*0744*)    END
(*0745*)ELSE IF (ORD(CH)<=73) OR (ORD(CH)>=190)                                         
(*0746*)    THEN BEGIN OP:=NOOP; SY:=OTHERSY; NEXTCH; END
(*0747*)    ELSE CASE ORD(CH) OF
(*0748*)(*'*) 125:
(*0749*)        BEGIN OP:=NOOP; LGTH:=0; I:=0;
(*0750*)          CONSTP:=NIL; NEXTCH;
(*0751*)          LOOP
(*0752*)            IF SWEOL THEN BEGIN ERROR(202); EXIT; END;
(*0753*)            IF CH='''' THEN
(*0754*)              BEGIN NEXTCH; IF CH<>'''' THEN EXIT; END;
(*0755*)            IF I = STRGFRL THEN
(*0756*)              BEGIN NEW(TAILP);
(*0757*)                WITH TAILP@ DO
(*0758*)                  BEGIN NXTCSP := CONSTP; STFR := BYTEPACK(SF) END;
(*0759*)                CONSTP := TAILP; I := 0;
(*0760*)              END;
(*0761*)            I := I + 1; LGTH := LGTH + 1;
(*0762*)            SF(.I.):=ORD(CH);
(*0763*)            NEXTCH
(*0764*)          END;
(*0765*)          IF LGTH = 1 THEN
(*0766*)            BEGIN SY:=CHARCONST; IVAL:=SF(.1.); END
(*0767*)          ELSE
(*0768*)            BEGIN FOR I:=I+1 TO STRGFRL DO SF(.I.):=ORD(' ');
(*0769*)              NEW(TAILP);
(*0770*)              WITH TAILP@ DO
(*0771*)                BEGIN NXTCSP := CONSTP; STFR := BYTEPACK(SF) END;
(*0772*)              (*REVERSE POINTERS:*)
(*0773*)              CONSTP := NIL;
(*0774*)              WHILE TAILP <> NIL DO
(*0775*)                WITH TAILP@ DO
(*0776*)                  BEGIN NXTP := NXTCSP; NXTCSP := CONSTP;
(*0777*)                    CONSTP := TAILP; TAILP := NXTP
(*0778*)                  END;
(*0779*)              SY:=STRINGCONST;
(*0780*)            END
(*0781*)        END;
(*0782*)(*:*) 122: BEGIN OP:=NOOP; NEXTCH;
(*0783*)                 IF CH='=' THEN BEGIN SY:=BECOMES; NEXTCH; END
(*0784*)             ELSE BEGIN SY:=COLON; IF DOTFLG THEN
(*0785*)                BEGIN DOTFLG:=FALSE; DOTDOT:=TRUE END
(*0786*)              ELSE DOTDOT:=FALSE;
(*0787*)            END;
(*0788*)           END;
(*0789*)(*.*) 75:  BEGIN OP:=NOOP; NEXTCH;
(*0790*)          IF CH='.' THEN BEGIN SY:=COLON;DOTDOT:=TRUE;NEXTCH END
(*0791*)                   ELSE IF CH=')' THEN BEGIN SY:=RBRACK; NEXTCH END
(*0792*)                     ELSE SY:=PERIOD;
(*0793*)           END;
(*0794*)(*(*) 77:
(*0795*)    BEGIN NEXTCH;
(*0796*)      IF CH = '*' THEN
(*0797*)        BEGIN NEXTCH;
(*0798*)          IF CH = '$' THEN OPTIONS;
(*0799*)          REPEAT
(*0800*)            WHILE CH<>'*' DO NEXTCH;
(*0801*)            NEXTCH
(*0802*)          UNTIL CH = ')';
(*0803*)          NEXTCH; GOTO 1
(*0804*)        END;
(*0805*)      OP:=NOOP;
(*0806*)      IF CH='.' THEN BEGIN SY:=LBRACK; NEXTCH END
(*0807*)                ELSE IF CH='#' THEN BEGIN SY:=LCBRACK; NEXTCH; END
(*0808*)                  ELSE SY:=LPARENT;
(*0809*)    END;
(*0810*)(*<*) 76:  BEGIN NEXTCH; SY:=RELOP;
(*0811*)             IF CH='=' THEN BEGIN OP:=LEOP; NEXTCH; END
(*0812*)               ELSE IF CH='>' THEN BEGIN OP:=NEOP; NEXTCH; END
(*0813*)                 ELSE OP:=LTOP;
(*0814*)           END;
(*0815*)(*>*) 110: BEGIN NEXTCH; SY:=RELOP;
(*0816*)             IF CH='=' THEN BEGIN OP:=GEOP; NEXTCH; END
(*0817*)                       ELSE OP:=GTOP;
(*0818*)           END;
(*0819*)(*#*) 123: BEGIN NEXTCH; OP:=NOOP;
(*0820*)                 IF CH=')' THEN BEGIN SY:=RCBRACK; NEXTCH; END
(*0821*)                           ELSE SY:=OTHERSY;
(*0822*)           END;
(*0823*)(* ** *)
(*0824*)      92 : BEGIN   NEXTCH; SY:=MULOP;OP:=MUL;
(*0825*)            IF CH = '*' THEN
(*0826*)            BEGIN
(*0827*)             NEXTCH; SY:=EXPONOP
(*0828*)            END;
(*0829*)          END;
(*0830*)(* /+-=)%,;@XXXX *)                                                             
(*0831*)    173,189,79,80,                                                              
(*0832*)      97,78,96,126,93,108,107,94,124:
(*0833*)        BEGIN SY:=SSY(.CH.); OP:=SOP(.CH.); NEXTCH; END;
(*0834*)  74,91,98..102,                                                                
(*0835*)      103,104,105,106,109,111,112,113,114,115,116,117,118,119,120,121,127:
(*0836*)   BEGIN OP:=NOOP; SY:=OTHERSY; NEXTCH; END;                                    
(*0837*)(* ^ *)   95:BEGIN                                                              
(*0838*)               NEXTCH; SY :=NOTSY;                                              
(*0839*)                 IF CH = '=' THEN                                               
(*0840*)                 BEGIN                                                          
(*0841*)                   SY := RELOP;                                                 
(*0842*)                   OP := NEOP;                                                  
(*0843*)                   NEXTCH;                                                      
(*0844*)                  END ELSE OP := NOOP;                                          
(*0845*)               END;                                                             
(*0846*)(*   *)   139: BEGIN                                                            
(*0847*)                 NEXTCH; IF CH='$' THEN OPTIONS;                                
(*0848*)                 WHILE ORD(CH) <> 155 DO NEXTCH;                                
(*0849*)                 NEXTCH; GOTO 1;                                                
(*0850*)               END;                                                             
(*0851*)  END (*CASE*);
(*0852*) END (*INSYMBOL*) ;
(*0853*) 
$TITLE  IDENTIFIER TABLE ENTERING
(*0854*) PROCEDURE ENTERID(FCP: CTP);
(*0855*)  (*ENTER ID POINTED AT BY FCP INTO THE NAME-TABLE,
  0856     WHICH ON EACH DECLARATION LEVEL IS ORGANISED AS
  0857     AN UNBALANCED BINARY TREE*)
(*0858*)   VAR NAM: ALFA; LCP, LCP1: CTP; LLEFT: BOOLEAN;
(*0859*)   BEGIN NAM := FCP@.NAME;
(*0860*)     LCP := DISPLAY(.TOP.).FNAME;
(*0861*)     IF LCP = NIL THEN
(*0862*)       DISPLAY(.TOP.).FNAME := FCP
(*0863*)     ELSE
(*0864*)       BEGIN
(*0865*)         REPEAT LCP1 := LCP;
(*0866*)           IF LCP@.NAME = NAM THEN   (*NAME CONFLICT, FOLLOW RIGHT LINK*)
(*0867*)             BEGIN ERROR(101); LCP := LCP@.RLINK; LLEFT := FALSE END
(*0868*)           ELSE
(*0869*)             IF LCP@.NAME < NAM
(*0870*)               THEN BEGIN LCP := LCP@.RLINK; LLEFT := FALSE END
(*0871*)               ELSE BEGIN LCP := LCP@.LLINK; LLEFT := TRUE END
(*0872*)         UNTIL LCP = NIL;
(*0873*)         IF LLEFT THEN LCP1@.LLINK := FCP ELSE LCP1@.RLINK := FCP
(*0874*)       END;
(*0875*)     FCP@.LLINK := NIL; FCP@.RLINK := NIL
(*0876*)   END;
(*0877*) 
$TITLE  SEARCHSECTION,SEARCHID
(*0878*) PROCEDURE SEARCHSECTION(FCP: CTP; VAR FCP1: CTP);
(*0879*)  LABEL 1;
(*0880*) BEGIN
(*0881*)  WHILE FCP <> NIL DO
(*0882*)   IF FCP@.NAME = ID THEN GOTO 1
(*0883*)   ELSE IF FCP@.NAME < ID THEN FCP := FCP@.RLINK
(*0884*)    ELSE FCP := FCP@.LLINK;
(*0885*)1:  FCP1 := FCP
(*0886*) END;
(*0887*) 
(*0888*) PROCEDURE SEARCHID(FIDCLS: SETOFIDS; VAR FCP: CTP);
(*0889*)  LABEL 1;
(*0890*)  VAR LCP: CTP;
(*0891*) BEGIN
(*0892*)  FOR DISX := TOP DOWNTO 0 DO
(*0893*)   BEGIN LCP := DISPLAY(.DISX.).FNAME;
(*0894*)    WHILE LCP <> NIL DO
(*0895*)      WITH LCP@ DO
(*0896*)        IF NAME = ID THEN
(*0897*)          IF KLASS IN FIDCLS THEN GOTO 1
(*0898*)          ELSE
(*0899*)            BEGIN IF PRTERR THEN ERROR(103);
(*0900*)              LCP := RLINK
(*0901*)            END
(*0902*)        ELSE
(*0903*)          IF NAME<ID THEN LCP:=RLINK
(*0904*)                     ELSE LCP:=LLINK;
(*0905*)   END;
(*0906*)  (*SEARCH NOT SUCCSESSFUL; SUPPRESS ERROR MESSAGE IN CASE
  0907     OF FORWARD REFERENCED TYPE ID IN POINTER TYPE DEFINITION
  0908     OR VARIANTS WITHOUT TAGFIELDS
  0909     --> PROCEDURE FIELDLIST
  0910     --> PROCEDURE TYP*)
(*0911*)  IF PRTERR THEN
(*0912*)   BEGIN ERROR(104);
(*0913*)    (*TO AVOID RETURNING NIL, REFERENCE AN ENTRY
  0914       FOR AN UNDECLARED ID OF APPROPRIATE CLASS
  0915       --> PROCEDURE ENTERUNDECL*)
(*0916*)    IF TYPES IN FIDCLS THEN LCP := UTYPPTR
(*0917*)      ELSE IF VARS IN FIDCLS THEN LCP:=UVARPTR
(*0918*)        ELSE IF FIELD IN FIDCLS THEN LCP:=UFLDPTR
(*0919*)          ELSE IF KONST IN FIDCLS THEN LCP:=UCSTPTR
(*0920*)            ELSE IF PROC IN FIDCLS THEN LCP:=UPRCPTR
(*0921*)              ELSE IF FUNC IN FIDCLS THEN LCP:=UFCTPTR
(*0922*)                ELSE LCP:=UEVENTPTR;
(*0923*)   END;
(*0924*)1:  FCP := LCP
(*0925*) END (*SEARCHID*) ;
(*0926*) 
$TITLE GETBOUNDS ROUTINE ,SKIP,OBCLEAR
(*0927*) 
(*0928*)    PROCEDURE GETBOUNDS(FSP: STP; VAR FMIN,FMAX: INTEGER);
(*0929*)      (*GET INTERNAL BOUNDS OF SUBRANGE OR SCALAR TYPE*)
(*0930*)      (*ASSUME (FSP <> INTPTR) AND (FSP <> REALPTR)*)
(*0931*)      BEGIN
(*0932*)        IF FSP <> NIL THEN
(*0933*)         BEGIN
(*0934*)           IF FSP@.FORM = PACKDTYPE THEN FSP:=FSP@.BASETYPE;
(*0935*)          WITH FSP@ DO
(*0936*)            BEGIN
(*0937*)              IF FORM = SUBRANGE THEN
(*0938*)                BEGIN FMIN := MIN; FMAX := MAX END
(*0939*)              ELSE
(*0940*)                BEGIN FMIN := 0; FMAX := 0;
(*0941*)                  IF FORM = SCALAR THEN
(*0942*)                    BEGIN
(*0943*)                      IF SCALKIND = STANDARD THEN
(*0944*)                        BEGIN IF FSP = CHARPTR THEN FMAX := ORDCHARMAX
(*0945*)                          ELSE IF FSP=BOOLPTR THEN FMAX:=1;
(*0946*)                        END
(*0947*)                      ELSE
(*0948*)                        IF FSP@.FCONST <> NIL THEN
(*0949*)                         FMAX := FSP@.FCONST@.VALUES.IVAL
(*0950*)                    END
(*0951*)                END
(*0952*)            END;
(*0953*)           END
(*0954*)      END;
(*0955*) 
(*0956*) PROCEDURE SKIP(FSYS: SETOFSYS);
(*0957*)  (*SKIP INPUT STRING UNTIL RELEVANT SYMBOL FOUND*)
(*0958*)   BEGIN WHILE NOT (SY IN FSYS) DO INSYMBOL; END;
(*0959*) 
(*0960*)    PROCEDURE OBCLEAR;
(*0961*)      VAR I:INTEGER;
(*0962*)      BEGIN
(*0963*)        IF OBPOINTER<>0 THEN
(*0964*)          BEGIN
(*0965*)            FOR I:=OBPOINTER TO OBJLENGTH -1 DO $PASOBJ1@(.I.):=0;
(*0966*)            PUT($PASOBJ1); OBPOINTER:=0;
(*0967*)          END;
(*0968*)      END;
(*0969*) 
$TITLE   TEST1 , TEST2
(*0970*)PROCEDURE TEST1(X:SYMBOL; Y:INTEGER);
(*0971*) 
(*0972*) (*  REPLACES 'IF <COND> THEN INSYMBOL ELSE ERROR(<NUM>) *)
(*0973*) 
(*0974*) BEGIN (*TEST1*)
(*0975*)   IF SY = X THEN INSYMBOL ELSE ERROR(Y)
(*0976*) END;   (*TEST1*)
(*0977*) 
(*0978*) 
(*0979*) 
(*0980*)PROCEDURE TEST2(X:SETOFSYS; Y:INTEGER; Z:SETOFSYS);
(*0981*) BEGIN(*TEST2*)
(*0982*)   IF NOT (SY IN X) THEN
(*0983*)   BEGIN
(*0984*)     ERROR(Y);
(*0985*)     SKIP(X+Z)
(*0986*)   END
(*0987*) END;(*TEST2*)
(*0988*) 
(*0989*) 
(*0990*) 
$TITLE  BLOCK , INITSIZE AND ALIGNMENT
(*0991*) 
(*0992*) PROCEDURE BLOCK(FSYS: SETOFSYS; FSY: SYMBOL; FPROCP: CTP);
(*0993*)  VAR LSY:SYMBOL; FLABP:LBP; FWPROCS:CTP;
(*0994*) 
(*0995*)    PROCEDURE INITSIZE(VAR FSIZE : WBSIZE);
(*0996*)      BEGIN FSIZE.WBLENGTH:=4; FSIZE.BOUNDARY:=4;
(*0997*)      END;
(*0998*) 
(*0999*)    PROCEDURE ALIGNMENT(VAR COUNTER:INTEGER; UNIT:CELLUNIT);
(*1000*)      BEGIN IF COUNTER MOD UNIT>0 THEN
(*1001*)        COUNTER:=(COUNTER+UNIT) DIV UNIT*UNIT;
(*1002*)      END;
(*1003*) 
$TITLE COMPTYPES,COMPLISTS,EQUALBOUNDS
(*1004*)  FUNCTION COMPTYPES(FSP1,FSP2: STP) : BOOLEAN;
(*1005*)   LABEL 1;
(*1006*)   (*DECIDE WHETHER STRUCT POINTED AT BY FSP1 AND FSP2 ARE COMPATIBLE*)
(*1007*)   VAR NXT1,NXT2: CTP; COMP: BOOLEAN; LPCRP : PCRP; LP : MARKP;
(*1008*) 
(*1009*)    FUNCTION COMPLISTS(FCP1,FCP2:CTP; FSP1,FSP2:STP):BOOLEAN;
(*1010*)    (*DECIDE WHETHER FIELDLISTS ARE COMPATIBLE*)
(*1011*)    (*FCP1, FCP2: HEADS OF FIELDLISTS; FSP1, FSP2: POINTERS TO HEAD OF
  1012       VARIANT CHAIN*)
(*1013*)      VAR COMP:BOOLEAN; NXT1,NXT2:STP;
(*1014*)      BEGIN COMP := TRUE;
(*1015*)        WHILE COMP AND (FCP1 <> NIL)AND (FCP2 <> NIL) DO
(*1016*)          BEGIN COMP := COMPTYPES(FCP1@.IDTYPE,FCP2@.IDTYPE);
(*1017*)            FCP1 := FCP1@.NEXT; FCP2 := FCP2@.NEXT
(*1018*)          END;
(*1019*)        COMP := COMP AND (FCP1 = FCP2);
(*1020*)        IF (FSP1 <> NIL)AND (FSP2 <> NIL) THEN
(*1021*)          BEGIN
(*1022*)            IF (FSP1@.TGFLDP <> NIL)AND (FSP2@.TGFLDP <> NIL) THEN
(*1023*)              COMP := COMP AND COMPTYPES(FSP1@.TGFLDP@.IDTYPE,
(*1024*)                  FSP2@.TGFLDP@.IDTYPE);
(*1025*)            NXT1 := FSP1@.FSTVAR; NXT2 := FSP2@.FSTVAR;
(*1026*)            WHILE COMP AND (NXT1 <> NIL)AND (NXT2 <> NIL) DO
(*1027*)              BEGIN COMP := COMPLISTS(NXT1@.FSTVARFLD,NXT2@.FSTVARFLD,
(*1028*)                     NXT1@.SUBVAR,NXT2@.SUBVAR);
(*1029*)                NXT1 := NXT1@.NXTVAR; NXT2 := NXT2@.NXTVAR
(*1030*)              END;
(*1031*)            COMPLISTS := COMP AND (NXT1 = NXT2)
(*1032*)          END
(*1033*)        ELSE COMPLISTS := COMP AND (FSP1 = FSP2)
(*1034*)      END (*COMPLISTS*) ;
(*1035*) 
(*1036*)    FUNCTION EQUALBOUNDS(FSP1,FSP2: STP) : BOOLEAN;
(*1037*)      VAR LMIN1,LMIN2,LMAX1,LMAX2: INTEGER;
(*1038*)      BEGIN GETBOUNDS(FSP1,LMIN1,LMAX1);
(*1039*)            GETBOUNDS(FSP2,LMIN2,LMAX2);
(*1040*)            EQUALBOUNDS := (LMIN1 = LMIN2)AND (LMAX1 = LMAX2)
(*1041*)      END;
(*1042*) 
(*1043*)  BEGIN (*COMPTYPES*)
(*1044*)    IF FSP1 = FSP2 THEN COMPTYPES := TRUE
(*1045*)    ELSE IF (FSP1=NIL) OR (FSP2=NIL) THEN COMPTYPES:=TRUE
(*1046*)    ELSE
(*1047*)     BEGIN
(*1048*)       IF FSP1@.FORM=PACKDTYPE THEN FSP1:=FSP1@.BASETYPE;
(*1049*)       IF FSP2@.FORM=PACKDTYPE THEN FSP2:=FSP2@.BASETYPE;
(*1050*)       IF FSP1@.FORM=SUBRANGE THEN FSP1:=FSP1@.RANGETYPE;
(*1051*)       IF FSP2@.FORM=SUBRANGE THEN FSP2:=FSP2@.RANGETYPE;
(*1052*)       IF FSP1=FSP2 THEN COMPTYPES:=TRUE ELSE
(*1053*)       IF FSP1@.SIZE.WBLENGTH<>FSP2@.SIZE.WBLENGTH THEN COMPTYPES:=FALSE
(*1054*)       ELSE
(*1055*)       IF FSP1@.FORM<>FSP2@.FORM THEN COMPTYPES:=FALSE
(*1056*)       ELSE CASE FSP1@.FORM OF
(*1057*)         SCALAR:
(*1058*)           IF (FSP1@.SCALKIND = STANDARD)OR(FSP2@.SCALKIND = STANDARD) THEN
(*1059*)             COMPTYPES := FALSE
(*1060*)           ELSE
(*1061*)             BEGIN NXT1 := FSP1@.FCONST; NXT2 := FSP2@.FCONST;
(*1062*)              COMP := TRUE;
(*1063*)              WHILE COMP AND (NXT1 <> NIL)AND (NXT2 <> NIL) DO
(*1064*)                BEGIN COMP := (NXT1@.NAME = NXT2@.NAME);
(*1065*)                  NXT1 := NXT1@.NEXT; NXT2 := NXT2@.NEXT
(*1066*)                END;
(*1067*)              COMPTYPES := COMP AND (NXT1 = NXT2)
(*1068*)             END;
(*1069*)         PACKDTYPE,SUBRANGE,TAGFIELD,VARIANT: ERROR(400);
(*1070*)         POINTER:
(*1071*)           BEGIN
(*1072*)             LPCRP := FSTPCRP; COMP := TRUE;
(*1073*)             WHILE LPCRP <> NIL DO
(*1074*)               WITH LPCRP@ DO
(*1075*)                 BEGIN
(*1076*)                   IF (FSP1 = PTR1) AND (FSP2 = PTR2) THEN GOTO 1
(*1077*)                   ELSE
(*1078*)                     IF (FSP1 = PTR2) AND (FSP2 = PTR1) THEN GOTO 1;
(*1079*)                   LPCRP := NEXT
(*1080*)                 END;
(*1081*)             IF FSTPCRP = NIL THEN MARK(LP); NEW(LPCRP);
(*1082*)             WITH LPCRP@ DO
(*1083*)               BEGIN NEXT := FSTPCRP;
(*1084*)                 PTR1 := FSP1; PTR2 := FSP2
(*1085*)               END;
(*1086*)             FSTPCRP := LPCRP;
(*1087*)             COMP := COMPTYPES(FSP1@.ELTYPE,FSP2@.ELTYPE);
(*1088*)             FSTPCRP := FSTPCRP@.NEXT;
(*1089*)             IF FSTPCRP = NIL THEN RELEASE(LP);
(*1090*)       1:    COMPTYPES := COMP
(*1091*)           END;
(*1092*)         POWER:
(*1093*)           COMPTYPES := (FSP1@.PCKDSET = FSP2@.PCKDSET)
(*1094*)                 AND COMPTYPES(FSP1@.ELSET,FSP2@.ELSET);
(*1095*)         ARRAYS:
(*1096*)           COMPTYPES:=COMPTYPES(FSP1@.INXTYPE,FSP2@.INXTYPE)
(*1097*)                  AND COMPTYPES(FSP1@.AELTYPE,FSP2@.AELTYPE)
(*1098*)                  AND EQUALBOUNDS(FSP1@.INXTYPE,FSP2@.INXTYPE);
(*1099*)         RECORDS:
(*1100*)           COMPTYPES:= COMPLISTS(FSP1@.FSTFLD,FSP2@.FSTFLD,
(*1101*)                 FSP1@.RECVAR,FSP2@.RECVAR);
(*1102*)         FILES:
(*1103*)           COMPTYPES:=COMPTYPES(FSP1@.FILTYPE,FSP2@.FILTYPE);
(*1104*)        END (*CASE*)
(*1105*)    END
(*1106*)  END (*COMPTYPES*) ;
(*1107*) 
$TITLE  STRING,STRINGTYPE,REVERSE
(*1108*)    FUNCTION STRING(FSP: STP) : BOOLEAN;
(*1109*)      BEGIN STRING := FALSE;
(*1110*)        IF FSP <> NIL THEN
(*1111*)          WITH FSP@ DO
(*1112*)            IF SIZE.WBLENGTH<=256 THEN
(*1113*)              IF FORM = ARRAYS THEN
(*1114*)                IF AELTYPE<>NIL THEN
(*1115*)                  IF AELTYPE@.FORM=PACKDTYPE THEN
(*1116*)                    IF AELTYPE@.BASETYPE=CHARPTR THEN STRING:=TRUE;
(*1117*)      END;
(*1118*) 
(*1119*)    PROCEDURE STRINGTYPE(VAR FSP: STP);
(*1120*)   (*ENTER TYPE OF STRINGCONST (PACKED ARRAY (.1..LGTH.) OF CHAR) INTO
  1121      STRUCTURE TABLE*)
(*1122*)      VAR LSP,LSP1: STP;
(*1123*)      BEGIN NEW(LSP,SUBRANGE);
(*1124*)        WITH LSP@ DO
(*1125*)          BEGIN RANGETYPE:=INTPTR;
(*1126*)            MIN := 1; MAX := LGTH ; FTYPE := FALSE;
(*1127*)            SIZE.WBLENGTH:=4; SIZE.BOUNDARY:=4;
(*1128*)          END;
(*1129*)        NEW(LSP1,ARRAYS);
(*1130*)        WITH LSP1@ DO
(*1131*)          BEGIN
(*1132*)            AELTYPE := PACKDCHARPTR; INXTYPE := LSP;
(*1133*)            FTYPE:=FALSE; AELLENG:=1;
(*1134*)            SIZE.WBLENGTH:=LGTH; SIZE.BOUNDARY:=1;
(*1135*)          END;
(*1136*)        FSP := LSP1
(*1137*)      END;
(*1138*) 
(*1139*)    PROCEDURE REVERSE(A:CTP; VAR B:CTP);
(*1140*)      VAR WORK,ANSWER:CTP;
(*1141*)      BEGIN ANSWER:=NIL;
(*1142*)        WHILE A<>NIL DO
(*1143*)          WITH A@ DO
(*1144*)            BEGIN WORK:=NEXT; NEXT:=ANSWER;
(*1145*)                  ANSWER:=A; A:=WORK;
(*1146*)            END;
(*1147*)        B:=ANSWER;
(*1148*)      END;
(*1149*) 
$TITLE CONSTANT,SETELEMENT
(*1150*)  PROCEDURE CONSTANT(FSYS: SETOFSYS; VAR FSP: STP; VAR FVALU: VALU);
(*1151*)   VAR LSP: STP; LCP: CTP; SIGN: (NONE,POS,NEG);
(*1152*)       SETTYPE1,SETTYPE2:STP; SETVAL1,SETVAL2:VALU;
(*1153*)       N:INTEGER; NOERROR:BOOLEAN;
(*1154*) 
(*1155*)   PROCEDURE SETELEMENT(SETTYPE:STP; SETVALUE:VALU);
(*1156*)     VAR X:BOOLEAN;
(*1157*)     BEGIN X:=FALSE;
(*1158*)       IF SETTYPE=REALPTR THEN ERROR(109)
(*1159*)         ELSE IF SETTYPE@.FORM>SUBRANGE THEN ERROR(136)
(*1160*)           ELSE IF NOT COMPTYPES(LSP@.ELSET,SETTYPE) THEN ERROR(137)
(*1161*)             ELSE IF (SETVALUE.IVAL<SETMIN) OR (SETVALUE.IVAL>SETMAX) THEN ERROR(304)
(*1162*)               ELSE X:=TRUE;
(*1163*)       NOERROR:=NOERROR AND X;
(*1164*)     END;
(*1165*) 
(*1166*)  BEGIN LSP := NIL; FVALU.IVAL := 0; FVALU.CKIND:=INT;
(*1167*)   TEST2(CONSTBEGSYS,50,FSYS);
(*1168*)   IF SY IN CONSTBEGSYS THEN
(*1169*)    BEGIN
(*1170*)     IF SY = CHARCONST THEN
(*1171*)       BEGIN LSP:=CHARPTR; FVALU.CKIND:=INT; FVALU.IVAL:=IVAL; INSYMBOL END
(*1172*)     ELSE IF SY=STRINGCONST THEN
(*1173*)       BEGIN STRINGTYPE(LSP);
(*1174*)         FVALU.CKIND:=STRG; FVALU.VALP:=CONSTP;
(*1175*)         INSYMBOL
(*1176*)       END
(*1177*)     ELSE IF SY=LBRACK THEN
(*1178*)       BEGIN NEW(LSP,POWER);
(*1179*)         WITH LSP@ DO
(*1180*)           BEGIN ELSET:=NIL; PCKDSET:=FALSE; FTYPE:=FALSE;
(*1181*)                 SIZE.WBLENGTH:=8; SIZE.BOUNDARY:=8;
(*1182*)           END;
(*1183*)         FVALU.CKIND:=PSET; FVALU.PVAL:=(..);
(*1184*)         INSYMBOL;
(*1185*)         IF SY=RBRACK THEN INSYMBOL
(*1186*)         ELSE
(*1187*)           BEGIN
(*1188*)             LOOP NOERROR:=TRUE;
(*1189*)               CONSTANT(FSYS+(.COMMA,COLON,RBRACK.),SETTYPE1,SETVAL1);
(*1190*)               SETELEMENT(SETTYPE1,SETVAL1);
(*1191*)               IF SY=COLON THEN
(*1192*)                 BEGIN INSYMBOL; CONSTANT(FSYS+(.COMMA,RBRACK.),SETTYPE2,SETVAL2);
(*1193*)                   SETELEMENT(SETTYPE2,SETVAL2);
(*1194*)                   IF NOERROR THEN
(*1195*)                     BEGIN FOR N:=SETVAL1.IVAL TO SETVAL2.IVAL DO
(*1196*)                              FVALU.PVAL:=FVALU.PVAL+(.N.);
(*1197*)                       LSP@.ELSET:=SETTYPE1;
(*1198*)                     END;
(*1199*)                 END
(*1200*)               ELSE IF NOERROR THEN
(*1201*)                 BEGIN FVALU.PVAL:=FVALU.PVAL+(.SETVAL1.IVAL.);
(*1202*)                       LSP@.ELSET:=SETTYPE1;
(*1203*)                 END;
(*1204*)               IF SY<>COMMA THEN EXIT; INSYMBOL;
(*1205*)             END;
(*1206*)             TEST1(RBRACK,12);
(*1207*)           END;
(*1208*)       END
(*1209*)     ELSE
(*1210*)       BEGIN
(*1211*)         SIGN := NONE;
(*1212*)         IF OP IN (.PLUS,MINUS.) THEN
(*1213*)           BEGIN IF OP = PLUS THEN SIGN := POS ELSE SIGN := NEG;
(*1214*)             INSYMBOL
(*1215*)           END;
(*1216*)         IF SY = IDENT THEN
(*1217*)           BEGIN SEARCHID((.KONST.),LCP);
(*1218*)             WITH LCP@ DO
(*1219*)               BEGIN LSP := IDTYPE; FVALU := VALUES END;
(*1220*)             IF SIGN <> NONE THEN
(*1221*)               IF LSP = INTPTR THEN
(*1222*)                 BEGIN IF SIGN = NEG THEN FVALU.IVAL := -FVALU.IVAL END
(*1223*)               ELSE
(*1224*)                 IF LSP = REALPTR THEN
(*1225*)                   BEGIN
(*1226*)                     IF SIGN = NEG THEN FVALU.RVAL := -FVALU.RVAL
(*1227*)                   END
(*1228*)               ELSE ERROR(105);
(*1229*)             INSYMBOL;
(*1230*)           END
(*1231*)         ELSE
(*1232*)           IF SY = INTCONST THEN
(*1233*)             BEGIN IF SIGN = NEG THEN IVAL := -IVAL;
(*1234*)               LSP:=INTPTR; FVALU.CKIND:=INT; FVALU.IVAL:=IVAL; INSYMBOL
(*1235*)             END
(*1236*)           ELSE
(*1237*)             IF SY = REALCONST THEN
(*1238*)               BEGIN IF SIGN = NEG THEN RVAL := -RVAL;
(*1239*)                 LSP:=REALPTR; FVALU.CKIND:=REEL; FVALU.RVAL:=RVAL; INSYMBOL
(*1240*)               END
(*1241*)             ELSE
(*1242*)               BEGIN ERROR(106); SKIP(FSYS) END
(*1243*)       END;
(*1244*)       TEST2(FSYS,6,(. .));
(*1245*)     END;
(*1246*)   FSP := LSP
(*1247*)  END (*CONSTANT*) ;
(*1248*) 
$TITLE CONSTEXPRESSION,CONSTIMAGE,ERROR1
(*1249*)    PROCEDURE CONSTEXPRESSION(FSYS:SETOFSYS; VAR P:CEP);
(*1250*)      VAR X1,X2,T,W:CEP; LSP:STP; LVALU:VALU;
(*1251*)      BEGIN X1:=NIL;
(*1252*)        REPEAT INSYMBOL;
(*1253*)          CONSTANT(FSYS+(.COMMA,RCBRACK.),LSP,LVALU);
(*1254*)          IF (LSP@.FORM>=ARRAYS) AND (NOT STRING(LSP)) THEN ERROR(224)
(*1255*)          ELSE
(*1256*)            BEGIN NEW(X2);
(*1257*)              WITH X2@ DO
(*1258*)                BEGIN ELEMTYPE:=LSP; ELEMVALUE:=LVALU;
(*1259*)                      NEXTELEM:=X1;
(*1260*)                END;
(*1261*)              X1:=X2;
(*1262*)            END;
(*1263*)        UNTIL SY<>COMMA;
(*1264*)        T:=NIL;
(*1265*)        WHILE X1<>NIL DO WITH X1@ DO
(*1266*)          BEGIN W:=NEXTELEM; NEXTELEM:=T;
(*1267*)                T:=X1; X1:=W;
(*1268*)          END;
(*1269*)        P:=T;
(*1270*)        TEST1(RCBRACK,225);
(*1271*)        TEST2(FSYS,6,(..));
(*1272*)      END;
(*1273*) 
(*1274*)    PROCEDURE CONSTIMAGE(FSP:STP; FEP:CEP; VAR FVALU:VALU);
(*1275*)      VAR ERRFLAG:BOOLEAN; ANSWER,WORK,XX:CTAILP; CURRENT:ADDRRANGE;
(*1276*)          BYTEFLAG:BOOLEAN; BYTEPART,BUFFER:STRGFRAG;
(*1277*) 
(*1278*)     PROCEDURE ERROR1(N:INTEGER);
(*1279*)       BEGIN IF ERRFLAG THEN ERROR(N);
(*1280*)         ERRFLAG:=FALSE;
(*1281*)       END;
(*1282*) 
$TITLE WORDCONST,BUFFEROUT,BYTECONST,UNITCONST
(*1283*)     PROCEDURE WORDCONST(V:INTEGER);
(*1284*)       BEGIN NEW(WORK);
(*1285*)         WORK@.NXTCSP:=ANSWER; WORK@.STFR:=V;
(*1286*)         ANSWER:=WORK; CURRENT:=CURRENT+4;
(*1287*)       END;
(*1288*) 
(*1289*)     PROCEDURE BUFFEROUT;
(*1290*)       BEGIN NEW(WORK);
(*1291*)         WORK@.NXTCSP:=ANSWER; WORK@.STFR:=BYTEPACK(BYTEPART);
(*1292*)         ANSWER:=WORK; BYTEFLAG:=FALSE;
(*1293*)         CURRENT :=(CURRENT + 3) DIV 4 * 4;                                     
(*1294*)       END;
(*1295*) 
(*1296*)     PROCEDURE BYTECONST(V:INTEGER);
(*1297*)       BEGIN BYTEPART(.CURRENT MOD 4+1.):=V;
(*1298*)         BYTEFLAG:=TRUE; CURRENT:=CURRENT+1;
(*1299*)         IF CURRENT MOD 4=0 THEN BUFFEROUT;
(*1300*)       END;
(*1301*) 
(*1302*)     PROCEDURE UNITCONST(DISPL:ADDRRANGE; FSP:STP);
(*1303*)       VAR X:CTAILP; I,A1,A2:INTEGER;
(*1304*)       BEGIN IF FEP=NIL THEN ERROR1(222)
(*1305*)    ELSE IF NOT COMPTYPES(FEP@.ELEMTYPE,FSP) THEN
(*1306*)    BEGIN
(*1307*)      IF NOT COMPTYPES(FSP,NILPTR) THEN ERROR1(223)
(*1308*)    END
(*1309*)         ELSE
(*1310*)           BEGIN
(*1311*)             IF DISPL>CURRENT THEN
(*1312*)               BEGIN IF BYTEFLAG THEN
(*1313*)                 BUFFEROUT;                                                     
(*1314*)                 IF DISPL>CURRENT THEN WORDCONST(0);
(*1315*)               END;
(*1316*)             IF FSP@.FORM=ARRAYS THEN
(*1317*)               BEGIN X:=FEP@.ELEMVALUE.VALP;
(*1318*)                 FOR I:=0 TO FSP@.SIZE.WBLENGTH-1 DO
(*1319*)                   BEGIN IF (I MOD 4)=0 THEN BYTEUNPACK(BUFFER,X@.STFR);
(*1320*)                         BYTECONST(BUFFER(.I MOD 4+1.));
(*1321*)                         IF (I MOD 4)=3 THEN X:=X@.NXTCSP;
(*1322*)                   END;
(*1323*)               END
(*1324*)             ELSE IF FSP@.SIZE.WBLENGTH=1 THEN BYTECONST(FEP@.ELEMVALUE.IVAL)
(*1325*)             ELSE IF (FEP@.ELEMVALUE.CKIND=REEL) OR (FEP@.ELEMVALUE.CKIND=PSET) THEN
(*1326*)               BEGIN SETVALUE(FEP@.ELEMVALUE.PVAL,A1,A2);
(*1327*)                     WORDCONST(A1); WORDCONST(A2);
(*1328*)               END
(*1329*)             ELSE WORDCONST(FEP@.ELEMVALUE.IVAL);
(*1330*)           FEP:=FEP@.NEXTELEM;
(*1331*)         END;
(*1332*)       END;
(*1333*) 
$TITLE STCONST,BODY OF CONSTIMAGE
(*1334*)     PROCEDURE STCONST(DISPL:ADDRRANGE; FSP:STP);
(*1335*)       VAR LMIN,LMAX,I:INTEGER; LCP:CTP;
(*1336*)       BEGIN
(*1337*)         IF FSP<>NIL THEN
(*1338*)           CASE FSP@.FORM OF
(*1339*)             SCALAR,PACKDTYPE,SUBRANGE,POWER:
(*1340*)               UNITCONST(DISPL,FSP);
(*1341*)       POINTER: IF NOT COMPTYPES(NILPTR,FSP) THEN ERROR1(226)
(*1342*)                  ELSE UNITCONST(DISPL,FSP);
(*1343*)       FILES,TAGFIELD,VARIANT:
(*1344*)               ERROR1(226);
(*1345*)             ARRAYS:
(*1346*)               IF STRING(FSP) THEN UNITCONST(DISPL,FSP)
(*1347*)                 ELSE
(*1348*)                   BEGIN GETBOUNDS(FSP@.INXTYPE,LMIN,LMAX);
(*1349*)                     FOR I:=LMIN TO LMAX DO
(*1350*)                       BEGIN STCONST(DISPL,FSP@.AELTYPE);
(*1351*)                             DISPL:=DISPL+FSP@.AELLENG;
(*1352*)                       END;
(*1353*)                   END;
(*1354*)             RECORDS:
(*1355*)               IF FSP@.RECVAR<>NIL THEN ERROR1(227)
(*1356*)               ELSE
(*1357*)                 BEGIN LCP:=FSP@.FSTFLD;
(*1358*)                   WHILE LCP<>NIL DO
(*1359*)                     BEGIN STCONST(DISPL+LCP@.FLDADDR,LCP@.IDTYPE);
(*1360*)                           LCP:=LCP@.NEXT;
(*1361*)                     END;
(*1362*)                 END
(*1363*)           END;
(*1364*)       END;
(*1365*) 
(*1366*)      BEGIN (*CONSTIMAGE*)
(*1367*)        ERRFLAG:=TRUE; CURRENT:=0;
(*1368*)        ANSWER:=NIL; BYTEFLAG:=FALSE;
(*1369*)        STCONST(0,FSP);
(*1370*)        IF BYTEFLAG THEN BUFFEROUT;
(*1371*)        IF FSP@.SIZE.WBLENGTH>CURRENT THEN WORDCONST(0);
(*1372*)        IF FEP<>NIL THEN ERROR1(222);
(*1373*)        WORK:=NIL;
(*1374*)        WHILE ANSWER<>NIL DO WITH ANSWER@ DO
(*1375*)          BEGIN XX:=NXTCSP; NXTCSP:=WORK; WORK:=ANSWER; ANSWER:=XX; END;
(*1376*)        FVALU.CKIND:=STRG; FVALU.VALP:=WORK;
(*1377*)      END;
(*1378*) 
$TITLE  TYP - TYPE HANDLING ROUTINES,CHECKPACK
(*1379*)  PROCEDURE TYP(FSYS: SETOFSYS; VAR FSP: STP; PACKFLAG:BOOLEAN);
(*1380*)   VAR LSP,LSP1,LSP2: STP; OLDTOP: DISPRANGE; LCP: CTP;
(*1381*)     LMIN,LMAX: INTEGER;
(*1382*)     LFILTYP: BOOLEAN;
(*1383*)     DISPL : ADDRRANGE;      (*LOCATION COUNTER WITHIN A RECORD*)
(*1384*)     LSIZE:CELLUNIT;         (*BOUNDARY OF THE RECORD*)
(*1385*) 
(*1386*)    PROCEDURE CHECKPACK(VAR ORG:STP);
(*1387*)      VAR W:STP; XMIN,XMAX:INTEGER;
(*1388*)      BEGIN
(*1389*)        IF ORG<>NIL THEN
(*1390*)          IF (ORG@.FORM=SCALAR) OR (ORG@.FORM=SUBRANGE) THEN
(*1391*)            IF ORG<>INTPTR THEN
(*1392*)              IF ORG<>REALPTR THEN
(*1393*)                BEGIN GETBOUNDS(ORG,XMIN,XMAX);
(*1394*)                  IF (XMIN>=0) AND (XMAX<=255) THEN
(*1395*)                    BEGIN NEW(W,PACKDTYPE);
(*1396*)                      WITH W@ DO
(*1397*)                        BEGIN SIZE.WBLENGTH:=1; SIZE.BOUNDARY:=1;
(*1398*)                              BASETYPE:=ORG; FTYPE:=FALSE;
(*1399*)                        END;
(*1400*)                      ORG:=W;
(*1401*)                    END;
(*1402*)                END;
(*1403*)      END;
(*1404*) 
$TITLE  SIMPLETYPE,SUBRNGS
(*1405*)   PROCEDURE SIMPLETYPE(FSYS: SETOFSYS; VAR FSP: STP; PACKFLAG:BOOLEAN);
(*1406*)    VAR LSP,LSP1: STP; LCP,LCP1: CTP; TTOP: DISPRANGE;
(*1407*)        LVAL:INTEGER; LVALU:VALU;
(*1408*) 
(*1409*)    PROCEDURE SUBRNGS(FSP: STP; FVALU: VALU);
(*1410*)      BEGIN NEW(LSP,SUBRANGE);
(*1411*)        WITH LSP@ DO
(*1412*)          BEGIN RANGETYPE:=FSP;
(*1413*)            MIN := FVALU.IVAL; FTYPE := FALSE
(*1414*)          END;
(*1415*)        TEST1(COLON,5);
(*1416*)        CONSTANT(FSYS,LSP1,LVALU);
(*1417*)        WITH LSP@ DO
(*1418*)          BEGIN MAX := LVALU.IVAL;
(*1419*)            INITSIZE(SIZE);
(*1420*)            IF FSP<>NIL THEN
(*1421*)              IF NOT COMPTYPES(FSP,LSP1) THEN ERROR(107)
(*1422*)              ELSE IF (FSP=REALPTR) OR (FSP@.FORM>=POWER) THEN
(*1423*)                  BEGIN ERROR(148); RANGETYPE:=NIL; END
(*1424*)                ELSE IF MIN>MAX THEN ERROR(102);
(*1425*)          END
(*1426*)      END;
(*1427*) 
(*1428*)   BEGIN (*SIMPLETYPE*)
(*1429*)    TEST2(SIMPTYPEBEGSYS,1,FSYS);
(*1430*)    IF SY IN SIMPTYPEBEGSYS THEN
(*1431*)     BEGIN
(*1432*)      IF SY = LPARENT THEN
(*1433*)       BEGIN TTOP := TOP;   (*DECL. CONSTS LOCAL TO INNERMOST BLOCK*)
(*1434*)        WHILE DISPLAY(.TOP.).OCCUR <> BLCK DO TOP := TOP - 1;
(*1435*)        NEW(LSP,SCALAR,DECLARED);
(*1436*)        WITH LSP@ DO
(*1437*)          BEGIN FTYPE:=FALSE;
(*1438*)            FCONST := NIL; INITSIZE(SIZE)
(*1439*)          END;
(*1440*)        LCP1 := NIL; LVAL := -1;
(*1441*)        REPEAT INSYMBOL;
(*1442*)          IF SY = IDENT THEN
(*1443*)            BEGIN NEW(LCP,KONST); LVAL := LVAL + 1;
(*1444*)              WITH LCP@ DO
(*1445*)                BEGIN NAME := ID; IDTYPE := LSP; NEXT := LCP1;
(*1446*)                      VALUES.CKIND:=INT; VALUES.IVAL:=LVAL;
(*1447*)                END;
(*1448*)              ENTERID(LCP);
(*1449*)              LCP1 := LCP; INSYMBOL
(*1450*)            END
(*1451*)          ELSE ERROR(2);
(*1452*)        TEST2(FSYS+(.COMMA,RPARENT.),6,(..));
(*1453*)        UNTIL SY <> COMMA;
(*1454*)        LSP@.FCONST:=LCP1;
(*1455*)        TOP := TTOP;
(*1456*)        TEST1(RPARENT,4);
(*1457*)       END
(*1458*)      ELSE
(*1459*)        BEGIN
(*1460*)          IF SY = IDENT THEN
(*1461*)            BEGIN SEARCHID((.TYPES,KONST.),LCP);
(*1462*)              INSYMBOL;
(*1463*)              WITH LCP@ DO
(*1464*)                IF KLASS = KONST THEN SUBRNGS(IDTYPE,VALUES)
(*1465*)                                 ELSE LSP:=IDTYPE;
(*1466*)            END (*SY = IDENT*)
(*1467*)          ELSE
(*1468*)            BEGIN CONSTANT(FSYS+(.COLON.),LSP1,LVALU);
(*1469*)              SUBRNGS(LSP1,LVALU)
(*1470*)            END;
(*1471*)        END;
(*1472*)      IF PACKFLAG THEN CHECKPACK(LSP);
(*1473*)      FSP:=LSP;
(*1474*)      TEST2(FSYS,6,(..));
(*1475*)     END
(*1476*)      ELSE FSP := NIL
(*1477*)   END (*SIMPLETYPE*) ;
(*1478*) 
$TITLE  FIELDLIST,FIELDADDRESS
(*1479*)   PROCEDURE FIELDLIST(FSYS: SETOFSYS; VAR FRECVAR: STP;
(*1480*)             VAR FFSTFLD: CTP; VAR FTYP: BOOLEAN);
(*1481*)             (* FTYP IS TRUE IFF A FIELD OF THE LIST IS OR CONTAINS A FILE *)
(*1482*)    VAR A, LCP,LCP1,NXT,NXT1: CTP; LSP,LSP1,LSP2,LSP3,LSP4: STP;
(*1483*)      SAVEDISPL,MAXDISPL : ADDRRANGE; SAVESIZE,MAXSIZE: CELLUNIT;
(*1484*)      LVALU : VALU;
(*1485*)      LFILTYP: BOOLEAN;
(*1486*) 
(*1487*)    PROCEDURE FIELDADDRESS(FCP: CTP; FSP: STP);
(*1488*)      BEGIN
(*1489*)        WITH FCP@,FSP@ DO
(*1490*)          IF FSP=NIL THEN FLDADDR:=DISPL
(*1491*)          ELSE BEGIN ALIGNMENT(DISPL,SIZE.BOUNDARY); FLDADDR:=DISPL;
(*1492*)                     DISPL:=DISPL+SIZE.WBLENGTH;
(*1493*)                     IF LSIZE<SIZE.BOUNDARY THEN LSIZE:=SIZE.BOUNDARY;
(*1494*)              END;
(*1495*)      END;
(*1496*) 
(*1497*)   BEGIN (*FIELDLIST*) NXT1 := NIL; LSP := NIL;
(*1498*)    LSP1 := NIL;                                                                
(*1499*)    FTYP := FALSE;
(*1500*)    TEST2(FSYS+(.IDENT,CASESY.),19,(..));
(*1501*)    WHILE SY = IDENT DO
(*1502*)     BEGIN NXT := NXT1;
(*1503*)      LOOP
(*1504*)        IF SY = IDENT THEN
(*1505*)          BEGIN NEW(LCP,FIELD);
(*1506*)            WITH LCP@ DO
(*1507*)              BEGIN NAME:=ID; IDTYPE:=NIL; NEXT:=NXT; END;
(*1508*)            NXT:=LCP; ENTERID(LCP); INSYMBOL;
(*1509*)          END
(*1510*)        ELSE ERROR(2);
(*1511*)        TEST2((.COMMA,COLON.),6,FSYS+(.SEMICOLON,CASESY.));
(*1512*)       IF SY<>COMMA THEN EXIT; INSYMBOL;
(*1513*)     END;
(*1514*)      TEST1(COLON,5);
(*1515*)      TYP(FSYS+(.CASESY,SEMICOLON.),LSP,PACKFLAG);
(*1516*)      IF LSP<>NIL THEN FTYP:=FTYP OR LSP@.FTYPE;
(*1517*)      WHILE NXT <> NXT1 DO
(*1518*)        WITH NXT@ DO
(*1519*)          BEGIN IDTYPE := LSP;
(*1520*)            NXT:=NEXT;
(*1521*)          END;
(*1522*)       NXT1:=LCP;
(*1523*)       IF SY = SEMICOLON THEN
(*1524*)         BEGIN INSYMBOL;
(*1525*)           TEST2(FSYS+(.IDENT,CASESY.),19,(..));
(*1526*)         END
(*1527*)      END (*WHILE*);
(*1528*)    REVERSE(NXT1,FFSTFLD);
(*1529*)    NXT:=FFSTFLD;
(*1530*)    WHILE NXT<>NIL DO
(*1531*)      BEGIN FIELDADDRESS(NXT,NXT@.IDTYPE); NXT:=NXT@.NEXT; END;
(*1532*)    IF SY = CASESY THEN
(*1533*)     BEGIN NEW(LSP,TAGFIELD);
(*1534*)      WITH LSP@ DO
(*1535*)       BEGIN TGFLDP:=NIL; FSTVAR:=NIL;
(*1536*)             FTYPE:=FALSE;
(*1537*)       END;
(*1538*)      FRECVAR := LSP;
(*1539*)      INSYMBOL;
(*1540*)      IF SY = IDENT THEN
(*1541*)       BEGIN PRTERR := FALSE; SEARCHID((.TYPES.),LCP1); PRTERR := TRUE;
(*1542*)        NEW(LCP,FIELD);
(*1543*)        WITH LCP@ DO
(*1544*)         BEGIN IDTYPE:=NIL; NEXT:=NIL END;
(*1545*)        IF LCP1 = NIL THEN   (*EXPLICIT TAGFIELD*)
(*1546*)         BEGIN LCP@.NAME := ID; ENTERID(LCP);
(*1547*)          INSYMBOL;
(*1548*)          TEST1(COLON,5);
(*1549*)          IF SY = IDENT THEN SEARCHID((.TYPES.),LCP1)
(*1550*)          ELSE
(*1551*)           BEGIN ERROR(2); SKIP(FSYS+(.OFSY,LPARENT.));
(*1552*)            LCP1 := NIL
(*1553*)           END
(*1554*)         END
(*1555*)        ELSE LCP@.NAME := '        ';
(*1556*)        INSYMBOL;
(*1557*)        IF LCP1<>NIL THEN LSP1:=LCP1@.IDTYPE;
(*1558*)        IF PACKFLAG THEN CHECKPACK(LSP1);
(*1559*)        IF LSP1 <> NIL THEN
(*1560*)          BEGIN
(*1561*)            IF LSP1@.FORM>SUBRANGE THEN ERROR(110)
(*1562*)              ELSE IF LSP1=REALPTR THEN ERROR(109)
(*1563*)              ELSE
(*1564*)                BEGIN LSP@.TGFLDP := LCP;
(*1565*)                  WITH LCP@ DO
(*1566*)                    BEGIN IDTYPE := LSP1;
(*1567*)                      IF NAME <> '        ' THEN
(*1568*)                        FIELDADDRESS(LCP,LSP1)
(*1569*)                    END
(*1570*)                END
(*1571*)           END;
(*1572*)       END
(*1573*)      ELSE
(*1574*)       BEGIN ERROR(2); SKIP(FSYS+(.OFSY,LPARENT.)) END;
(*1575*)      LSP@.SIZE.WBLENGTH := DISPL;
(*1576*)      LSP@.SIZE.BOUNDARY := LSIZE;
(*1577*)      TEST1(OFSY,8);
(*1578*)      LSP1 := NIL; SAVEDISPL := DISPL; MAXDISPL := DISPL;
(*1579*)      SAVESIZE := LSIZE; MAXSIZE := LSIZE;
(*1580*)      (*LOOP UNTIL SY <> SEMICOLON:*)
(*1581*)      LOOP
(*1582*)       IF NOT (SY IN FSYS+(.SEMICOLON.)) THEN
(*1583*)         BEGIN LSP2 := NIL;
(*1584*)           LOOP CONSTANT(FSYS+(.COMMA,COLON,LPARENT.),LSP3,LVALU);
(*1585*)             IF LSP@.TGFLDP <> NIL THEN
(*1586*)               IF NOT COMPTYPES(LSP@.TGFLDP@.IDTYPE,LSP3) THEN
(*1587*)                 ERROR(111);
(*1588*)            NEW(LSP3,VARIANT);
(*1589*)            WITH LSP3@ DO
(*1590*)              BEGIN NXTVAR := LSP1; SUBVAR := LSP2; VARVAL := LVALU.IVAL;
(*1591*)                    FTYPE:=FALSE;
(*1592*)              END;
(*1593*)            LSP1 := LSP3; LSP2 := LSP3;
(*1594*)            IF SY<>COMMA THEN EXIT; INSYMBOL;
(*1595*)           END;
(*1596*)           TEST1(COLON,5);
(*1597*)           TEST1(LPARENT,9);
(*1598*)           FIELDLIST(FSYS+(.RPARENT,SEMICOLON.),LSP2,LCP,LFILTYP);
(*1599*)           IF LFILTYP THEN BEGIN ERROR(108); FTYP:=TRUE; END;
(*1600*)           IF DISPL>MAXDISPL THEN MAXDISPL:=DISPL;
(*1601*)           IF LSIZE>MAXSIZE THEN MAXSIZE:=LSIZE;
(*1602*)           WHILE LSP3 <> NIL DO
(*1603*)             WITH LSP3@ DO
(*1604*)               BEGIN LSP4 := SUBVAR; SUBVAR := LSP2;
(*1605*)                 SIZE.WBLENGTH:=DISPL;SIZE.BOUNDARY:=LSIZE;
(*1606*)                 FSTVARFLD := LCP;
(*1607*)                 LSP3 := LSP4
(*1608*)               END;
(*1609*)           IF SY = RPARENT THEN
(*1610*)            BEGIN INSYMBOL;
(*1611*)              TEST2(FSYS+(.SEMICOLON.),6,(. .));
(*1612*)            END
(*1613*)           ELSE ERROR(4);
(*1614*)         END (*NOT (SY IN ...*) ;
(*1615*)       IF SY<>SEMICOLON THEN EXIT;
(*1616*)       DISPL:=SAVEDISPL; LSIZE:=SAVESIZE; INSYMBOL;
(*1617*)     END;
(*1618*)      DISPL := MAXDISPL; LSIZE := MAXSIZE;
(*1619*)      LSP@.FSTVAR := LSP1;
(*1620*)     END
(*1621*)    ELSE
(*1622*)     FRECVAR := NIL
(*1623*)   END (*FIELDLIST*) ;
(*1624*) 
$TITLE  FILETYPE
(*1625*)    PROCEDURE FILETYPE;
(*1626*)      VAR COMPONENT,S:STP;
(*1627*)      BEGIN INSYMBOL;
(*1628*)        TEST1(OFSY,8);
(*1629*)        NEW(LSP,FILES);
(*1630*)        WITH LSP@ DO
(*1631*)          BEGIN FILTYPE:=NIL; FTYPE:=TRUE;
(*1632*)                TEXTFILE:=FALSE; SIZE.WBLENGTH:=16; SIZE.BOUNDARY:=4;
(*1633*)          END;
(*1634*)        TYP(FSYS,COMPONENT,PACKFLAG);
(*1635*)        IF COMPONENT<>NIL THEN
(*1636*)          IF COMPONENT@.FTYPE THEN
(*1637*)            BEGIN ERROR(108); COMPONENT:=NIL; END
(*1638*)          ELSE IF COMPONENT@.SIZE.WBLENGTH>=4096 THEN
(*1639*)            BEGIN ERROR(184); COMPONENT:=NIL; END;
(*1640*)        IF COMPONENT<>NIL THEN
(*1641*)          WITH LSP@ DO
(*1642*)            BEGIN FILTYPE:=COMPONENT; TEXTFILE:=COMPTYPES(COMPONENT,CHARPTR);
(*1643*)              IF TEXTFILE
(*1644*)                THEN BEGIN SIZE.WBLENGTH:=TEXTSIZE; SIZE.BOUNDARY:=4;
(*1645*)                       IF COMPONENT@.FORM=PACKDTYPE
(*1646*)                         THEN S:=COMPONENT
(*1647*)                         ELSE BEGIN NEW(S,PACKDTYPE);
(*1648*)                                WITH S@ DO
(*1649*)                                  BEGIN SIZE.WBLENGTH:=1; SIZE.BOUNDARY:=1;
(*1650*)                                    FTYPE:=FALSE; BASETYPE:=COMPONENT;
(*1651*)                                  END;
(*1652*)                              END;
(*1653*)                            FILTYPE:=S;
(*1654*)                     END
(*1655*)                ELSE BEGIN SIZE.WBLENGTH:=COMPONENT@.SIZE.WBLENGTH+8;
(*1656*)                           SIZE.BOUNDARY:=COMPONENT@.SIZE.BOUNDARY;
(*1657*)                           ALIGNMENT(SIZE.WBLENGTH,4); ALIGNMENT(SIZE.BOUNDARY,4);
(*1658*)                     END;
(*1659*)            END;
(*1660*)      END;
(*1661*) 
$TITLE  TYP - (BODY)
(*1662*)  BEGIN (*TYP*) LSP := NIL;
(*1663*)   TEST2(TYPEBEGSYS,10,FSYS);
(*1664*)   IF SY IN TYPEBEGSYS THEN
(*1665*)    BEGIN
(*1666*)     IF SY IN SIMPTYPEBEGSYS THEN SIMPLETYPE(FSYS,LSP,PACKFLAG)
(*1667*)     ELSE
(*1668*)  (*@*)
(*1669*)      IF SY = ARROW THEN
(*1670*)       BEGIN NEW(LSP,POINTER);
(*1671*)        WITH LSP@ DO
(*1672*)         BEGIN ELTYPE := NIL; FTYPE := FALSE;
(*1673*)          INITSIZE(SIZE)
(*1674*)         END;
(*1675*)        INSYMBOL;
(*1676*)        IF SY = IDENT THEN
(*1677*)         BEGIN PRTERR := FALSE;   (*NO ERROR IF SEARCH NOT SUCCESSFUL*)
(*1678*)          SEARCHID((.TYPES.),LCP); PRTERR := TRUE;
(*1679*)          IF LCP = NIL THEN   (*FORWARD REFERENCED TYPE ID*)
(*1680*)           BEGIN NEW(LCP,TYPES);
(*1681*)            WITH LCP@ DO
(*1682*)             BEGIN NAME := ID; IDTYPE := LSP;
(*1683*)              NEXT := FWPTR
(*1684*)             END;
(*1685*)            FWPTR := LCP
(*1686*)           END
(*1687*)          ELSE
(*1688*)           BEGIN
(*1689*)            IF LCP@.IDTYPE <> NIL THEN
(*1690*)             IF LCP@.IDTYPE@.FTYPE THEN ERROR(108)
(*1691*)               ELSE LSP@.ELTYPE:=LCP@.IDTYPE;
(*1692*)           END;
(*1693*)          INSYMBOL;
(*1694*)         END
(*1695*)        ELSE ERROR(2);
(*1696*)       END
(*1697*)      ELSE
(*1698*)       BEGIN
(*1699*)        IF SY = PACKEDSY THEN
(*1700*)         BEGIN PACKFLAG := TRUE; INSYMBOL END;
(*1701*)         TEST2(TYPEDELS,10,FSYS);
(*1702*)  (*ARRAY*)
(*1703*)        IF SY = ARRAYSY THEN
(*1704*)         BEGIN INSYMBOL;
(*1705*)          TEST1(LBRACK,11);
(*1706*)          LSP1 := NIL;
(*1707*)          LOOP NEW(LSP,ARRAYS);
(*1708*)           WITH LSP@ DO
(*1709*)            BEGIN AELTYPE := LSP1; INXTYPE := NIL;
(*1710*)             FTYPE := FALSE; INITSIZE(SIZE)
(*1711*)            END;
(*1712*)           LSP1 := LSP;
(*1713*)           SIMPLETYPE(FSYS+(.COMMA,RBRACK,OFSY.),LSP2,FALSE);
(*1714*)           IF LSP2 <> NIL THEN
(*1715*)            IF LSP2@.FORM <= SUBRANGE THEN
(*1716*)             IF LSP2 = INTPTR THEN ERROR(149)
(*1717*)             ELSE
(*1718*)              IF LSP2=REALPTR THEN ERROR(112)
(*1719*)             ELSE LSP@.INXTYPE := LSP2
(*1720*)            ELSE ERROR(113);
(*1721*)           IF SY<>COMMA THEN EXIT; INSYMBOL;
(*1722*)         END;
(*1723*)          TEST1(RBRACK,12);
(*1724*)          TEST1(OFSY,8);
(*1725*)          TYP(FSYS,LSP,PACKFLAG);
(*1726*)          (*REVERSE POINTERS, COMPUTE SIZE *)
(*1727*)          IF LSP <> NIL THEN
(*1728*)           BEGIN
(*1729*)            REPEAT
(*1730*)             WITH LSP1@ DO
(*1731*)              BEGIN LSP2 := AELTYPE; AELTYPE := LSP;
(*1732*)               FTYPE := LSP@.FTYPE;
(*1733*)               IF INXTYPE<>NIL THEN
(*1734*)                 BEGIN AELLENG:=AELTYPE@.SIZE.WBLENGTH;
(*1735*)                   ALIGNMENT(AELLENG,AELTYPE@.SIZE.BOUNDARY); GETBOUNDS(INXTYPE,LMIN,LMAX);
(*1736*)                   SIZE.WBLENGTH:=AELLENG*(LMAX-LMIN+1);
(*1737*)                   SIZE.BOUNDARY:=AELTYPE@.SIZE.BOUNDARY;
(*1738*)                 END;
(*1739*)              END (*WITH LSP1@*) ;
(*1740*)             LSP := LSP1; LSP1 := LSP2
(*1741*)            UNTIL LSP1 = NIL
(*1742*)           END (*LSP <> NIL*)
(*1743*)         END
(*1744*)        ELSE
(*1745*)  (*RECORD*)
(*1746*)         IF SY = RECORDSY THEN
(*1747*)          BEGIN INSYMBOL;
(*1748*)           OLDTOP := TOP;
(*1749*)           IF TOP < DISPLIMIT THEN
(*1750*)            BEGIN TOP := TOP + 1;
(*1751*)             WITH DISPLAY(.TOP.) DO
(*1752*)              BEGIN FNAME := NIL; OCCUR := REC END
(*1753*)            END
(*1754*)           ELSE ERROR(250);
(*1755*)           DISPL:=0; LSIZE:=1;
(*1756*)           FIELDLIST(FSYS-(.SEMICOLON.)+(.ENDSY.),LSP1,LCP,LFILTYP);
(*1757*)           NEW(LSP,RECORDS);
(*1758*)           WITH LSP@ DO
(*1759*)            BEGIN FIELDS := DISPLAY(.TOP.).FNAME; FTYPE := LFILTYP;
(*1760*)             FSTFLD := LCP; RECVAR := LSP1;
(*1761*)             SIZE.WBLENGTH:=DISPL; SIZE.BOUNDARY:=LSIZE;
(*1762*)            END;
(*1763*)           TOP := OLDTOP;
(*1764*)           TEST1(ENDSY,13);
(*1765*)          END
(*1766*)         ELSE
(*1767*)  (*SET*)
(*1768*)         IF SY = SETSY THEN
(*1769*)           BEGIN INSYMBOL;
(*1770*)            TEST1(OFSY,8);
(*1771*)            NEW(LSP,POWER);
(*1772*)            WITH LSP@ DO
(*1773*)              BEGIN ELSET:=NIL; PCKDSET:=FALSE; FTYPE:=FALSE;
(*1774*)                    SIZE.WBLENGTH:=8; SIZE.BOUNDARY:=8;
(*1775*)              END;
(*1776*)            SIMPLETYPE(FSYS,LSP1,FALSE);
(*1777*)            IF LSP1 <> NIL THEN
(*1778*)              IF LSP1@.FORM > SUBRANGE THEN ERROR(115)
(*1779*)                ELSE IF LSP1=REALPTR THEN ERROR(114)
(*1780*)                  ELSE IF LSP1=INTPTR THEN ERROR(169)
(*1781*)                    ELSE
(*1782*)                      BEGIN GETBOUNDS(LSP1,LMIN,LMAX);
(*1783*)                        IF (LMIN < SETMIN)OR (LMAX > SETMAX) THEN ERROR(169);
(*1784*)                        LSP@.ELSET:=LSP1;
(*1785*)                      END
(*1786*)                END
(*1787*)          ELSE
(*1788*)  (*FILE*) IF SY = FILESY THEN FILETYPE;
(*1789*)       END;
(*1790*)       TEST2(FSYS,6,(. .));
(*1791*)    END;
(*1792*)   FSP := LSP
(*1793*)  END (*TYP*) ;
(*1794*) 
$TITLE LABEL DECLARATIONS
(*1795*)  PROCEDURE LABELDECLARATION;
(*1796*)    LABEL 1;
(*1797*)    VAR LLP: LBP;
(*1798*)    BEGIN
(*1799*)      REPEAT INSYMBOL;
(*1800*)        IF SY = INTCONST THEN
(*1801*)          BEGIN LLP := FSTLABP;
(*1802*)            WHILE LLP <> FLABP DO
(*1803*)              IF LLP@.LABVAL = IVAL THEN
(*1804*)                BEGIN ERROR(166); GOTO 1 END
(*1805*)              ELSE LLP := LLP@.NEXTLAB;
(*1806*)            NEW(LLP);
(*1807*)            WITH LLP@ DO
(*1808*)              BEGIN LABVAL := IVAL; DEFINED := FALSE; NEXTLAB := FSTLABP;
(*1809*)                    LCNT:=0; FSTOCC:=NIL;
(*1810*)              END;
(*1811*)            FSTLABP := LLP;
(*1812*)        1:  INSYMBOL
(*1813*)          END
(*1814*)        ELSE ERROR(15);
(*1815*)        TEST2(FSYS+(.COMMA,SEMICOLON.),6,(. .));
(*1816*)      UNTIL SY<>COMMA;
(*1817*)      IF SY = SEMICOLON THEN INSYMBOL ELSE ERROR(14)
(*1818*)    END (*LABELDECLARATION*) ;
(*1819*) 
$TITLE CONST DECLARATIONS
(*1820*)  PROCEDURE CONSTDECLARATION;
(*1821*)    VAR LCP:CTP; LSP:STP; LVALU:VALU; EXPR:CEP;
(*1822*)    BEGIN
(*1823*)      IF SY <> IDENT THEN
(*1824*)        BEGIN ERROR(2); SKIP(FSYS+(.IDENT.)) END;
(*1825*)      WHILE SY = IDENT DO
(*1826*)        BEGIN NEW(LCP,KONST);
(*1827*)          WITH LCP@ DO
(*1828*)            BEGIN NAME := ID; IDTYPE := NIL; NEXT := NIL;
(*1829*)            END;
(*1830*)          INSYMBOL;
(*1831*)          IF OP = EQOP THEN INSYMBOL ELSE ERROR(16);
(*1832*)          IF SY=LCBRACK THEN
(*1833*)          BEGIN
(*1834*)              IF EXTWARN THEN ERROR(291);
(*1835*)              CONSTEXPRESSION(FSYS+(.COLON,SEMICOLON.),EXPR);
(*1836*)              IF SY=COLON THEN INSYMBOL ELSE ERROR(5);
(*1837*)              TYP(FSYS+(.SEMICOLON.)+TYPEDELS,LSP,FALSE);
(*1838*)              CONSTIMAGE(LSP,EXPR,LVALU);
(*1839*)            END
(*1840*)          ELSE CONSTANT(FSYS+(.SEMICOLON.),LSP,LVALU);
(*1841*)          ENTERID(LCP);
(*1842*)          LCP@.IDTYPE := LSP; LCP@.VALUES := LVALU;
(*1843*)          IF SY = SEMICOLON THEN
(*1844*)            BEGIN INSYMBOL;
(*1845*)              TEST2(FSYS+(.IDENT.),6,(. .));
(*1846*)            END
(*1847*)          ELSE ERROR(14)
(*1848*)        END
(*1849*)    END (*CONSTDECLARATION*) ;
(*1850*) 
$TITLE  UNDEFINED
(*1851*)    PROCEDURE UNDEFINED(VAR F:CTP; STRING:PACKED ARRAY(.1..9.) OF CHAR);
(*1852*)      VAR I,SAVECNT:INTEGER;
(*1853*)      BEGIN
(*1854*)        IF F<>NIL THEN
(*1855*)          BEGIN ERROR(117); SAVECNT:=CHCNT; ENDOFLINE;
(*1856*)            REPEAT ENDOFLINE;
(*1857*)                   WRITELN(' UNDEFINED ',STRING,'  ',F@.NAME);
(*1858*)                   F:=F@.NEXT;
(*1859*)            UNTIL F=NIL;
(*1860*)          END;
(*1861*)      END;
(*1862*) 
$TITLE TYPE DECLARATIONS
(*1863*)  PROCEDURE TYPEDECLARATION;
(*1864*)    VAR LCP,LCP1,LCP2: CTP; LSP: STP;
(*1865*)    BEGIN
(*1866*)      IF SY <> IDENT THEN
(*1867*)        BEGIN ERROR(2); SKIP(FSYS+(.IDENT.)) END;
(*1868*)      WHILE SY = IDENT DO
(*1869*)        BEGIN NEW(LCP,TYPES);
(*1870*)          WITH LCP@ DO
(*1871*)            BEGIN NAME := ID; IDTYPE := NIL END;
(*1872*)          INSYMBOL;
(*1873*)          IF OP = EQOP THEN INSYMBOL ELSE ERROR(16);
(*1874*)          TYP(FSYS+(.SEMICOLON.),LSP,FALSE);
(*1875*)          ENTERID(LCP);
(*1876*)          LCP@.IDTYPE := LSP;
(*1877*)          (*HAS ANY FORWARD REFERENCE BEEN SATISFIED:*)
(*1878*)          LCP1 := FWPTR;
(*1879*)          WHILE LCP1 <> NIL DO
(*1880*)            BEGIN
(*1881*)              IF LCP1@.NAME = LCP@.NAME THEN
(*1882*)                BEGIN
(*1883*)                  WITH LCP@ DO
(*1884*)                    BEGIN LCP1@.IDTYPE@.ELTYPE := IDTYPE;
(*1885*)                      IF IDTYPE <> NIL THEN
(*1886*)                        IF IDTYPE@.FTYPE THEN ERROR(108)
(*1887*)                    END;
(*1888*)                  IF LCP1 <> FWPTR THEN
(*1889*)                    LCP2@.NEXT := LCP1@.NEXT
(*1890*)                  ELSE FWPTR := LCP1@.NEXT;
(*1891*)                END;
(*1892*)              LCP2 := LCP1; LCP1 := LCP1@.NEXT
(*1893*)            END;
(*1894*)          IF SY = SEMICOLON THEN
(*1895*)            BEGIN INSYMBOL;
(*1896*)              TEST2(FSYS+(.IDENT.),6,(. .));
(*1897*)            END
(*1898*)          ELSE ERROR(14)
(*1899*)        END;
(*1900*)      UNDEFINED(FWPTR,'TYPE-ID  ');
(*1901*)    END (*TYPEDECLARATION*) ;
(*1902*) 
$TITLE VAR DECLARATIONS, ADDRESS
(*1903*)    PROCEDURE ADDRESS(FCP:CTP);
(*1904*)      BEGIN ALIGNMENT(LC,4);
(*1905*)        WITH FCP@ DO
(*1906*)          IF KLASS=VARS THEN
(*1907*)            IF VKIND=DRCT THEN
(*1908*)              BEGIN IF IDTYPE<>NIL
(*1909*)                THEN BEGIN ALIGNMENT(LC,IDTYPE@.SIZE.BOUNDARY); VADDR:=LC;
(*1910*)                           LC:=VADDR+IDTYPE@.SIZE.WBLENGTH;
(*1911*)                     END
(*1912*)                ELSE BEGIN VADDR:=LC; LC:=LC+4; END
(*1913*)              END
(*1914*)            ELSE BEGIN PARADDR:=LC; LC:=LC+4 END
(*1915*)          ELSE IF (KLASS=PROC) OR (KLASS=FUNC)
(*1916*)           THEN BEGIN PFADDR:=LC; LC:=LC+8 END
(*1917*)            ELSE ERROR(400);
(*1918*)      END;
(*1919*) 
(*1920*)  PROCEDURE VARDECLARATION;
(*1921*)    VAR LCP,NXT: CTP; LSP: STP;
(*1922*)    BEGIN NXT := NIL;
(*1923*)      REPEAT
(*1924*)        LOOP
(*1925*)          IF SY = IDENT THEN
(*1926*)            BEGIN NEW(LCP,VARS);
(*1927*)              WITH LCP@ DO
(*1928*)                BEGIN NAME := ID; NEXT := NXT;
(*1929*)                      IDTYPE := NIL; VKIND := DRCT; VLEV := LEVEL
(*1930*)                END;
(*1931*)              ENTERID(LCP); NXT:=LCP; INSYMBOL;
(*1932*)            END
(*1933*)          ELSE ERROR(2);
(*1934*)          TEST2(FSYS+(.COMMA,COLON.)+TYPEDELS,6,(.SEMICOLON.));
(*1935*)          IF SY<>COMMA THEN EXIT; INSYMBOL;
(*1936*)        END;
(*1937*)        TEST1(COLON,5);
(*1938*)        TYP(FSYS+(.SEMICOLON.)+TYPEDELS,LSP,FALSE);
(*1939*)        WHILE NXT <> NIL DO
(*1940*)          WITH  NXT@ DO
(*1941*)            BEGIN IDTYPE := LSP; ADDRESS(NXT);
(*1942*)                 NXT := NEXT
(*1943*)            END;
(*1944*)        IF SY = SEMICOLON THEN
(*1945*)          BEGIN INSYMBOL;
(*1946*)            TEST2(FSYS+(.IDENT.),6,(. .));
(*1947*)          END
(*1948*)        ELSE ERROR(14)
(*1949*)      UNTIL (SY <> IDENT)AND NOT (SY IN TYPEDELS);
(*1950*)      UNDEFINED(FWPTR,'TYPE-ID  ');
(*1951*)    END (*VARDECLARATION*);
(*1952*) 
$TITLE  VARINIT,DATA1,INITDATA
(*1953*) 
(*1954*)   PROCEDURE DATA1(X:INTEGER);
(*1955*)     BEGIN $PASOBJ1@(.OBPOINTER.):=X; OBPOINTER:=OBPOINTER+1;
(*1956*)         IF OBPOINTER = OBJLENGTH THEN
(*1957*)         BEGIN PUT($PASOBJ1); OBPOINTER:=0; END;
(*1958*)     END;
(*1959*)  PROCEDURE VARINITIALIZATION;
(*1960*)   VAR LCP:CTP; LSP:STP; LVALU:VALU; EXPR:CEP;
(*1961*) 
(*1962*)   PROCEDURE INITDATA(FCP:CTP; FVALU:VALU);
(*1963*)     VAR A1,A2,X:INTEGER; P:CTAILP;
(*1964*)     BEGIN
(*1965*)       CASE FVALU.CKIND OF
(*1966*)        INT : BEGIN                                                             
(*1967*)                IF FCP@.IDTYPE@.FORM=SUBRANGE THEN                              
(*1968*)                  IF (FVALU.IVAL>FCP@.IDTYPE@.MAX) OR                           
(*1969*)                     (FVALU.IVAL<FCP@.IDTYPE@.MIN) THEN ERROR(303);             
(*1970*)                DATA1(4); DATA1(FCP@.VADDR);                                    
(*1971*)                DATA1(FVALU.IVAL);                                              
(*1972*)              END;                                                              
(*1973*)         REEL,PSET:
(*1974*)               BEGIN SETVALUE(FVALU.PVAL,A1,A2); DATA1(8);
(*1975*)                     DATA1(FCP@.VADDR); DATA1(A1); DATA1(A2);
(*1976*)               END;
(*1977*)         STRG: IF FCP@.IDTYPE<>NIL THEN
(*1978*)                 BEGIN P:=FVALU.VALP; X:=FCP@.IDTYPE@.SIZE.WBLENGTH;
(*1979*)                   ALIGNMENT(X,4); DATA1(X); DATA1(FCP@.VADDR);
(*1980*)                   WHILE P<>NIL DO
(*1981*)                     BEGIN DATA1(P@.STFR); P:=P@.NXTCSP; END;
(*1982*)                 END
(*1983*)       END;
(*1984*)     END;
(*1985*) 
$TITLE  VARINIT - BODY
(*1986*)    BEGIN (*VARINITIALIZATION*)
(*1987*)      IF LEVEL<>1 THEN
(*1988*)        BEGIN ERROR(220); SKIP(FSYS); END
(*1989*)      ELSE
(*1990*)        BEGIN
(*1991*)          IF SY<>IDENT THEN
(*1992*)            BEGIN ERROR(2); SKIP(FSYS+(.IDENT.)); END;
(*1993*)          WHILE SY=IDENT DO
(*1994*)            BEGIN SEARCHID((.VARS.),LCP); INSYMBOL;
(*1995*)              INITNUMBER:=INITNUMBER+1;
(*1996*)              TEST1(BECOMES,51);
(*1997*)              IF SY IN CONSTBEGSYS THEN
(*1998*)                BEGIN CONSTANT(FSYS+(.SEMICOLON.),LSP,LVALU);
(*1999*)                  IF COMPTYPES(LSP,LCP@.IDTYPE)
(*2000*)                    THEN INITDATA(LCP,LVALU)
(*2001*)                    ELSE ERROR(221);
(*2002*)                END
(*2003*)              ELSE IF SY=LCBRACK THEN
(*2004*)                BEGIN CONSTEXPRESSION(FSYS+(.SEMICOLON.),EXPR);
(*2005*)                  CONSTIMAGE(LCP@.IDTYPE,EXPR,LVALU);
(*2006*)                  INITDATA(LCP,LVALU);
(*2007*)                END
(*2008*)              ELSE BEGIN ERROR(50); SKIP(FSYS+(.SEMICOLON.)); END;
(*2009*)              IF SY<>SEMICOLON THEN ERROR(14)
(*2010*)                ELSE BEGIN INSYMBOL;
(*2011*)                      TEST2(FSYS+(.IDENT.),6,(. .));
(*2012*)                     END;
(*2013*)            END;
(*2014*)          OBCLEAR;
(*2015*)        END;
(*2016*)    END;
(*2017*) 
(*2018*) 
$TITLE PROCEDURE/FUNCTION DECLARATIONS
(*2019*)  PROCEDURE PROCDECLARATION(FSY: SYMBOL);
(*2020*)   (* 'FSY' WILL BE EITHER 'PROCSY' OR 'FUNCTSY' *)
(*2021*)   VAR OLDLEV: 0..MAXLEVEL; LSY: SYMBOL; LCP,LCP1,COMPARE,SAVE: CTP; LSP: STP;
(*2022*)     FORW: BOOLEAN; OLDTOP: DISPRANGE;
(*2023*)     LLC: ADDRRANGE; LP : MARKP;
(*2024*) 
(*2025*)   PROCEDURE PARAMETERLIST(FSY: SETOFSYS; VAR FPAR: CTP);
(*2026*)    VAR LCP,LCP1,LCP2,LCP3: CTP; LSP: STP; LKIND: DRCTINDRCT;
(*2027*) 
(*2028*)    PROCEDURE DEFINESKELETON(FID:IDCLASS);
(*2029*)      VAR LCP,WORK1,WORK2,SKLTOP:CTP; LSP:STP;
(*2030*)          LCSAVE:ADDRRANGE;
(*2031*)      BEGIN INSYMBOL;
(*2032*)        IF SY<>IDENT THEN ERROR(2)
(*2033*)        ELSE
(*2034*)          BEGIN LCSAVE:=LC;
(*2035*)            IF FID=PROC THEN BEGIN NEW(LCP,PROC,DECLARED,FORMAL); LC:=64; END
(*2036*)                        ELSE BEGIN NEW(LCP,FUNC,DECLARED,FORMAL); LC:=72; END;
(*2037*)            WITH LCP@ DO
(*2038*)              BEGIN NAME:=ID; IDTYPE:=NIL; NEXT:=LCP1;
(*2039*)                    PFLEV:=LEVEL; PARAMS:=NIL;
(*2040*)              END;
(*2041*)            INSYMBOL;
(*2042*)            IF SY=LPARENT THEN
(*2043*)             BEGIN
(*2044*)                IF EXTWARN THEN ERROR(291);
(*2045*)                SKLTOP:=NIL;
(*2046*)                REPEAT INSYMBOL;
(*2047*)                  TYP(FSYS+(.COMMA,RPARENT.),LSP,FALSE);
(*2048*)                  IF LSP<>NIL THEN IF LSP@.FTYPE THEN ERROR(121);
(*2049*)                  NEW(WORK1,VARS);
(*2050*)                  WITH WORK1@ DO
(*2051*)                    BEGIN NAME:='        '; IDTYPE:=LSP; VKIND:=DRCT;
(*2052*)                          NEXT:=SKLTOP; VLEV:=LEVEL+1;
(*2053*)                    END;
(*2054*)                  SKLTOP:=WORK1; ADDRESS(WORK1);
(*2055*)                UNTIL SY<>COMMA;
(*2056*)                REVERSE(SKLTOP,LCP@.PARAMS);
(*2057*)                TEST1(RPARENT,4);
(*2058*)              END;
(*2059*)            ENTERID(LCP); LCP1:=LCP; LC:=LCSAVE;
(*2060*)          END;
(*2061*)      END;
(*2062*) 
(*2063*)   BEGIN (*PARAMETERLIST*)
(*2064*)    LCP1:=NIL;
(*2065*)    TEST2(FSY+(.LPARENT.),7,FSYS);
(*2066*)    IF SY = LPARENT THEN
(*2067*)     BEGIN IF FORW THEN ERROR(119);
(*2068*)      INSYMBOL;
(*2069*)      IF NOT (SY IN (.IDENT,VARSY,PROCSY,FUNCTSY.)) THEN
(*2070*)       BEGIN ERROR(7); SKIP(FSYS+(.IDENT,RPARENT.)) END;
(*2071*)      WHILE SY IN (.IDENT,VARSY,PROCSY,FUNCTSY.) DO
(*2072*)       BEGIN
(*2073*)        IF SY = PROCSY THEN
(*2074*)         BEGIN
(*2075*)          REPEAT
(*2076*)            DEFINESKELETON(PROC);
(*2077*)            TEST2(FSYS+(.COMMA,SEMICOLON,RPARENT.),7,(. .));
(*2078*)          UNTIL SY <> COMMA
(*2079*)         END
(*2080*)        ELSE
(*2081*)         BEGIN LCP2 := LCP1; LSP := NIL;
(*2082*)          IF SY = FUNCTSY THEN
(*2083*)           BEGIN
(*2084*)            REPEAT
(*2085*)              DEFINESKELETON(FUNC);
(*2086*)              IF NOT (SY IN (.COMMA,COLON.)+FSYS) THEN
(*2087*)                BEGIN ERROR(7); SKIP(FSYS+(.COMMA,SEMICOLON,RPARENT.))
(*2088*)                END
(*2089*)            UNTIL SY <> COMMA;
(*2090*)            IF SY = COLON THEN
(*2091*)              BEGIN INSYMBOL;
(*2092*)                 IF SY <> IDENT THEN
(*2093*)                  IF EXTWARN THEN ERROR(291);
(*2094*)                    TYP(FSYS+(.SEMICOLON,RPARENT.),LSP,FALSE);
(*2095*)                    IF LSP<>NIL THEN
(*2096*)                      IF NOT (LSP@.FORM IN (.SCALAR,SUBRANGE,POINTER.)) THEN
(*2097*)                        BEGIN ERROR(120); LSP:=NIL; END;
(*2098*)              END
(*2099*)            ELSE ERROR(5)
(*2100*)           END
(*2101*)          ELSE
(*2102*)           BEGIN
(*2103*)            IF SY=VARSY THEN BEGIN LKIND:=INDRCT; INSYMBOL; END
(*2104*)                        ELSE LKIND:=DRCT;
(*2105*)            LOOP
(*2106*)             IF SY = IDENT THEN
(*2107*)               BEGIN NEW(LCP,VARS);
(*2108*)                 WITH LCP@ DO
(*2109*)                   BEGIN NAME := ID; IDTYPE := NIL;
(*2110*)                     VKIND := LKIND; NEXT := LCP1; VLEV := LEVEL;
(*2111*)                   END;
(*2112*)                 ENTERID(LCP); LCP1:=LCP; INSYMBOL;
(*2113*)               END
(*2114*)             ELSE ERROR(2);
(*2115*)             IF NOT (SY IN (.COMMA,COLON.)+FSYS) THEN
(*2116*)               BEGIN ERROR(7); SKIP(FSYS+(.COMMA,SEMICOLON,RPARENT.))
(*2117*)               END;
(*2118*)             IF SY<>COMMA THEN EXIT; INSYMBOL;
(*2119*)           END;
(*2120*)            IF SY = COLON THEN
(*2121*)              BEGIN INSYMBOL;
(*2122*)                TYP(FSYS+(.RPARENT,SEMICOLON.),LSP,FALSE);
(*2123*)                IF LSP<>NIL THEN
(*2124*)                  IF (LKIND=DRCT) AND LSP@.FTYPE THEN ERROR(121);
(*2125*)              END
(*2126*)            ELSE ERROR(5);
(*2127*)           END;
(*2128*)          LCP3 := LCP1;
(*2129*)          WHILE LCP3 <> LCP2 DO
(*2130*)            BEGIN LCP3@.IDTYPE:=LSP; LCP3:=LCP3@.NEXT; END;
(*2131*)         END;
(*2132*)        IF SY = SEMICOLON THEN
(*2133*)         BEGIN INSYMBOL;
(*2134*)          IF NOT (SY IN FSYS+(.IDENT,VARSY,PROCSY,FUNCTSY.)) THEN
(*2135*)           BEGIN ERROR(7); SKIP(FSYS+(.IDENT,RPARENT.)) END
(*2136*)         END
(*2137*)       END (*WHILE*) ;
(*2138*)      IF SY = RPARENT THEN
(*2139*)       BEGIN INSYMBOL;
(*2140*)        TEST2(FSY+FSYS,6,(. .));
(*2141*)       END
(*2142*)      ELSE ERROR(4);
(*2143*)      REVERSE(LCP1,LCP3);
(*2144*)      LCP1 := LCP3;
(*2145*)      WHILE LCP1 <> NIL DO
(*2146*)        BEGIN ADDRESS(LCP1); LCP1:=LCP1@.NEXT; END;
(*2147*)      FPAR := LCP3
(*2148*)     END
(*2149*)      ELSE FPAR := NIL
(*2150*)  END (*PARAMETERLIST*) ;
(*2151*) 
(*2152*)  BEGIN (*PROCDECLARATION*)
(*2153*)   LLC:=LC; FORW:=FALSE;
(*2154*)   DP := TRUE;
(*2155*)   IF FSY=PROCSY THEN LC:=64 ELSE LC:=72;
(*2156*)   IF SY<>IDENT
(*2157*)     THEN BEGIN ERROR(2); LCP:=UFCTPTR; END
(*2158*)     ELSE
(*2159*)       BEGIN COMPARE:=FWPROCS; LCP:=NIL;
(*2160*)         WHILE COMPARE<>NIL DO
(*2161*)           BEGIN
(*2162*)             IF ID=COMPARE@.NAME THEN
(*2163*)               BEGIN LCP:=COMPARE;
(*2164*)                 IF COMPARE=FWPROCS THEN FWPROCS:=COMPARE@.NEXT
(*2165*)                                    ELSE SAVE@.NEXT:=COMPARE@.NEXT;
(*2166*)               END;
(*2167*)             SAVE:=COMPARE; COMPARE:=COMPARE@.NEXT;
(*2168*)           END;
(*2169*)         IF LCP=NIL
(*2170*)           THEN FORW:=FALSE
(*2171*)           ELSE
(*2172*)             BEGIN IF LCP@.KLASS=PROC THEN FORW:=(FSY=PROCSY)
(*2173*)                                      ELSE FORW:=(FSY=FUNCTSY);
(*2174*)                   IF NOT FORW THEN ERROR(160);
(*2175*)             END;
(*2176*)         IF FORW
(*2177*)           THEN LC:=LCP@.LCSAVE
(*2178*)           ELSE
(*2179*)             BEGIN
(*2180*)               IF FSY=PROCSY THEN NEW(LCP,PROC,DECLARED,ACTUAL)
(*2181*)                             ELSE NEW(LCP,FUNC,DECLARED,ACTUAL);
(*2182*)               WITH LCP@ DO
(*2183*)                 BEGIN NAME:=ID; IDTYPE:=NIL; NEXT:=NIL; PFLEV:=LEVEL; PARAMS:=NIL;
(*2184*)                  IF PCNT<MAXPROCFUNC THEN
(*2185*)                  BEGIN
(*2186*)                    PCNT:=PCNT+1;
(*2187*)                    $PASOBJ2@:=ID;
(*2188*)                    PUT($PASOBJ2)
(*2189*)                  END
(*2190*)                  ELSE BEGIN ERROR(261); PCNT:=1 END;
(*2191*)                   PFCNT:=PCNT;
(*2192*)                 END;
(*2193*)               ENTERID(LCP);
(*2194*)             END;
(*2195*)         INSYMBOL;
(*2196*)       END;
(*2197*)   OLDLEV := LEVEL; OLDTOP := TOP;
(*2198*)   IF LEVEL < MAXLEVEL THEN LEVEL := LEVEL + 1 ELSE ERROR(251);
(*2199*)   IF TOP>=DISPLIMIT
(*2200*)     THEN ERROR(250)
(*2201*)     ELSE BEGIN TOP:=TOP+1;
(*2202*)            WITH DISPLAY(.TOP.) DO
(*2203*)              BEGIN OCCUR:=BLCK;
(*2204*)                IF FORW THEN FNAME:=LCP@.PARAMS ELSE FNAME:=NIL;
(*2205*)              END;
(*2206*)          END;
(*2207*)   IF FSY = PROCSY THEN
(*2208*)     BEGIN PARAMETERLIST((.SEMICOLON.),LCP1);
(*2209*)       IF NOT FORW THEN LCP@.PARAMS := LCP1
(*2210*)     END
(*2211*)   ELSE
(*2212*)     BEGIN PARAMETERLIST((.SEMICOLON,COLON.),LCP1);
(*2213*)       IF NOT FORW THEN LCP@.PARAMS := LCP1;
(*2214*)       IF SY=COLON THEN
(*2215*)         BEGIN INSYMBOL; IF FORW THEN ERROR(122);
(*2216*)           TYP(FSYS+(.SEMICOLON.),LSP,FALSE);
(*2217*)           LCP@.IDTYPE:=LSP;
(*2218*)           IF LSP<>NIL THEN
(*2219*)             IF NOT (LSP@.FORM IN (.SCALAR,SUBRANGE,POINTER.)) THEN
(*2220*)               BEGIN ERROR(120); LCP@.IDTYPE:=NIL; END;
(*2221*)         END
(*2222*)       ELSE IF NOT FORW THEN ERROR(123);
(*2223*)     END;
(*2224*)   TEST1(SEMICOLON,14);
(*2225*)   IF (SY = IDENT) AND (ID='FORWARD ') THEN
(*2226*)     BEGIN IF FORW THEN ERROR(161);
(*2227*)       LCP@.LCSAVE:=LC; LCP@.NEXT:=FWPROCS; FWPROCS:=LCP;
(*2228*)       INSYMBOL;
(*2229*)       IF SY = SEMICOLON THEN INSYMBOL ELSE ERROR(14);
(*2230*)       TEST2(FSYS,6,(. .));
(*2231*)     END
(*2232*)   ELSE
(*2233*)     BEGIN MARK(LP);
(*2234*)       REPEAT BLOCK(FSYS,SEMICOLON,LCP);
(*2235*)         IF SY = SEMICOLON THEN
(*2236*)           BEGIN INSYMBOL;
(*2237*)             IF NOT (SY IN (.BEGINSY,PROCSY,FUNCTSY.)) THEN
(*2238*)               BEGIN ERROR(6); SKIP(FSYS) END
(*2239*)           END
(*2240*)         ELSE ERROR(14)
(*2241*)       UNTIL SY IN (.BEGINSY,PROCSY,FUNCTSY.);
(*2242*)       RELEASE(LP);
(*2243*)     END;
(*2244*)   LEVEL := OLDLEV; TOP := OLDTOP; LC := LLC;
(*2245*)  END (*PROCDECLARATION*) ;
(*2246*) 
(*2247*) 
(*2248*) 
(*2249*) 
$TITLE  BODY - (HEADING)
(*2250*)  PROCEDURE BODY(FSYS: SETOFSYS);
(*2251*)    CONST
(*2252*)      ZA=90;     ZAD=106;   ZADR=42;   ZAR=26;
(*2253*)      ZAW=110;   ZBAL=69;   ZBALR=5;   ZBC=71;
(*2254*)      ZBCR=7;    ZBCTR=6;   ZC=89;     ZCD=105;
(*2255*)      ZCDR=41;   ZCL=85;    ZCLC=213;  ZCLR=21;
(*2256*)      ZCR=25;    ZD=93;     ZDD=109;   ZIC=67;
(*2257*)      ZL=88;     ZLA=65;               ZLCDR=35;
(*2258*)      ZLCR=19;   ZLD=104;   ZLM=152;   ZLNDR=33;
(*2259*)      ZLNR=17;   ZLPDR=32;  ZLPR=16;   ZLR=24;
(*2260*)      ZLTDR=34;  ZLTR=18;   ZM=92;     ZMD=108;
(*2261*)      ZMDR=44;   ZMR=28;    ZMVC=210;  ZN=84;
(*2262*)      ZNR=20;    ZO=86;     ZS=91;     ZSD=107;
(*2263*)      ZSDR=43;   ZSLA=139;  ZSLDA=143; ZSLDL=141;
(*2264*)      ZSLL=137;  ZSR=27;    ZSRDA=142; ZSRDL=140;
(*2265*)      ZSRL=136;  ZST=80;    ZSTC=66;   ZSTD=96;
(*2266*)      ZSTM=144;  ZTM=145;   ZX=87;     ZXR=23;
(*2267*)      ZBCT=70; ZEX = 68;
(*2268*)      ZMVI = 146;
(*2269*) 
(*2270*)      PBASE1=14; R0=0; BASEWORK=9;
(*2271*)      NEWPOINTER=7; STACKPOINTER=8;
(*2272*)      SAVEAREA=64;             (*LENGTH OF SAVEAREA*)
(*2273*)      JUMPERR1=376; JUMPERR2=384; JUMPERR3=392; JUMPERR4=400;
(*2274*)      PROCBASE = 560;  NPINIT = 140;
(*2275*)      IRCONVWORK = 152;
(*2276*)      ENTRYSIN=160; ENTRYAL=408; ENTRYCL=416;
(*2277*)      ENTRYWB=256; ENTRYWC=288; ENTRYWI=264; ENTRYWS=296; ENTRYWR1=272; ENTRYWR2=280;
(*2278*)      ENTRYRC=320; ENTRYRI=304; ENTRYRR=312; ENTRYRL=328;
(*2279*)      ENTRYRS = 496;  (* READ STRING ENTRY POINT *)
(*2280*)      ENTRYRET = 80; (* PROCEDURE RETURN DISPLACEMENT *)
(*2281*)      ENTRYVARPROC = 8 ; (* PASSED PROCEDURE CALL *)
(*2282*)      OPENINPUT=336; ENTGETCH=344; ENTWRITLN=472;
(*2283*)      ENTRYCLOCK=424; ENTRYTIME=432;
(*2284*)      ENTRYGET=224; ENTOPEXT=208; ENTCLEXT=216;
(*2285*)      ENTOPLOC=448; ENTCLLOC=456;
(*2286*)      ENTPAGE=464;
(*2287*)      ENTRYHALT = 456; ENTRYEXPON = 448;
(*2288*)      ENTRYMESSAGE = 480; (* MESSAGE ENTRY POINT *)
(*2289*)      ENTRYLONGJUMP = 488; (* LONG JUMP LANDING ENTRY *)
(*2290*)      CONDZ=8; CONDP=2; CONDM=4; CONDNZ=7; CONDNP=13; CONDNM=11;
(*2291*)   TYPE
(*2292*) 
(*2293*)               (*TO DESCRIBE EXPRESSION CURRENTLY COMPILED*)
(*2294*)               (*******************************************)
(*2295*) 
(*2296*)      ATTRP = @ ATTR;
(*2297*)      ATTRKIND = (CST,VARBL,EXPR);
(*2298*)      CMP=@TEMPREC;
(*2299*)      TEMPREC=RECORD TEMPADRS:ADDRRANGE; TEMPLNGTH:INTEGER; (* 4 OR 8 *)
(*2300*)                     NEXTTEMP:CMP; TEMPCONT:ATTRP END;
(*2301*)      REGKIND=(SINGLE,DOUBLE,FLOAT);
(*2302*)      EXPRKIND=(REGIST,TEMPORARY);
(*2303*)      ACCESSKIND=(DIRECT,INDIRECT);           (*INDIRECT: INDEXED OR POINTED VARIABLE*)
(*2304*)      REGORTEMP=RECORD CASE REGTEMP:EXPRKIND OF
(*2305*)                             REGIST:(RNO:REGNO);
(*2306*)                             TEMPORARY:(ATEMP:CMP)
(*2307*)                END;
(*2308*)      REGRECORD=RECORD USED:BOOLEAN; REGCONT:ATTRP END;
(*2309*) 
(*2310*)      ATTR = RECORD TYPTR: STP;
(*2311*)               FOLLOW: ATTRP;
(*2312*)               CASE KIND: ATTRKIND OF
(*2313*)                CST:   (CVAL: VALU);
(*2314*)                VARBL: (VADRS:ADDRRANGE;
(*2315*)                        ACCESS:ACCESSKIND; INDEXREG:REGORTEMP;
(*2316*)                        CASE VARKIND:DRCTINDRCT OF
(*2317*)                              DRCT: (VLEVEL:LEVRANGE);
(*2318*)                              INDRCT: (BASELEV:LEVRANGE; BASEADD:ADDRRANGE));
(*2319*)                EXPR:(REXPR:REGORTEMP)
(*2320*)          END;
(*2321*) 
(*2322*)     CONSTCHAIN=@CONSTCREC;
(*2323*)     CONSTCREC=RECORD SAVECONST:VALU; CCHAIN:LOCOFREF;
(*2324*)                      NEXTCONST:CONSTCHAIN
(*2325*)               END;
(*2326*) 
(*2327*) 
(*2328*)          (*  CODE  BUFFERS  *)
(*2329*)          (*******************)
(*2330*) 
(*2331*) 
(*2332*)   CODESPTR = @CODESEG;         (* POINTER TO CODE SEGMENT *)
(*2333*)   CODESEG  = RECORD            (* CODE SEGMENT DESCRIPTOR *)
(*2334*)                     CASE BOOLEAN OF
(*2335*)                  TRUE : (FULLWORDS:ARRAY(.0..CODEBLCK.) OF INTEGER);
(*2336*)                  FALSE: (BYTES    :PACKED ARRAY
(*2337*)                                       (. 0.. 255 .) OF CHAR )
(*2338*)              END; (* OF CODE SEGMENT *)
(*2339*)   VAR
(*2340*)     REGISTER:ARRAY(.REGNO.) OF REGRECORD;
(*2341*)     DISPLEVEL: LEVRANGE;                 (*NUMBER OF USED DISPLAY REGISTERS,  C.F. WITHSTATEMENT*)
(*2342*)     GATTRP,ATTRHEAD: ATTRP;
(*2343*)     RINDEX,RBASE:INTEGER;              (*INDEX AND BASE REGISTER NUMBER *)
(*2344*)     EFFADRS:INTEGER;                   (*EFFECTIVE ADDRESS*)
(*2345*)     RMAIN:INTEGER;                     (* WORKING REGISTER NUMBER *)
(*2346*)     RWORK:REGNO;
(*2347*)     FREETEMP:CMP;
(*2348*)     STACKTOP:INTEGER;
(*2349*)     BOOLFLAG: BOOLEAN;
(*2350*) 
(*2351*)     CONSTTOP:CONSTCHAIN;
(*2352*)     STACKSIZE:LOCOFREF;
(*2353*)     CODEPTR : ARRAY (.0..95.) OF CODESPTR;
(*2354*)     EXTENDEDADDRESS : BOOLEAN; (* FLAG FOR EXTENDED ADDRESSING *)
(*2355*)     REG6USED,REG5USED:BOOLEAN;
(*2356*)     PROCPASS : BOOLEAN;
(*2357*) 
(*2358*) 
$TITLE  CODE GEN - ATTRNEW,ATTRDISP,COPYATTR,COPYREG
(*2359*)    PROCEDURE ATTRNEW(VAR FATTRP: ATTRP);
(*2360*)      BEGIN
(*2361*)        IF ATTRHEAD = NIL THEN NEW(FATTRP)
(*2362*)        ELSE BEGIN FATTRP:=ATTRHEAD; ATTRHEAD:=ATTRHEAD@.FOLLOW
(*2363*)             END;
(*2364*)      END;
(*2365*) 
(*2366*)  PROCEDURE ATTRDISP(FATTRP:ATTRP);                                             
(*2367*)                                                                                
(*2368*)    PROCEDURE TEMPDISP(ATP:CMP);                                                
(*2369*)      BEGIN                                                                     
(*2370*)        IF ATP@.TEMPCONT=FATTRP THEN                                            
(*2371*)         BEGIN ATP@.NEXTTEMP := FREETEMP; FREETEMP := ATP END;                  
(*2372*)      END;                                                                      
(*2373*)                                                                                
(*2374*)  BEGIN                                                                         
(*2375*)    WITH FATTRP@ DO                                                             
(*2376*)      BEGIN                                                                     
(*2377*)        FOLLOW := ATTRHEAD; ATTRHEAD := FATTRP;                                 
(*2378*)        IF KIND = EXPR THEN                                                     
(*2379*)         WITH REXPR DO                                                          
(*2380*)          BEGIN                                                                 
(*2381*)           IF REGTEMP = REGIST THEN                                             
(*2382*)             BEGIN                                                              
(*2383*)               IF REGISTER(.RNO.).REGCONT=FATTRP THEN                           
(*2384*)                BEGIN REGISTER(.RNO.).USED := FALSE;                            
(*2385*)                  IF FATTRP@.TYPTR@.FORM=POWER THEN                             
(*2386*)                    REGISTER(.SUCC(RNO).).USED := FALSE;                        
(*2387*)                END                                                             
(*2388*)             END                                                                
(*2389*)            ELSE TEMPDISP(ATEMP);                                               
(*2390*) END;                                                                           
(*2391*)    END;                                                                        
(*2392*)  END;                                                                          
(*2393*) 
(*2394*)   PROCEDURE COPYATTR(SOURCEATTRP,DESTATTRP : ATTRP);
(*2395*) 
(*2396*)    PROCEDURE COPYREG(R: REGORTEMP);
(*2397*)       BEGIN IF R.REGTEMP = REGIST THEN REGISTER(.R.RNO.).REGCONT:= DESTATTRP
(*2398*)              ELSE R.ATEMP@.TEMPCONT:=DESTATTRP;
(*2399*)     END;
(*2400*) 
(*2401*)     BEGIN DESTATTRP@ := SOURCEATTRP@;
(*2402*)       IF SOURCEATTRP@.KIND=VARBL THEN
(*2403*)         BEGIN IF SOURCEATTRP@.ACCESS=INDIRECT THEN
(*2404*)         COPYREG(SOURCEATTRP@.INDEXREG)
(*2405*)        END
(*2406*)       ELSE IF SOURCEATTRP@.KIND=EXPR THEN
(*2407*)        BEGIN COPYREG(SOURCEATTRP@.REXPR);
(*2408*)         IF (SOURCEATTRP@.TYPTR@.FORM=POWER) AND (SOURCEATTRP@.REXPR.REGTEMP=REGIST)
(*2409*)           THEN REGISTER(.SUCC(SOURCEATTRP@.REXPR.RNO).).REGCONT:=DESTATTRP;
(*2410*)        END
(*2411*)     END;
(*2412*) 
$TITLE   CODE HANDLING-MAKECODE,GETCODE
(*2413*)PROCEDURE MAKECODE( LOC,HALF : INTEGER );
(*2414*)  VAR
(*2415*)    LOCSEG : CODESPTR;
(*2416*)    N      : 0..CODEPERSEG;
(*2417*)    DUMMY  : RECORD
(*2418*)               CASE BOOLEAN OF
(*2419*)              TRUE:(A: PACKED ARRAY (.1..4.) OF CHAR);
(*2420*)              FALSE:(X:INTEGER)
(*2421*)             END;
(*2422*) 
(*2423*)BEGIN (* MAKECODE *)
(*2424*)  LOCSEG := CODEPTR(. LOC DIV CODEPERSEG .);   (* PICK UP SEGMENT *)
(*2425*)  IF LOCSEG = NIL THEN   (* PERHAPS NOT CREATED YET *)
(*2426*)  BEGIN
(*2427*)    NEW(LOCSEG,TRUE);
(*2428*)    CODEPTR(. LOC DIV CODEPERSEG .) := LOCSEG;
(*2429*)  END;   (* NEW SEGMENT NOW CREATED *)
(*2430*)  N := LOC MOD CODEPERSEG;
(*2431*)  DUMMY.X:=HALF;      (* NOW PICK UP HALF WORD *)
(*2432*)  LOCSEG@.BYTES(. N .) := DUMMY.A(. 3 .);
(*2433*)  LOCSEG@.BYTES(. N + 1 .) := DUMMY.A(. 4 .);
(*2434*)END; (*  MAKECODE *)
(*2435*) 
(*2436*) 
(*2437*)FUNCTION GETCODE(LOC : INTEGER):INTEGER;
(*2438*)  VAR
(*2439*)    DUMMY : RECORD
(*2440*)              CASE BOOLEAN OF
(*2441*)                TRUE : (INT:INTEGER);
(*2442*)                FALSE: (CH : PACKED ARRAY(.1..4.) OF CHAR)
(*2443*)              END;
(*2444*)    LOCPTR : CODESPTR;
(*2445*)    X      : INTEGER;
(*2446*)BEGIN (*GETCODE*)
(*2447*)  LOCPTR := CODEPTR(. LOC DIV CODEPERSEG .);
(*2448*)  DUMMY.INT := 0;
(*2449*)  X := LOC MOD CODEPERSEG;
(*2450*)  DUMMY.CH(. 3 .) := LOCPTR@.BYTES(.X.);
(*2451*)  DUMMY.CH(. 4 .) := LOCPTR@.BYTES(.X+1.);
(*2452*)  GETCODE := DUMMY.INT;
(*2453*)END; (* GETCODE *)
(*2454*) 
$TITLE  CODE GEN-EXCATTR,RESETG,ERRORSET
(*2455*) 
(*2456*) 
(*2457*)    PROCEDURE EXCATTR(F1ATTRP,F2ATTRP:ATTRP);
(*2458*)      VAR ATTRWORK:ATTRP;
(*2459*)      BEGIN ATTRNEW(ATTRWORK); COPYATTR(F1ATTRP,ATTRWORK);
(*2460*)        COPYATTR(F2ATTRP,F1ATTRP); COPYATTR(ATTRWORK,F2ATTRP); ATTRDISP(ATTRWORK)
(*2461*)      END;
(*2462*) 
(*2463*)    PROCEDURE RESETG;
(*2464*)      BEGIN ATTRDISP(GATTRP); ATTRNEW(GATTRP);
(*2465*)        WITH GATTRP@ DO
(*2466*)          BEGIN TYPTR:=NIL; KIND:=CST; END;
(*2467*)      END;
(*2468*) 
(*2469*)    PROCEDURE ERRORRESET(N:INTEGER);
(*2470*)      BEGIN ERROR(N);
(*2471*)            GATTRP@.TYPTR:=NIL;
(*2472*)      END;
(*2473*) 
$TITLE  CODE GEN - GENRX,GENRXP,GENRR,GENRRP1
(*2474*)    PROCEDURE GENRX(OP,REG,INDEX,BASE,ADDR:INTEGER);
(*2475*)      BEGIN
(*2476*)         IF IC >= 4096*(7-LEVEL)-2 THEN
(*2477*)         BEGIN ERROR(253); IC:=0 END;
(*2478*)         IF (BASE=14) AND (ADDR >= 4096) THEN
(*2479*)         BEGIN EXTENDEDADDRESS:=TRUE; BASE:=LEVEL END;
(*2480*)         MAKECODE(IC,256*OP+16*REG+INDEX);
(*2481*)         MAKECODE(IC+2,4096*BASE+ADDR);
(*2482*)            IC:=IC+4; BOOLFLAG:=FALSE;
(*2483*)      END;
(*2484*) 
(*2485*)    PROCEDURE GENRXP(OP:INTEGER; R:REGNO; INDEX,BASE,ADDR:INTEGER);
(*2486*)      BEGIN GENRX(OP,REALREG(.R.),INDEX,BASE,ADDR);
(*2487*)      END;
(*2488*) 
(*2489*)    PROCEDURE GENRR(OP,R1,R2:INTEGER);
(*2490*)        BEGIN
(*2491*)          IF IC >= 4096*(7-LEVEL) THEN
(*2492*)          BEGIN
(*2493*)            ERROR(253); IC:=0
(*2494*)          END;
(*2495*)          MAKECODE(IC,256*OP+16*R1+R2);
(*2496*)            IC:=IC+2; BOOLFLAG:=FALSE;
(*2497*)      END;
(*2498*) 
(*2499*)    PROCEDURE GENRRP1(OP:INTEGER; R:REGNO);
(*2500*)      BEGIN GENRR(OP,REALREG(.R.),REALREG(.R.));
(*2501*)      END;
(*2502*) 
$TITLE CODE GEN - GENRRP,GENSS,INSERTIC
(*2503*)    PROCEDURE GENRRP(OP:INTEGER; R1,R2:REGNO);
(*2504*)      BEGIN GENRR(OP,REALREG(.R1.),REALREG(.R2.));
(*2505*)      END;
(*2506*) 
(*2507*)    PROCEDURE GENSS(OP,L,R1,D1,R2,D2:INTEGER);
(*2508*)     BEGIN
(*2509*)        IF IC >= 4096*(7-LEVEL)-4 THEN
(*2510*)        BEGIN ERROR(253); IC:=0 END;
(*2511*)        IF (R2=14) AND (D2 >=4096) THEN
(*2512*)        BEGIN EXTENDEDADDRESS:=TRUE; R2:=LEVEL END;
(*2513*)        MAKECODE(IC,256*OP+L);
(*2514*)        MAKECODE(IC+2,4096*R1+D1);
(*2515*)        MAKECODE(IC+4,4096*R2+D2);
(*2516*)            IC:=IC+6; BOOLFLAG:=FALSE;
(*2517*)      END;
(*2518*) 
(*2519*)    PROCEDURE INSERTIC(FCIX:ADDRRANGE);
(*2520*)     VAR BASE : INTEGER;
(*2521*)      BEGIN
(*2522*)        IF IC >= 4096 THEN
(*2523*)        BEGIN BASE:=LEVEL; EXTENDEDADDRESS:=TRUE
(*2524*)        END
(*2525*)       ELSE BASE:=PBASE1;
(*2526*)        MAKECODE(FCIX+2,4096*BASE+IC);
(*2527*)      END;
(*2528*) 
$TITLE CODE GEN - LINKOCC,MAKECONST,MKEINTCNST
(*2529*)    PROCEDURE INSERTCHAIN(CHAIN:LOCOFREF);
(*2530*)      BEGIN
(*2531*)        WHILE CHAIN<>NIL DO
(*2532*)          WITH CHAIN@ DO
(*2533*)            BEGIN INSERTIC(LOC); CHAIN:=NXTREF; END;
(*2534*)      END;
(*2535*) 
(*2536*)    PROCEDURE LINKOCC(VAR FPTR: LOCOFREF; FCIX: ADDRRANGE);
(*2537*)      VAR LOCP: LOCOFREF;
(*2538*)      BEGIN NEW(LOCP);
(*2539*)        WITH LOCP@ DO
(*2540*)          BEGIN NXTREF:=FPTR; LOC:=FCIX; END;
(*2541*)        FPTR:=LOCP;
(*2542*)      END;
(*2543*) 
(*2544*)    PROCEDURE MAKECONSTANT(X:VALU);
(*2545*)      LABEL 1;
(*2546*)      VAR EQUAL:BOOLEAN; P,Q:CTAILP; C:CONSTCHAIN;
(*2547*)      BEGIN C:=CONSTTOP;
(*2548*)        WHILE C<>NIL DO
(*2549*)          WITH C@ DO
(*2550*)            BEGIN
(*2551*)              IF SAVECONST.CKIND=X.CKIND THEN
(*2552*)                BEGIN CASE X.CKIND OF
(*2553*)                  INT: EQUAL:=(X.IVAL=SAVECONST.IVAL);
(*2554*)                  REEL:EQUAL:=(X.RVAL=SAVECONST.RVAL);
(*2555*)                  PSET:EQUAL:=(X.PVAL=SAVECONST.PVAL);
(*2556*)                  STRG:BEGIN EQUAL:=TRUE;
(*2557*)                         P:=X.VALP; Q:=SAVECONST.VALP;
(*2558*)                         WHILE EQUAL AND (P<>NIL) AND (Q<>NIL) DO
(*2559*)                           BEGIN EQUAL:=(P@.STFR=Q@.STFR);
(*2560*)                             P:=P@.NXTCSP; Q:=Q@.NXTCSP;
(*2561*)                           END;
(*2562*)                         EQUAL:=EQUAL AND (P=Q);
(*2563*)                       END
(*2564*)                  END;
(*2565*)                  IF EQUAL THEN
(*2566*)                    BEGIN LINKOCC(CCHAIN,IC); GOTO 1; END;
(*2567*)                END;
(*2568*)              C:=C@.NEXTCONST;
(*2569*)            END;
(*2570*)        NEW(C);
(*2571*)        WITH C@ DO
(*2572*)          BEGIN SAVECONST:=X; CCHAIN:=NIL;
(*2573*)                NEXTCONST:=CONSTTOP; LINKOCC(CCHAIN,IC);
(*2574*)          END;
(*2575*)        CONSTTOP:=C;
(*2576*)   1: END;
(*2577*) 
(*2578*)    PROCEDURE MAKEINTCONST(N:INTEGER);
(*2579*)      VAR X:VALU;
(*2580*)      BEGIN X.CKIND:=INT; X.IVAL:=N;
(*2581*)            MAKECONSTANT(X);
(*2582*)      END;
(*2583*) 
$TITLE CODE GEN - GETTEMP,DELTEMP,USING
(*2584*)    PROCEDURE GETTEMP(LENGTH:INTEGER; VAR X:CMP);
(*2585*)      LABEL 1,2;
(*2586*)      VAR P,Q:CMP;
(*2587*)      BEGIN Q:=NIL; P:=FREETEMP;
(*2588*)        WHILE P<>NIL DO
(*2589*)          IF P@.TEMPLNGTH=LENGTH THEN GOTO 1
(*2590*)          ELSE BEGIN Q:=P; P:=P@.NEXTTEMP END;
(*2591*)        NEW(P);
(*2592*)        ALIGNMENT(LC,LENGTH);
(*2593*)        P@.TEMPADRS:=LC; LC:=LC+LENGTH;
(*2594*)        P@.TEMPLNGTH:=LENGTH; GOTO 2;
(*2595*)     1: IF Q=NIL THEN FREETEMP:=P@.NEXTTEMP
(*2596*)                 ELSE Q@.NEXTTEMP:=P@.NEXTTEMP;
(*2597*)     2: X:=P;
(*2598*)      END;
(*2599*) 
(*2600*)    PROCEDURE DELETETEMP(X:CMP);
(*2601*)      BEGIN X@.NEXTTEMP:=FREETEMP; FREETEMP:=X;
(*2602*)      END;
(*2603*) 
(*2604*)    FUNCTION USING(R:REGNO; FATTRP:ATTRP):BOOLEAN;      (*CHECK IF R IS OCCUPIED BY FATTRP*)
(*2605*)      BEGIN IF FATTRP=NIL THEN USING:=FALSE
(*2606*)        ELSE
(*2607*)          BEGIN WITH FATTRP@ DO CASE KIND OF
(*2608*)            CST:   USING:=FALSE;
(*2609*)            VARBL: IF ACCESS=INDIRECT THEN IF INDEXREG.REGTEMP=REGIST
(*2610*)                       THEN USING:=(R=INDEXREG.RNO)
(*2611*)                      ELSE USING:=FALSE
(*2612*)                     ELSE USING:=FALSE;
(*2613*)            EXPR:  IF REXPR.REGTEMP=REGIST
(*2614*)                      THEN USING:=(REGISTER(.R.).REGCONT=FATTRP)
(*2615*)                      ELSE USING:=FALSE
(*2616*)            END;
(*2617*)          END;
(*2618*)      END;
(*2619*) 
$TITLE CODE GEN - DISPLCMNT,BASEREG,SAVE
(*2620*)    PROCEDURE DISPLACEMENT(ADRS:INTEGER; VAR REM:INTEGER);
(*2621*)      VAR I:INTEGER;
(*2622*)      BEGIN
(*2623*)        IF ADRS>=0 THEN I:=ADRS DIV 4096*4096
(*2624*)                   ELSE I:=((ADRS+1) DIV 4096-1)*4096;
(*2625*)        MAKEINTCONST(I); REM:=ADRS-I;
(*2626*)      END;
(*2627*) 
(*2628*)    PROCEDURE BASEREGISTER(LEVEL:LEVRANGE; ADRS:ADDRRANGE);
(*2629*)      BEGIN
(*2630*)        IF (ADRS>=4096) OR (ADRS<0) THEN
(*2631*)          BEGIN DISPLACEMENT(ADRS,EFFADRS); GENRX(ZL,BASEWORK,0,0,0);
(*2632*)                IF LEVEL<>0 THEN GENRR(ZAR,BASEWORK,LEVEL);
(*2633*)                RBASE:=BASEWORK;
(*2634*)          END
(*2635*)        ELSE BEGIN EFFADRS:=ADRS; RBASE:=LEVEL; END;
(*2636*)      END;
(*2637*) 
(*2638*)    PROCEDURE SAVE(R:REGNO);
(*2639*)      VAR TEMP:CMP;
(*2640*)      BEGIN IF REGISTER(.R.).USED THEN
(*2641*)        BEGIN
(*2642*)          IF R>=F0 THEN
(*2643*)            BEGIN GETTEMP(8,TEMP);
(*2644*)                  BASEREGISTER(LEVEL,TEMP@.TEMPADRS);
(*2645*)                  GENRXP(ZSTD,R,0,RBASE,EFFADRS)
(*2646*)            END
(*2647*)          ELSE IF (REGISTER(.R.).REGCONT@.TYPTR@.FORM=POWER)
(*2648*)               AND (REGISTER(.R.).REGCONT@.KIND=EXPR) THEN
(*2649*)            BEGIN GETTEMP(8,TEMP);
(*2650*)                  BASEREGISTER(LEVEL,TEMP@.TEMPADRS);
(*2651*)                  GENRXP(ZSTM,R,REALREG(.SUCC(R).),RBASE,EFFADRS);
(*2652*)                  REGISTER(.SUCC(R).).USED:=FALSE
(*2653*)            END
(*2654*)          ELSE BEGIN GETTEMP(4,TEMP);
(*2655*)                     BASEREGISTER(LEVEL,TEMP@.TEMPADRS);
(*2656*)                     GENRXP(ZST,R,0,RBASE,EFFADRS)
(*2657*)               END;
(*2658*)          REGISTER(.R.).USED:=FALSE;
(*2659*)          TEMP@.TEMPCONT:=REGISTER(.R.).REGCONT;
(*2660*)          WITH REGISTER(.R.).REGCONT@ DO
(*2661*)            IF KIND=EXPR THEN
(*2662*)              BEGIN REXPR.REGTEMP:=TEMPORARY;
(*2663*)                    REXPR.ATEMP:=TEMP
(*2664*)              END
(*2665*)            ELSE BEGIN INDEXREG.REGTEMP:=TEMPORARY;
(*2666*)                       INDEXREG.ATEMP:=TEMP
(*2667*)                 END
(*2668*)        END;
(*2669*)      END;
(*2670*) 
$TITLE CODE GEN - REGSEARCH,LOADINDX,LDBASE
(*2671*)    PROCEDURE REGSEARCH(FATTRP:ATTRP; T:REGKIND);
(*2672*)      LABEL 1;
(*2673*)      BEGIN CASE T OF
(*2674*)        SINGLE: BEGIN FOR RWORK:=R10 TO R13 DO
(*2675*)                         IF NOT REGISTER(.RWORK.).USED THEN GOTO 1;
(*2676*)                      FOR RWORK:=R10 TO R13 DO
(*2677*)                         IF NOT USING(RWORK,FATTRP) THEN BEGIN SAVE(RWORK); GOTO 1 END;
(*2678*)                      ERROR(400);
(*2679*)                 END;
(*2680*)        FLOAT: BEGIN FOR RWORK:=F0 TO F6 DO
(*2681*)                         IF NOT REGISTER(.RWORK.).USED THEN GOTO 1;
(*2682*)                     FOR RWORK:=F0 TO F6 DO
(*2683*)                         IF NOT USING(RWORK,FATTRP) THEN BEGIN SAVE(RWORK); GOTO 1 END;
(*2684*)                     ERROR(400);
(*2685*)               END;
(*2686*)        DOUBLE: IF NOT(REGISTER(.R10.).USED OR REGISTER(.R11.).USED) THEN RWORK:=R10
(*2687*)                  ELSE IF NOT(REGISTER(.R12.).USED OR REGISTER(.R13.).USED) THEN RWORK:=R12
(*2688*)                  ELSE IF NOT(USING(R10,FATTRP) OR USING(R11,FATTRP)) THEN
(*2689*)                           BEGIN SAVE(R10); SAVE(R11); RWORK:=R10 END
(*2690*)                  ELSE BEGIN SAVE(R12); SAVE(R13); RWORK:=R12 END
(*2691*)           END;
(*2692*)     1: RMAIN:=REALREG(.RWORK.);
(*2693*)      END;
(*2694*) 
(*2695*)    PROCEDURE LOADINDEX(F1ATTRP,F2ATTRP:ATTRP);
(*2696*)      BEGIN
(*2697*)        WITH F1ATTRP@ DO
(*2698*)          BEGIN
(*2699*)            IF ACCESS=DIRECT THEN RINDEX:=0
(*2700*)              ELSE IF INDEXREG.REGTEMP=REGIST THEN
(*2701*)                BEGIN REGISTER(.INDEXREG.RNO.).USED:=FALSE;
(*2702*)                      RINDEX:=REALREG(.INDEXREG.RNO.); RWORK:=INDEXREG.RNO;
(*2703*)                END
(*2704*)              ELSE WITH INDEXREG.ATEMP@ DO
(*2705*)                BEGIN REGSEARCH(F2ATTRP,SINGLE); BASEREGISTER(LEVEL,TEMPADRS);
(*2706*)                      GENRX(ZL,RMAIN,0,RBASE,EFFADRS);
(*2707*)                      RINDEX:=RMAIN; DELETETEMP(INDEXREG.ATEMP);
(*2708*)                END;
(*2709*)          END;
(*2710*)      END;
(*2711*) 
(*2712*)    PROCEDURE LOADBASE(FATTRP:ATTRP);
(*2713*)      BEGIN
(*2714*)        WITH FATTRP@ DO
(*2715*)          BEGIN
(*2716*)            IF VARKIND=DRCT
(*2717*)              THEN BASEREGISTER(VLEVEL,VADRS)
(*2718*)              ELSE
(*2719*)                BEGIN
(*2720*)                  BASEREGISTER(BASELEV,BASEADD);
(*2721*)                  GENRX(ZL,BASEWORK,0,RBASE,EFFADRS);
(*2722*)                  RBASE:=BASEWORK;
(*2723*)                  IF (VADRS>=4096) OR (VADRS<0) THEN
(*2724*)                    BEGIN DISPLACEMENT(VADRS,EFFADRS); GENRX(ZA,BASEWORK,0,0,0); END
(*2725*)                  ELSE EFFADRS:=VADRS;
(*2726*)                END;
(*2727*)          END;
(*2728*)      END;
$TITLE   LOADINTCONST
(*2729*)    PROCEDURE LOADINTCONST(REG:0..15; VAL:INTEGER);
(*2730*)      BEGIN
(*2731*)        IF VAL=0
(*2732*)          THEN GENRR(ZXR,REG,REG)
(*2733*)          ELSE IF (VAL>0) AND (VAL<4096)
(*2734*)            THEN GENRX(ZLA,REG,0,0,VAL)
(*2735*)            ELSE BEGIN MAKEINTCONST(VAL);
(*2736*)                       GENRX(ZL,REG,0,0,0);
(*2737*)                 END;
(*2738*)      END;
(*2739*) 
$TITLE  LOAD
(*2740*)    PROCEDURE LOAD(F1ATTRP,F2ATTRP:ATTRP);
(*2741*)      VAR RKIND:REGKIND; LOADOP:INTEGER;
(*2742*)      BEGIN WITH F1ATTRP@ DO
(*2743*)        BEGIN
(*2744*)          IF (KIND<>EXPR) OR (REXPR.REGTEMP<>REGIST) THEN
(*2745*)            BEGIN
(*2746*)              IF TYPTR@.FORM=POWER THEN RKIND:=DOUBLE
(*2747*)                ELSE IF COMPTYPES(TYPTR,REALPTR)
(*2748*)                  THEN BEGIN RKIND:=FLOAT; LOADOP:=ZLD; END
(*2749*)                  ELSE BEGIN RKIND:=SINGLE; LOADOP:=ZL; END;
(*2750*)              CASE KIND OF
(*2751*)          CST:   BEGIN REGSEARCH(F2ATTRP,RKIND);
(*2752*)                   IF RKIND=SINGLE
(*2753*)                     THEN LOADINTCONST(RMAIN,CVAL.IVAL)
(*2754*)                     ELSE BEGIN MAKECONSTANT(CVAL);
(*2755*)                            IF RKIND=DOUBLE THEN GENRX(ZLM,RMAIN,RMAIN+1,0,0)
(*2756*)                                            ELSE GENRX(LOADOP,RMAIN,0,0,0);
(*2757*)                          END;
(*2758*)                 END;
(*2759*)          VARBL: BEGIN LOADINDEX(F1ATTRP,F2ATTRP); REGSEARCH(F2ATTRP,RKIND);
(*2760*)                   LOADBASE(F1ATTRP);
(*2761*)                   IF RKIND=DOUBLE THEN
(*2762*)         BEGIN
(*2763*)           IF RINDEX=0 THEN
(*2764*)              GENRX(ZLM,RMAIN,RMAIN+1,RBASE,EFFADRS) ELSE
(*2765*)                     IF RINDEX=RMAIN
(*2766*)                       THEN BEGIN GENRX(ZL,RMAIN+1,RINDEX,RBASE,EFFADRS+4);
(*2767*)                                  GENRX(ZL,RMAIN,RINDEX,RBASE,EFFADRS);
(*2768*)                            END
(*2769*)                       ELSE BEGIN GENRX(ZL,RMAIN,RINDEX,RBASE,EFFADRS);
(*2770*)                                  GENRX(ZL,RMAIN+1,RINDEX,RBASE,EFFADRS+4);
(*2771*)                            END
(*2772*)         END
(*2773*)                   ELSE IF TYPTR@.SIZE.WBLENGTH=1
(*2774*)                     THEN BEGIN GENRX(ZIC,RMAIN,RINDEX,RBASE,EFFADRS);
(*2775*)                                MAKEINTCONST(255); GENRX(ZN,RMAIN,0,0,0);
(*2776*)                          END
(*2777*)                     ELSE GENRX(LOADOP,RMAIN,RINDEX,RBASE,EFFADRS);
(*2778*)                 END;
(*2779*)          EXPR:  BEGIN REGSEARCH(F2ATTRP,RKIND);
(*2780*)                   BASEREGISTER(LEVEL,REXPR.ATEMP@.TEMPADRS);
(*2781*)                   IF RKIND=DOUBLE
(*2782*)                     THEN GENRX(ZLM,RMAIN,RMAIN+1,RBASE,EFFADRS)
(*2783*)                     ELSE GENRX(LOADOP,RMAIN,0,RBASE,EFFADRS);
(*2784*)                   DELETETEMP(REXPR.ATEMP);
(*2785*)                 END
(*2786*)              END; (*CASE*)
(*2787*)              KIND:=EXPR; REXPR.REGTEMP:=REGIST; REXPR.RNO:=RWORK;
(*2788*)              REGISTER(.RWORK.).USED:=TRUE; REGISTER(.RWORK.).REGCONT:=F1ATTRP;
(*2789*)              IF RKIND=DOUBLE THEN
(*2790*)                BEGIN REGISTER(.SUCC(RWORK).).USED:=TRUE;
(*2791*)                      REGISTER(.SUCC(RWORK).).REGCONT:=F1ATTRP;
(*2792*)                END;
(*2793*)            END;
(*2794*)        END;
(*2795*)      END;
(*2796*) 
$TITLE  LOADEVENODD,LOADADDRESS
(*2797*)    PROCEDURE LOADEVENODD(F1ATTRP,F2ATTRP:ATTRP; SWITCH:INTEGER);    (*SWITCH=0: EVEN, 1: ODD*)
(*2798*)      BEGIN WITH F1ATTRP@ DO
(*2799*)        BEGIN CASE KIND OF
(*2800*)          CST:  BEGIN REGSEARCH(F2ATTRP,DOUBLE);
(*2801*)                      LOADINTCONST(RMAIN+SWITCH,CVAL.IVAL);
(*2802*)                END;
(*2803*)          VARBL:BEGIN LOADINDEX(F1ATTRP,F2ATTRP); REGSEARCH(F2ATTRP,DOUBLE);
(*2804*)                  LOADBASE(F1ATTRP);
(*2805*)                  IF TYPTR@.SIZE.WBLENGTH=1
(*2806*)                    THEN BEGIN GENRX(ZIC,RMAIN+SWITCH,RINDEX,RBASE,EFFADRS);
(*2807*)                               MAKEINTCONST(255); GENRX(ZN,RMAIN+SWITCH,0,0,0);
(*2808*)                         END
(*2809*)                    ELSE GENRX(ZL,RMAIN+SWITCH,RINDEX,RBASE,EFFADRS);
(*2810*)                END;
(*2811*)          EXPR: IF REXPR.REGTEMP=REGIST
(*2812*)                  THEN BEGIN REGISTER(.REXPR.RNO.).USED:=FALSE;
(*2813*)                             REGSEARCH(F2ATTRP,DOUBLE);
(*2814*)                             IF RMAIN+SWITCH<>REALREG(.REXPR.RNO.) THEN
(*2815*)                               GENRR(ZLR,RMAIN+SWITCH,REALREG(.REXPR.RNO.));
(*2816*)                       END
(*2817*)                  ELSE BEGIN REGSEARCH(F2ATTRP,DOUBLE);
(*2818*)                             BASEREGISTER(LEVEL,REXPR.ATEMP@.TEMPADRS);
(*2819*)                             GENRX(ZL,RMAIN+SWITCH,0,RBASE,EFFADRS);
(*2820*)                             DELETETEMP(REXPR.ATEMP);
(*2821*)                       END
(*2822*)          END;
(*2823*)          IF SWITCH=1 THEN RWORK:=SUCC(RWORK);
(*2824*)          KIND:=EXPR; REXPR.REGTEMP:=REGIST; REXPR.RNO:=RWORK;
(*2825*)          REGISTER(.RWORK.).USED:=TRUE; REGISTER(.RWORK.).REGCONT:=F1ATTRP;
(*2826*)        END;
(*2827*)      END;
(*2828*) 
(*2829*)    PROCEDURE LOADADDRESS(F1ATTRP,F2ATTRP:ATTRP);
(*2830*)     VAR SWITCH:BOOLEAN;
(*2831*)     BEGIN
(*2832*)       SWITCH:=FALSE;
(*2833*)       WITH F1ATTRP@ DO
(*2834*)        CASE KIND OF
(*2835*)          EXPR: ERROR(400);
(*2836*)          CST:  BEGIN REGSEARCH(F2ATTRP,SINGLE); MAKECONSTANT(CVAL);
(*2837*)                      GENRX(ZLA,RMAIN,0,0,0);
(*2838*)                END;
(*2839*)          VARBL:BEGIN LOADINDEX(F1ATTRP,F2ATTRP);
(*2840*)                  IF RINDEX=0
(*2841*)                    THEN
(*2842*)                      BEGIN REGSEARCH(F2ATTRP,SINGLE);
(*2843*)                            IF VARKIND=DRCT
(*2844*)                              THEN BEGIN IF VLEVEL=0 THEN ERROR(400)
(*2845*)                             ELSE
(*2846*)                            IF (VADRS<4096) AND (VADRS>0) THEN
(*2847*)                            BEGIN SWITCH:=TRUE;
(*2848*)                              GENRX(ZLA,RMAIN,0,VLEVEL,VADRS)
(*2849*)                            END ELSE GENRR(ZLR,RMAIN,VLEVEL);
(*2850*)                                   END
(*2851*)                              ELSE
(*2852*)                                BEGIN BASEREGISTER(BASELEV,BASEADD);
(*2853*)                                      GENRX(ZL,RMAIN,0,RBASE,EFFADRS);
(*2854*)                                END;
(*2855*)                      END
(*2856*)                    ELSE
(*2857*)                      BEGIN RMAIN:=RINDEX;
(*2858*)                        IF VARKIND=DRCT THEN
(*2859*)                          BEGIN IF VLEVEL<>0 THEN GENRR(ZAR,RMAIN,VLEVEL); END
(*2860*)                        ELSE BEGIN BASEREGISTER(BASELEV,BASEADD);
(*2861*)                                   GENRX(ZA,RMAIN,0,RBASE,EFFADRS);
(*2862*)                             END;
(*2863*)                      END;
(*2864*)                 IF (VADRS<>0) AND (NOT SWITCH) THEN
(*2865*)                     BEGIN MAKEINTCONST(VADRS); GENRX(ZA,RMAIN,0,0,0); END;
(*2866*)                END
(*2867*)            END;
(*2868*)        WITH F1ATTRP@ DO
(*2869*)          BEGIN TYPTR:=INTPTR; KIND:=EXPR;
(*2870*)                REXPR.REGTEMP:=REGIST; REXPR.RNO:=RWORK;
(*2871*)          END;
(*2872*)        REGISTER(.RWORK.).USED:=TRUE; REGISTER(.RWORK.).REGCONT:=F1ATTRP;
(*2873*)      END;
(*2874*) 
$TITLE  SETOPERATION,SETOP1
(*2875*)    PROCEDURE SETOPERATION(F1ATTRP,F2ATTRP:ATTRP; OPRX1,OPRR1,OPRX2,OPRR2:INTEGER);
(*2876*) 
(*2877*)     PROCEDURE SETOP1(OPRX,OPRR:INTEGER);
(*2878*)       VAR A1,A2:INTEGER;
(*2879*)       BEGIN
(*2880*)         WITH F2ATTRP@ DO CASE KIND OF
(*2881*)           CST:   BEGIN SETVALUE(CVAL.PVAL,A1,A2); MAKEINTCONST(A1);
(*2882*)                    GENRXP(OPRX,F1ATTRP@.REXPR.RNO,0,0,0);
(*2883*)                    IF OPRX=ZCL THEN GENRX(ZBC,CONDNZ,0,PBASE1,IC+8);
(*2884*)                    MAKEINTCONST(A2); GENRXP(OPRX,SUCC(F1ATTRP@.REXPR.RNO),0,0,0);
(*2885*)                  END;
(*2886*)           VARBL: BEGIN GENRXP(OPRX,F1ATTRP@.REXPR.RNO,RINDEX,RBASE,EFFADRS);
(*2887*)                    IF OPRX=ZCL THEN GENRX(ZBC,CONDNZ,0,PBASE1,IC+8);
(*2888*)                    GENRXP(OPRX,SUCC(F1ATTRP@.REXPR.RNO),RINDEX,RBASE,EFFADRS+4);
(*2889*)                  END;
(*2890*)           EXPR:  IF REXPR.REGTEMP=REGIST
(*2891*)                    THEN BEGIN GENRRP(OPRR,F1ATTRP@.REXPR.RNO,REXPR.RNO);
(*2892*)                           IF OPRX=ZCL THEN GENRX(ZBC,CONDNZ,0,PBASE1,IC+6);
(*2893*)                           GENRRP(OPRR,SUCC(F1ATTRP@.REXPR.RNO),SUCC(REXPR.RNO));
(*2894*)                         END
(*2895*)                    ELSE BEGIN GENRXP(OPRX,F1ATTRP@.REXPR.RNO,0,RBASE,EFFADRS);
(*2896*)                           IF OPRX=ZCL THEN GENRX(ZBC,CONDNZ,0,PBASE1,IC+8);
(*2897*)                           GENRXP(OPRX,SUCC(F1ATTRP@.REXPR.RNO),0,RBASE,EFFADRS+4);
(*2898*)                         END
(*2899*)         END;
(*2900*)       END;
(*2901*) 
(*2902*)      BEGIN
(*2903*)        WITH F2ATTRP@ DO
(*2904*)          IF KIND=VARBL THEN
(*2905*)            BEGIN LOADINDEX(F2ATTRP,F1ATTRP); LOADBASE(F2ATTRP); END
(*2906*)          ELSE IF KIND=EXPR THEN
(*2907*)            IF REXPR.REGTEMP<>REGIST THEN
(*2908*)              BASEREGISTER(LEVEL,REXPR.ATEMP@.TEMPADRS);
(*2909*)        SETOP1(OPRX1,OPRR1);
(*2910*)        IF OPRX2<>0 THEN SETOP1(OPRX2,OPRR2);
(*2911*)      END;
(*2912*) 
$TITLE  OPERATION
(*2913*)    PROCEDURE OPERATION(F1ATTRP,F2ATTRP:ATTRP; OPRX,OPRR:INTEGER);
(*2914*)      BEGIN
(*2915*)        WITH F2ATTRP@ DO
(*2916*)          BEGIN
(*2917*)            IF KIND=VARBL THEN IF TYPTR@.SIZE.WBLENGTH=1
(*2918*)              THEN LOAD(F2ATTRP,F1ATTRP);
(*2919*)            CASE KIND OF
(*2920*)             CST:   BEGIN MAKECONSTANT(CVAL);
(*2921*)                      GENRXP(OPRX,F1ATTRP@.REXPR.RNO,0,0,0)
(*2922*)                    END;
(*2923*)             VARBL: BEGIN LOADINDEX(F2ATTRP,F1ATTRP); LOADBASE(F2ATTRP);
(*2924*)                      GENRXP(OPRX,F1ATTRP@.REXPR.RNO,RINDEX,RBASE,EFFADRS);
(*2925*)                    END;
(*2926*)             EXPR: IF REXPR.REGTEMP=REGIST
(*2927*)                     THEN GENRRP(OPRR,F1ATTRP@.REXPR.RNO,REXPR.RNO)
(*2928*)                     ELSE BEGIN BASEREGISTER(LEVEL,REXPR.ATEMP@.TEMPADRS);
(*2929*)                                GENRXP(OPRX,F1ATTRP@.REXPR.RNO,0,RBASE,EFFADRS)
(*2930*)                          END
(*2931*)          END;
(*2932*)        END;
(*2933*)      END;
(*2934*) 
$TITLE  INTTOREAL,INTARITH
(*2935*)    PROCEDURE INTTOREAL(FATTRP : ATTRP);
(*2936*)      BEGIN
(*2937*)        LOAD(FATTRP,NIL);
(*2938*)        GENRR(ZLPR,R0,REALREG(.FATTRP@.REXPR.RNO.));
(*2939*)        GENRX(ZST,R0,0,1,IRCONVWORK+4); REGSEARCH(NIL,FLOAT);
(*2940*)        GENRR(ZSDR,RMAIN,RMAIN); GENRX(ZAD,RMAIN,0,1,IRCONVWORK);
(*2941*)        GENRRP1(ZLTR,FATTRP@.REXPR.RNO); GENRX(ZBC,CONDNM,0,PBASE1,IC+6);
(*2942*)        GENRR(ZLNDR,RMAIN,RMAIN);
(*2943*)        WITH FATTRP@ DO
(*2944*)          BEGIN REGISTER(.REXPR.RNO.).USED:=FALSE;
(*2945*)                TYPTR:=REALPTR; REXPR.RNO:=RWORK;
(*2946*)          END;
(*2947*)        REGISTER(.RWORK.).USED:=TRUE; REGISTER(.RWORK.).REGCONT:=FATTRP;
(*2948*)      END;
(*2949*) 
(*2950*)     (*IN THE FOLLOWING ROUTINES CONSIDER THE RULES:
  2951         - IF A ROUTINE HAS TWO ARGUMENTS 'F1ATTRP' AND 'F2ATTRP', THE ARGUMENTS
  2952           HAVE TO BE TAKEN IN THIS ORDER. THE DESCRIPTION OF THE RESULT IS
  2953           ALWAYS TO BE PUT IN 'F2ATTRP@'.
  2954         - IF A ROUTINE HAS ONE ARGUMENT 'FATTRP' THE DESCRIPTION OF THE RESULT
  2955           HAS TO REPLACE THE DESCRIPTION OF THE ARGUMENT IN 'FATTRP@' *)
(*2956*) 
(*2957*)    PROCEDURE INTARITH(F1ATTRP,F2ATTRP:ATTRP; FOP:OPERATOR);
(*2958*)      VAR X:INTEGER;
(*2959*)      BEGIN
(*2960*)        IF FOP IN (.PLUS,MUL.) THEN
(*2961*)          IF F2ATTRP@.KIND=EXPR THEN
(*2962*)            IF F2ATTRP@.REXPR.REGTEMP=REGIST THEN EXCATTR(F1ATTRP,F2ATTRP);
(*2963*)        CASE FOP OF
(*2964*)          PLUS:  BEGIN X:=0; LOAD(F1ATTRP,F2ATTRP); END;
(*2965*)          MINUS: BEGIN X:=ZS-ZA; LOAD(F1ATTRP,F2ATTRP); END;
(*2966*)          MUL:   BEGIN X:=ZM-ZA; LOADEVENODD(F1ATTRP,F2ATTRP,1); END;
(*2967*)          IDIV,IMOD: BEGIN X:=ZD-ZA; LOADEVENODD(F1ATTRP,F2ATTRP,0);
(*2968*)                           GENRXP(ZSRDA,F1ATTRP@.REXPR.RNO,0,0,32);
(*2969*)                     END
(*2970*)        END;
(*2971*)        OPERATION(F1ATTRP,F2ATTRP,ZA+X,ZAR+X);
(*2972*)        IF FOP=MUL THEN
(*2973*)          IF (F2ATTRP@.KIND=EXPR) AND (F2ATTRP@.REXPR.REGTEMP=REGIST)
(*2974*)          THEN MAKECODE(IC-2,GETCODE(IC-2)-16)
(*2975*)          ELSE MAKECODE(IC-4,GETCODE(IC-4)-16);
(*2976*)        IF FOP=IDIV THEN
(*2977*)          WITH F1ATTRP@.REXPR DO
(*2978*)            BEGIN REGISTER(.SUCC(RNO).):=REGISTER(.RNO.);
(*2979*)                  REGISTER(.RNO.).USED:=FALSE; RNO:=SUCC(RNO);
(*2980*)            END;
(*2981*)        EXCATTR(F1ATTRP,F2ATTRP);
(*2982*)      END;
(*2983*) 
$TITLE  REALARITH,SETARITH,NEGATE,NOTFACTOR
(*2984*)    PROCEDURE REALARITH(F1ATTRP,F2ATTRP: ATTRP; FOP: OPERATOR);
(*2985*)      VAR X:INTEGER;
(*2986*)      BEGIN
(*2987*)        IF COMPTYPES(F1ATTRP@.TYPTR,INTPTR) THEN INTTOREAL(F1ATTRP);
(*2988*)        IF COMPTYPES(F2ATTRP@.TYPTR,INTPTR) THEN INTTOREAL(F2ATTRP);
(*2989*)        IF FOP IN (.PLUS,MUL.) THEN
(*2990*)          IF F2ATTRP@.KIND=EXPR THEN
(*2991*)            IF F2ATTRP@.REXPR.REGTEMP=REGIST THEN EXCATTR(F1ATTRP,F2ATTRP);
(*2992*)        LOAD(F1ATTRP,F2ATTRP);
(*2993*)        CASE FOP OF
(*2994*)          PLUS:  X:=0;
(*2995*)          MINUS: X:=ZSD-ZAD;
(*2996*)          MUL:   X:=ZMD-ZAD;
(*2997*)          RDIV:  X:=ZDD-ZAD
(*2998*)        END;
(*2999*)        OPERATION(F1ATTRP,F2ATTRP,ZAD+X,ZADR+X);
(*3000*)        EXCATTR(F1ATTRP,F2ATTRP);
(*3001*)      END;
(*3002*) 
(*3003*)    PROCEDURE SETARITH(F1ATTRP,F2ATTRP: ATTRP; FOP: OPERATOR);
(*3004*)      VAR X:INTEGER;
(*3005*)      BEGIN
(*3006*)        IF FOP<>MINUS THEN
(*3007*)          BEGIN IF F2ATTRP@.KIND=EXPR THEN
(*3008*)              IF F2ATTRP@.REXPR.REGTEMP=REGIST THEN EXCATTR(F1ATTRP,F2ATTRP);
(*3009*)            IF FOP=MUL THEN X:=0 ELSE X:=ZO-ZN;
(*3010*)            LOAD(F1ATTRP,F2ATTRP);
(*3011*)            SETOPERATION(F1ATTRP,F2ATTRP,ZN+X,ZNR+X,0,0);
(*3012*)            EXCATTR(F1ATTRP,F2ATTRP);
(*3013*)          END
(*3014*)        ELSE (*FOP=MINUS*)
(*3015*)          BEGIN LOAD(F2ATTRP,F1ATTRP);
(*3016*)            SETOPERATION(F2ATTRP,F1ATTRP,ZN,ZNR,ZX,ZXR);
(*3017*)          END;
(*3018*)      END;
(*3019*) 
(*3020*)    PROCEDURE NEGATE(FATTRP: ATTRP);
(*3021*)      BEGIN
(*3022*)        WITH FATTRP@ DO
(*3023*)          IF KIND=CST
(*3024*)            THEN IF COMPTYPES(TYPTR,INTPTR)
(*3025*)              THEN CVAL.IVAL:=-CVAL.IVAL
(*3026*)              ELSE CVAL.RVAL:=-CVAL.RVAL
(*3027*)            ELSE
(*3028*)              BEGIN LOAD(FATTRP,NIL);
(*3029*)                IF COMPTYPES(TYPTR,INTPTR)
(*3030*)                  THEN GENRRP1(ZLCR,REXPR.RNO)
(*3031*)                  ELSE GENRRP1(ZLCDR,REXPR.RNO);
(*3032*)              END;
(*3033*)      END;
(*3034*) 
(*3035*)    PROCEDURE NOTFACTOR(FATTRP: ATTRP);
(*3036*)      BEGIN
(*3037*)        LOAD(FATTRP,NIL);
(*3038*)        IF BOOLFLAG THEN
(*3039*)          MAKECODE(IC-6,256*ZBC+240 -(GETCODE(IC-6) MOD 256))
(*3040*)        ELSE BEGIN MAKEINTCONST(1); GENRXP(ZX,FATTRP@.REXPR.RNO,0,0,0); END;
(*3041*)      END;
$TITLE BOOLARITH,BOOLVALUE,RELINT,RELREAL,INPWR
(*3042*)    PROCEDURE BOOLARITH(F1ATTRP,F2ATTRP: ATTRP; FOP: OPERATOR);
(*3043*)      VAR X:INTEGER;
(*3044*)      BEGIN
(*3045*)        IF F2ATTRP@.KIND=EXPR THEN EXCATTR(F1ATTRP,F2ATTRP);
(*3046*)        LOAD(F1ATTRP,F2ATTRP);
(*3047*)        IF FOP=ANDOP THEN X:=0 ELSE X:=ZO-ZN;
(*3048*)        OPERATION(F1ATTRP,F2ATTRP,ZN+X,ZNR+X);
(*3049*)        EXCATTR(F1ATTRP,F2ATTRP);
(*3050*)      END;
(*3051*) 
(*3052*)    PROCEDURE BOOLVALUE(REG,TRUECOND: INTEGER);
(*3053*)      BEGIN GENRX(ZLA,REG,0,0,1);
(*3054*)            GENRX(ZBC,TRUECOND,0,PBASE1,IC+6);
(*3055*)            GENRR(ZXR,REG,REG); BOOLFLAG:=TRUE;
(*3056*)      END;
(*3057*) 
(*3058*)    PROCEDURE RELINT(F1ATTRP,F2ATTRP: ATTRP; FOP: OPERATOR);
(*3059*)      BEGIN
(*3060*)        IF F2ATTRP@.KIND=EXPR THEN
(*3061*)          BEGIN FOP:=DUALOP(.FOP.); EXCATTR(F1ATTRP,F2ATTRP);
(*3062*)          END;
(*3063*)        LOAD(F1ATTRP,F2ATTRP);
(*3064*)        OPERATION(F1ATTRP,F2ATTRP,ZC,ZCR);
(*3065*)        BOOLVALUE(REALREG(.F1ATTRP@.REXPR.RNO.),BMASK(.FOP.));
(*3066*)        F1ATTRP@.TYPTR := BOOLPTR;
(*3067*)        EXCATTR(F1ATTRP,F2ATTRP);
(*3068*)      END;
(*3069*) 
(*3070*)    PROCEDURE RELREAL(F1ATTRP,F2ATTRP: ATTRP; FOP: OPERATOR);
(*3071*)      BEGIN
(*3072*)        IF F2ATTRP@.KIND=EXPR THEN
(*3073*)          BEGIN FOP:=DUALOP(.FOP.); EXCATTR(F1ATTRP,F2ATTRP); END;
(*3074*)        LOAD(F1ATTRP,F2ATTRP);
(*3075*)        OPERATION(F1ATTRP,F2ATTRP,ZCD,ZCDR);
(*3076*)        REGISTER(.F1ATTRP@.REXPR.RNO.).USED:=FALSE;
(*3077*)        REGSEARCH(NIL,SINGLE);
(*3078*)        BOOLVALUE(RMAIN,BMASK(.FOP.));
(*3079*)        WITH F1ATTRP@ DO
(*3080*)          BEGIN TYPTR:=BOOLPTR; REXPR.RNO:=RWORK; END;
(*3081*)        REGISTER(.RWORK.).USED:=TRUE; REGISTER(.RWORK.).REGCONT:=F1ATTRP;
(*3082*)        EXCATTR(F1ATTRP,F2ATTRP);
(*3083*)      END;
(*3084*) 
(*3085*)    PROCEDURE INPOWER(F1ATTRP,F2ATTRP: ATTRP);
(*3086*)      BEGIN
(*3087*)        LOAD(F2ATTRP,F1ATTRP); LOAD(F1ATTRP,F2ATTRP);
(*3088*)        GENRXP(ZSLDL,F2ATTRP@.REXPR.RNO,0,REALREG(.F1ATTRP@.REXPR.RNO.),0);
(*3089*)        GENRRP1(ZLTR,F2ATTRP@.REXPR.RNO);
(*3090*)        EXCATTR(F1ATTRP,F2ATTRP);
(*3091*)        BOOLVALUE(REALREG(.F2ATTRP@.REXPR.RNO.),CONDM);
(*3092*)        F2ATTRP@.TYPTR:=BOOLPTR;
(*3093*)      END;
(*3094*) 
(*3095*)    PROCEDURE RELPOWER(F1ATTRP,F2ATTRP: ATTRP; FOP: OPERATOR);
(*3096*)      BEGIN
(*3097*)        IF FOP=LEOP THEN BEGIN EXCATTR(F1ATTRP,F2ATTRP); FOP:=GEOP END
(*3098*)          ELSE IF FOP<>GEOP THEN
(*3099*)            IF F2ATTRP@.KIND=EXPR THEN
(*3100*)              IF F2ATTRP@.REXPR.REGTEMP=REGIST THEN EXCATTR(F1ATTRP,F2ATTRP);
(*3101*)        LOAD(F1ATTRP,F2ATTRP);
(*3102*)        IF FOP=GEOP THEN SETOPERATION(F1ATTRP,F2ATTRP,ZN,ZNR,ZCL,ZCLR)
(*3103*)                    ELSE SETOPERATION(F1ATTRP,F2ATTRP,ZCL,ZCLR,0,0);
(*3104*)        IF FOP=NEOP
(*3105*)          THEN BOOLVALUE(REALREG(.F1ATTRP@.REXPR.RNO.),CONDNZ)
(*3106*)          ELSE BOOLVALUE(REALREG(.F1ATTRP@.REXPR.RNO.),CONDZ);
(*3107*)        REGISTER(.SUCC(F1ATTRP@.REXPR.RNO).).USED:=FALSE;
(*3108*)        F1ATTRP@.TYPTR:=BOOLPTR; EXCATTR(F1ATTRP,F2ATTRP);
(*3109*)      END;
(*3110*) 
$TITLE LONGOPERATION,SSOPERAND
(*3111*)    PROCEDURE LONGOPERATION(F1ATTRP,F2ATTRP:ATTRP; OPSS,ENTRY:INTEGER);
(*3112*)      VAR LENGTH,BR1,BR2,DISPL1,DISPL2: INTEGER;
(*3113*) 
(*3114*)     PROCEDURE SSOPERAND(F1ATTRP,F2ATTRP:ATTRP; VAR BR,DISPL:INTEGER);
(*3115*)       VAR VARFLAG:BOOLEAN;
(*3116*)       BEGIN WITH F1ATTRP@ DO
(*3117*)         BEGIN VARFLAG:=TRUE;
(*3118*)           LOADINDEX(F1ATTRP,F2ATTRP);
(*3119*)           IF RINDEX=0 THEN
(*3120*)             BEGIN IF VARKIND<>DRCT THEN
(*3121*)               BEGIN REGSEARCH(F2ATTRP,SINGLE); BASEREGISTER(BASELEV,BASEADD);
(*3122*)                     GENRX(ZL,RMAIN,0,RBASE,EFFADRS); VARFLAG:=FALSE;
(*3123*)               END;
(*3124*)              END
(*3125*)           ELSE BEGIN RMAIN:=RINDEX; VARFLAG:=FALSE;
(*3126*)                  IF VARKIND<>DRCT THEN
(*3127*)                    BEGIN BASEREGISTER(BASELEV,BASEADD); GENRX(ZA,RMAIN,0,RBASE,EFFADRS); END
(*3128*)                  ELSE IF VLEVEL<>0 THEN GENRR(ZAR,RMAIN,VLEVEL);
(*3129*)                END;
(*3130*)           IF (VADRS>=4096) OR (VADRS<0) THEN
(*3131*)             BEGIN IF VARFLAG THEN
(*3132*)                 BEGIN REGSEARCH(F2ATTRP,SINGLE); VARFLAG:=FALSE; GENRR(ZLR,RMAIN,VLEVEL); END;
(*3133*)               DISPLACEMENT(VADRS,VADRS); GENRX(ZA,RMAIN,0,0,0);
(*3134*)             END;
(*3135*)           DISPL:=VADRS;
(*3136*)           IF VARFLAG THEN BR:=VLEVEL
(*3137*)             ELSE BEGIN TYPTR:=INTPTR; KIND:=EXPR; REXPR.REGTEMP:=REGIST;
(*3138*)                        REXPR.RNO:=RWORK; REGISTER(.RWORK.).USED:=TRUE;
(*3139*)                        REGISTER(.RWORK.).REGCONT:=F1ATTRP; BR:=RMAIN;
(*3140*)                  END;
(*3141*)         END;
(*3142*)       END;
(*3143*) 
(*3144*)      BEGIN LENGTH:=F1ATTRP@.TYPTR@.SIZE.WBLENGTH;
(*3145*)        IF LENGTH<=256 THEN
(*3146*)          BEGIN
(*3147*)            WITH F1ATTRP@ DO CASE KIND OF
(*3148*)              EXPR:  ERROR(400);
(*3149*)              CST:   BEGIN MAKECONSTANT(CVAL); BR1:=0; DISPL1:=0; END;
(*3150*)              VARBL: SSOPERAND(F1ATTRP,F2ATTRP,BR1,DISPL1)
(*3151*)            END;
(*3152*)            WITH F2ATTRP@ DO CASE KIND OF
(*3153*)              EXPR:  ERROR(400);
(*3154*)              CST:   BEGIN IC:=IC+2; MAKECONSTANT(CVAL); IC:=IC-2;
(*3155*)                       BR2:=0; DISPL2:=0;
(*3156*)                     END;
(*3157*)              VARBL: SSOPERAND(F2ATTRP,F1ATTRP,BR2,DISPL2);
(*3158*)            END;
(*3159*)            GENSS(OPSS,LENGTH-1,BR1,DISPL1,BR2,DISPL2);
(*3160*)          END
(*3161*)        ELSE
(*3162*)          BEGIN LOADADDRESS(F1ATTRP,F2ATTRP); LOADADDRESS(F2ATTRP,F1ATTRP);
(*3163*)            LOADINTCONST(R0,256*LENGTH+16*REALREG(.F1ATTRP@.REXPR.RNO.)+REALREG(.F2ATTRP@.REXPR.RNO.));
(*3164*)            GENRX(ZBAL,BASEWORK,0,1,ENTRY);
(*3165*)          END;
(*3166*)      END;
(*3167*) 
$TITLE ASSIGNLONG,RELLONG
(*3168*)    PROCEDURE ASSIGNLONG(F1ATTRP,F2ATTRP:ATTRP);
(*3169*)      BEGIN
(*3170*)        LONGOPERATION(F1ATTRP,F2ATTRP,ZMVC,ENTRYAL);
(*3171*)      END;
(*3172*) 
(*3173*)    PROCEDURE RELLONG(F1ATTRP,F2ATTRP:ATTRP; FOP:OPERATOR);
(*3174*)      BEGIN
(*3175*)        LONGOPERATION(F1ATTRP,F2ATTRP,ZCLC,ENTRYCL);
(*3176*)        IF F2ATTRP@.KIND<>EXPR THEN
(*3177*)          IF F1ATTRP@.KIND=EXPR THEN EXCATTR(F1ATTRP,F2ATTRP)
(*3178*)            ELSE BEGIN REGSEARCH(NIL,SINGLE);
(*3179*)                   WITH F2ATTRP@ DO
(*3180*)                     BEGIN KIND:=EXPR; REXPR.REGTEMP:=REGIST;
(*3181*)                           REXPR.RNO:=RWORK;
(*3182*)                     END;
(*3183*)                   REGISTER(.RWORK.).USED:=TRUE; REGISTER(.RWORK.).REGCONT:=F2ATTRP;
(*3184*)                 END;
(*3185*)        BOOLVALUE(REALREG(.F2ATTRP@.REXPR.RNO.),BMASK(.FOP.));
(*3186*)        F2ATTRP@.TYPTR:=BOOLPTR;
(*3187*)      END;
(*3188*) 
$TITLE CHKREG,CHKRANGE,CHKPOINTER,OVFLOW
(*3189*)    PROCEDURE CHECKREGISTER(R,FMIN,FMAX:INTEGER);
(*3190*)    BEGIN
(*3191*)            GENRR(ZBALR,9,0);
(*3192*)            MAKEINTCONST(FMIN); GENRX(ZC,R,0,0,0);
(*3193*)            GENRX(ZBC,CONDM,0,1,JUMPERR2);
(*3194*)            MAKEINTCONST(FMAX); GENRX(ZC,R,0,0,0);
(*3195*)            GENRX(ZBC,CONDP,0,1,JUMPERR2);
(*3196*)      END;
(*3197*) 
(*3198*)    PROCEDURE CHECKRANGE(FATTRP:ATTRP; FMIN,FMAX,ERRORNO:INTEGER);
(*3199*)      BEGIN
(*3200*)        IF FATTRP@.KIND=CST THEN
(*3201*)          BEGIN IF (FATTRP@.CVAL.IVAL<FMIN) OR (FATTRP@.CVAL.IVAL>FMAX) THEN ERROR(ERRORNO)
(*3202*)          END
(*3203*)        ELSE IF DEBUG THEN
(*3204*)          BEGIN LOAD(FATTRP,NIL);
(*3205*)            CHECKREGISTER(REALREG(.FATTRP@.REXPR.RNO.),FMIN,FMAX);
(*3206*)          END;
(*3207*)      END;
(*3208*) 
(*3209*)    PROCEDURE CHECKPOINTER(FATTRP: ATTRP; NILALLOWED: BOOLEAN);
(*3210*)      BEGIN
(*3211*)        IF FATTRP@.KIND=CST THEN
(*3212*)          BEGIN IF NOT NILALLOWED THEN ERROR(305); END
(*3213*)        ELSE
(*3214*)          IF DEBUG THEN
(*3215*)            BEGIN LOAD(FATTRP,NIL);
(*3216*)              GENRR(ZBALR,9,0);
(*3217*)              IF NILALLOWED THEN
(*3218*)                BEGIN GENRRP1(ZLTR,FATTRP@.REXPR.RNO);
(*3219*)                      GENRX(ZBC,CONDZ,0,PBASE1,IC+18);
(*3220*)                END;
(*3221*)              GENRR(ZCLR,REALREG(.FATTRP@.REXPR.RNO.),NEWPOINTER);
(*3222*)              GENRX(ZBC,CONDM,0,1,JUMPERR3);
(*3223*)              GENRXP(ZCL,FATTRP@.REXPR.RNO,0,1,NPINIT);
(*3224*)                 GENRX(ZBC,CONDP,0,1,JUMPERR3);                                 
(*3225*)            END;
(*3226*)      END;
(*3227*) 
(*3228*)    PROCEDURE OVERFLOWTEST;
(*3229*)      BEGIN
(*3230*)        IF DEBUG THEN GENRR(ZBALR,9,0);
(*3231*)        GENRR(ZCLR,NEWPOINTER,STACKPOINTER);
(*3232*)        GENRX(ZBC,CONDM,0,1,JUMPERR4);
(*3233*)      END;
(*3234*) 
$TITLE  STORE,ASSIGN
(*3235*)    PROCEDURE STORE(F1ATTRP,F2ATTRP:ATTRP; ERRORNO:INTEGER);
(*3236*)      VAR LMIN,LMAX:INTEGER;
(*3237*) 
(*3238*)      PROCEDURE ASSIGN(X:INTEGER);
(*3239*)        BEGIN LOAD(F2ATTRP,F1ATTRP); LOADINDEX(F1ATTRP,F2ATTRP);
(*3240*)          LOADBASE(F1ATTRP);
(*3241*)          GENRXP(X,F2ATTRP@.REXPR.RNO,RINDEX,RBASE,EFFADRS);
(*3242*)        END;
(*3243*) 
(*3244*)      BEGIN
(*3245*)        IF F1ATTRP@.TYPTR=REALPTR THEN
(*3246*)          BEGIN
(*3247*)            IF COMPTYPES(F2ATTRP@.TYPTR,INTPTR) THEN
(*3248*)              INTTOREAL(F2ATTRP);
(*3249*)            IF F2ATTRP@.TYPTR=REALPTR THEN ASSIGN(ZSTD)
(*3250*)              ELSE ERROR(ERRORNO)
(*3251*)          END
(*3252*)        ELSE
(*3253*)          IF COMPTYPES(F2ATTRP@.TYPTR,F1ATTRP@.TYPTR) THEN
(*3254*)            CASE F1ATTRP@.TYPTR@.FORM OF
(*3255*)              SCALAR,
(*3256*)              SUBRANGE:
(*3257*)                BEGIN
(*3258*)                  IF F1ATTRP@.TYPTR <> INTPTR THEN
(*3259*)                    BEGIN
(*3260*)                      GETBOUNDS(F1ATTRP@.TYPTR,LMIN,LMAX);
(*3261*)                      CHECKRANGE(F2ATTRP,LMIN,LMAX,303);
(*3262*)                    END;
(*3263*)                  ASSIGN(ZST)
(*3264*)                END;
(*3265*)              PACKDTYPE: ASSIGN(ZSTC);
(*3266*)              POINTER:
(*3267*)                BEGIN CHECKPOINTER(F2ATTRP,TRUE); ASSIGN(ZST) END;
(*3268*)              POWER: BEGIN LOAD(F2ATTRP,F1ATTRP); LOADINDEX(F1ATTRP,F2ATTRP);
(*3269*)                       LOADBASE(F1ATTRP);
(*3270*)                       GENRXP(ZST,F2ATTRP@.REXPR.RNO,RINDEX,RBASE,EFFADRS);
(*3271*)                       GENRXP(ZST,SUCC(F2ATTRP@.REXPR.RNO),RINDEX,RBASE,EFFADRS+4);
(*3272*)                     END;
(*3273*)              ARRAYS,
(*3274*)              RECORDS: IF F1ATTRP@.TYPTR@.FTYPE THEN ERROR(146)
(*3275*)                         ELSE ASSIGNLONG(F1ATTRP,F2ATTRP);
(*3276*)              FILES: ERROR(146)
(*3277*)            END
(*3278*)          ELSE ERROR(ERRORNO)
(*3279*)      END;
(*3280*) 
(*3281*) 
$TITLE  SELECTOR,IDADDRESS
(*3282*)    PROCEDURE EXPRESSION(FSYS: SETOFSYS); FORWARD;
(*3283*) 
(*3284*)    PROCEDURE SELECTOR(FSYS: SETOFSYS; FCP: CTP);
(*3285*)     VAR LATTRP: ATTRP; LCP: CTP;
(*3286*) 
(*3287*)     PROCEDURE IDADDRESS;
(*3288*)      (* PUT IN 'LATTRP@' THE DESCRIPTION OF THE IDENTIFIER POINTED AT BY 'FCP'.
  3289         USEFUL GLOBAL VARIABLES:
  3290          LATTRP: ATTRP (POINTS TO THE ATTRIBUTE TO BE BUILT UP)
  3291          FCP: CTP; (POINTS TO THE IDENTIFIER WE ARE WORKING ON)
  3292          DISX: DISPRANGE (IN THE CASE WHERE THE IDENTIFIER 'ID' IS A FIELD
  3293                           IDENTIFIER: 'DISX' IS THE LEVEL ON WHICH 'ID'
  3294                           WAS DEFINED)  *)
(*3295*)     BEGIN
(*3296*)      WITH FCP@, LATTRP@ DO
(*3297*)       BEGIN TYPTR := IDTYPE; KIND := VARBL;
(*3298*)        IF TYPTR=NIL THEN
(*3299*)          BEGIN VADRS:=0; ACCESS:=DIRECT;
(*3300*)                VARKIND:=DRCT; VLEVEL:=0;
(*3301*)          END
(*3302*)        ELSE
(*3303*)         CASE KLASS OF
(*3304*)          VARS:
(*3305*)           BEGIN
(*3306*)             IF VKIND=DRCT THEN BEGIN VLEVEL:=VLEV; VADRS:=VADDR END
(*3307*)                 ELSE BEGIN BASELEV:=VLEV; BASEADD:=PARADDR; VADRS:=0 END;
(*3308*)               ACCESS:=DIRECT; VARKIND:=VKIND;
(*3309*)           END;
(*3310*)          FIELD:
(*3311*)           WITH DISPLAY(.DISX.) DO
(*3312*)            BEGIN
(*3313*)              VADRS:=DADRS+FLDADDR; ACCESS:=DIRECT; VARKIND:=DISPKIND;
(*3314*)              IF VARKIND=DRCT THEN VLEVEL:=DLEVEL
(*3315*)                 ELSE BEGIN BASELEV:=DBASEL; BASEADD:=DBASEA END;
(*3316*)            END;
(*3317*)          FUNC:
(*3318*)           IF PFDECKIND = STANDARD THEN ERROR(150)
(*3319*)             ELSE IF PFKIND = FORMAL THEN ERROR(151)
(*3320*)               ELSE IF PFLEV = LEVEL THEN ERROR(182)
(*3321*)                 ELSE
(*3322*)                  BEGIN VLEVEL:=PFLEV+1; VARKIND:=DRCT;
(*3323*)                        VADRS:=SAVEAREA; ACCESS:=DIRECT;
(*3324*)                  END
(*3325*)         END (*CASE*)
(*3326*)       END (*WITH*)
(*3327*)     END (*IDADDRESS*) ;
(*3328*) 
$TITLE INDEXCODE
(*3329*)    PROCEDURE INDEXCODE;
(*3330*)      LABEL 1;
(*3331*)      VAR ATTRWORK:ATTRP; LMIN,LMAX,LENGTH,SHIFT,N:INTEGER;
(*3332*)      BEGIN
(*3333*)        LENGTH:=LATTRP@.TYPTR@.AELLENG;
(*3334*)        GETBOUNDS(LATTRP@.TYPTR@.INXTYPE,LMIN,LMAX);
(*3335*)        CHECKRANGE(GATTRP,LMIN,LMAX,302);
(*3336*)        IF GATTRP@.KIND=CST
(*3337*)          THEN LATTRP@.VADRS:=LATTRP@.VADRS+(GATTRP@.CVAL.IVAL-LMIN)*LENGTH
(*3338*)        ELSE
(*3339*)          BEGIN
(*3340*)            LATTRP@.VADRS:=LATTRP@.VADRS-LMIN*LENGTH;
(*3341*)            LOAD(GATTRP,NIL);
(*3342*)            IF LENGTH<>1 THEN
(*3343*)              BEGIN N:=2;
(*3344*)                FOR SHIFT:=1 TO 12 DO
(*3345*)                  BEGIN
(*3346*)                    IF LENGTH=N THEN
(*3347*)                      BEGIN GENRXP(ZSLA,GATTRP@.REXPR.RNO,0,0,SHIFT);
(*3348*)                            GOTO 1;
(*3349*)                      END;
(*3350*)                    N:=N*2;
(*3351*)                  END;
(*3352*)                ATTRNEW(ATTRWORK);
(*3353*)                WITH ATTRWORK@ DO
(*3354*)                  BEGIN TYPTR:=INTPTR; KIND:=CST; CVAL.CKIND:=INT;
(*3355*)                        CVAL.IVAL:=LENGTH;
(*3356*)                  END;
(*3357*)                INTARITH(ATTRWORK,GATTRP,MUL);
(*3358*)                ATTRDISP(ATTRWORK);
(*3359*)              END;
(*3360*)        1:  IF LATTRP@.ACCESS=INDIRECT THEN
(*3361*)              BEGIN ATTRNEW(ATTRWORK);
(*3362*)                WITH ATTRWORK@ DO
(*3363*)                  BEGIN TYPTR:=INTPTR; KIND:=EXPR;
(*3364*)                        REXPR:=LATTRP@.INDEXREG;
(*3365*)                        IF REXPR.REGTEMP=REGIST
(*3366*)                          THEN REGISTER(.REXPR.RNO.).REGCONT:=ATTRWORK
(*3367*)                          ELSE REXPR.ATEMP@.TEMPCONT:=ATTRWORK;
(*3368*)                  END;
(*3369*)                INTARITH(ATTRWORK,GATTRP,PLUS); ATTRDISP(ATTRWORK);
(*3370*)              END;
(*3371*)            WITH LATTRP@ DO
(*3372*)              BEGIN INDEXREG:=GATTRP@.REXPR;
(*3373*)                    ACCESS:=INDIRECT; KIND:=VARBL;
(*3374*)                    REGISTER(.INDEXREG.RNO.).USED:=TRUE;
(*3375*)                    REGISTER(.INDEXREG.RNO.).REGCONT:=LATTRP;
(*3376*)              END;
(*3377*)          END;
(*3378*)     END (*INDEXCODE*);
(*3379*) 
$TITLE  RECFIELD,FILEBUFFER,POINTDELEMENT
(*3380*)    PROCEDURE RECFIELD;
(*3381*)      BEGIN WITH LCP@, LATTRP@ DO
(*3382*)        BEGIN
(*3383*)          VADRS:=VADRS+FLDADDR;
(*3384*)          TYPTR:=IDTYPE;
(*3385*)          KIND := VARBL;
(*3386*)        END
(*3387*)      END;
(*3388*) 
(*3389*)    PROCEDURE FILEBUFFER;
(*3390*)      VAR R:REGNO;
(*3391*)      BEGIN
(*3392*)        WITH LATTRP@ DO
(*3393*)          BEGIN
(*3394*)            IF TYPTR@.TEXTFILE
(*3395*)              THEN BEGIN LOADADDRESS(LATTRP,NIL); R:=REXPR.RNO;
(*3396*)                     GENRXP(ZL,R,0,REALREG(.R.),8);
(*3397*)                     ACCESS:=INDIRECT; INDEXREG.REGTEMP:=REGIST;
(*3398*)                     INDEXREG.RNO:=R;VARKIND:=DRCT;
(*3399*)                     VADRS:=0; VLEVEL:=0;
(*3400*)                     TYPTR:=PACKDCHARPTR;
(*3401*)                   END
(*3402*)              ELSE BEGIN VADRS:=VADRS+8; TYPTR:=TYPTR@.FILTYPE; END;
(*3403*)            KIND:=VARBL;
(*3404*)          END;
(*3405*)      END;
(*3406*) 
(*3407*)    PROCEDURE POINTEDELEMENT;
(*3408*)      VAR WORK:REGORTEMP;
(*3409*)      BEGIN
(*3410*)        WITH LATTRP@ DO
(*3411*)          BEGIN
(*3412*)            CHECKPOINTER(LATTRP,FALSE);
(*3413*)            LOAD(LATTRP,NIL); WORK:=REXPR;
(*3414*)            INDEXREG:=WORK; ACCESS:=INDIRECT;
(*3415*)            VADRS:=0; VARKIND:=DRCT; VLEVEL:=0;
(*3416*)            TYPTR := TYPTR@.ELTYPE; KIND := VARBL;
(*3417*)          END
(*3418*)      END;
(*3419*) 
$TITLE SELECTOR - (BODY)
(*3420*)    BEGIN (*SELECTOR*)
(*3421*)     ATTRNEW(LATTRP);
(*3422*)     IDADDRESS;
(*3423*)     IF NOT (SY IN SELECTSYS+FSYS) THEN
(*3424*)      BEGIN ERROR(59); SKIP(SELECTSYS+FSYS) END;
(*3425*)     WHILE SY IN SELECTSYS DO
(*3426*)      BEGIN
(*3427*)(*(.*)   IF SY = LBRACK THEN
(*3428*)        BEGIN
(*3429*)         REPEAT
(*3430*)          WITH LATTRP@ DO
(*3431*)           IF TYPTR <> NIL THEN
(*3432*)            IF TYPTR@.FORM <> ARRAYS THEN
(*3433*)             BEGIN ERROR(138); TYPTR := NIL END;
(*3434*)          INSYMBOL; EXPRESSION(FSYS+(.COMMA,RBRACK.));
(*3435*)          IF GATTRP@.TYPTR <> NIL THEN
(*3436*)           IF GATTRP@.TYPTR@.FORM > SUBRANGE THEN ERROR(113);
(*3437*)          IF LATTRP@.TYPTR <> NIL THEN
(*3438*)           WITH LATTRP@.TYPTR@ DO
(*3439*)            BEGIN
(*3440*)             IF COMPTYPES(INXTYPE,GATTRP@.TYPTR) THEN
(*3441*)              BEGIN
(*3442*)               IF (INXTYPE <> NIL)AND (AELTYPE <> NIL) THEN INDEXCODE
(*3443*)              END
(*3444*)             ELSE ERROR(139);
(*3445*)             LATTRP@.TYPTR := AELTYPE
(*3446*)            END
(*3447*)         UNTIL SY <> COMMA;
(*3448*)         TEST1(RBRACK,12);
(*3449*)        END (*IF SY = LBRACK*)
(*3450*)       ELSE
(*3451*)(*.*)    IF SY = PERIOD THEN
(*3452*)         BEGIN
(*3453*)          WITH LATTRP@ DO
(*3454*)           BEGIN
(*3455*)            IF TYPTR <> NIL THEN
(*3456*)             IF TYPTR@.FORM <> RECORDS THEN
(*3457*)              BEGIN ERROR(140); TYPTR := NIL END;
(*3458*)            INSYMBOL;
(*3459*)            IF SY = IDENT THEN
(*3460*)             BEGIN
(*3461*)              IF TYPTR <> NIL THEN
(*3462*)               BEGIN SEARCHSECTION(TYPTR@.FIELDS,LCP);
(*3463*)                IF LCP = NIL THEN
(*3464*)                 BEGIN ERROR(152); TYPTR := NIL END
(*3465*)                ELSE
(*3466*)                 RECFIELD;
(*3467*)               END;
(*3468*)              INSYMBOL
(*3469*)             END (*SY = IDENT*)
(*3470*)            ELSE ERROR(2)
(*3471*)           END (*WITH LATTRP@*)
(*3472*)         END (*IF SY = PERIOD*)
(*3473*)        ELSE
(*3474*)(*@*)    BEGIN
(*3475*)          IF LATTRP@.TYPTR <> NIL THEN
(*3476*)           BEGIN
(*3477*)            WITH LATTRP@.TYPTR@ DO
(*3478*)             IF FORM = FILES THEN FILEBUFFER
(*3479*)             ELSE
(*3480*)              IF FORM = POINTER THEN POINTEDELEMENT
(*3481*)              ELSE ERROR(141);
(*3482*)           END;
(*3483*)          INSYMBOL
(*3484*)         END;
(*3485*)         TEST2(FSYS+SELECTSYS,6,(. .));
(*3486*)      END (*WHILE*) ;
(*3487*)     COPYATTR(LATTRP,GATTRP);
(*3488*)     ATTRDISP(LATTRP);
(*3489*)    END (*SELECTOR*) ;
(*3490*) 
(*3491*) 
(*3492*)    PROCEDURE CALL(FSYS: SETOFSYS; FCP: CTP);
(*3493*)     VAR LKEY: 1..NRSTDNAMES;
(*3494*) 
(*3495*)     PROCEDURE VARIABLE(FSYS: SETOFSYS);
(*3496*)       VAR LCP: CTP;
(*3497*)       BEGIN
(*3498*)         IF SY = IDENT
(*3499*)           THEN BEGIN SEARCHID((.VARS,FIELD.),LCP); INSYMBOL END
(*3500*)           ELSE BEGIN ERROR(2); LCP := UVARPTR END;
(*3501*)         SELECTOR(FSYS,LCP)
(*3502*)       END;
(*3503*) 
$TITLE STDFLPROCS,SETSFILATTR
(*3504*)     PROCEDURE STDFLPROCS;
(*3505*)       VAR ENTRY:INTEGER;
(*3506*)     BEGIN
(*3507*)        TEST1(LPARENT,9);
(*3508*)         VARIABLE(FSYS+(.COMMA,RPARENT.));
(*3509*)         WITH GATTRP@ DO
(*3510*)           IF TYPTR <> NIL THEN
(*3511*)             IF TYPTR@.FORM = FILES THEN
(*3512*)               BEGIN
(*3513*)                 IF TYPTR@.TEXTFILE
(*3514*)                   THEN ENTRY:=ENTGETCH+8*(LKEY-1)
(*3515*)                   ELSE ENTRY:=ENTRYGET+8*(LKEY-1);
(*3516*)                 LOADADDRESS(GATTRP,NIL); GENRR(ZLR,15,REALREG(.REXPR.RNO.));
(*3517*)                 GENRX(ZBAL,BASEWORK,0,1,ENTRY);
(*3518*)                 RESETG;
(*3519*)               END
(*3520*)             ELSE ERROR(116);
(*3521*)         TEST1(RPARENT,4);
(*3522*)       END;
(*3523*) 
(*3524*)      PROCEDURE SETSTFILATTR(FATTRP:ATTRP; FCP:CTP);
(*3525*)        BEGIN
(*3526*)          WITH FATTRP@ DO
(*3527*)            BEGIN TYPTR:=TEXTPTR; KIND:=VARBL; ACCESS:=DIRECT;
(*3528*)              IF FCP=OUTPUTPTR
(*3529*)                THEN BEGIN VARKIND:=INDRCT; BASELEV:=1;
(*3530*)                       BASEADD:=PTROUTBLCK; VADRS:=0;
(*3531*)                     END
(*3532*)                ELSE BEGIN VARKIND:=DRCT; VLEVEL:=1; VADRS:=LCSTART; END;
(*3533*)            END;
(*3534*)        END;
(*3535*) 
$TITLE  STDWIDTH,STRINGIO
(*3536*)PROCEDURE STDWIDTH(VAR FORMP:ATTRP; WIDTH : INTEGER);
(*3537*) VAR SW:BOOLEAN;
(*3538*)BEGIN (* STDWIDTH *)
(*3539*)  IF FORMP = NIL THEN SW:=TRUE
(*3540*)    ELSE IF FORMP@.TYPTR = NIL THEN SW := TRUE
(*3541*)      ELSE SW := FALSE;
(*3542*)  IF SW THEN
(*3543*)  BEGIN
(*3544*)    ATTRNEW(FORMP);
(*3545*)    WITH FORMP@ DO
(*3546*)    BEGIN
(*3547*)      TYPTR := INTPTR; KIND := CST;
(*3548*)      CVAL.CKIND := INT; CVAL.IVAL := WIDTH;
(*3549*)    END;
(*3550*)  END;
(*3551*)END; (* STDWIDTH *)
(*3552*) 
(*3553*) 
(*3554*)PROCEDURE STRINGIO(VAR LATTRP,FORM1P:ATTRP; ENTRY:INTEGER);
(*3555*) VAR LENGTH : INTEGER;
(*3556*) BEGIN (* STRINGIO *)
(*3557*)   LENGTH := LATTRP@.TYPTR@.SIZE.WBLENGTH;
(*3558*)   LOADADDRESS(LATTRP,FORM1P);
(*3559*)   STDWIDTH(FORM1P,LENGTH); LOAD(FORM1P,LATTRP);
(*3560*)   LOADINTCONST(R0, 256*LENGTH +
(*3561*)            16*REALREG(.LATTRP@.REXPR.RNO.)+
(*3562*)            REALREG(.FORM1P@.REXPR.RNO.));
(*3563*)   GENRX(ZBAL,BASEWORK,0,1,ENTRY);
(*3564*) END; (* STRINGIO *)
(*3565*) 
(*3566*) 
$TITLE  READWRITE;
(*3567*)     PROCEDURE READWRITE;
(*3568*)       VAR GETIN,DEFAULT:BOOLEAN; ENTRY:INTEGER; FILATTRP:ATTRP;
(*3569*)          FORM1P,FORM2P,LATTRP,FIL1ATTRP:ATTRP;
(*3570*)          LSP:STP;
(*3571*)          FCP:CTP;
(*3572*) 
(*3573*) 
(*3574*)PROCEDURE WRITEINT(ENTRY,STDLENG:INTEGER);
(*3575*) BEGIN
(*3576*)   LOAD(LATTRP,FORM1P);
(*3577*)   STDWIDTH(FORM1P,STDLENG); LOAD(FORM1P,LATTRP);
(*3578*)   LOADINTCONST(R0,16*REALREG(.LATTRP@.REXPR.RNO.)+
(*3579*)                      REALREG(.FORM1P@.REXPR.RNO.));
(*3580*)   GENRX(ZBAL,BASEWORK,0,1,ENTRY);
(*3581*) END;
(*3582*) 
(*3583*) 
(*3584*)PROCEDURE WRITEREAL;
(*3585*) VAR ENTRY:INTEGER;
(*3586*) BEGIN (* WRITEREAL *)
(*3587*)   LOAD(LATTRP,NIL);
(*3588*)   STDWIDTH(FORM1P,24);
(*3589*)   IF FORM2P = NIL THEN ENTRY:=ENTRYWR1 ELSE ENTRY:=ENTRYWR2;
(*3590*)   STDWIDTH(FORM2P,0);
(*3591*)   LOAD(FORM1P,NIL);  LOAD(FORM2P,FORM1P);
(*3592*)   LOADINTCONST(R0,256*REALREG(.LATTRP@.REXPR.RNO.)
(*3593*)             +16*REALREG(.FORM1P@.REXPR.RNO.)+
(*3594*)             REALREG(.FORM2P@.REXPR.RNO.));
(*3595*)   GENRX(ZBAL,BASEWORK,0,1,ENTRY);
(*3596*) END; (* WRITEREAL *)
(*3597*) 
(*3598*) 
(*3599*)       BEGIN
(*3600*)         ATTRNEW(FILATTRP);ATTRNEW(FIL1ATTRP);
(*3601*)         IF (LKEY<=7) THEN FCP:=INPUTPTR ELSE
(*3602*)          FCP:=OUTPUTPTR;
(*3603*)         SETSTFILATTR(FILATTRP,FCP);
(*3604*)         GETIN:=FALSE; DEFAULT:=TRUE;
(*3605*)         IF SY=LPARENT THEN
(*3606*)            BEGIN
(*3607*)             GETIN:=TRUE; INSYMBOL;
(*3608*)             IF LKEY<=7 THEN
(*3609*)             VARIABLE(FSYS+(.COMMA,RPARENT,COLON,IDENT.))
(*3610*)             ELSE
(*3611*)             EXPRESSION(FSYS+(.COMMA,COLON,RPARENT,IDENT.));
(*3612*)             IF GATTRP@.TYPTR<>NIL THEN
(*3613*)               IF GATTRP@.TYPTR@.FORM=FILES THEN
(*3614*)                 BEGIN
(*3615*)                     IF NOT GATTRP@.TYPTR@.TEXTFILE THEN
(*3616*)                        IF EXTWARN THEN ERROR(291);
(*3617*)                   COPYATTR(GATTRP,FILATTRP); DEFAULT:=FALSE;
(*3618*)                   IF SY=RPARENT
(*3619*)                     THEN BEGIN INSYMBOL; GETIN:=FALSE; END
(*3620*)                     ELSE IF SY=COMMA THEN
(*3621*)                        BEGIN INSYMBOL;
(*3622*)                       IF LKEY <=7 THEN VARIABLE(FSYS+
(*3623*)                            (.COMMA,RPARENT.))
(*3624*)                         ELSE EXPRESSION(FSYS+(.COMMA,COLON,
(*3625*)                                 RPARENT,IDENT.))
(*3626*)                         END;
(*3627*)                 END;
(*3628*)           END;
(*3629*)         IF DEFAULT THEN
(*3630*)          IF FCP=NIL THEN
(*3631*)            IF LKEY<=7 THEN ERROR(175) ELSE ERROR(176);
(*3632*)          COPYATTR(FILATTRP,FIL1ATTRP);
(*3633*)          PROCPASS:=FALSE;
(*3634*)         LOADADDRESS(FILATTRP,NIL); GENRR(ZLR,15,REALREG(.FILATTRP@.REXPR.RNO.));
(*3635*)         ATTRDISP(FILATTRP);
(*3636*)         IF GETIN THEN
(*3637*)           BEGIN
(*3638*)             LOOP ENTRY:=0;
(*3639*)            LSP:=GATTRP@.TYPTR;ATTRNEW(LATTRP);
(*3640*)              IF LKEY <= 7 THEN
(*3641*)            IF STRING(LSP) THEN IF EXTWARN THEN ERROR(291);
(*3642*)            COPYATTR(GATTRP,LATTRP);
(*3643*)            FORM1P:=NIL;FORM2P:=NIL;
(*3644*)            IF FIL1ATTRP@.TYPTR@.TEXTFILE THEN
(*3645*)            IF SY=COLON THEN
(*3646*)            BEGIN
(*3647*)              INSYMBOL;
(*3648*)              EXPRESSION(FSYS+(.COMMA,COLON,RPARENT,IDENT.));
(*3649*)              IF COMPTYPES(GATTRP@.TYPTR,INTPTR) THEN
(*3650*)              BEGIN
(*3651*)                ATTRNEW(FORM1P); COPYATTR(GATTRP,FORM1P);
(*3652*)              END ELSE ERROR(116);
(*3653*)              (* FOR FUTURE IMPLEMENTATION *)
(*3654*)              (*****************************)
(*3655*)              IF SY = COLON THEN
(*3656*)              BEGIN
(*3657*)                 INSYMBOL; EXPRESSION(FSYS+(.COMMA,RPARENT.));
(*3658*)                 IF COMPTYPES(GATTRP@.TYPTR,INTPTR) THEN
(*3659*)                 BEGIN
(*3660*)                   ATTRNEW(FORM2P);COPYATTR(GATTRP,FORM2P);
(*3661*)                 END ELSE ERROR(116);
(*3662*)                IF LSP<>REALPTR THEN ERROR(124);
(*3663*)               END;
(*3664*)             END;
(*3665*)             IF PROCPASS THEN
(*3666*)             BEGIN
(*3667*)               ATTRNEW(FILATTRP);COPYATTR(FIL1ATTRP,FILATTRP);
(*3668*)               LOADADDRESS(FILATTRP,NIL);
(*3669*)               GENRR(ZLR,15,REALREG(.FILATTRP@.REXPR.RNO.));
(*3670*)               ATTRDISP(FILATTRP);
(*3671*)             END;
(*3672*)            IF LKEY <= 7 THEN
(*3673*)            BEGIN
(*3674*)               WITH GATTRP@ DO
(*3675*)                 IF TYPTR<>NIL THEN
(*3676*)                   BEGIN
(*3677*)                     IF NOT FIL1ATTRP@.TYPTR@.TEXTFILE THEN
(*3678*)                     BEGIN
(*3679*)                       ATTRNEW(FILATTRP);
(*3680*)                       COPYATTR(FIL1ATTRP,FILATTRP);
(*3681*)                 WITH FILATTRP@ DO
(*3682*)                 BEGIN TYPTR:=TYPTR@.FILTYPE; VADRS:=VADRS+8 END;
(*3683*)                       STORE(GATTRP,FILATTRP,118);
(*3684*)                       ATTRDISP(FILATTRP);
(*3685*)            GENRX(ZBAL,BASEWORK,0,1,ENTRYGET);
(*3686*)                    END ELSE
(*3687*)                      IF STRING(LSP) OR(LSP@.SIZE.WBLENGTH=1) THEN
(*3688*)                         STRINGIO(LATTRP,FORM1P,ENTRYRS) ELSE
(*3689*)                       IF COMPTYPES(TYPTR,CHARPTR) THEN ENTRY:=ENTRYRC
(*3690*)                         ELSE IF COMPTYPES(TYPTR,INTPTR) THEN ENTRY:=ENTRYRI
(*3691*)                           ELSE IF TYPTR=REALPTR THEN ENTRY:=ENTRYRR
(*3692*)                               ELSE ERROR(153);
(*3693*)                     IF ENTRY<>0 THEN
(*3694*)                       BEGIN LOADADDRESS(GATTRP,NIL);
(*3695*)                         GENRR(ZLR,R0,REALREG(.REXPR.RNO.));
(*3696*)                         GENRX(ZBAL,BASEWORK,0,1,ENTRY);
(*3697*)                       END;
(*3698*)                   END;
(*3699*)             END ELSE
(*3700*)             BEGIN
(*3701*)              IF LSP <> NIL THEN
(*3702*)                  IF NOT FIL1ATTRP@.TYPTR@.TEXTFILE THEN
(*3703*)                  BEGIN
(*3704*)                    ATTRNEW(FILATTRP);
(*3705*)                    COPYATTRP(FIL1ATTRP,FILATTRP);
(*3706*)                WITH FILATTRP@ DO
(*3707*)                BEGIN VADRS:=VADRS+8; TYPTR:=TYPTR@.FILTYPE END;
(*3708*)                    STORE(FILATTRP,GATTRP,116);
(*3709*)                    GENRX(ZBAL,BASEWORK,0,1,ENTRYGET+8);
(*3710*)                    ATTRDISP(FILATTRP);
(*3711*)                  END ELSE
(*3712*)               IF COMPTYPES(LSP,CHARPTR) THEN WRITEINT(ENTRYWC,1)
(*3713*)                ELSE IF COMPTYPES(LSP,INTPTR) THEN WRITEINT(ENTRYWI,12)
(*3714*)                 ELSE IF LSP=REALPTR THEN WRITEREAL
(*3715*)                  ELSE IF COMPTYPES(LSP,BOOLPTR) THEN
(*3716*)                        WRITEINT(ENTRYWB,5)
(*3717*)                   ELSE IF STRING(LSP) THEN
(*3718*)                        STRINGIO(LATTRP,FORM1P,ENTRYWS)
(*3719*)                    ELSE ERROR(116)
(*3720*)            END;
(*3721*)               ATTRDISP(LATTRP);
(*3722*)               IF FORM1P<>NIL THEN ATTRDISP(FORM1P);
(*3723*)               IF FORM2P <> NIL THEN ATTRDISP(FORM2P);
(*3724*)               RESETG;
(*3725*)               IF SY<>COMMA THEN EXIT;
(*3726*)            INSYMBOL; IF LKEY <= 7 THEN
(*3727*)               VARIABLE(FSYS+(.COMMA,RPARENT.)) ELSE
(*3728*)               EXPRESSION(FSYS+(.COMMA,COLON,RPARENT,IDENT.));
(*3729*)             END;
(*3730*)             TEST1(RPARENT,4);
(*3731*)           END
(*3732*)          ELSE IF (LKEY=6) OR (LKEY=8) THEN ERROR(116);
(*3733*)          IF ( LKEY IN (.7,9.) ) AND
(*3734*)             (NOT FIL1ATTRP@.TYPTR@.TEXTFILE) THEN ERROR(116);
(*3735*)          IF LKEY = 7 THEN GENRX(ZBAL,BASEWORK,0,1,ENTRYRL) ELSE
(*3736*)            IF LKEY = 9 THEN GENRX(ZBAL,BASEWORK,0,1,ENTWRITLN);
(*3737*)       END;
(*3738*) 
(*3739*) 
$TITLE  PAGE
(*3740*)     PROCEDURE PAGE;
(*3741*)       BEGIN
(*3742*)         IF SY<>LPARENT
(*3743*)        THEN BEGIN IF OUTPUTPTR= NIL THEN ERROR(176) ELSE
(*3744*)                SETSTFILATTR(GATTRP,OUTPUTPTR)
(*3745*)             END
(*3746*)           ELSE BEGIN INSYMBOL; VARIABLE(FSYS+(.RPARENT.));
(*3747*)                  IF SY=RPARENT THEN INSYMBOL ELSE ERROR(9);
(*3748*)                END;
(*3749*)         IF GATTRP@.TYPTR <> NIL THEN
(*3750*)           BEGIN
(*3751*)             WITH GATTRP@.TYPTR@ DO
(*3752*)               IF FORM = FILES THEN
(*3753*)                 BEGIN IF NOT TEXTFILE THEN ERROR(116);
(*3754*)                   LOADADDRESS(GATTRP,NIL);
(*3755*)                   GENRR(ZLR,15,REALREG(.GATTRP@.REXPR.RNO.));
(*3756*)                   GENRX(ZBAL,BASEWORK,0,1,ENTPAGE);
(*3757*)                   RESETG;
(*3758*)                 END
(*3759*)               ELSE ERROR(116)
(*3760*)           END;
(*3761*)       END (*PAGE*) ;
(*3762*) 
$TITLE   PACK
(*3763*)PROCEDURE PACK;
(*3764*)  VAR
(*3765*)    LATTRP,CATTRP : ATTRP;
(*3766*)    LOW,HIGH,LMIN,LMAX:INTEGER;
(*3767*)    LSP,LSP1:STP;
(*3768*) 
(*3769*)BEGIN (* PACK *)
(*3770*)  TEST1(LPARENT,9);
(*3771*)  VARIABLE(FSYS+(.COMMA,RPARENT.));
(*3772*)  ATTRNEW(LATTRP); COPYATTR(GATTRP,LATTRP);
(*3773*)  LOW:=0; HIGH:=0; LSP:=NIL; LSP1:=NIL;
(*3774*)  IF GATTRP@.TYPTR <> NIL THEN
(*3775*)   WITH GATTRP@.TYPTR@ DO
(*3776*)     IF FORM = ARRAYS THEN
(*3777*)      IF AELTYPE@.FORM <> PACKDTYPE THEN
(*3778*)      BEGIN
(*3779*)        LSP:=INXTYPE; LSP1:=AELTYPE;
(*3780*)       IF LSP <> NIL THEN GETBOUNDS(LSP,LOW,HIGH);
(*3781*)     END
(*3782*)   ELSE ERROR(116)
(*3783*)  ELSE ERROR(116);
(*3784*)  TEST1(COMMA,20);
(*3785*)  EXPRESSION(FSYS+(.COMMA,RPARENT.));
(*3786*)  IF NOT COMPTYPES(GATTRP@.TYPTR,LSP) THEN ERROR(116);
(*3787*)  TEST1(COMMA,20);
(*3788*)  ATTRNEW(CATTRP);  COPYATTR(GATTRP,CATTRP);
(*3789*)  VARIABLE(FSYS+(.RPARENT.));
(*3790*)  IF GATTRP@.TYPTR <> NIL THEN
(*3791*)  WITH GATTRP@.TYPTR@ DO
(*3792*)  BEGIN
(*3793*)    IF FORM = ARRAYS THEN
(*3794*)           IF (AELTYPE@.FORM = PACKDTYPE) THEN
(*3795*)             IF COMPTYPES(AELTYPE,LSP1) AND
(*3796*)         COMPTYPES(INXTYPE,LSP) THEN
(*3797*)     BEGIN
(*3798*)       LMIN:=0; LMAX:=0;
(*3799*)       IF INXTYPE <> NIL THEN GETBOUNDS(INXTYPE,LMIN,LMAX);
(*3800*)       IF LMAX-LMIN>HIGH-LOW THEN ERROR(116);
(*3801*)            CHECKRANGE(CATTRP,LOW,LMIN-LMAX+HIGH,116);
(*3802*)  IF (LATTRP@.TYPTR<>NIL) AND (CATTRP@.TYPTR<>NIL) THEN
(*3803*)  BEGIN
(*3804*)    LATTRP@.VADRS:=LATTRP@.VADRS-4*LOW;
(*3805*)    IF CATTRP@.KIND=CST THEN
(*3806*)    BEGIN
(*3807*)      LATTRP@.VADRS:=LATTRP@.VADRS+4*CATTRP@.CVAL.IVAL;
(*3808*)      LOADADDRESS(LATTRP,NIL);
(*3809*)    END ELSE
(*3810*)    BEGIN
(*3811*)      LOADADDRESS(LATTRP,NIL);
(*3812*)     LOAD(CATTRP,NIL);
(*3813*)     GENRX(ZSLL,REALREG(.CATTRP@.REXPR.RNO.),0,0,2);
(*3814*)     GENRR(ZAR,REALREG(.LATTRP@.REXPR.RNO.),
(*3815*)     REALREG(.CATTRP@.REXPR.RNO.));
(*3816*)   END
(*3817*)   END;
(*3818*)        LOAD(CATTRP,NIL);
(*3819*)        IF GATTRP@.TYPTR <> NIL THEN
(*3820*)        LOADADDRESS(GATTRP,CATTRP);
(*3821*)        LOADINTCONST(REALREG(.CATTRP@.REXPR.RNO.),ABS(LMAX-LMIN)+1);
(*3822*)            GENRX(ZL,0,0,REALREG(.LATTRP@.REXPR.RNO.),0);
(*3823*)            GENRX(ZSTC,0,0,REALREG(.GATTRP@.REXPR.RNO.),0);
(*3824*)            GENRX(ZLA,REALREG(.LATTRP@.REXPR.RNO.),0,
(*3825*)                      REALREG(.LATTRP@.REXPR.RNO.),4);
(*3826*)            GENRX(ZLA,REALREG(.GATTRP@.REXPR.RNO.),0,
(*3827*)                      REALREG(.GATTRP@.REXPR.RNO.),1);
(*3828*)            GENRX(ZBCT,REALREG(.CATTRP@.REXPR.RNO.),0,PBASE1,IC-16);
(*3829*)         END ELSE ERROR(116)
(*3830*)            ELSE ERROR(118)
(*3831*)       ELSE ERROR(116);
(*3832*) 
(*3833*)     END;
(*3834*)     ATTRDISP(CATTRP); ATTRDISP(LATTRP);
(*3835*)     RESETG;
(*3836*)     TEST1(RPARENT,4);
(*3837*)END; (* PACK *)
(*3838*) 
(*3839*) 
$TITLE   UNPACK
(*3840*)PROCEDURE UNPACK;
(*3841*)VAR
(*3842*)  SOURCE,DEST:ATTRP;
(*3843*)  LOW,HIGH,LMIN,LMAX : INTEGER;
(*3844*)  LSP,LSP1 : STP;
(*3845*) 
(*3846*)BEGIN (* UNPACK *)
(*3847*)  TEST1(LPARENT,9);
(*3848*)  EXPRESSION(FSYS+(.COMMA,RPARENT.));
(*3849*)  LSP:=NIL; LSP1:=NIL; LMIN:=0; LMAX:=0;
(*3850*)  IF GATTRP@.TYPTR <> NIL THEN
(*3851*)    WITH GATTRP@.TYPTR@ DO
(*3852*)      IF FORM = ARRAYS THEN
(*3853*)        IF AELTYPE@.FORM = PACKDTYPE THEN
(*3854*)        BEGIN
(*3855*)          LSP:=INXTYPE; LSP1:=AELTYPE;
(*3856*)          IF LSP <> NIL THEN GETBOUNDS(LSP,LMIN,LMAX);
(*3857*)        END
(*3858*)        ELSE ERROR(118)
(*3859*)      ELSE ERROR(116);
(*3860*)  ATTRNEW(SOURCE); COPYATTR(GATTRP,SOURCE);
(*3861*)  TEST1(COMMA,20);
(*3862*)  VARIABLE(FSYS+(.COMMA,RPARENT.));
(*3863*)  ATTRNEW(DEST); COPYATTR(GATTRP,DEST);
(*3864*)  IF DEST@.TYPTR <> NIL THEN
(*3865*)    WITH DEST@.TYPTR@ DO
(*3866*)      IF FORM = ARRAYS THEN
(*3867*)        IF ( AELTYPE@.FORM <> PACKDTYPE) THEN
(*3868*)          IF COMPTYPES(INXTYPE,LSP) AND COMPTYPES(AELTYPE,LSP1) THEN
(*3869*)          BEGIN
(*3870*)            LOW:=0; HIGH :=0;
(*3871*)            IF INXTYPE <> NIL THEN GETBOUNDS(INXTYPE,LOW,HIGH);
(*3872*)            IF LMAX-LMIN > HIGH - LOW THEN ERROR(116);
(*3873*)          END
(*3874*)          ELSE ERROR(116)
(*3875*)        ELSE ERROR(116)
(*3876*)      ELSE ERROR(116);
(*3877*)  TEST1(COMMA,20);
(*3878*)  EXPRESSION(FSYS+(.RPARENT.));
(*3879*)  CHECKRANGE(GATTRP,LOW,LMIN-LMAX+HIGH,116);
(*3880*)  IF (DEST@.TYPTR <> NIL) AND (GATTRP@.TYPTR<>NIL) THEN
(*3881*)  BEGIN
(*3882*)    DEST@.VADRS := DEST@.VADRS - 4*LOW;
(*3883*)    IF GATTRP@.KIND = CST THEN
(*3884*)    BEGIN
(*3885*)      DEST@.VADRS := DEST@.VADRS + 4*GATTRP@.CVAL.IVAL;
(*3886*)      LOADADDRESS(DEST,NIL);
(*3887*)    END ELSE
(*3888*)    BEGIN
(*3889*)      LOADADDRESS(DEST,NIL);
(*3890*)      LOAD(GATTRP,NIL);
(*3891*)      GENRX(ZSLL,REALREG(.GATTRP@.REXPR.RNO.),0,0,2);
(*3892*)      GENRR(ZAR,REALREG(.DEST@.REXPR.RNO.),REALREG(.GATTRP@.REXPR.RNO.));
(*3893*)    END;
(*3894*)  END;
(*3895*)  LOAD(GATTRP,NIL);
(*3896*)  IF SOURCE@.TYPTR <> NIL THEN
(*3897*)  LOADADDRESS(SOURCE,NIL);
(*3898*)  LOADINTCONST(REALREG(.GATTRP@.REXPR.RNO.),ABS(LMAX-LMIN)+1);
(*3899*)  LOADINTCONST(R0,0);
(*3900*)  GENRX(ZIC,0,0,REALREG(.SOURCE@.REXPR.RNO.),0);
(*3901*)  GENRX(ZST,0,0,REALREG(.DEST@.REXPR.RNO.),0);
(*3902*)  GENRX(ZLA,REALREG(.SOURCE@.REXPR.RNO.),0,
(*3903*)            REALREG(.SOURCE@.REXPR.RNO.),1);
(*3904*)  GENRX(ZLA,REALREG(.DEST@.REXPR.RNO.),0,
(*3905*)            REALREG(.DEST@.REXPR.RNO.),4);
(*3906*)  GENRX(ZBCT,REALREG(.GATTRP@.REXPR.RNO.),0,PBASE1,IC-16);
(*3907*)  ATTRDISP(SOURCE); ATTRDISP(DEST); RESETG;
(*3908*)  TEST1(RPARENT,4);
(*3909*)END; (* UNPACK *)
$TITLE  TIME AND DATE FUNCTIONS
(*3910*)    PROCEDURE TIMEDATE;
(*3911*)      VAR LMIN,LMAX:INTEGER;
(*3912*)      BEGIN
(*3913*)        TEST1(LPARENT,9);
(*3914*)        VARIABLE(FSYS+(.RPARENT.));
(*3915*)        WITH GATTRP@ DO
(*3916*)          IF TYPTR<>NIL THEN
(*3917*)            IF TYPTR@.FORM<>ARRAYS THEN ERRORRESET(116)
(*3918*)              ELSE IF TYPTR@.AELTYPE<>NIL THEN
(*3919*)                IF (TYPTR@.AELTYPE@.FORM<>PACKDTYPE) OR
(*3920*)                    (TYPTR@.AELTYPE@.BASETYPE<>CHARPTR)
(*3921*)                  THEN ERRORRESET(116)
(*3922*)                  ELSE BEGIN GETBOUNDS(TYPTR@.INXTYPE,LMIN,LMAX);
(*3923*)                          IF LMAX-LMIN<>7 THEN ERRORRESET(116);
(*3924*)                       END;
(*3925*)        IF GATTRP@.TYPTR<>NIL THEN
(*3926*)          BEGIN LOADADDRESS(GATTRP,NIL);
(*3927*)            GENRR(ZLR,R0,REALREG(.GATTRP@.REXPR.RNO.));
(*3928*)            GENRX(ZBAL,BASEWORK,0,1,ENTRYTIME+8*(LKEY-10));
(*3929*)          END;
(*3930*)        RESETG;
(*3931*)        TEST1(RPARENT,4);
(*3932*)      END;
(*3933*) 
$TITLE  NEW - PROCEDURE
(*3934*)    PROCEDURE NEWPROC;
(*3935*)      LABEL 1;
(*3936*)      TYPE TAGPTR = @TAGSTORE;
(*3937*)       TAGSTORE = RECORD OP,VAL,OFFST:INTEGER;NXT:TAGPTR END;
(*3938*)      VAR LSP,LSP1: STP; LVAL: VALU; LSIZE: WBSIZE; LMIN,LMAX: INTEGER;
(*3939*)          STOREOP:INTEGER;
(*3940*)          STMARK : @BOOLEAN;
(*3941*)          SAVEDISP,FIRSTDISP : TAGPTR;
(*3942*)      BEGIN IF SY = LPARENT THEN INSYMBOL ELSE ERROR(9);
(*3943*)        VARIABLE(FSYS+(.COMMA,RPARENT.));
(*3944*)        FIRSTDISP :=NIL;
(*3945*)        LSP := NIL; INITSIZE(LSIZE);
(*3946*)        IF GATTRP@.TYPTR <> NIL THEN
(*3947*)          WITH GATTRP@.TYPTR@ DO
(*3948*)            IF FORM = POINTER THEN
(*3949*)              BEGIN
(*3950*)                IF ELTYPE <> NIL THEN
(*3951*)                  BEGIN LSIZE := ELTYPE@.SIZE;
(*3952*)                    IF ELTYPE@.FORM = RECORDS THEN LSP := ELTYPE@.RECVAR
(*3953*)                  END
(*3954*)              END
(*3955*)            ELSE ERROR(116);
(*3956*)        WHILE SY = COMMA DO
(*3957*)          BEGIN INSYMBOL; CONSTANT(FSYS+(.COMMA,RPARENT.),LSP1,LVAL);
(*3958*)            IF LSP = NIL THEN ERROR(158)
(*3959*)            ELSE
(*3960*)              IF LSP@.TGFLDP <> NIL THEN
(*3961*)                IF (LSP1 = REALPTR) OR STRING(LSP1) THEN ERROR(159)
(*3962*)                ELSE
(*3963*)                  IF COMPTYPES(LSP@.TGFLDP@.IDTYPE,LSP1) THEN
(*3964*)                    BEGIN
(*3965*)                      GETBOUNDS(LSP@.TGFLDP@.IDTYPE,LMIN,LMAX);
(*3966*)                      IF (LVAL.IVAL > LMAX) OR (LVAL.IVAL < LMIN) THEN ERROR(181);
(*3967*)                      IF LSP@.TGFLDP@.NAME<>'        ' THEN
(*3968*)                        BEGIN
(*3969*)                          IF LSP@.TGFLDP@.IDTYPE<>NIL THEN
(*3970*)                            BEGIN IF LSP@.TGFLDP@.IDTYPE@.FORM=PACKDTYPE
(*3971*)                                    THEN STOREOP:=ZSTC
(*3972*)                                    ELSE STOREOP:=ZST;
(*3973*)                              NEW(SAVEDISP); SAVEDISP@.NXT:=NIL;
(*3974*)                               IF FIRSTDISP=NIL THEN
(*3975*)                                FIRSTDISP:=SAVEDISP
(*3976*)                               ELSE
(*3977*)                               BEGIN
(*3978*)                                SAVEDISP@.NXT:=FIRSTDISP;
(*3979*)                                FIRSTDISP:=SAVEDISP
(*3980*)                               END;
(*3981*)                               WITH SAVEDISP@ DO
(*3982*)                               BEGIN
(*3983*)                                OP:=STOREOP;
(*3984*)                                VAL := LVAL.IVAL;
(*3985*)                                OFFST :=LSP@.TGFLDP@.FLDADDR
(*3986*)                               END;
(*3987*)                            END;
(*3988*)                        END;
(*3989*)                      LSP1 := LSP@.FSTVAR;
(*3990*)                      WHILE LSP1 <> NIL DO
(*3991*)                        WITH LSP1@ DO
(*3992*)                          IF VARVAL=LVAL.IVAL THEN
(*3993*)                            BEGIN LSP := SUBVAR;
(*3994*)                              LSIZE:=SIZE;
(*3995*)                              GOTO 1
(*3996*)                            END
(*3997*)                          ELSE LSP1 := NXTVAR;
(*3998*)                      LSIZE:=LSP@.SIZE;
(*3999*)                      LSP:=NIL;
(*4000*)                    END
(*4001*)                  ELSE ERROR(116);
(*4002*)      1:  END (*WHILE*) ;
(*4003*)  ALIGNMENT(LSIZE.WBLENGTH,4); MAKEINTCONST(LSIZE.WBLENGTH);
(*4004*)  GENRX(ZS,NEWPOINTER,0,0,0);
(*4005*)  IF LSIZE.BOUNDARY=8 THEN
(*4006*)    BEGIN MAKEINTCONST(-8); GENRX(ZN,NEWPOINTER,0,0,0); END;
(*4007*)  OVERFLOWTEST;
(*4008*)  LOADINDEX(GATTRP,NIL); LOADBASE(GATTRP);
(*4009*)  GENRX(ZST,NEWPOINTER,RINDEX,RBASE,EFFADRS);
(*4010*)  WHILE FIRSTDISP <> NIL DO
(*4011*)  BEGIN
(*4012*)    WITH FIRSTDISP@ DO
(*4013*)    BEGIN
(*4014*)      LOADINTCONST(R0,VAL);
(*4015*)      GENRX(OP,R0,0,NEWPOINTER,OFFST)
(*4016*)    END;
(*4017*)    FIRSTDISP:=FIRSTDISP@.NXT
(*4018*)  END;
(*4019*)     TEST1(RPARENT,4);
(*4020*)      END (*NEWPROC*) ;
(*4021*) 
$TITLE  MARK AND RELEASE
(*4022*)    PROCEDURE MARKRELEASE;
(*4023*)    BEGIN
(*4024*)        TEST1(LPARENT,9);
(*4025*)        VARIABLE(FSYS+(.COMMA,RPARENT.));
(*4026*)        IF GATTRP@.TYPTR <> NIL THEN
(*4027*)          IF GATTRP@.TYPTR@.FORM = POINTER THEN
(*4028*)            BEGIN
(*4029*)              IF LKEY = 13 THEN
(*4030*)                BEGIN LOADINDEX(GATTRP,NIL); LOADBASE(GATTRP);
(*4031*)                      GENRX(ZST,NEWPOINTER,RINDEX,RBASE,EFFADRS);
(*4032*)                END
(*4033*)              ELSE
(*4034*)                BEGIN
(*4035*)                  CHECKPOINTER(GATTRP,FALSE);
(*4036*)                  LOAD(GATTRP,NIL); GENRR(ZLR,NEWPOINTER,REALREG(.GATTRP@.REXPR.RNO.))
(*4037*)                END
(*4038*)            END
(*4039*)          ELSE ERROR(116);
(*4040*)        RESETG;
(*4041*)        TEST1(RPARENT,4);
(*4042*)      END;
(*4043*) 
$TITLE  STANDARD PROCEDURES AND FUNCTS
(*4044*) 
(*4045*)PROCEDURE LEFTXPRS;
(*4046*)  BEGIN (* LEFTXPRS *)
(*4047*)    TEST1(LPARENT,9);
(*4048*)    EXPRESSION(FSYS+(.RPARENT.));
(*4049*)  END; (* LEFTXPRS *)
(*4050*) 
(*4051*) 
(*4052*) 
(*4053*)    PROCEDURE ROUNDTRUNCF;
(*4054*)      VAR TEMP:CMP; ZERO,HALF,ONE:VALU;
(*4055*)      BEGIN
(*4056*)         LEFTXPRS;
(*4057*)        IF GATTRP@.TYPTR<>REALPTR THEN ERROR(125);
(*4058*)        LOAD(GATTRP,NIL); ZERO.CKIND:=PSET; ZERO.PVAL:=(.1,4,5,6.);
(*4059*)        IF LKEY=4 THEN
(*4060*)          BEGIN HALF.CKIND:=REEL; HALF.RVAL:=0.5;
(*4061*)                ONE.CKIND:=REEL; ONE.RVAL:=1.0;
(*4062*)                MAKECONSTANT(HALF); GENRXP(ZAD,GATTRP@.REXPR.RNO,0,0,0);
(*4063*)                GENRX(ZBC,CONDP,0,PBASE1,IC+8);
(*4064*)                MAKECONSTANT(ONE); GENRXP(ZSD,GATTRP@.REXPR.RNO,0,0,0);
(*4065*)          END;
(*4066*)        MAKECONSTANT(ZERO); GENRXP(ZAW,GATTRP@.REXPR.RNO,0,0,0);
(*4067*)        GETTEMP(8,TEMP); REGSEARCH(NIL,DOUBLE);
(*4068*)        BASEREGISTER(LEVEL,TEMP@.TEMPADRS);
(*4069*)        GENRXP(ZSTD,GATTRP@.REXPR.RNO,0,RBASE,EFFADRS);
(*4070*)        GENRX(ZLM,RMAIN,RMAIN+1,RBASE,EFFADRS);
(*4071*)        GENRX(ZLA,RMAIN,0,RMAIN,0); GENRX(ZSLDA,RMAIN,0,0,32);
(*4072*)        GENRX(ZTM,8,0,RBASE,EFFADRS); GENRX(ZBC,14,0,PBASE1,IC+6);
(*4073*)        GENRR(ZLNR,RMAIN,RMAIN);
(*4074*)        WITH GATTRP@ DO
(*4075*)          BEGIN REGISTER(.REXPR.RNO.).USED:=FALSE;
(*4076*)                TYPTR:=INTPTR; REXPR.RNO:=RWORK;
(*4077*)          END;
(*4078*)        REGISTER(.RWORK.).USED:=TRUE; REGISTER(.RWORK.).REGCONT:=GATTRP;
(*4079*)        DELETETEMP(TEMP);
(*4080*)        TEST1(RPARENT,4);
(*4081*)      END;
(*4082*) 
(*4083*)    PROCEDURE ABSF;
(*4084*)     BEGIN
(*4085*)        LEFTXPRS;
(*4086*)        LOAD(GATTRP,NIL);
(*4087*)        WITH GATTRP@ DO
(*4088*)          IF COMPTYPES(TYPTR,INTPTR)
(*4089*)            THEN GENRRP1(ZLPR,REXPR.RNO)
(*4090*)            ELSE IF TYPTR=REALPTR
(*4091*)              THEN GENRRP1(ZLPDR,REXPR.RNO)
(*4092*)              ELSE ERROR(125);
(*4093*)        TEST1(RPARENT,4);
(*4094*)      END;
(*4095*) 
(*4096*)    PROCEDURE SQRF;
(*4097*)      BEGIN
(*4098*)        LEFTXPRS;
(*4099*)        WITH GATTRP@ DO
(*4100*)          BEGIN
(*4101*)            IF TYPTR=REALPTR THEN
(*4102*)              BEGIN LOAD(GATTRP,NIL); GENRRP1(ZMDR,REXPR.RNO); END
(*4103*)            ELSE IF COMPTYPES(TYPTR,INTPTR) THEN
(*4104*)              BEGIN LOADEVENODD(GATTRP,NIL,1); GENRRP(ZMR,PRED(REXPR.RNO),REXPR.RNO);
(*4105*)              END
(*4106*)            ELSE ERROR(125);
(*4107*)          END;
(*4108*)       TEST1(RPARENT,4);
(*4109*)      END;
(*4110*) 
(*4111*)    PROCEDURE ODDP;
(*4112*)     BEGIN
(*4113*)        LEFTXPRS;
(*4114*)        IF NOT COMPTYPES(GATTRP@.TYPTR,INTPTR) THEN ERROR(125);
(*4115*)        LOAD(GATTRP,NIL); MAKEINTCONST(1);
(*4116*)        GENRXP(ZN,GATTRP@.REXPR.RNO,0,0,0);
(*4117*)        GATTRP@.TYPTR := BOOLPTR;
(*4118*)        TEST1(RPARENT,4);
(*4119*)      END;
(*4120*) 
(*4121*)    PROCEDURE ORDF;
(*4122*)     BEGIN
(*4123*)        LEFTXPRS;
(*4124*)        WITH GATTRP@ DO
(*4125*)          IF TYPTR <> NIL THEN
(*4126*)            IF TYPTR@.FORM>=POWER
(*4127*)              THEN ERROR(125)
(*4128*)              ELSE IF TYPTR=REALPTR
(*4129*)                THEN ERROR(125)
(*4130*)                ELSE IF TYPTR@.SIZE.WBLENGTH<>1
(*4131*)                  THEN TYPTR:=INTPTR
(*4132*)                  ELSE TYPTR:=PACKDINTPTR;
(*4133*)        TEST1(RPARENT,4);
(*4134*)      END;
(*4135*) 
(*4136*)    PROCEDURE CHRF;
(*4137*)    BEGIN
(*4138*)        LEFTXPRS;
(*4139*)        WITH GATTRP@ DO
(*4140*)          IF COMPTYPES(TYPTR,INTPTR)
(*4141*)            THEN IF TYPTR@.SIZE.WBLENGTH<>1
(*4142*)              THEN TYPTR:=CHARPTR
(*4143*)              ELSE TYPTR:=PACKDCHARPTR
(*4144*)            ELSE ERROR(125);
(*4145*)        TEST1(RPARENT,4);
(*4146*)      END;
(*4147*) 
(*4148*)    PROCEDURE PREDSUCCF;
(*4149*)    BEGIN
(*4150*)        LEFTXPRS;
(*4151*)        IF GATTRP@.TYPTR <> NIL THEN
(*4152*)          WITH GATTRP@ DO
(*4153*)            IF TYPTR@.FORM > SUBRANGE THEN ERROR(125)
(*4154*)            ELSE IF TYPTR=REALPTR THEN ERROR(125);
(*4155*)        LOAD(GATTRP,NIL);
(*4156*)        IF LKEY = 11
(*4157*)          THEN BEGIN MAKEINTCONST(1);
(*4158*)                 GENRXP(ZA,GATTRP@.REXPR.RNO,0,0,0);
(*4159*)               END
(*4160*)          ELSE GENRR(ZBCTR,REALREG(.GATTRP@.REXPR.RNO.),0);
(*4161*)          TEST1(RPARENT,4);
(*4162*)      END;
(*4163*) 
(*4164*)PROCEDURE HALT;
(*4165*)BEGIN
(*4166*)  GENRX(ZBAL,BASEWORK,0,1,ENTRYHALT);
(*4167*)END;
(*4168*) 
(*4169*) 
(*4170*) 
(*4171*)PROCEDURE MESSAGE;
(*4172*)   VAR LSP:STP;
(*4173*)BEGIN(*MESSAGE*)
(*4174*)  LEFTXPRS;
(*4175*)  IF GATTRP@.TYPTR <> NIL THEN
(*4176*)         IF STRING(GATTRP@.TYPTR) OR
(*4177*)            COMPTYPES(GATTRP@.TYPTR,CHARPTR) THEN
(*4178*)    BEGIN
(*4179*)     LSP:=GATTRP@.TYPTR;
(*4180*)     LOADADDRESS(GATTRP,NIL);
(*4181*)      LOADINTCONST(R0,16*LSP@.SIZE.WBLENGTH+
(*4182*)                         REALREG(.GATTRP@.REXPR.RNO.));
(*4183*)      GENRX(ZBAL,BASEWORK,0,1,ENTRYMESSAGE);
(*4184*)      RESETG;
(*4185*)    END ELSE ERROR(116);
(*4186*)   TEST1(RPARENT,4);
(*4187*)END; (* MESSAGE *)
(*4188*) 
(*4189*) 
(*4190*) 
(*4191*)PROCEDURE CARD;
(*4192*)BEGIN (* CARD *)
(*4193*)  LEFTXPRS;
(*4194*)  LOAD(GATTRP,NIL);
(*4195*)  IF GATTRP@.TYPTR <> NIL THEN
(*4196*)    IF GATTRP@.TYPTR@.FORM = POWER THEN
(*4197*)    BEGIN
(*4198*)      REGSEARCH(NIL,SINGLE);
(*4199*)      GENRR(ZSR,RMAIN,RMAIN);
(*4200*)      GENRXP(ZSLDA,GATTRP@.REXPR.RNO,0,0,0);
(*4201*)      GENRX(ZBC,CONDZ,0,PBASE1,IC+20);
(*4202*)      GENRX(ZBC,CONDP,0,PBASE1,IC+8);
(*4203*)      GENRX(ZLA,RMAIN,0,RMAIN,1);
(*4204*)      GENRXP(ZSLDL,GATTRP@.REXPR.RNO,0,0,1);
(*4205*)      GENRX(ZBC,15,0,PBASE1,IC-20);
(*4206*)      WITH GATTRP@ DO
(*4207*)      BEGIN
(*4208*)        TYPTR := INTPTR;
(*4209*)        REGISTER(.REXPR.RNO.).USED := FALSE;
(*4210*)        REXPR.RNO:=RWORK;
(*4211*)      END;
(*4212*)      REGISTER(.RWORK.).USED := TRUE;
(*4213*)      REGISTER(.RWORK.).REGCONT := GATTRP;
(*4214*)    END ELSE ERROR(116);
(*4215*)  TEST1(RPARENT,4);
(*4216*)END; (* CARD *)
(*4217*)     PROCEDURE STDFLFUNCS;
(*4218*)       BEGIN
(*4219*)         IF SY<>LPARENT
(*4220*)           THEN BEGIN IF INPUTPTR=NIL THEN ERROR(175);
(*4221*)                  SETSTFILATTR(GATTRP,INPUTPTR);
(*4222*)                END
(*4223*)           ELSE BEGIN INSYMBOL; VARIABLE(FSYS+(.RPARENT.));
(*4224*)                  IF SY=RPARENT THEN INSYMBOL ELSE ERROR(9);
(*4225*)                END;
(*4226*)         IF GATTRP@.TYPTR <> NIL THEN
(*4227*)           WITH GATTRP@, TYPTR@ DO
(*4228*)             IF FORM = FILES THEN
(*4229*)               BEGIN
(*4230*)                 VADRS:=VADRS+4; LOAD(GATTRP,NIL);
(*4231*)                 IF LKEY=2 THEN
(*4232*)                   BEGIN IF NOT TEXTFILE THEN ERROR(125);
(*4233*)                     GENRXP(ZSRL,GATTRP@.REXPR.RNO,0,0,1);
(*4234*)                   END;
(*4235*)                 MAKEINTCONST(1); GENRXP(ZN,GATTRP@.REXPR.RNO,0,0,0);
(*4236*)                 TYPTR := BOOLPTR; KIND := EXPR;
(*4237*)               END
(*4238*)             ELSE ERROR(125);
(*4239*)     END;
(*4240*) 
(*4241*)    PROCEDURE STDARITHFUNCS;
(*4242*)    BEGIN
(*4243*)        LEFTXPRS;
(*4244*)        IF COMPTYPES(GATTRP@.TYPTR,INTPTR)
(*4245*)          THEN INTTOREAL(GATTRP);
(*4246*)        IF GATTRP@.TYPTR<>REALPTR THEN ERROR(125);
(*4247*)        LOAD(GATTRP,NIL);
(*4248*)        LOADINTCONST(R0,REALREG(.GATTRP@.REXPR.RNO.));
(*4249*)        GENRX(ZBAL,BASEWORK,0,1,ENTRYSIN+(LKEY-12)*8);
(*4250*)        TEST1(RPARENT,4);
(*4251*)      END;
(*4252*) 
(*4253*)    PROCEDURE CLOCKF;
(*4254*)      BEGIN
(*4255*)        REGSEARCH(NIL,SINGLE);
(*4256*)        GENRX(ZBAL,BASEWORK,0,1,ENTRYCLOCK);
(*4257*)        GENRR(ZLR,RMAIN,R0);
(*4258*)        WITH GATTRP@ DO
(*4259*)          BEGIN TYPTR:=INTPTR; KIND:=EXPR;
(*4260*)                REXPR.REGTEMP:=REGIST; REXPR.RNO:=RWORK;
(*4261*)          END;
(*4262*)        REGISTER(.RWORK.).USED:=TRUE; REGISTER(.RWORK.).REGCONT:=GATTRP;
(*4263*)      END;
(*4264*) 
$TITLE   CALL OF NON STANDARD PROCEDURES
(*4265*)  PROCEDURE CALLNONSTANDARD;
(*4266*)    VAR NXT,LCP,NXT1,NXT2:CTP; PASSPROC:BOOLEAN;
(*4267*)        OLDSTACK:INTEGER; FORMAL:ATTRP;
(*4268*)        FSP:STP; KIND:REGKIND; X:INTEGER;
(*4269*)        FLOATREG:REGNO;
(*4270*)    BEGIN
(*4271*)      NXT:=FCP@.PARAMS;
(*4272*)      OLDSTACK:=STACKTOP;
(*4273*)      IF OLDSTACK<>0 THEN
(*4274*)        BEGIN ALIGNMENT(OLDSTACK,8); MAKEINTCONST(OLDSTACK);
(*4275*)              GENRX(ZA,STACKPOINTER,0,0,0); STACKTOP:=0;
(*4276*)        END;
(*4277*)      IF SY=LPARENT THEN
(*4278*)        BEGIN
(*4279*)          REPEAT PASSPROC:=FALSE;
(*4280*)            IF NXT=NIL THEN ERROR(126)
(*4281*)              ELSE IF NXT@.KLASS IN (.PROC,FUNC.) THEN PASSPROC:=TRUE;
(*4282*)            INSYMBOL;
(*4283*)            IF PASSPROC THEN
(*4284*)              BEGIN
(*4285*)                IF SY<>IDENT
(*4286*)                  THEN BEGIN ERROR(2); SKIP(FSYS+(.COMMA,RPARENT.)); END
(*4287*)                ELSE
(*4288*)                  BEGIN
(*4289*)                    IF NXT@.KLASS=PROC THEN SEARCHID((.PROC.),LCP)
(*4290*)                    ELSE BEGIN SEARCHID((.FUNC.),LCP);
(*4291*)                           IF NOT COMPTYPES(NXT@.IDTYPE,LCP@.IDTYPE) THEN ERROR(128);
(*4292*)                         END;
(*4293*)                    IF LCP@.PFDECKIND=STANDARD THEN ERROR(164)
(*4294*)                    ELSE
(*4295*)                      BEGIN NXT1:=NXT@.PARAMS;
(*4296*)                        NXT2:=LCP@.PARAMS;
(*4297*)                        WHILE (NXT1<>NIL) AND (NXT2<>NIL) DO
(*4298*)                          BEGIN
(*4299*)                            IF NXT2@.KLASS<>VARS THEN ERROR(170)
(*4300*)                            ELSE IF NXT2@.VKIND=INDRCT THEN ERROR(170)
(*4301*)                            ELSE IF NOT COMPTYPES(NXT1@.IDTYPE,NXT2@.IDTYPE) THEN ERROR(186);
(*4302*)                            NXT1:=NXT1@.NEXT; NXT2:=NXT2@.NEXT;
(*4303*)                          END;
(*4304*)                        IF NXT1<>NXT2 THEN ERROR(186);
(*4305*)                        WITH LCP@ DO
(*4306*)                          IF PFKIND=ACTUAL
(*4307*)                        THEN
(*4308*)                        BEGIN
(*4309*)                          GENRX(ZLA,R0,0,0,PROCBASE+4*PFCNT-4);
(*4310*)                          BASEREGISTER(STACKPOINTER,NXT@.PFADDR);
(*4311*)                          GENRX(ZST,R0,0,RBASE,EFFADRS);
(*4312*)                          BASEREGISTER(STACKPOINTER,NXT@.PFADDR+4);
(*4313*)                          GENRX(ZST,STACKPOINTER,0,RBASE,EFFADRS);
(*4314*)                        END ELSE
(*4315*)                        BEGIN
(*4316*)                          BASEREGISTER(PFLEV,PFADDR);
(*4317*)                          GENSS(ZMVC,7,8,NXT@.PFADDR,RBASE,EFFADRS);
(*4318*)                        END;
(*4319*)                       STACKTOP:=NXT@.PFADDR+8;
(*4320*)                      END;
(*4321*)                    INSYMBOL;
(*4322*)                  END;
(*4323*)              END (*PROC/FUNC PARAMETER*)
(*4324*)            ELSE
(*4325*)              BEGIN EXPRESSION(FSYS+(.COMMA,RPARENT.));
(*4326*)                IF (NXT<>NIL) AND (GATTRP@.TYPTR<>NIL) THEN
(*4327*)                  IF NXT@.VKIND=DRCT
(*4328*)                    THEN
(*4329*)                      BEGIN ATTRNEW(FORMAL);
(*4330*)                        WITH FORMAL@,NXT@ DO
(*4331*)                          BEGIN TYPTR:=IDTYPE; KIND:=VARBL; VADRS:=VADDR;
(*4332*)                                ACCESS:=DIRECT; VARKIND:=DRCT; VLEVEL:=STACKPOINTER;
(*4333*)                          END;
(*4334*)                        IF NXT@.IDTYPE<>NIL THEN
(*4335*)                          BEGIN STORE(FORMAL,GATTRP,142);
(*4336*)                            STACKTOP:=NXT@.VADDR+NXT@.IDTYPE@.SIZE.WBLENGTH;
(*4337*)                          END;
(*4338*)                        ATTRDISP(FORMAL);
(*4339*)                      END
(*4340*)                    ELSE
(*4341*)                      BEGIN
(*4342*)                        IF GATTRP@.KIND<>VARBL THEN
(*4343*)                          BEGIN ERROR(154); GATTRP@.TYPTR:=NIL; END
(*4344*)                        ELSE IF COMPTYPES(NXT@.IDTYPE,GATTRP@.TYPTR) THEN
(*4345*)                          IF GATTRP@.TYPTR@.SIZE.WBLENGTH=1 THEN ERROR(187)
(*4346*)                          ELSE
(*4347*)                            BEGIN LOADADDRESS(GATTRP,NIL);
(*4348*)                BASEREGISTER(STACKPOINTER,NXT@.PARADDR);
(*4349*)                GENRXP(ZST,GATTRP@.REXPR.RNO,0,RBASE,EFFADRS);
(*4350*)                              STACKTOP:=NXT@.PARADDR+4;
(*4351*)                            END
(*4352*)                        ELSE ERROR(142);
(*4353*)                      END;
(*4354*)                RESETG;
(*4355*)              END;
(*4356*)          IF NXT<>NIL THEN NXT:=NXT@.NEXT;
(*4357*)        UNTIL SY<>COMMA;
(*4358*)        TEST1(RPARENT,4);
(*4359*)      END;
(*4360*)    IF NXT<>NIL THEN ERROR(126);
(*4361*)    FOR FLOATREG:=F0 TO F6 DO
(*4362*)      IF REGISTER(.FLOATREG.).USED THEN SAVE(FLOATREG);
(*4363*)     PROCPASS:=TRUE;
(*4364*)    WITH FCP@ DO
(*4365*)     IF PFKIND <> ACTUAL THEN
(*4366*)     BEGIN
(*4367*)       BASEREGISTER(PFLEV,PFADDR);
(*4368*)       GENRX(ZL,15,0,RBASE,EFFADRS);
(*4369*)      IF (PFADDR+4)>=4096 THEN GENRX(ZLA,9,0,PBASE1,IC+24) ELSE
(*4370*)        GENRX(ZLA,9,0,PBASE1,IC+18);
(*4371*)       GENRX(ZSTM,8,6,STACKPOINTER,0);
(*4372*)       BASEREGISTER(PFLEV,PFADDR+4);
(*4373*)       GENRX(ZL,2,0,RBASE,EFFADRS);
(*4374*)       GENRX(ZLM,2,6,2,40);
(*4375*)        GENRX(ZBC,15,0,1,ENTRYVARPROC);
(*4376*)     END
(*4377*)     ELSE
(*4378*)     BEGIN
(*4379*)          GENRR(ZBALR,9,1);
(*4380*)          MAKECODE(IC,PROCBASE+4*FCP@.PFCNT-4);
(*4381*)          IC := IC +2;
(*4382*)     END;
(*4383*)    IF FCP@.KLASS=FUNC THEN
(*4384*)      BEGIN
(*4385*)        FSP:=FCP@.IDTYPE;
(*4386*)        IF FSP=REALPTR
(*4387*)          THEN BEGIN KIND:=FLOAT; X:=ZLD END
(*4388*)          ELSE BEGIN KIND:=SINGLE; X:=ZL END;
(*4389*)        REGSEARCH(NIL,KIND);
(*4390*)        GENRX(X,RMAIN,0,STACKPOINTER,SAVEAREA);
(*4391*)        WITH GATTRP@ DO
(*4392*)          BEGIN TYPTR := FSP; KIND := EXPR;
(*4393*)                REXPR.REGTEMP:=REGIST; REXPR.RNO:=RWORK;
(*4394*)          END;
(*4395*)        REGISTER(.RWORK.).USED:=TRUE; REGISTER(.RWORK.).REGCONT:=GATTRP;
(*4396*)      END ;
(*4397*)    IF OLDSTACK<>0 THEN
(*4398*)      BEGIN MAKEINTCONST(OLDSTACK); GENRX(ZS,STACKPOINTER,0,0,0); END;
(*4399*)    STACKTOP:=OLDSTACK;
(*4400*)  END;
(*4401*) 
(*4402*)    BEGIN (*CALL*)
(*4403*)      IF FCP@.PFDECKIND=DECLARED THEN CALLNONSTANDARD
(*4404*)      ELSE
(*4405*)        BEGIN
(*4406*)          LKEY := FCP@.KEY;
(*4407*)          IF FCP@.KLASS = PROC THEN
(*4408*)            CASE LKEY OF
(*4409*)              1,2,
(*4410*)              3,4:   STDFLPROCS;      (*GET,PUT,RESET,REWRITE*)
(*4411*)              5:     PAGE;
(*4412*)        6,7,8,9:READWRITE;
(*4413*)              10,11: TIMEDATE;
(*4414*)              12:    NEWPROC;
(*4415*)              13,14:MARKRELEASE;
(*4416*)              15:PACK;
(*4417*)              16:UNPACK;
(*4418*)              17:MESSAGE;
(*4419*)              18:HALT;
(*4420*)            END
(*4421*)          ELSE
(*4422*)            CASE LKEY OF
(*4423*)              1,2:   STDFLFUNCS;      (*EOF,EOLN*)
(*4424*)              3:     ODDP;
(*4425*)              4,5:   ROUNDTRUNCF;
(*4426*)              6:     ABSF;
(*4427*)              7:     SQRF;
(*4428*)              8:     ORDF;
(*4429*)              9:     CHRF;
(*4430*)              10,11: PREDSUCCF;
(*4431*)              12,13,
(*4432*)              14,15,
(*4433*)              16,17: STDARITHFUNCS;   (*SIN,COS,EXP,SQRT,LN,ARCTAN*)
(*4434*)              18:CLOCKF;
(*4435*)              19:CARD;
(*4436*)            END
(*4437*)        END;
(*4438*)    END (*CALL*) ;
(*4439*) 
$TITLE EXPRSSN - REGULAROP,SETTYPCHK
(*4440*)    PROCEDURE EXPRESSION;
(*4441*)     VAR LATTRP: ATTRP; LOP: OPERATOR;
(*4442*) 
(*4443*)    PROCEDURE REGULAROPERATION(FATTRP:ATTRP; FOP:OPERATOR);
(*4444*)      BEGIN
(*4445*)        IF COMPTYPES(FATTRP@.TYPTR,INTPTR) THEN
(*4446*)          IF COMPTYPES(GATTRP@.TYPTR,INTPTR)
(*4447*)            THEN INTARITH(FATTRP,GATTRP,FOP)
(*4448*)            ELSE
(*4449*)              IF GATTRP@.TYPTR=REALPTR
(*4450*)                THEN REALARITH(FATTRP,GATTRP,FOP)
(*4451*)                ELSE ERRORRESET(134)
(*4452*)        ELSE
(*4453*)          IF (FATTRP@.TYPTR=REALPTR) THEN
(*4454*)            IF (GATTRP@.TYPTR=REALPTR) OR
(*4455*)                COMPTYPES(GATTRP@.TYPTR,INTPTR)
(*4456*)              THEN REALARITH(FATTRP,GATTRP,FOP)
(*4457*)              ELSE ERRORRESET(134)
(*4458*)          ELSE
(*4459*)            IF (FATTRP@.TYPTR@.FORM = POWER) AND
(*4460*)                COMPTYPES(FATTRP@.TYPTR,GATTRP@.TYPTR)
(*4461*)              THEN SETARITH(FATTRP,GATTRP,FOP)
(*4462*)              ELSE ERRORRESET(134);
(*4463*)      END;
(*4464*) 
(*4465*)    PROCEDURE SETTYPECHECK(FSP:STP);
(*4466*)      BEGIN
(*4467*)        IF GATTRP@.TYPTR=REALPTR THEN ERRORRESET(109);
(*4468*)        IF GATTRP@.TYPTR <> NIL THEN
(*4469*)          IF GATTRP@.TYPTR@.FORM > SUBRANGE THEN ERRORRESET(136)
(*4470*)          ELSE
(*4471*)            IF NOT COMPTYPES(FSP,GATTRP@.TYPTR) THEN ERRORRESET(137);
(*4472*)        IF GATTRP@.TYPTR<>NIL THEN CHECKRANGE(GATTRP,SETMIN,SETMAX,304);
(*4473*)      END;
(*4474*) 
$TITLE  POWERSET OPERATIONS
(*4475*)    PROCEDURE POWERSET;
(*4476*)      VAR LSP:STP; LCSTATTRP,LVARATTRP,LATTRP,ATTRWORK:ATTRP;
(*4477*)          VARPART:BOOLEAN; N:INTEGER;
(*4478*)      BEGIN INSYMBOL;
(*4479*)       NEW(LSP,POWER);
(*4480*)       WITH LSP@ DO
(*4481*)        BEGIN ELSET := NIL; PCKDSET := FALSE;
(*4482*)         FTYPE := FALSE;
(*4483*)         SIZE.WBLENGTH:=8; SIZE.BOUNDARY:=8;
(*4484*)        END;
(*4485*)       VARPART := FALSE;
(*4486*)       ATTRNEW(LCSTATTRP);
(*4487*)       WITH LCSTATTRP@ DO
(*4488*)        BEGIN TYPTR:=LSP; KIND:=CST; CVAL.CKIND:=PSET; CVAL.PVAL:=(. .);
(*4489*)        END;
(*4490*)       IF SY = RBRACK THEN INSYMBOL
(*4491*)       ELSE
(*4492*)        BEGIN
(*4493*)         (*LOOP UNTIL SY <> COMMA:*)
(*4494*)         LOOP
(*4495*)          EXPRESSION(FSYS+(.COMMA,COLON,RBRACK.));
(*4496*)          SETTYPECHECK(LSP@.ELSET);
(*4497*)          IF GATTRP@.TYPTR<>NIL THEN LSP@.ELSET:=GATTRP@.TYPTR;
(*4498*)          IF SY = COLON THEN
(*4499*)           BEGIN ATTRNEW(LATTRP); COPYATTR(GATTRP,LATTRP);
(*4500*)            INSYMBOL;
(*4501*)            EXPRESSION(FSYS+(.COMMA,RBRACK.));
(*4502*)            SETTYPECHECK(LATTRP@.TYPTR);
(*4503*)            IF (LATTRP@.TYPTR <> NIL)AND (GATTRP@.TYPTR <> NIL)
(*4504*)             THEN
(*4505*)             BEGIN
(*4506*)              IF (LATTRP@.KIND = CST)AND (GATTRP@.KIND = CST)
(*4507*)               THEN
(*4508*)               BEGIN
(*4509*)                 FOR N := LATTRP@.CVAL.IVAL TO GATTRP@.CVAL.IVAL DO
(*4510*)                   IF (N>=SETMIN) AND (N<=SETMAX) THEN
(*4511*)                    LCSTATTRP@.CVAL.PVAL := LCSTATTRP@.CVAL.PVAL+(.N.);
(*4512*)                  ATTRDISP(LATTRP);
(*4513*)               END
(*4514*)              ELSE
(*4515*)      BEGIN
(*4516*)        LOAD(GATTRP,LATTRP); OPERATION(GATTRP,LATTRP,ZS,ZSR);
(*4517*)        ATTRNEW(ATTRWORK);
(*4518*)        WITH ATTRWORK@ DO
(*4519*)          BEGIN TYPTR:=LSP; KIND:=CST; CVAL.CKIND:=PSET;
(*4520*)                CVAL.PVAL:=(.0.);
(*4521*)          END;
(*4522*)        LOAD(ATTRWORK,GATTRP);
(*4523*)        GENRXP(ZSRDA,ATTRWORK@.REXPR.RNO,0,REALREG(.GATTRP@.REXPR.RNO.),0);
(*4524*)        EXCATTR(LATTRP,GATTRP); ATTRDISP(LATTRP);
(*4525*)        LOAD(GATTRP,ATTRWORK);
(*4526*)        GENRXP(ZSRDL,ATTRWORK@.REXPR.RNO,0,REALREG(.GATTRP@.REXPR.RNO.),0);
(*4527*)        EXCATTR(ATTRWORK,GATTRP); ATTRDISP(ATTRWORK);
(*4528*)        IF VARPART THEN SETARITH(GATTRP,LVARATTRP,PLUS)
(*4529*)        ELSE BEGIN VARPART:=TRUE; ATTRNEW(LVARATTRP); COPYATTR(GATTRP,LVARATTRP) END;
(*4530*)      END;
(*4531*)             END;
(*4532*)           END (*COLON*)
(*4533*)         ELSE
(*4534*)           IF GATTRP@.TYPTR <> NIL THEN
(*4535*)             BEGIN
(*4536*)               IF GATTRP@.KIND = CST THEN
(*4537*)                 LCSTATTRP@.CVAL.PVAL := LCSTATTRP@.CVAL.PVAL
(*4538*)                 +(.GATTRP@.CVAL.IVAL.)
(*4539*)               ELSE
(*4540*)      BEGIN
(*4541*)        ATTRNEW(ATTRWORK);
(*4542*)        WITH ATTRWORK@ DO
(*4543*)          BEGIN TYPTR:=LSP; KIND:=CST; CVAL.CKIND:=PSET;
(*4544*)                CVAL.PVAL:=(.0.);
(*4545*)          END;
(*4546*)        LOAD(ATTRWORK,GATTRP); LOAD(GATTRP,ATTRWORK);
(*4547*)        GENRXP(ZSRDL,ATTRWORK@.REXPR.RNO,0,REALREG(.GATTRP@.REXPR.RNO.),0);
(*4548*)        EXCATTR(ATTRWORK,GATTRP); ATTRDISP(ATTRWORK);
(*4549*)        IF VARPART THEN SETARITH(GATTRP,LVARATTRP,PLUS)
(*4550*)        ELSE BEGIN VARPART:=TRUE; ATTRNEW(LVARATTRP); COPYATTR(GATTRP,LVARATTRP) END;
(*4551*)      END;
(*4552*)             END;
(*4553*)         IF SY<>COMMA THEN EXIT; INSYMBOL;
(*4554*)       END;
(*4555*)       TEST1(RBRACK,12);
(*4556*)        END;
(*4557*)       IF VARPART THEN
(*4558*)        BEGIN
(*4559*)         IF LCSTATTRP@.CVAL.PVAL <> (. .) THEN
(*4560*)          SETARITH(LCSTATTRP,LVARATTRP,PLUS);
(*4561*)         COPYATTR(LVARATTRP,GATTRP); ATTRDISP(LVARATTRP);
(*4562*)        END
(*4563*)       ELSE COPYATTR(LCSTATTRP,GATTRP);
(*4564*)       ATTRDISP(LCSTATTRP);
(*4565*)      END;
(*4566*) 
$TITLE  FACTOR PROCEDURE
(*4567*)     PROCEDURE FACTOR(FSYS: SETOFSYS);
(*4568*)       VAR LCP: CTP;
(*4569*)       LATTRP:ATTRP;
(*4570*)       FLT   :0..1;
(*4571*)       BEGIN
(*4572*)         IF NOT (SY IN FACBEGSYS) THEN
(*4573*)           BEGIN ERROR(58); SKIP(FSYS+FACBEGSYS);
(*4574*)             GATTRP@.TYPTR := NIL
(*4575*)           END;
(*4576*)         REPEAT
(*4577*)           IF SY IN FACBEGSYS THEN
(*4578*)             BEGIN
(*4579*)               CASE SY OF
(*4580*)       (*ID*)    IDENT:
(*4581*)                     BEGIN
(*4582*)                       SEARCHID((.KONST,VARS,FIELD,FUNC,TYPES.),LCP);
(*4583*)                     INSYMBOL;
(*4584*)                     CASE LCP@.KLASS OF
(*4585*)                       KONST: WITH LCP@,GATTRP@ DO
(*4586*)                                BEGIN TYPTR:=IDTYPE; KIND:=CST;
(*4587*)                                      CVAL:=VALUES;
(*4588*)                                END;
(*4589*)       (* TYPES *)      TYPES:BEGIN
(*4590*)                                IF EXTWARN THEN ERROR(291);
(*4591*)                                TEST1(LPARENT,9);
(*4592*)                                EXPRESSION(FSYS+(.RPARENT.));
(*4593*)                                WITH GATTRP@ DO
(*4594*)                                  IF TYPTR<>NIL THEN
(*4595*)                                  BEGIN
(*4596*)                                    IF KIND=CST THEN ERROR(292);
(*4597*)                                    TYPTR:=LCP@.IDTYPE
(*4598*)                                 END;
(*4599*)                                 TEST1(RPARENT,4);
(*4600*)                            END;
(*4601*)                       VARS,
(*4602*)                       FIELD: SELECTOR(FSYS,LCP);
(*4603*)                       FUNC:  CALL(FSYS,LCP)
(*4604*)                     END
(*4605*)                   END;
(*4606*)       (*CST*)   INTCONST:
(*4607*)                   BEGIN
(*4608*)                     WITH GATTRP@ DO
(*4609*)                       BEGIN TYPTR := INTPTR; KIND := CST;
(*4610*)                             CVAL.CKIND:=INT; CVAL.IVAL:=IVAL;
(*4611*)                       END;
(*4612*)                     INSYMBOL
(*4613*)                   END;
(*4614*)                 REALCONST:
(*4615*)                   BEGIN
(*4616*)                     WITH GATTRP@ DO
(*4617*)                       BEGIN TYPTR := REALPTR; KIND := CST;
(*4618*)                             CVAL.CKIND:=REEL; CVAL.RVAL:=RVAL;
(*4619*)                       END;
(*4620*)                     INSYMBOL
(*4621*)                   END;
(*4622*)                 CHARCONST:
(*4623*)                   BEGIN
(*4624*)                     WITH GATTRP@ DO
(*4625*)                       BEGIN TYPTR := CHARPTR; KIND := CST;
(*4626*)                             CVAL.CKIND:=INT; CVAL.IVAL:=IVAL;
(*4627*)                       END;
(*4628*)                     INSYMBOL
(*4629*)                   END;
(*4630*)                 STRINGCONST:
(*4631*)                   BEGIN
(*4632*)                     WITH GATTRP@ DO
(*4633*)                       BEGIN STRINGTYPE(TYPTR); KIND := CST;
(*4634*)                             CVAL.CKIND:=STRG; CVAL.VALP:=CONSTP;
(*4635*)                       END;
(*4636*)                     INSYMBOL
(*4637*)                   END;
(*4638*)       (*(*)     LPARENT:
(*4639*)                   BEGIN INSYMBOL; EXPRESSION(FSYS+(.RPARENT.));
(*4640*)                     IF SY = RPARENT THEN INSYMBOL ELSE ERROR(4)
(*4641*)                   END;
(*4642*)       (*NOT*)   NOTSY:
(*4643*)                   BEGIN INSYMBOL; FACTOR(FSYS);
(*4644*)                     IF GATTRP@.TYPTR<>NIL THEN
(*4645*)                       IF COMPTYPES(GATTRP@.TYPTR,BOOLPTR) THEN NOTFACTOR(GATTRP)
(*4646*)                         ELSE BEGIN ERROR(135); GATTRP@.TYPTR:=NIL; END;
(*4647*)                   END;
(*4648*)       (*(.*)    LBRACK:  POWERSET;
(*4649*)               END (*CASE*) ;
(*4650*)               TEST2(FSYS,6,FACBEGSYS)
(*4651*)             END (*IF*)
(*4652*)         UNTIL SY IN FSYS;
(*4653*) 
(*4654*) 
(*4655*)(*  EXPONENTIATION *)
(*4656*)(*******************)
(*4657*) 
(*4658*) 
(*4659*)IF SY = EXPONOP THEN
(*4660*)BEGIN
(*4661*)  IF EXTWARN THEN ERROR(291);
(*4662*)  IF (NOT COMPTYPES(GATTRP@.TYPTR,INTPTR)) AND
(*4663*)     (GATTRP@.TYPTR <> REALPTR) THEN ERROR(399);
(*4664*)  INSYMBOL;
(*4665*)  ATTRNEW(LATTRP); COPYATTR(GATTRP,LATTRP);
(*4666*)  FACTOR(FSYS);
(*4667*)  IF COMPTYPES(GATTRP@.TYPTR,INTPTR) THEN
(*4668*)  BEGIN
(*4669*)        LOAD(LATTRP,GATTRP);
(*4670*)        LOAD(GATTRP,LATTRP);
(*4671*)        IF LATTRP@.TYPTR=INTPTR THEN FLT:=0 ELSE FLT:=1;
(*4672*)        LOADINTCONST(R0,FLT*256+16*REALREG(.LATTRP@.REXPR.RNO.)
(*4673*)                         +REALREG(.GATTRP@.REXPR.RNO.));
(*4674*)        GENRX(ZBAL,BASEWORK,0,1,ENTRYEXPON);
(*4675*)        EXCATTR(LATTRP,GATTRP);
(*4676*) 
(*4677*)  END
(*4678*)  ELSE
(*4679*)  IF GATTRP@.TYPTR=REALPTR THEN
(*4680*)  BEGIN
(*4681*)         IF COMPTYPES(LATTRP@.TYPTR,INTPTR) THEN
(*4682*)            INTTOREAL(LATTRP);
(*4683*)    LOAD(LATTRP,GATTRP);
(*4684*)    LOADINTCONST(R0,REALREG(.LATTRP@.REXPR.RNO.));
(*4685*)    GENRX(ZBAL,BASEWORK,0,1,ENTRYSIN+(16-12)*8);
(*4686*)    REGULAROPERATION(LATTRP,MUL);
(*4687*)    LOAD(GATTRP,LATTRP);
(*4688*)    LOADINTCONST(R0,REALREG(.GATTRP@.REXPR.RNO.));
(*4689*)    GENRX(ZBAL,BASEWORK,0,1,ENTRYSIN+(14-12)*8);
(*4690*)  END;
(*4691*)  ATTRDISP(LATTRP);
(*4692*)END;
(*4693*)       END (*FACTOR*) ;
(*4694*) 
$TITLE PROCEDURE TERM
(*4695*)    PROCEDURE TERM(FSYS: SETOFSYS);
(*4696*)      VAR LATTRP: ATTRP; LOP: OPERATOR;
(*4697*)      BEGIN
(*4698*)        FACTOR(FSYS+(.MULOP,EXPONOP.));
(*4699*)        WHILE SY = MULOP DO
(*4700*)          BEGIN
(*4701*)            ATTRNEW(LATTRP); COPYATTR(GATTRP,LATTRP);
(*4702*)            LOP := OP;
(*4703*)              INSYMBOL; FACTOR(FSYS+(.MULOP,EXPONOP.));
(*4704*)            IF (LATTRP@.TYPTR <> NIL)AND (GATTRP@.TYPTR <> NIL) THEN
(*4705*)              CASE LOP OF
(*4706*)(***)           MUL : REGULAROPERATION(LATTRP,MUL);
(*4707*)(*/*)           RDIV: IF COMPTYPES(LATTRP@.TYPTR,INTPTR) OR
(*4708*)                           (LATTRP@.TYPTR=REALPTR) THEN
(*4709*)                        IF COMPTYPES(GATTRP@.TYPTR,INTPTR) OR
(*4710*)                             (GATTRP@.TYPTR=REALPTR) THEN
(*4711*)                          REALARITH(LATTRP,GATTRP,RDIV)
(*4712*)                        ELSE ERRORRESET(134)
(*4713*)                      ELSE ERRORRESET(134);
(*4714*)(*DIV,MOD*)     IDIV,IMOD: IF COMPTYPES(LATTRP@.TYPTR,INTPTR) AND
(*4715*)                                COMPTYPES(GATTRP@.TYPTR,INTPTR) THEN
(*4716*)                             INTARITH(LATTRP,GATTRP,LOP)
(*4717*)                           ELSE ERRORRESET(134);
(*4718*)(*AND*)         ANDOP:IF COMPTYPES(LATTRP@.TYPTR,BOOLPTR)AND
(*4719*)                          COMPTYPES(GATTRP@.TYPTR,BOOLPTR) THEN
(*4720*)                        BOOLARITH(LATTRP,GATTRP,ANDOP)
(*4721*)                      ELSE ERRORRESET(134)
(*4722*)              END (*CASE*)
(*4723*)            ELSE GATTRP@.TYPTR := NIL;
(*4724*)            ATTRDISP(LATTRP)
(*4725*)          END (*WHILE*);
(*4726*)      END;
(*4727*) 
$TITLE  SIMPLE EXPRESSION
(*4728*)    PROCEDURE SIMPLEEXPRESSION(FSYS: SETOFSYS);
(*4729*)      VAR LATTRP: ATTRP; LOP: OPERATOR;
(*4730*)      BEGIN
(*4731*)        LOP:=NOOP;
(*4732*)        IF OP IN (.PLUS,MINUS.) THEN
(*4733*)          BEGIN LOP:=OP; INSYMBOL; END;
(*4734*)        TERM(FSYS+(.ADDOP.));
(*4735*)        IF LOP<>NOOP THEN
(*4736*)          BEGIN
(*4737*)            IF NOT ((GATTRP@.TYPTR=REALPTR) OR COMPTYPES(GATTRP@.TYPTR,INTPTR))
(*4738*)              THEN ERRORRESET(134)
(*4739*)              ELSE IF LOP=MINUS THEN NEGATE(GATTRP);
(*4740*)          END;
(*4741*)        WHILE SY = ADDOP DO
(*4742*)          BEGIN ATTRNEW(LATTRP); COPYATTR(GATTRP,LATTRP); LOP := OP;
(*4743*)            INSYMBOL; TERM(FSYS+(.ADDOP.));
(*4744*)            IF (LATTRP@.TYPTR <> NIL)AND (GATTRP@.TYPTR <> NIL) THEN
(*4745*)              CASE LOP OF
(*4746*)(*+,-*)         PLUS,MINUS:
(*4747*)                  REGULAROPERATION(LATTRP,LOP);
(*4748*)(*OR*)          OROP:
(*4749*)                  IF COMPTYPES(LATTRP@.TYPTR,BOOLPTR) AND COMPTYPES(GATTRP@.TYPTR,BOOLPTR)
(*4750*)                    THEN BOOLARITH(LATTRP,GATTRP,OROP)
(*4751*)                    ELSE ERRORRESET(134)
(*4752*)              END (*CASE*)
(*4753*)            ELSE GATTRP@.TYPTR := NIL;
(*4754*)            ATTRDISP(LATTRP);
(*4755*)          END (*WHILE*);
(*4756*)      END;
(*4757*) 
$TITLE  EXPRESSION - (BODY)
(*4758*)    BEGIN (*EXPRESSION*)
(*4759*)      SIMPLEEXPRESSION(FSYS+(.RELOP.));
(*4760*)      IF SY = RELOP THEN
(*4761*)        BEGIN
(*4762*)          LOP:=OP;
(*4763*)          ATTRNEW(LATTRP); COPYATTR(GATTRP,LATTRP);
(*4764*)          INSYMBOL; SIMPLEEXPRESSION(FSYS);
(*4765*)          IF (LATTRP@.TYPTR <> NIL)AND (GATTRP@.TYPTR <> NIL) THEN
(*4766*)            IF LOP = INOP THEN
(*4767*)              IF GATTRP@.TYPTR@.FORM = POWER THEN
(*4768*)                IF COMPTYPES(LATTRP@.TYPTR,GATTRP@.TYPTR@.ELSET) THEN
(*4769*)                  INPOWER(LATTRP,GATTRP)
(*4770*)                ELSE ERRORRESET(129)
(*4771*)              ELSE ERRORRESET(130)
(*4772*)            ELSE
(*4773*)             BEGIN
(*4774*)              IF COMPTYPES(LATTRP@.TYPTR,INTPTR) AND
(*4775*)                 (GATTRP@.TYPTR=REALPTR) THEN INTTOREAL(LATTRP);
(*4776*)              IF COMPTYPES(GATTRP@.TYPTR,INTPTR) AND
(*4777*)                 (LATTRP@.TYPTR=REALPTR) THEN INTTOREAL(GATTRP);
(*4778*)              IF NOT COMPTYPES(LATTRP@.TYPTR,GATTRP@.TYPTR) THEN
(*4779*)                ERRORRESET(129)
(*4780*)              ELSE
(*4781*)               CASE LATTRP@.TYPTR@.FORM OF
(*4782*)                 SCALAR,SUBRANGE,PACKDTYPE:
(*4783*)                   IF (LATTRP@.TYPTR=REALPTR)
(*4784*)                     THEN RELREAL(LATTRP,GATTRP,LOP)
(*4785*)                     ELSE RELINT(LATTRP,GATTRP,LOP);
(*4786*)                 POINTER:
(*4787*)                   IF LOP IN (.EQOP,NEOP.)
(*4788*)                     THEN RELINT(LATTRP,GATTRP,LOP)
(*4789*)                     ELSE ERRORRESET(131);
(*4790*)                 POWER:
(*4791*)                   IF LOP IN (.LTOP,GTOP.)
(*4792*)                     THEN ERRORRESET(132)
(*4793*)                     ELSE RELPOWER(LATTRP,GATTRP,LOP);
(*4794*)                 ARRAYS,RECORDS:
(*4795*)                   IF STRING(LATTRP@.TYPTR) THEN RELLONG(LATTRP,GATTRP,LOP)
(*4796*)                     ELSE IF LOP IN (.LTOP,GTOP,LEOP,GEOP.)
(*4797*)                       THEN ERROR(131)
(*4798*)                       ELSE ERROR(399);
(*4799*)                 FILES:
(*4800*)                   ERRORRESET(133)
(*4801*)               END (*CASE*);
(*4802*)             END (*SY <> INOP*)
(*4803*)            ELSE GATTRP@.TYPTR := NIL;
(*4804*)          ATTRDISP(LATTRP);
(*4805*)        END (*SY = RELOP*) ;
(*4806*)    END (*EXPRESSION*) ;
(*4807*) 
$TITLE  STATEMENT AND JMPS
(*4808*)    PROCEDURE COMPOUNDSTATEMENT(FSYS:SETOFSYS); FORWARD;
(*4809*) 
(*4810*)   PROCEDURE STATEMENT(FSYS: SETOFSYS);
(*4811*)      LABEL 1;
(*4812*)    VAR LCP:CTP; LLP:LBP; LCIX:ADDRRANGE;
(*4813*) 
(*4814*)    PROCEDURE GENFJMP(FADDR:ADDRRANGE);
(*4815*)      VAR X: INTEGER;
(*4816*)      BEGIN
(*4817*)        IF NOT COMPTYPES(GATTRP@.TYPTR,BOOLPTR) THEN ERROR(145);
(*4818*)        LOAD(GATTRP,NIL);
(*4819*)        IF BOOLFLAG THEN
(*4820*)          BEGIN IC:=IC-10;
(*4821*)                X:=15-GETCODE(IC+4) MOD 256 DIV 16;
(*4822*)                GENRX(ZBC,X,0,PBASE1,FADDR);
(*4823*)          END
(*4824*)        ELSE
(*4825*)          BEGIN GENRRP1(ZLTR,GATTRP@.REXPR.RNO);
(*4826*)                GENRX(ZBC,CONDZ,0,PBASE1,FADDR);
(*4827*)          END;
(*4828*)      END;
(*4829*) 
(*4830*)    PROCEDURE GENJMP(FADDR:ADDRRANGE);
(*4831*)      BEGIN
(*4832*)        GENRX(ZBC,15,0,PBASE1,FADDR);
(*4833*)      END;
(*4834*) 
(*4835*)    PROCEDURE PREPFJMP(VAR FIX: ADDRRANGE);
(*4836*)      BEGIN
(*4837*)        GENFJMP(-4096*PBASE1); FIX:=IC-4;
(*4838*)      END;
(*4839*) 
(*4840*)    PROCEDURE PREPJMP(VAR FIX: ADDRRANGE);
(*4841*)      BEGIN
(*4842*)        FIX:=IC; GENRX(ZBC,15,0,0,0);
(*4843*)      END;
(*4844*) 
$TITLE ASSIGNMENT
(*4845*)    PROCEDURE ASSIGNMENT(FCP: CTP);
(*4846*)      VAR LATTRP:ATTRP;
(*4847*)      BEGIN
(*4848*)        SELECTOR(FSYS+(.BECOMES.),FCP);
(*4849*)        IF SY = BECOMES THEN
(*4850*)          BEGIN
(*4851*)            ATTRNEW(LATTRP); COPYATTR(GATTRP,LATTRP);
(*4852*)            INSYMBOL; EXPRESSION(FSYS);
(*4853*)            IF (LATTRP@.TYPTR <> NIL)AND (GATTRP@.TYPTR <> NIL) THEN
(*4854*)              STORE(LATTRP,GATTRP,129);
(*4855*)            ATTRDISP(LATTRP); RESETG;
(*4856*)          END
(*4857*)        ELSE ERROR(51);
(*4858*)      END;
(*4859*) 
$TITLE  GOTO STATEMENT
(*4860*)    PROCEDURE GOTOSTATEMENT;
(*4861*)     LABEL 1;
(*4862*)     VAR LLP:LBP; LCIX:ADDRRANGE;
(*4863*)      BEGIN
(*4864*)        IF SY = INTCONST THEN
(*4865*)          BEGIN LLP := FSTLABP;
(*4866*)            WHILE LLP <> FLABP DO (*DECIDE WHETHER LOCALLY DECLARED*)
(*4867*)              WITH LLP@ DO
(*4868*)                IF LABVAL = IVAL THEN
(*4869*)                  BEGIN
(*4870*)                    IF DEFINED THEN GENJMP(LABADDR)
(*4871*)                    ELSE
(*4872*)                      BEGIN PREPJMP(LCIX); LINKOCC(FSTOCC,LCIX); END;
(*4873*)                    GOTO 1
(*4874*)                  END
(*4875*)                ELSE LLP := NEXTLAB;
(*4876*)            WHILE LLP<>NIL DO
(*4877*)              WITH LLP@ DO
(*4878*)                IF LABVAL<>IVAL THEN LLP:=NEXTLAB
(*4879*)                ELSE
(*4880*)                  BEGIN
(*4881*)                    IF LCNT=0 THEN
(*4882*)                      IF PCNT>=MAXPROCFUNC THEN ERROR(261)
(*4883*)                         ELSE BEGIN
(*4884*)                                PCNT:=PCNT+1;LCNT:=PCNT;
(*4885*)                                $PASOBJ2@:='????????'; PUT($PASOBJ2);
(*4886*)                              END;
(*4887*)                     GENRX(ZLA,15,0,0,PROCBASE+4*LCNT-4);
(*4888*)                     GENRX(ZLA,9,0,0,240);
(*4889*)                     GENRX(ZEX,9,0,1,8);
(*4890*)                     GENRR(ZBCR,15,15);
(*4891*)                     GOTO 1;
(*4892*)                  END;
(*4893*)            ERROR(167);
(*4894*)      1:    INSYMBOL
(*4895*)          END
(*4896*)        ELSE ERROR(15);
(*4897*)      END (*GOTOSTATEMENT*) ;
(*4898*) 
$TITLE IFSTATEMENT
(*4899*)    PROCEDURE IFSTATEMENT;
(*4900*)      VAR LCIX1,LCIX2: ADDRRANGE;
(*4901*)      BEGIN EXPRESSION(FSYS+(.THENSY.));
(*4902*)        PREPFJMP(LCIX1); RESETG;
(*4903*)        TEST1(THENSY,52);
(*4904*)        STATEMENT(FSYS+(.ELSESY.));
(*4905*)        IF SY = ELSESY THEN
(*4906*)          BEGIN PREPJMP(LCIX2); INSERTIC(LCIX1); INSYMBOL;
(*4907*)                STATEMENT(FSYS); INSERTIC(LCIX2);
(*4908*)          END
(*4909*)        ELSE INSERTIC(LCIX1);
(*4910*)      END;
(*4911*) 
$TITLE CASE STATEMENT
(*4912*)    PROCEDURE CASESTATEMENT;
(*4913*)   LABEL 1,2;
(*4914*)     TYPE CIP = @CASEREC;
(*4915*)        CASEREC=
(*4916*)             RECORD NEXT: CIP;
(*4917*)              CSLAB: INTEGER;
(*4918*)              CSADDR: ADDRRANGE;
(*4919*)             END;
(*4920*)     VAR LSP,LSP1: STP; FSTPTR,LPT1,LPT2,LPT3: CIP; LVAL: VALU;
(*4921*)         SWITCHIX,LCIX:ADDRRANGE;
(*4922*)         LMIN,LMAX: INTEGER;
(*4923*)         JUMPREG: REGNO; CHAIN:LOCOFREF;
(*4924*)         INDEXJUMP:BOOLEAN;
(*4925*)         COUNT : INTEGER;
(*4926*)         DEFAULT:INTEGER;
(*4927*)         LSP2:STP;
(*4928*) 
(*4929*)    PROCEDURE GENSWITCH(FCIP: CIP);
(*4930*)      VAR LVAL,JUMPBASE: INTEGER;
(*4931*)      BEGIN
(*4932*)        IF ((LMAX-LMIN)<CIXMAX) AND INDEXJUMP
(*4933*)          THEN
(*4934*)            BEGIN
(*4935*)             IF DEFAULT=0 THEN
(*4936*)             BEGIN
(*4937*)               IF DEBUG THEN
(*4938*)               CHECKREGISTER(REALREG(.JUMPREG.),LMIN,LMAX)
(*4939*)             END ELSE
(*4940*)               BEGIN
(*4941*)                GENRR(ZBALR,9,0);
(*4942*)                MAKEINTCONST(LMIN); GENRX(ZC,REALREG(.JUMPREG.),0,0,0);
(*4943*)                GENRX(ZBC,CONDM,0,14,DEFAULT);
(*4944*)                MAKEINTCONST(LMAX); GENRX(ZC,REALREG(.JUMPREG.),0,0,0);
(*4945*)                GENRX(ZBC,CONDP,0,14,DEFAULT);
(*4946*)              END;
(*4947*)              GENRXP(ZSLL,JUMPREG,0,0,2);
(*4948*)              JUMPBASE:=IC+4-4*LMIN;
(*4949*)              IF (JUMPBASE<0) OR (JUMPBASE>=4096)
(*4950*)                THEN
(*4951*)                  BEGIN GENRR(ZLR,BASEWORK,PBASE1); MAKEINTCONST(JUMPBASE+6);
(*4952*)                        GENRX(ZA,BASEWORK,0,0,0);
(*4953*)                        GENRX(ZBC,15,REALREG(.JUMPREG.),BASEWORK,0);
(*4954*)                  END
(*4955*)                ELSE GENRX(ZBC,15,REALREG(.JUMPREG.),PBASE1,JUMPBASE);
(*4956*)              LVAL:=LMIN;
(*4957*)              REPEAT
(*4958*)                WITH FCIP@ DO
(*4959*)                  BEGIN
(*4960*)                    WHILE CSLAB > LVAL DO
(*4961*)                   BEGIN IF DEFAULT = 0 THEN
(*4962*)                         GENRX(ZBAL,9,0,1,JUMPERR1) ELSE
(*4963*)                         GENJMP(DEFAULT);
(*4964*)                            LVAL:=LVAL+1;
(*4965*)                      END;
(*4966*)                    GENJMP(CSADDR);
(*4967*)                    LVAL := LVAL + 1; FCIP := NEXT
(*4968*)                  END
(*4969*)              UNTIL FCIP = NIL
(*4970*)            END
(*4971*)          ELSE
(*4972*)            BEGIN
(*4973*)              REPEAT
(*4974*)                WITH FCIP@ DO
(*4975*)                  BEGIN MAKEINTCONST(CSLAB); GENRXP(ZC,JUMPREG,0,0,0);
(*4976*)                    GENRX(ZBC,CONDZ,0,PBASE1,CSADDR); FCIP:=NEXT;
(*4977*)                  END;
(*4978*)              UNTIL FCIP=NIL;
(*4979*)             IF DEFAULT = 0 THEN GENRX(ZBAL,9,0,1,JUMPERR1)
(*4980*)                 ELSE GENJMP(DEFAULT);
(*4981*)            END;
(*4982*)      END;
(*4983*) 
(*4984*)    BEGIN
(*4985*)      EXPRESSION(FSYS+(.OFSY,COMMA,COLON.));
(*4986*)      LSP := GATTRP@.TYPTR;
(*4987*)      IF LSP <> NIL THEN
(*4988*)        IF (LSP@.FORM>SUBRANGE) OR (LSP=REALPTR) THEN
(*4989*)          BEGIN ERROR(144); LSP := NIL END;
(*4990*)      LOAD(GATTRP,NIL); JUMPREG:=GATTRP@.REXPR.RNO; RESETG;
(*4991*)      PREPJMP(SWITCHIX);
(*4992*)      TEST1(OFSY,8);
(*4993*)      DEFAULT := 0;
(*4994*)      FSTPTR:=NIL; LPT3:=NIL; CHAIN:=NIL;
(*4995*)      (*LOOP UNTIL SY <> SEMICOLON*)
(*4996*)      LOOP
(*4997*)        IF NOT (SY IN (.SEMICOLON,ENDSY.)) THEN
(*4998*)          BEGIN
(*4999*)            (*LOOP UNTIL SY <> COMMA:*)
(*5000*)       IF SY = ELSESY THEN
(*5001*)       BEGIN
(*5002*)         IF EXTWARN THEN ERROR(291);
(*5003*)         INSYMBOL; IF DEFAULT = 0 THEN DEFAULT:=IC ELSE ERROR(156)
(*5004*)       END ELSE
(*5005*)       LOOP
(*5006*)         CONSTANT(FSYS+(.COMMA,COLON.),LSP1,LVAL);
(*5007*)         IF LSP1 <> NIL THEN
(*5008*)          IF COMPTYPES(LSP,LSP1) THEN LMIN:=LVAL.IVAL
(*5009*)          ELSE BEGIN ERROR(147); LSP1:=NIL END;
(*5010*)          IF (SY=COLON) AND DOTDOT THEN
(*5011*)          BEGIN
(*5012*)            IF EXTWARN THEN ERROR(291);
(*5013*)            INSYMBOL; CONSTANT(FSYS+(.COMMA,COLON.),LSP2,LVAL);
(*5014*)            IF LSP2 <> NIL THEN
(*5015*)              IF COMPTYPES(LSP,LSP2) THEN
(*5016*)                IF LMIN<=LVAL.IVAL THEN LMAX:=LVAL.IVAL
(*5017*)                 ELSE
(*5018*)                   BEGIN ERROR(102);LMAX:=LMIN;LMIN:=LVAL.IVAL;END
(*5019*)              ELSE
(*5020*)              BEGIN ERROR(147); LSP2:=NIL END
(*5021*)         END ELSE BEGIN LSP2:=LSP1;LMAX:=LMIN END;
(*5022*)       IF (LSP1 <> NIL) AND (LSP2<> NIL) THEN
(*5023*)       BEGIN LPT1:=FSTPTR; LPT2:=NIL;
(*5024*)         WHILE LPT1 <> NIL DO
(*5025*)           WITH LPT1@ DO
(*5026*)            IF LMIN <= CSLAB THEN
(*5027*)                IF (CSLAB=LMIN) OR (LMAX >= CSLAB) THEN
(*5028*)                BEGIN ERROR(156); GOTO 2 END
(*5029*)                 ELSE GOTO 1
(*5030*)            ELSE BEGIN LPT2:=LPT1; LPT1:=NEXT END;
(*5031*)   1:    FOR COUNT:=LMIN TO LMAX DO
(*5032*)         BEGIN
(*5033*)           NEW(LPT3);
(*5034*)           WITH LPT3@ DO
(*5035*)           BEGIN
(*5036*)             CSLAB:=COUNT;
(*5037*)             CSADDR:=IC
(*5038*)           END;
(*5039*)           IF LPT2 = NIL THEN FSTPTR:=LPT3 ELSE
(*5040*)              LPT2@.NEXT:=LPT3;
(*5041*)           LPT2:=LPT3;
(*5042*)         END;
(*5043*)           LPT2@.NEXT:=LPT1
(*5044*)         END;
(*5045*)2:
(*5046*)              IF SY<>COMMA THEN EXIT; INSYMBOL;
(*5047*)            END;
(*5048*)            TEST1(COLON,5);
(*5049*)            REPEAT STATEMENT(FSYS+(.SEMICOLON.));
(*5050*)              IF SY IN STATBEGSYS THEN ERROR(14);
(*5051*)            UNTIL NOT (SY IN STATBEGSYS);
(*5052*)            PREPJMP(LCIX); LINKOCC(CHAIN,LCIX);
(*5053*)          END (*SY <> ENDSY*) ;
(*5054*)        IF SY<>SEMICOLON THEN EXIT; INSYMBOL;
(*5055*)      END;
(*5056*)      IF FSTPTR <> NIL THEN
(*5057*)        BEGIN
(*5058*)          LPT1:=FSTPTR;
(*5059*)          WHILE LPT1<>NIL DO
(*5060*)            BEGIN LPT2:=LPT1; LPT1:=LPT1@.NEXT; END;
(*5061*)          LMAX:=LPT2@.CSLAB; LMIN:=FSTPTR@.CSLAB;
(*5062*)          IF (LMAX>MXINT DIV 4-4100) OR (LMIN<-MXINT DIV 4)
(*5063*)            THEN INDEXJUMP:=FALSE
(*5064*)            ELSE INDEXJUMP:=TRUE;
(*5065*)          INSERTIC(SWITCHIX);
(*5066*)          GENSWITCH(FSTPTR);
(*5067*)          INSERTCHAIN(CHAIN);
(*5068*)        END
(*5069*)      ELSE ERROR(6);
(*5070*)  IF SY = ENDSY THEN
(*5071*)  BEGIN
(*5072*)    RIGHTCHECK; INSYMBOL
(*5073*)  END ELSE ERROR(13);
(*5074*)    END (*CASESTATEMENT*) ;
(*5075*) 
$TITLE REPEAT,WHILE STATEMENT
(*5076*)    PROCEDURE REPEATSTATEMENT;
(*5077*)      VAR LADDR: ADDRRANGE;
(*5078*)      BEGIN
(*5079*)        LADDR := IC;
(*5080*)        REPEAT
(*5081*)          STATEMENT(FSYS+(.SEMICOLON,UNTILSY.));
(*5082*)          IF SY IN STATBEGSYS THEN ERROR(14)
(*5083*)        UNTIL NOT (SY IN STATBEGSYS);
(*5084*)        WHILE SY = SEMICOLON DO
(*5085*)          BEGIN INSYMBOL;
(*5086*)            REPEAT STATEMENT(FSYS+(.SEMICOLON,UNTILSY.))
(*5087*)            UNTIL NOT (SY IN STATBEGSYS);
(*5088*)          END;
(*5089*)        IF SY = UNTILSY THEN
(*5090*)           BEGIN
(*5091*)             RIGHTCHECK; INSYMBOL; EXPRESSION(FSYS);
(*5092*)            GENFJMP(LADDR); RESETG;
(*5093*)          END
(*5094*)        ELSE ERROR(53);
(*5095*)      END;
(*5096*) 
(*5097*)    PROCEDURE WHILESTATEMENT;
(*5098*)      VAR LADDR,LCIX:ADDRRANGE;
(*5099*)      BEGIN
(*5100*)        LADDR:=IC;
(*5101*)        EXPRESSION(FSYS+(.DOSY.));
(*5102*)        PREPFJMP(LCIX); RESETG;
(*5103*)        TEST1(DOSY,54);
(*5104*)        STATEMENT(FSYS);
(*5105*)        GENJMP(LADDR);  INSERTIC(LCIX);
(*5106*)      END;
(*5107*) 
$TITLE LOOP STATEMENT
(*5108*)    PROCEDURE LOOPSTATEMENT;
(*5109*)      VAR OLDTOP:DISPRANGE; CHAIN:LOCOFREF; LCIX,LADDR:ADDRRANGE;
(*5110*)          LCP,LCP1:CTP;
(*5111*)     BEGIN
(*5112*)       IF EXTWARN THEN ERROR(291);
(*5113*)       CHAIN:=NIL; OLDTOP:=TOP;
(*5114*)        IF TOP<DISPLIMIT THEN
(*5115*)          BEGIN TOP:=TOP+1; DISPLAY(.TOP.).FNAME:=NIL;
(*5116*)                DISPLAY(.TOP.).OCCUR:=REC;
(*5117*)          END
(*5118*)        ELSE ERROR(250);
(*5119*)        LCP1:=NIL; NEW(LCP,EVENT);
(*5120*)        WITH LCP@ DO
(*5121*)          BEGIN NAME:='EXIT    '; IDTYPE:=NIL; NEXT:=LCP1;
(*5122*)                EVENTJUMP:=NIL; EVENTDEF:=FALSE;
(*5123*)          END;
(*5124*)        ENTERID(LCP); LCP1:=LCP;
(*5125*)        IF SY=UNTILSY THEN
(*5126*)          BEGIN
(*5127*)            REPEAT INSYMBOL;
(*5128*)              IF SY<>IDENT THEN
(*5129*)                BEGIN ERROR(2); SKIP(FSYS+(.COMMA,COLON.)); END
(*5130*)              ELSE
(*5131*)                BEGIN NEW(LCP,EVENT);
(*5132*)                  WITH LCP@ DO
(*5133*)                    BEGIN NAME:=ID; NEXT:=LCP1; IDTYPE:=NIL;
(*5134*)                          EVENTJUMP:=NIL; EVENTDEF:=FALSE;
(*5135*)                    END;
(*5136*)                  ENTERID(LCP); LCP1:=LCP; INSYMBOL;
(*5137*)                END;
(*5138*)            UNTIL SY<>COMMA;
(*5139*)            IF SY=COLON THEN INSYMBOL ELSE ERROR(5);
(*5140*)          END;
(*5141*)        LADDR:=IC;
(*5142*)        REPEAT STATEMENT(FSYS+(.SEMICOLON,ENDSY,POSTSY.));
(*5143*)          IF SY IN STATBEGSYS THEN ERROR(14);
(*5144*)        UNTIL NOT (SY IN STATBEGSYS);
(*5145*)        WHILE SY=SEMICOLON DO
(*5146*)          BEGIN INSYMBOL;
(*5147*)            REPEAT STATEMENT(FSYS+(.SEMICOLON,ENDSY,POSTSY.));
(*5148*)            UNTIL NOT (SY IN STATBEGSYS);
(*5149*)          END;
(*5150*)        GENJMP(LADDR);
(*5151*)        IF SY=POSTSY THEN
(*5152*)          BEGIN
(*5153*)            REPEAT INSYMBOL;
(*5154*)              IF SY<>IDENT THEN
(*5155*)                BEGIN ERROR(2); SKIP(FSYS+(.COLON.)); END
(*5156*)              ELSE
(*5157*)                BEGIN SEARCHID((.EVENT.),LCP);
(*5158*)                  WITH LCP@ DO
(*5159*)                    IF DISX<>TOP THEN ERROR(280)
(*5160*)                      ELSE IF NAME='EXIT    ' THEN ERROR(281)
(*5161*)                        ELSE IF EVENTDEF THEN ERROR(282)
(*5162*)                          ELSE
(*5163*)                            BEGIN INSERTCHAIN(EVENTJUMP);
(*5164*)                              EVENTJUMP:=NIL; EVENTDEF:=TRUE;
(*5165*)                            END;
(*5166*)                  INSYMBOL;
(*5167*)                  TEST1(COLON,5);
(*5168*)                END;
(*5169*)              REPEAT STATEMENT(FSYS+(.SEMICOLON.));
(*5170*)                IF SY IN STATBEGSYS THEN ERROR(14);
(*5171*)              UNTIL NOT (SY IN STATBEGSYS);
(*5172*)              PREPJMP(LCIX); LINKOCC(CHAIN,LCIX);
(*5173*)            UNTIL SY<>SEMICOLON;
(*5174*)          END;
(*5175*)        IF SY=ENDSY THEN
(*5176*)        BEGIN RIGHTCHECK; INSYMBOL END ELSE ERROR(13);
(*5177*)        WHILE LCP1<>NIL DO
(*5178*)          BEGIN INSERTCHAIN(LCP1@.EVENTJUMP);
(*5179*)            LCP1:=LCP1@.NEXT;
(*5180*)          END;
(*5181*)        INSERTCHAIN(CHAIN);
(*5182*)        TOP:=OLDTOP;
(*5183*)      END;
(*5184*) 
$TITLE FOR STATEMENT
(*5185*)    PROCEDURE CONTROLVARIABLE(VAR LCP:CTP; VAR CVAR1,CVAR2:INTEGER);
(*5186*)      BEGIN INSYMBOL;
(*5187*)        IF SY = IDENT THEN
(*5188*)          BEGIN SEARCHID((.VARS.),LCP);
(*5189*)            WITH LCP@ DO
(*5190*)              IF IDTYPE <> NIL THEN
(*5191*)                IF (IDTYPE@.FORM>SUBRANGE) OR (IDTYPE=REALPTR)
(*5192*)                    OR (IDTYPE@.FORM=PACKDTYPE)
(*5193*)                  THEN ERROR(143)
(*5194*)                  ELSE IF VKIND=DRCT
(*5195*)                    THEN BEGIN CVAR1:=VLEV; CVAR2:=VADDR END
(*5196*)                    ELSE ERROR(155);
(*5197*)            INSYMBOL
(*5198*)          END
(*5199*)        ELSE
(*5200*)          BEGIN ERROR(2); SKIP(FSYS+(.BECOMES,TOSY,DOWNTOSY,DOSY.));
(*5201*)                LCP:=UVARPTR;
(*5202*)          END;
(*5203*)      END;
(*5204*) 
(*5205*)    PROCEDURE FORSTATEMENT;
(*5206*)      VAR LIMITP: ATTRP; LSP: STP; LSY: SYMBOL;
(*5207*)         LCIX: ADDRRANGE;  LCP: CTP;
(*5208*)         LMIN,LMAX: INTEGER; LADDR: ADDRRANGE;
(*5209*)         CVAR1,CVAR2:INTEGER;(*ADDRESS OF CONTROL VARIABLE*)
(*5210*)         COND:INTEGER;
(*5211*)      BEGIN
(*5212*)        CONTROLVARIABLE(LCP,CVAR1,CVAR2);
(*5213*)        IF SY = BECOMES THEN
(*5214*)          BEGIN INSYMBOL; EXPRESSION(FSYS+(.TOSY,DOWNTOSY,DOSY.));
(*5215*)            IF GATTRP@.TYPTR <> NIL THEN
(*5216*)              IF COMPTYPES(LCP@.IDTYPE,GATTRP@.TYPTR) THEN
(*5217*)                BEGIN LOAD(GATTRP,NIL); BASEREGISTER(CVAR1,CVAR2);
(*5218*)                      GENRXP(ZST,GATTRP@.REXPR.RNO,0,RBASE,EFFADRS);
(*5219*)                END
(*5220*)              ELSE ERROR(145);
(*5221*)            RESETG;
(*5222*)          END
(*5223*)        ELSE
(*5224*)          BEGIN ERROR(51); SKIP(FSYS+(.TOSY,DOWNTOSY,DOSY.)) END;
(*5225*)        LSY := SY; ATTRNEW(LIMITP); LIMITP@.TYPTR := NIL;
(*5226*)        IF SY IN (.TOSY,DOWNTOSY.) THEN
(*5227*)          BEGIN
(*5228*)            INSYMBOL; EXPRESSION(FSYS+(.DOSY.));
(*5229*)            IF GATTRP@.TYPTR <> NIL THEN
(*5230*)              IF COMPTYPES(LCP@.IDTYPE,GATTRP@.TYPTR) THEN
(*5231*)                BEGIN COPYATTR(GATTRP,LIMITP);
(*5232*)                  IF LIMITP@.KIND<>CST THEN
(*5233*)                    BEGIN LOAD(LIMITP,NIL); SAVE(LIMITP@.REXPR.RNO); END;
(*5234*)                END
(*5235*)              ELSE ERROR(145)
(*5236*)          END
(*5237*)        ELSE BEGIN ERROR(55); SKIP(FSYS+(.DOSY.)) END;
(*5238*)        TEST1(DOSY,54);
(*5239*)        BASEREGISTER(CVAR1,CVAR2); GENRX(ZL,R0,0,RBASE,EFFADRS);
(*5240*)        LADDR:=IC;
(*5241*)        IF LIMITP@.TYPTR <> NIL THEN
(*5242*)        IF LIMITP@.KIND=CST
(*5243*)          THEN BEGIN MAKECONSTANT(LIMITP@.CVAL);
(*5244*)                     GENRX(ZC,R0,0,0,0);
(*5245*)               END
(*5246*)          ELSE BEGIN BASEREGISTER(LEVEL,LIMITP@.REXPR.ATEMP@.TEMPADRS);
(*5247*)                     GENRX(ZC,R0,0,RBASE,EFFADRS);
(*5248*)               END;
(*5249*)        IF LSY=TOSY THEN COND:=CONDP ELSE COND:=CONDM;
(*5250*)        LCIX:=IC; GENRX(ZBC,COND,0,0,0);
(*5251*)        IF LCP@.IDTYPE <> NIL THEN
(*5252*)          BEGIN
(*5253*)            IF DEBUG THEN     IF LCP@.IDTYPE<>INTPTR THEN
(*5254*)              BEGIN GETBOUNDS(LCP@.IDTYPE,LMIN,LMAX);
(*5255*)                CHECKREGISTER(R0,LMIN,LMAX);
(*5256*)              END;
(*5257*)          END;
(*5258*)        STATEMENT(FSYS);
(*5259*)        BASEREGISTER(CVAR1,CVAR2);
(*5260*)        GENRX(ZL,R0,0,RBASE,EFFADRS);
(*5261*)        IF LSY=TOSY
(*5262*)          THEN BEGIN MAKEINTCONST(1); GENRX(ZA,R0,0,0,0) END
(*5263*)          ELSE GENRR(ZBCTR,R0,0);
(*5264*)        GENRX(ZST,R0,0,RBASE,EFFADRS);
(*5265*)        GENJMP(LADDR); INSERTIC(LCIX);
(*5266*)        ATTRDISP(LIMITP);
(*5267*)      END (*FORSTATEMENT*) ;
(*5268*) 
$TITLE FOR ALL STATEMENT
(*5269*)    PROCEDURE FORALLSTATEMENT;
(*5270*)      VAR LCP:CTP; CVAR1,CVAR2:INTEGER; SETREG:INTEGER;
(*5271*)          TEMP:CMP; LADDR,LCIX:ADDRRANGE;
(*5272*)        BEGIN
(*5273*)          IF EXTWARN THEN ERROR(291);
(*5274*)          CONTROLVARIABLE(LCP,CVAR1,CVAR2);
(*5275*)        IF OP=INOP THEN
(*5276*)          BEGIN INSYMBOL; EXPRESSION(FSYS+(.DOSY.));
(*5277*)            IF GATTRP@.TYPTR<>NIL THEN
(*5278*)              IF GATTRP@.TYPTR@.FORM<>POWER THEN ERROR(130)
(*5279*)              ELSE IF COMPTYPES(LCP@.IDTYPE,GATTRP@.TYPTR@.ELSET)
(*5280*)                THEN BEGIN LOAD(GATTRP,NIL); SETREG:=REALREG(.GATTRP@.REXPR.RNO.); END
(*5281*)                ELSE BEGIN ERROR(129); SETREG:=10; END;
(*5282*)          END
(*5283*)        ELSE BEGIN ERROR(60); SKIP(FSYS+(.DOSY.)); SETREG:=10; END;
(*5284*)        IF SY=DOSY THEN INSYMBOL ELSE ERROR(54);
(*5285*)        RESETG; GENRR(ZXR,R0,R0); LADDR:=IC; GENRR(ZLTR,SETREG,SETREG);
(*5286*)        LCIX:=IC; GENRX(ZBC,CONDNM,0,0,0); BASEREGISTER(CVAR1,CVAR2);
(*5287*)        GENRX(ZST,R0,0,RBASE,EFFADRS); GETTEMP(8,TEMP);
(*5288*)        BASEREGISTER(LEVEL,TEMP@.TEMPADRS);
(*5289*)        GENRX(ZSTM,SETREG,SETREG+1,RBASE,EFFADRS);
(*5290*)        STATEMENT(FSYS);
(*5291*)        BASEREGISTER(CVAR1,CVAR2); GENRX(ZL,R0,0,RBASE,EFFADRS);
(*5292*)        BASEREGISTER(LEVEL,TEMP@.TEMPADRS); GENRX(ZLM,SETREG,SETREG+1,RBASE,EFFADRS);
(*5293*)        INSERTIC(LCIX); MAKEINTCONST(1);
(*5294*)        GENRX(ZA,R0,0,0,0); MAKEINTCONST(64); GENRX(ZCL,R0,0,0,0);
(*5295*)        GENRX(ZBC,CONDNM,0,PBASE1,IC+12); GENRX(ZSLDL,SETREG,0,0,1);
(*5296*)        GENJMP(LADDR); DELETETEMP(TEMP);
(*5297*)      END;
(*5298*) 
$TITLE  WITH - STATEMENT
(*5299*)    PROCEDURE WITHSTATEMENT;
(*5300*)      VAR LCP:CTP; OLDTOP:DISPRANGE; OLDLEVEL:LEVRANGE;
(*5301*)          TEMP:CMP;
(*5302*)      BEGIN OLDTOP:=TOP; OLDLEVEL:=DISPLEVEL;
(*5303*)        REPEAT INSYMBOL;
(*5304*)          IF SY = IDENT THEN
(*5305*)            BEGIN SEARCHID((.VARS,FIELD.),LCP); INSYMBOL END
(*5306*)          ELSE BEGIN ERROR(2); LCP := UVARPTR END;
(*5307*)          SELECTOR(FSYS+(.COMMA,DOSY.),LCP);
(*5308*)          IF GATTRP@.TYPTR <> NIL THEN
(*5309*)            IF GATTRP@.TYPTR@.FORM = RECORDS THEN
(*5310*)              IF TOP < DISPLIMIT THEN
(*5311*)                BEGIN TOP := TOP + 1;
(*5312*)                  WITH DISPLAY(.TOP.), GATTRP@ DO
(*5313*)                    BEGIN FNAME:=TYPTR@.FIELDS; OCCUR:=REC;
(*5314*)                      IF ((ACCESS=INDIRECT)
(*5315*)                      OR (VARKIND = INDRCT)) AND
(*5316*)                      (DISPLEVEL>=5) AND
(*5317*)                      ((LEVEL<5) OR ((LEVEL=5)AND(DISPLEVEL=6)))
(*5318*)                      THEN
(*5319*)                      BEGIN
(*5320*)                       DADRS:=0; DISPKIND:=DRCT;
(*5321*)                       IF DISPLEVEL=6 THEN
(*5322*)                       BEGIN
(*5323*)                        REG6USED:=TRUE
(*5324*)                       END ELSE
(*5325*)                      BEGIN  REG5USED:=TRUE; END;
(*5326*)                          DLEVEL:=DISPLEVEL; LOADADDRESS(GATTRP,NIL);
(*5327*)                          GENRR(ZLR,DISPLEVEL,REALREG(.GATTRP@.REXPR.RNO.));
(*5328*)                        DISPLEVEL:=DISPLEVEL-1;
(*5329*)                        END
(*5330*)                      ELSE IF ACCESS=DIRECT THEN
(*5331*)                        BEGIN DADRS:=VADRS; DISPKIND:=VARKIND;
(*5332*)                          IF VARKIND=DRCT THEN DLEVEL:=VLEVEL
(*5333*)                                          ELSE BEGIN DBASEL:=BASELEV; DBASEA:=BASEADD; END;
(*5334*)                        END
(*5335*)                      ELSE BEGIN DADRS:=0; DISPKIND:=INDRCT;
(*5336*)                             LOADADDRESS(GATTRP,NIL);
(*5337*)                             GETTEMP(4,TEMP);
(*5338*)                             BASEREGISTER(LEVEL,TEMP@.TEMPADRS); GENRXP(ZST,REXPR.RNO,0,RBASE,EFFADRS);
(*5339*)                             DBASEL:=LEVEL; DBASEA:=TEMP@.TEMPADRS;
(*5340*)                           END;
(*5341*)                      RESETG;
(*5342*)                    END
(*5343*)                END
(*5344*)              ELSE ERROR(250)
(*5345*)            ELSE ERROR(140);
(*5346*)        UNTIL SY<>COMMA;
(*5347*)        TEST1(DOSY,54);
(*5348*)        STATEMENT(FSYS);
(*5349*)        TOP:=OLDTOP; DISPLEVEL:=OLDLEVEL;
(*5350*)      END (*WITHSTATEMENT*) ;
(*5351*) 
$TITLE  STATEMENT - (BODY)
(*5352*)   BEGIN (*STATEMENT*)
(*5353*)    IF SY = INTCONST THEN (*LABEL*)
(*5354*)     BEGIN
(*5355*)      LLP := FSTLABP;
(*5356*)      WHILE LLP <> FLABP DO
(*5357*)       WITH LLP@ DO
(*5358*)        IF LABVAL = IVAL THEN
(*5359*)         BEGIN
(*5360*)          IF DEFINED THEN ERROR(165)
(*5361*)          ELSE
(*5362*)           BEGIN INSERTCHAIN(FSTOCC);
(*5363*)            DEFINED := TRUE; LABADDR := IC;
(*5364*)            IF LCNT<>0 THEN       (*LONG JUMP*)
(*5365*)              BEGIN
(*5366*)                GENRX(ZBC,15,0,PBASE1,IC+10);
(*5367*)                PROCADDRESS(.LCNT.):=PROGCOUNT+IC;
(*5368*)                GENRR(ZLR,0,LEVEL);
(*5369*)                GENRX(ZBAL,BASEWORK,0,1,ENTRYLONGJUMP);
(*5370*)              END;
(*5371*)           END;
(*5372*)          GOTO 1
(*5373*)         END
(*5374*)        ELSE LLP := NEXTLAB;
(*5375*)      ERROR(167);
(*5376*)   1: INSYMBOL;
(*5377*)      TEST1(COLON,5);
(*5378*)     END;
(*5379*)    IF NOT (SY IN FSYS+(.IDENT.)) THEN
(*5380*)     BEGIN ERROR(6); SKIP(FSYS) END;
(*5381*)    IF SY IN STATBEGSYS+(.IDENT.) THEN
(*5382*)     BEGIN
(*5383*)      CASE SY OF
(*5384*)       IDENT:    BEGIN SEARCHID((.VARS,FIELD,FUNC,PROC,EVENT.),LCP); INSYMBOL;
(*5385*)                   CASE LCP@.KLASS OF
(*5386*)                     PROC:  CALL(FSYS,LCP);
(*5387*)                     VARS,FIELD,FUNC:  ASSIGNMENT(LCP);
(*5388*)                     EVENT: BEGIN PREPJMP(LCIX); LINKOCC(LCP@.EVENTJUMP,LCIX); END
(*5389*)                   END;
(*5390*)                 END;
(*5391*)       BEGINSY:  BEGIN
(*5392*)                  LEFTCHECK;INSYMBOL;
(*5393*)                  COMPOUNDSTATEMENT(FSYS+(.SEMICOLON,ENDSY.));
(*5394*)                  INSYMBOL;
(*5395*)                 END;
(*5396*)       GOTOSY:   BEGIN INSYMBOL; GOTOSTATEMENT END;
(*5397*)       IFSY:     BEGIN INSYMBOL; IFSTATEMENT END;
(*5398*)       CASESY:   BEGIN LEFTCHECK;INSYMBOL;CASESTATEMENT END;
(*5399*)       WHILESY:  BEGIN INSYMBOL; WHILESTATEMENT END;
(*5400*)       REPEATSY: BEGIN LEFTCHECK;INSYMBOL;REPEATSTATEMENT END;
(*5401*)       LOOPSY:   BEGIN LEFTCHECK;INSYMBOL; LOOPSTATEMENT END;
(*5402*)       FORSY:    FORSTATEMENT;
(*5403*)       FORALLSY: FORALLSTATEMENT;
(*5404*)       WITHSY:   WITHSTATEMENT
(*5405*)      END;
(*5406*)      TEST2(FSYS,6,(..));
(*5407*)     END;
(*5408*)   END (*STATEMENT*) ;
(*5409*) 
$TITLE  COMPOUNDSTATEMENT
(*5410*)    PROCEDURE COMPOUNDSTATEMENT;
(*5411*)      BEGIN
(*5412*)        REPEAT
(*5413*)          STATEMENT(FSYS);
(*5414*)          IF SY IN STATBEGSYS THEN ERROR(14);
(*5415*)        UNTIL NOT (SY IN STATBEGSYS);
(*5416*)        WHILE SY = SEMICOLON DO
(*5417*)          BEGIN INSYMBOL;
(*5418*)            REPEAT
(*5419*)              STATEMENT(FSYS);
(*5420*)              IF SY IN STATBEGSYS THEN ERROR(14);
(*5421*)            UNTIL NOT (SY IN STATBEGSYS)
(*5422*)          END;
(*5423*)       IF SY=ENDSY THEN
(*5424*)       RIGHTCHECK ELSE ERROR(13);
(*5425*)      END;
(*5426*) 
$TITLE  BODYINIT,CLOSECODEGEN
(*5427*)    PROCEDURE BODYINITIALIZE;
(*5428*)      VAR R:REGNO;
(*5429*)      I : INTEGER;
(*5430*)      BEGIN
(*5431*)        DP := FALSE;
(*5432*)        REG5USED:=FALSE; REG6USED:=FALSE;
(*5433*)        DISPLEVEL:=6;
(*5434*)        FOR I:=0 TO NCODESEGS-1 DO CODEPTR(.I.):=NIL;
(*5435*)        FOR R:=R10 TO F6 DO
(*5436*)          REGISTER(.R.).USED:=FALSE;
(*5437*)        ATTRHEAD:=NIL;
(*5438*)        ATTRNEW(GATTRP);
(*5439*)        WITH GATTRP@ DO
(*5440*)          BEGIN TYPTR:=NIL; KIND:=CST; END;
(*5441*)        FREETEMP:=NIL;
(*5442*)        IC:=4; STACKTOP:=0;
(*5443*)        EXTENDEDADDRESS:=FALSE;
(*5444*)        CONSTTOP:=NIL; STACKSIZE:=NIL;
(*5445*)        GENRR(ZLR,LEVEL,0);
(*5446*)        IF PMD THEN GENRX(ZBC,0,0,0,0);
(*5447*)      END;
(*5448*) 
(*5449*)    PROCEDURE CLOSECODEGEN;
(*5450*)      VAR I,A,B,X,CODEEND,OP,A1,A2,Y,Z:INTEGER; P:CTAILP;
(*5451*)         LOCODEPTR : CODESPTR;
(*5452*) 
(*5453*)      PROCEDURE ALIGNCONST(X:INTEGER);
(*5454*)        VAR X1,X2:INTEGER;
(*5455*)        BEGIN HALFWORD(X,X1,X2);
(*5456*)              IF IC >= 4096*(7-LEVEL)-2 THEN
(*5457*)              BEGIN ERROR(253); IC:=0 END;
(*5458*)              MAKECODE(IC,X1); MAKECODE(IC+2,X2);
(*5459*)              IC:=IC+4;
(*5460*)        END;
(*5461*) 
$TITLE POST MORTEM DUMP (PMDINFO)
(*5462*)PROCEDURE PMDINFO ( FCP : CTP);
(*5463*)   VAR I ,DISPT : INTEGER;
(*5464*)       STCNVRT : RECORD
(*5465*)                   CASE X : BOOLEAN OF
(*5466*)                     TRUE : (NME : ALFA);
(*5467*)                      FALSE : (A1,A2:INTEGER);
(*5468*)                   END;
(*5469*)BEGIN (* PMDINFO *)
(*5470*)  IF FCP <> NIL THEN
(*5471*)   WITH FCP@ DO
(*5472*)   BEGIN
(*5473*)     PMDINFO(LLINK);
(*5474*)     IF KLASS = VARS THEN
(*5475*)       IF IDTYPE <> NIL THEN
(*5476*)         IF ((IDTYPE@.FORM <= POINTER) AND
(*5477*)            (IDTYPE@.FORM <> PACKDTYPE))  OR
(*5478*)             COMPTYPES(IDTYPE,ALFAPTR) THEN
(*5479*)         BEGIN
(*5480*)           IF IDTYPE@.FORM = POINTER THEN I := 0
(*5481*)            ELSE
(*5482*)             IF COMPTYPES(IDTYPE,INTPTR) THEN I:= 2
(*5483*)              ELSE
(*5484*)               IF COMPTYPES(IDTYPE,REALPTR) THEN I:=4
(*5485*)               ELSE
(*5486*)                IF COMPTYPES(IDTYPE,CHARPTR) THEN I:=6
(*5487*)                ELSE
(*5488*)                 IF COMPTYPES(IDTYPE,BOOLPTR) THEN I:=8
(*5489*)                  ELSE IF COMPTYPES(IDTYPE,ALFAPTR) THEN I:=10
(*5490*)                     ELSE I:=12;
(*5491*)           STCNVRT.NME := NAME;
(*5492*)           IF VKIND = INDRCT THEN
(*5493*)           BEGIN
(*5494*)             I := I + 1;  DISPT := PARADDR;
(*5495*)           END ELSE DISPT := VADDR;
(*5496*)       DATA1(16777216*I+DISPT);                                                 
(*5497*)           DATA1(STCNVRT.A1);
(*5498*)           DATA1(STCNVRT.A2);
(*5499*)           A:=A+12;
(*5500*)        END;
(*5501*)        PMDINFO(RLINK);
(*5502*)      END
(*5503*)END; (* PMDINFO *)
(*5504*) 
(*5505*)      BEGIN
(*5506*)        ALIGNMENT(LC,8);
(*5507*)        GENRX(ZBC,15,0,1,ENTRYRET+LEVEL*8);
(*5508*)        CODEEND:=IC;
(*5509*)        IF IC MOD 4<>0 THEN
(*5510*)        BEGIN MAKECODE(IC,0); IC:=IC+2 END;
(*5511*)        WHILE CONSTTOP<>NIL DO
(*5512*)          WITH CONSTTOP@.SAVECONST DO
(*5513*)            BEGIN CASE CKIND OF
(*5514*)              INT: BEGIN INSERTCHAIN(CONSTTOP@.CCHAIN); ALIGNCONST(IVAL); END;
(*5515*)              REEL,PSET:
(*5516*)                   BEGIN
(*5517*)                     IF IC MOD 8<>0 THEN ALIGNCONST(0);
(*5518*)                     INSERTCHAIN(CONSTTOP@.CCHAIN);
(*5519*)                     SETVALUE(PVAL,A1,A2);
(*5520*)                     ALIGNCONST(A1); ALIGNCONST(A2);
(*5521*)                   END;
(*5522*)              STRG:BEGIN P:=VALP; INSERTCHAIN(CONSTTOP@.CCHAIN);   (*BOUNDARY CHECK (8*N) IS*)
(*5523*)                     WHILE P<>NIL DO WITH P@ DO                    (*NOT NECESSARY. THE ONLY USE*)
(*5524*)                       BEGIN ALIGNCONST(STFR); P:=NXTCSP; END;     (*OF STRUCTURED CONSTANT IS*)
(*5525*)                   END                                             (*ASSIGNMENT AS A WHOLE*)
(*5526*)              END;
(*5527*)              CONSTTOP:=CONSTTOP@.NEXTCONST;
(*5528*)            END;
(*5529*)        IF PMD THEN
(*5530*)        MAKECODE(8,IC)  ELSE
(*5531*)        IF IC MOD 8<>0 THEN ALIGNCONST(0);
(*5532*)           HALFWORD(LC,A1,A2);
(*5533*)           IF EXTENDEDADDRESS THEN A1:=A1+4*256*LEVEL;
(*5534*)           MAKECODE(0,A1); MAKECODE(2,A2);
(*5535*)           IF REG6USED THEN
(*5536*)           BEGIN
(*5537*)              IF IC>4096*(6-LEVEL) THEN ERROR(253)
(*5538*)           END ELSE
(*5539*)           IF REG5USED THEN
(*5540*)           BEGIN
(*5541*)             IF IC >4096*(5-LEVEL) THEN ERROR(253)
(*5542*)           END;
(*5543*)           X:=0; Y:=0; LOCODEPTR:= CODEPTR(.0.);
(*5544*)          FOR A := 0 TO (IC DIV 4) -1 DO
(*5545*)          BEGIN
(*5546*)           $PASOBJ1@(.OBPOINTER.):=LOCODEPTR@.FULLWORDS(.X.);
(*5547*)           X:=X+1; OBPOINTER := OBPOINTER+1;
(*5548*)           IF X = CODEBLCK+1 THEN
(*5549*)           BEGIN
(*5550*)             X:=0; Y := Y + 1;
(*5551*)             LOCODEPTR:=CODEPTR(.Y.);
(*5552*)           END;
(*5553*)           IF OBPOINTER=OBJLENGTH THEN
(*5554*)           BEGIN PUT($PASOBJ1); OBPOINTER:=0 END;
(*5555*)         END;
(*5556*)         IF PMD THEN
(*5557*)         BEGIN
(*5558*)           A:=0;
(*5559*)           PMDINFO(DISPLAY(.LEVEL.).FNAME);
(*5560*)           DATA1(0);
(*5561*)           IF ( A+IC + 4) MOD 8 <> 0 THEN
(*5562*)           BEGIN A:=A+4; DATA1(0) END;
(*5563*)           PROGCOUNT:=PROGCOUNT+IC+A+4
(*5564*)           END ELSE PROGCOUNT:=PROGCOUNT+IC;
(*5565*)        IF PRINTCODE THEN
(*5566*)          BEGIN I:=0;
(*5567*)               ENDOFLINE;
(*5568*)            WHILE I<CODEEND DO
(*5569*)                     BEGIN WRITE(' ');WRITEHEX(I);WRITE(' ');                   
(*5570*)                     X:=GETCODE(I); WRITEHEX(X);                                
(*5571*)                OP:=X DIV 256; X:=X MOD 256;
(*5572*)                IF OP<64 THEN
(*5573*)           BEGIN WRITE(' ':14,MNEMONIC(.OP.),'  ',X DIV 16:1,                   
(*5574*)                              ',', X MOD 16:1); I:=I+2;
(*5575*)                  END
(*5576*)                ELSE IF OP<192 THEN
(*5577*)                    BEGIN  Y:=GETCODE(I+2);WRITEHEX(Y);
(*5578*)                WRITE(' ':8,MNEMONIC(.OP.),'  ',X DIV 16:1, ',',                
(*5579*)                          Y MOD 4096:1, '(', X MOD 16:1,
(*5580*)                          ',', Y DIV 4096:1, ')'); I:=I+4;
(*5581*)                  END
(*5582*)                ELSE
(*5583*)                  BEGIN
(*5584*)                 Y := GETCODE(I+2);WRITEHEX(Y);                                 
(*5585*)                     Z:=GETCODE(I+4); WRITEHEX(Z);
(*5586*)                    WRITE('  ', MNEMONIC(.OP.), '  ',
(*5587*)                          Y MOD 4096:1, '(', X+1:1, ',', Y DIV 4096:1,
(*5588*)                          '),', Z MOD 4096:1, '(', Z DIV 4096:1, ')'); I:=I+6;
(*5589*)                  END;
(*5590*)                WRITELN;
(*5591*)               ENDOFLINE;
(*5592*)              END;
(*5593*)            IF I MOD 4<>0 THEN
(*5594*)              BEGIN WRITE(' '); WRITEHEX(I);
(*5595*)             WRITE('  '); WRITEHEX(GETCODE(I));
(*5596*)                    WRITELN; I:=I+2;
(*5597*)             ENDOFLINE;
(*5598*)              END;
(*5599*)            WHILE I<IC DO
(*5600*)                 BEGIN WRITE(' ');  WRITEHEX(I); WRITE('  ');
(*5601*)                   WRITEHEX(GETCODE(I)); WRITE(' ');
(*5602*)                   WRITEHEX(GETCODE(I+2));
(*5603*)                    WRITELN; I:=I+4;
(*5604*)                  ENDOFLINE;
(*5605*)              END;
(*5606*)         END;
(*5607*)      END;
(*5608*) 
$TITLE OPENFILES,OPENEXT,OPENLOC,OPEN1
(*5609*)    PROCEDURE OPENFILES(FCP:CTP);
(*5610*)      VAR EXTFILE:BOOLEAN; CLSP,EXFILP:FILEP;
(*5611*) 
(*5612*)      PROCEDURE OPENEXT(FSIZE,FADDR:ADDRRANGE);
(*5613*)        VAR WNAME: RECORD CASE FLAG:BOOLEAN OF
(*5614*)                     FALSE: (STR: PACKED ARRAY(.1..8.) OF CHAR);
(*5615*)                     TRUE:  (INT: ARRAY(.1..2.) OF INTEGER)
(*5616*)                   END;
(*5617*)        BEGIN
(*5618*)          GENRR(ZLR,BASEWORK,LEVEL); MAKEINTCONST(FADDR);
(*5619*)          GENRX(ZA,BASEWORK,0,0,0);
(*5620*)          LOADINTCONST(R0,FSIZE);
(*5621*)          GENRX(ZST,R0,0,BASEWORK,0);
(*5622*)          WNAME.STR:=FCP@.NAME; MAKEINTCONST(WNAME.INT(.1.));
(*5623*)          GENRX(ZL,R0,0,0,0); GENRX(ZST,R0,0,BASEWORK,4);
(*5624*)          MAKEINTCONST(WNAME.INT(.2.));
(*5625*)          GENRX(ZL,R0,0,0,0); GENRX(ZST,R0,0,BASEWORK,8);
(*5626*)          GENRR(ZLR,R0,BASEWORK); GENRX(ZBAL,BASEWORK,0,1,ENTOPEXT);
(*5627*)        END;
(*5628*) 
(*5629*)     PROCEDURE OPENLOC(FSIZE,FADDR:ADDRRANGE);
(*5630*)        BEGIN
(*5631*)          GENRR(ZLR,BASEWORK,LEVEL); MAKEINTCONST(FADDR);
(*5632*)          GENRX(ZA,BASEWORK,0,0,0);
(*5633*)          LOADINTCONST(R0,FSIZE);
(*5634*)          GENRX(ZST,R0,0,BASEWORK,0);
(*5635*)          GENRX(ZMVI,R0,0,BASEWORK,4);
(*5636*)          GENRR(ZLR,R0,BASEWORK);
(*5637*)          GENRX(ZBAL,BASEWORK,0,1,ENTOPEXT);
(*5638*)        END;
(*5639*) 
(*5640*)      PROCEDURE OPEN1(FSP:STP; FADDR:ADDRRANGE);
(*5641*)        VAR I,LMIN,LMAX,S:INTEGER; LCP:CTP;
(*5642*)        BEGIN
(*5643*)          IF FSP<>NIL THEN
(*5644*)            WITH FSP@ DO
(*5645*)              IF FORM IN (.RECORDS,ARRAYS,FILES.) THEN
(*5646*)                CASE FORM OF
(*5647*)         RECORDS: BEGIN LCP:=FSTFLD;
(*5648*)                    WHILE LCP<>NIL DO
(*5649*)                      WITH LCP@ DO
(*5650*)                        BEGIN OPEN1(IDTYPE,FADDR+FLDADDR);
(*5651*)                              LCP:=NEXT;
(*5652*)                        END;
(*5653*)                  END;
(*5654*)         ARRAYS:  IF INXTYPE<>NIL THEN
(*5655*)                    BEGIN GETBOUNDS(INXTYPE,LMIN,LMAX);
(*5656*)                      FOR I:=0 TO LMAX-LMIN DO
(*5657*)                        OPEN1(AELTYPE,FADDR+AELLENG*I);
(*5658*)                    END;
(*5659*)         FILES:   BEGIN
(*5660*)                    IF TEXTFILE
(*5661*)                      THEN S:=-1
(*5662*)                      ELSE S:=SIZE.WBLENGTH-8;
(*5663*)                        IF EXTFILE THEN
(*5664*)                             OPENEXT(S,FADDR)
(*5665*)                        ELSE
(*5666*)                       OPENLOC(S,FADDR);
(*5667*)                            NEW(CLSP);
(*5668*)                            WITH CLSP@ DO
(*5669*)                              BEGIN NXTP:=LOCFILP; ADDR:=FADDR; END;
(*5670*)                            LOCFILP:=CLSP;
(*5671*)                  END
(*5672*)                END;
(*5673*)        END;
(*5674*) 
(*5675*)      BEGIN
(*5676*)        IF FCP<>NIL THEN
(*5677*)          WITH FCP@ DO
(*5678*)            BEGIN OPENFILES(LLINK); OPENFILES(RLINK);
(*5679*)              IF (KLASS=VARS) AND (VKIND=DRCT) THEN
(*5680*)                IF IDTYPE<>NIL THEN
(*5681*)                  IF IDTYPE@.FTYPE THEN
(*5682*)                    BEGIN EXTFILE:=FALSE;
(*5683*)                      IF IDTYPE@.FORM=FILES THEN
(*5684*)                        BEGIN EXFILP:=FEXFILP;
(*5685*)                          LOOP IF EXFILP=NIL THEN EXIT;
(*5686*)                            WITH EXFILP@ DO
(*5687*)                              BEGIN
(*5688*)                                IF FILENAME=NAME THEN
(*5689*)                                  BEGIN EXTFILE:=TRUE; DECLARED:=TRUE; ADDR:=VADDR; EXIT; END;
(*5690*)                                EXFILP:=NXTP;
(*5691*)                              END;
(*5692*)                          END;
(*5693*)                        END;
(*5694*)                      IF (FCP<>INPUTPTR) AND (FCP<>OUTPUTPTR) THEN
(*5695*)                        OPEN1(IDTYPE,VADDR);
(*5696*)                    END;
(*5697*)            END;
(*5698*)      END;
(*5699*) 
$TITLE  FILECHECK,LABELCHECK
(*5700*)    PROCEDURE FILECHECK;
(*5701*)      VAR FP:FILEP; FIRST:BOOLEAN; LCHCNT:INTEGER;
(*5702*)      BEGIN FP:=FEXFILP; FIRST:=TRUE;
(*5703*)        WHILE FP<>NIL DO
(*5704*)          WITH FP@ DO
(*5705*)            BEGIN
(*5706*)              IF NOT DECLARED THEN
(*5707*)                BEGIN
(*5708*)                  IF FIRST THEN
(*5709*)                    BEGIN ERROR(172); LCHCNT:=CHCNT; ENDOFLINE;
(*5710*)                                             FIRST:=FALSE;
(*5711*)                    END;
(*5712*)                            ENDOFLINE;
(*5713*)                  WRITELN('  FILE-ID ', FILENAME);
(*5714*)                END;
(*5715*)              FP:=NXTP;
(*5716*)            END;
(*5717*)      END;
(*5718*) 
(*5719*)    PROCEDURE LABELCHECK;
(*5720*)      VAR FIRST:BOOLEAN; LCHCNT:INTEGER;
(*5721*)      BEGIN
(*5722*)        FIRST := TRUE;
(*5723*)        WHILE FSTLABP <> FLABP DO
(*5724*)          WITH FSTLABP@ DO
(*5725*)            BEGIN
(*5726*)              IF NOT DEFINED THEN
(*5727*)                IF (LCNT<>0) OR (FSTOCC<>NIL) THEN
(*5728*)                  BEGIN
(*5729*)                    IF FIRST THEN
(*5730*)                      BEGIN ERROR(168); LCHCNT := CHCNT; ENDOFLINE;
(*5731*)                                FIRST := FALSE;
(*5732*)                      END;
(*5733*)                          ENDOFLINE;
(*5734*)                    WRITELN('  LABEL ',LABVAL)
(*5735*)                  END;
(*5736*)              FSTLABP := NEXTLAB
(*5737*)            END;
(*5738*)      END;
(*5739*) 
$TITLE  CLOSE FILES
(*5740*)    PROCEDURE CLOSEFILES;
(*5741*) 
(*5742*)   PROCEDURE CLOSEALLFILES(FP:FILEP);
(*5743*)   BEGIN
(*5744*)     GENRR(ZLR,R0,LEVEL);  MAKEINTCONST(FP@.ADDR);
(*5745*)     GENRX(ZA,R0,0,0,0); GENRX(ZBAL,BASEWORK,0,1,ENTCLEXT);
(*5746*)   END;
(*5747*) 
(*5748*) 
(*5749*)      BEGIN
(*5750*)    IF LEVEL = 1 THEN
(*5751*)        IF INPUTPTR<>NIL THEN
(*5752*)          BEGIN GENRX(ZLA,R0,0,1,LCSTART);
(*5753*)            GENRX(ZBAL,BASEWORK,0,1,ENTCLEXT);
(*5754*)          END;
(*5755*)        WHILE LOCFILP<>NIL DO
(*5756*)       BEGIN CLOSEALLFILES(LOCFILP);LOCFILP:=LOCFILP@.NXTP END;
(*5757*)      END;
(*5758*) 
$TITLE  BLOCK,BODY - (BODY)
(*5759*)  BEGIN (*BODY*)
(*5760*)    BODYINITIALIZE;
(*5761*)    OPENFILES(DISPLAY(.TOP.).FNAME);
(*5762*)    IF LEVEL=1 THEN
(*5763*)     BEGIN FILECHECK;
(*5764*)        IF INPUTPTR<>NIL THEN
(*5765*)          BEGIN GENRX(ZLA,15,0,1,LCSTART);
(*5766*)            GENRX(ZBAL,BASEWORK,0,1,OPENINPUT);
(*5767*)          END;
(*5768*)      END;
(*5769*)    IF SY =  BEGINSY THEN  INSYMBOL;
(*5770*)    COMPOUNDSTATEMENT(FSYS+(.SEMICOLON,ENDSY.));
(*5771*)    IF LEVEL > 1 THEN PROCLEV:=CHR(ORD('A')+LEVEL-2);
(*5772*)    INSYMBOL;
(*5773*)    LABELCHECK;
(*5774*)    CLOSEFILES;
(*5775*)    CLOSECODEGEN;
(*5776*)  END (*BODY*);
(*5777*) 
(*5778*) BEGIN (*BLOCK*)
(*5779*)   FLABP:=FSTLABP; FWPROCS:=NIL;
(*5780*)   REPEAT
(*5781*)     IF SY=LABELSY THEN LABELDECLARATION;
(*5782*)     IF SY = CONSTSY THEN
(*5783*)       BEGIN INSYMBOL; CONSTDECLARATION END;
(*5784*)     IF SY = TYPESY THEN
(*5785*)       BEGIN INSYMBOL; TYPEDECLARATION END;
(*5786*)     IF SY = VARSY THEN
(*5787*)       BEGIN INSYMBOL; VARDECLARATION END;
(*5788*)     IF SY=VALUESY THEN
(*5789*)        BEGIN IF EXTWARN THEN ERROR(291); INSYMBOL;
(*5790*)            VARINITIALIZATION END;
(*5791*)     WHILE SY IN (.PROCSY,FUNCTSY.) DO
(*5792*)      BEGIN
(*5793*)       PROCLEV:=CHR(ORD('A')+LEVEL-1);
(*5794*)       LSY :=SY; INSYMBOL;
(*5795*)       PROCDECLARATION(LSY)
(*5796*)      END;
(*5797*)     IF SY <> BEGINSY THEN
(*5798*)       BEGIN ERROR(18); SKIP(FSYS) END
(*5799*)   UNTIL SY IN STATBEGSYS;
(*5800*)   UNDEFINED(FWPROCS,'PROC/FUNC');
(*5801*)IF SY=BEGINSY THEN
(*5802*)BEGIN
(*5803*)  IF LEVEL > 1 THEN PROCLEV:=CHR(ORD('A')+LEVEL-2);
(*5804*)  LEFTCHECK;
(*5805*)  IC := 0; DP := FALSE;
(*5806*)   LOCATION := 0;
(*5807*)END ELSE ERROR(17);
(*5808*)   PROCADDRESS(.FPROCP@.PFCNT.):=PROGCOUNT;
(*5809*)   BODY(FSYS+(.CASESY.));
(*5810*)   IF SY <> FSY THEN
(*5811*)     BEGIN ERROR(6); SKIP(FSYS) END;
(*5812*) END (*BLOCK*) ;
(*5813*) 
(*5814*) 
$TITLE  PROGRAMME
(*5815*) PROCEDURE PROGRAMME(FSYS: SETOFSYS);
(*5816*)   VAR EXFILP:FILEP; LCP:CTP;
(*5817*)   BEGIN
(*5818*)     WITH DISPLAY(.1.) DO BEGIN FNAME:=NIL; OCCUR:=BLCK; END;
(*5819*)    NEW(LCP,VARS);
(*5820*)    WITH LCP@ DO
(*5821*)      BEGIN NAME := 'OUTPUT  '; IDTYPE := TEXTPTR;
(*5822*)        VKIND := INDRCT; NEXT := NIL;
(*5823*)        VLEV:=1;  PARADDR := PTROUTBLCK;
(*5824*)      END;
(*5825*)    ENTERID(LCP);
(*5826*)    IF SY = PROGRAMSY THEN
(*5827*)     BEGIN INSYMBOL;
(*5828*)      IF SY = IDENT THEN
(*5829*)       BEGIN INSYMBOL;
(*5830*)        IF NOT (SY IN (.SEMICOLON,LPARENT.)) THEN
(*5831*)         BEGIN ERROR(7); SKIP(FSYS+(.SEMICOLON,LPARENT.)) END;
(*5832*)        IF SY = LPARENT THEN
(*5833*)         BEGIN
(*5834*)          REPEAT INSYMBOL;
(*5835*)           IF SY = IDENT THEN
(*5836*)            BEGIN
(*5837*)             IF ID = 'INPUT   ' THEN
(*5838*)               BEGIN NEW(INPUTPTR,VARS);
(*5839*)                 WITH INPUTPTR@ DO
(*5840*)                   BEGIN NAME := 'INPUT   '; IDTYPE := TEXTPTR;
(*5841*)                     VKIND := DRCT; NEXT := NIL;
(*5842*)                     VLEV:=1; VADDR:=LC; LC:=LC+TEXTSIZE;
(*5843*)                   END;
(*5844*)                 ENTERID(INPUTPTR);
(*5845*)               END
(*5846*)             ELSE
(*5847*)              IF ID = 'OUTPUT  ' THEN OUTPUTPTR := LCP;
(*5848*)             EXFILP := FEXFILP;
(*5849*)             WHILE EXFILP <> NIL DO
(*5850*)               WITH EXFILP@ DO
(*5851*)                 BEGIN
(*5852*)                   IF FILENAME=ID THEN ERROR(101);
(*5853*)                   EXFILP := NXTP
(*5854*)                 END;
(*5855*)             IF (ID<>'INPUT   ') AND (ID<>'OUTPUT  ') THEN
(*5856*)               BEGIN NEW(EXFILP);
(*5857*)                 WITH EXFILP@ DO
(*5858*)                   BEGIN FILENAME := ID; NXTP := FEXFILP;
(*5859*)                    DECLARED := FALSE;
(*5860*)                   END;
(*5861*)                 FEXFILP := EXFILP
(*5862*)               END;
(*5863*)             INSYMBOL;
(*5864*)            END
(*5865*)           ELSE ERROR(2);
(*5866*)           IF NOT (SY IN (.COMMA,RPARENT.)) THEN
(*5867*)            BEGIN ERROR(6); SKIP(FSYS+(.COMMA,RPARENT.)) END
(*5868*)          UNTIL SY <> COMMA;
(*5869*)          IF SY = RPARENT THEN INSYMBOL ELSE ERROR(4)
(*5870*)         END;
(*5871*)   IF OUTPUTPTR = NIL THEN IF EXTWARN THEN ERROR(291);
(*5872*)        IF SY = SEMICOLON THEN INSYMBOL ELSE ERROR(14)
(*5873*)       END
(*5874*)      ELSE BEGIN ERROR(2); SKIP(FSYS) END
(*5875*)     END
(*5876*)    ELSE BEGIN ERROR(3); SKIP(FSYS) END;
(*5877*)    $PASOBJ2@:='P.MAIN  ';
(*5878*)    PUT($PASOBJ2);
(*5879*)    BLOCK(FSYS,PERIOD,UPRCPTR);
(*5880*)   END (*PROGRAMME*) ;
(*5881*) 
$TITLE STDTYPENTRIES.
(*5882*) PROCEDURE STDTYPENTRIES;
(*5883*)   VAR SP:STP;
(*5884*) BEGIN
(*5885*)  NEW(INTPTR,SCALAR,STANDARD);
(*5886*)  WITH INTPTR@ DO
(*5887*)    BEGIN FTYPE:=FALSE; SIZE.WBLENGTH:=4; SIZE.BOUNDARY:=4; END;
(*5888*)  NEW(REALPTR,SCALAR,STANDARD);
(*5889*)  WITH REALPTR@ DO
(*5890*)    BEGIN FTYPE:=FALSE; SIZE.WBLENGTH:=8; SIZE.BOUNDARY:=8; END;
(*5891*)  NEW(CHARPTR,SCALAR,STANDARD);
(*5892*)  WITH CHARPTR@ DO
(*5893*)    BEGIN FTYPE:=FALSE; SIZE.WBLENGTH:=4; SIZE.BOUNDARY:=4; END;
(*5894*)  NEW(BOOLPTR,SCALAR,DECLARED);
(*5895*)  WITH BOOLPTR@ DO
(*5896*)    BEGIN FTYPE:=FALSE; SIZE.WBLENGTH:=4; SIZE.BOUNDARY:=4; END;
(*5897*)  NEW(NILPTR,POINTER);
(*5898*)  WITH NILPTR@ DO
(*5899*)   BEGIN ELTYPE := NIL; FTYPE := FALSE;
(*5900*)         SIZE.WBLENGTH:=4; SIZE.BOUNDARY:=4;
(*5901*)   END;
(*5902*)  NEW(PACKDINTPTR,PACKDTYPE);
(*5903*)  WITH PACKDINTPTR@ DO
(*5904*)    BEGIN SIZE.WBLENGTH:=1; SIZE.BOUNDARY:=1;
(*5905*)          FTYPE:=FALSE; BASETYPE:=INTPTR;
(*5906*)    END;
(*5907*)  NEW(PACKDCHARPTR,PACKDTYPE);
(*5908*)  WITH PACKDCHARPTR@ DO
(*5909*)    BEGIN SIZE.WBLENGTH:=1; SIZE.BOUNDARY:=1;
(*5910*)          FTYPE:=FALSE; BASETYPE:=CHARPTR;
(*5911*)    END;
(*5912*)  NEW(TEXTPTR,FILES);
(*5913*)  WITH TEXTPTR@ DO
(*5914*)    BEGIN FILTYPE:=PACKDCHARPTR;
(*5915*)          TEXTFILE  :=  TRUE;  FTYPE  :=  TRUE;
(*5916*)          SIZE.WBLENGTH:=TEXTSIZE;  SIZE.BOUNDARY:=4;
(*5917*)    END;
(*5918*)NEW(SP,SUBRANGE);
(*5919*)WITH SP@ DO
(*5920*)BEGIN
(*5921*)  RANGETYPE:=INTPTR;
(*5922*)  FTYPE:=FALSE;
(*5923*)  MIN:=1; MAX:=ALFALENG;
(*5924*)  SIZE.WBLENGTH:=4;
(*5925*)  SIZE.BOUNDARY:=4;
(*5926*)END;
(*5927*) 
(*5928*)NEW(ALFAPTR,ARRAYS);
(*5929*)WITH ALFAPTR@ DO
(*5930*)BEGIN
(*5931*)  AELTYPE:=PACKDCHARPTR;
(*5932*)  INXTYPE:=SP;
(*5933*)  FTYPE:=FALSE; AELLENG:=1;
(*5934*)  SIZE.WBLENGTH:=ALFALENG;
(*5935*)  SIZE.BOUNDARY:=1
(*5936*)END
(*5937*) END (*STDTYPENTRIES*);
(*5938*) 
$TITLE STDNAMENTRIES,TYPENAME,CONSTNAME
(*5939*) PROCEDURE STDNAMENTRIES;
(*5940*)  VAR CP,CP1:CTP; I:INTEGER;
(*5941*) 
(*5942*)  PROCEDURE TYPENAME(S:ALFA; P:STP);
(*5943*)    BEGIN NEW(CP,TYPES);
(*5944*)      WITH CP@ DO
(*5945*)        BEGIN NAME:=S; IDTYPE:=P; END;
(*5946*)      ENTERID(CP);
(*5947*)    END;
(*5948*) 
(*5949*)  PROCEDURE CONSTNAME(S:ALFA; P:STP; V:INTEGER);
(*5950*)    BEGIN NEW(CP,KONST);
(*5951*)      WITH CP@ DO
(*5952*)        BEGIN NAME:=S; IDTYPE:=P; NEXT:=NIL;
(*5953*)              VALUES.CKIND:=INT; VALUES.IVAL:=V;
(*5954*)        END;
(*5955*)      ENTERID(CP);
(*5956*)    END;
(*5957*) 
(*5958*)  BEGIN
(*5959*)    TYPENAME('INTEGER ',INTPTR);  TYPENAME('REAL    ',REALPTR);
(*5960*)    TYPENAME('CHAR    ',CHARPTR); TYPENAME('BOOLEAN ',BOOLPTR);
(*5961*)    TYPENAME('TEXT    ',TEXTPTR);
(*5962*)    TYPENAME('ALFA    ',ALFAPTR);
(*5963*)    CONSTNAME('NIL     ',NILPTR,NILVAL);
(*5964*)    CONSTNAME('MAXINT  ',INTPTR,MXINT);
(*5965*)    CONSTNAME('FALSE   ',BOOLPTR,0); CP1:=CP;
(*5966*)    CONSTNAME('TRUE    ',BOOLPTR,1); CP@.NEXT:=CP1; BOOLPTR@.FCONST:=CP;
(*5967*)    FOR I := 1 TO NRSTDPROC DO
(*5968*)      BEGIN NEW(CP,PROC,STANDARD);                  (*STANDARD PROCEDURES*)
(*5969*)        WITH CP@ DO
(*5970*)          BEGIN NAME := NA(.I.); IDTYPE := NIL;
(*5971*)            NEXT := NIL; KEY := I;
(*5972*)          END;
(*5973*)        ENTERID(CP)
(*5974*)      END;
(*5975*)    FOR I := 1 TO NRSTDFUNC DO                     (*STANDARD FUNCTIONS*)
(*5976*)      BEGIN NEW(CP,FUNC,STANDARD);
(*5977*)        WITH CP@ DO
(*5978*)          BEGIN NAME := NA(.NRSTDPROC+I.); IDTYPE := NIL;
(*5979*)            NEXT := NIL; KEY := I;
(*5980*)          END;
(*5981*)        ENTERID(CP)
(*5982*)      END;
(*5983*)  END (*STDNAMENTRIES*);
(*5984*) 
$TITLE ENTERUNDECL,INITSCALARS
(*5985*) PROCEDURE ENTERUNDECL;
(*5986*)   BEGIN
(*5987*)     NEW(UTYPPTR,TYPES);
(*5988*)     WITH UTYPPTR@ DO
(*5989*)       BEGIN NAME:='        '; IDTYPE:=NIL; END;
(*5990*)     NEW(UCSTPTR,KONST);
(*5991*)     WITH UCSTPTR@ DO
(*5992*)        BEGIN NAME := '        '; IDTYPE := NIL; NEXT := NIL;
(*5993*)    VALUES.IVAL := 0; VALUES.CKIND := INT;
(*5994*)        END;
(*5995*)     NEW(UVARPTR,VARS);
(*5996*)     WITH UVARPTR@ DO
(*5997*)        BEGIN NAME := '        '; IDTYPE := NIL; VKIND := DRCT;
(*5998*)           NEXT := NIL; VLEV := 0; VADDR := 0
(*5999*)        END;
(*6000*)     NEW(UFLDPTR,FIELD);
(*6001*)     WITH UFLDPTR@ DO
(*6002*)        BEGIN NAME := '        '; IDTYPE := NIL; NEXT := NIL;
(*6003*)           FLDADDR := 0
(*6004*)        END;
(*6005*)     NEW(UPRCPTR,PROC,DECLARED,ACTUAL);
(*6006*)     WITH UPRCPTR@ DO
(*6007*)        BEGIN NAME := '        '; IDTYPE := NIL;
(*6008*)          NEXT:=NIL; PFLEV:=0; PFCNT:=1; PARAMS:=NIL;
(*6009*)        END;
(*6010*)     NEW(UFCTPTR,FUNC,DECLARED,ACTUAL);
(*6011*)     WITH UFCTPTR@ DO
(*6012*)        BEGIN NAME := '        '; IDTYPE := NIL; NEXT := NIL;
(*6013*)              PFLEV := 0; PFCNT:=1; PARAMS:=NIL;
(*6014*)        END;
(*6015*)     NEW(UEVENTPTR,EVENT);
(*6016*)     WITH UEVENTPTR@ DO
(*6017*)        BEGIN NAME:='        '; IDTYPE:=NIL; NEXT:=NIL;
(*6018*)              EVENTJUMP:=NIL; EVENTDEF:=FALSE;
(*6019*)        END;
(*6020*)   END (*ENTERUNDECL*) ;
(*6021*) 
(*6022*) PROCEDURE INITSCALARS;
(*6023*)   BEGIN
(*6024*)     CH:=' '; SWEOL:=FALSE; CHCNT:=0; PROGCOUNT:=0;
(*6025*)     LC:=LCSTART;
(*6026*)     PCNT:=1;
(*6027*)     DOTFLG := FALSE;
(*6028*)     EXTWARN:=FALSE;
(*6029*)     PRTERR:=TRUE;
(*6030*)     DEBUG:=TRUE; LISTON:=TRUE; PMD:=TRUE; PRINTCODE:=FALSE;
(*6031*)     INPUTPTR:=NIL; OUTPUTPTR:=NIL;
(*6032*)     FWPTR:=NIL; FSTLABP:=NIL; FEXFILP:=NIL; LOCFILP:=NIL;
(*6033*)     FSTPCRP:=NIL;
(*6034*)     ERRINX:=0; ERRORS:=FALSE;
(*6035*)     INITNUMBER:=0; OBPOINTER:=0;
(*6036*)     SWEOL:=TRUE; LEFT:='-';RIGHT :='-';
(*6037*)     MAXLN := FALSE;
(*6038*)     PAGEE:=1; FOR ZLEV:=1 TO 40 DO TTL(.ZLEV.):=' ';
(*6039*)     ZLEV:=-1; DATE(DDATE); TIME(TTIME); PRINTED:=0;
(*6040*)     PROCLEV:=' '; LINEE:=LINESPERPAGE-1;
(*6041*)     MAXLINE:=MAXCHCNT;
(*6042*)     ERRORTOT := 0;
(*6043*)     DP:=TRUE;
(*6044*)   END;
(*6045*) 
$TITLE INITSETS,SYMBOLS
(*6046*) PROCEDURE INITSETS;
(*6047*)     VAR I : 0..MAXMSGSDIV64;
(*6048*) BEGIN
(*6049*)  CONSTBEGSYS := (.ADDOP,INTCONST,REALCONST,CHARCONST,STRINGCONST,IDENT,LBRACK.);
(*6050*)  SIMPTYPEBEGSYS := (.LPARENT.)+CONSTBEGSYS-(.LBRACK,STRINGCONST.);
(*6051*)  TYPEBEGSYS := (.ARROW,PACKEDSY,ARRAYSY,RECORDSY,SETSY,
(*6052*)         FILESY.)+SIMPTYPEBEGSYS;
(*6053*)  TYPEDELS := (.ARRAYSY,RECORDSY,SETSY,FILESY.);
(*6054*)  BLOCKBEGSYS := (.LABELSY,CONSTSY,TYPESY,VARSY,VALUESY,PROCSY,FUNCTSY,
(*6055*)          BEGINSY.);
(*6056*)  SELECTSYS := (.ARROW,PERIOD,LBRACK.);
(*6057*)  FACBEGSYS := (.INTCONST,REALCONST,CHARCONST,STRINGCONST,IDENT,LPARENT,
(*6058*)         LBRACK,NOTSY.);
(*6059*)  STATBEGSYS := (.BEGINSY,GOTOSY,IFSY,WHILESY,REPEATSY,LOOPSY,FORSY,FORALLSY,WITHSY,
(*6060*)          CASESY.);
(*6061*)     BMASK(.EQOP.):=8; BMASK(.NEOP.):=7; BMASK(.GTOP.):=2;
(*6062*)     BMASK(.LTOP.):=4; BMASK(.GEOP.):=11; BMASK(.LEOP.):=13;
(*6063*)     DUALOP(.EQOP.):=EQOP; DUALOP(.NEOP.):=NEOP; DUALOP(.GTOP.):=LTOP;
(*6064*)     DUALOP(.GEOP.):=LEOP; DUALOP(.LTOP.):=GTOP; DUALOP(.LEOP.):=GEOP;
(*6065*)     FOR I := 0 TO MAXMSGSDIV64 DO
(*6066*)        ERRMSGS(. I .) := (. .);
(*6067*) END (*INITSETS*) ;
(*6068*) 
(*6069*)  PROCEDURE SYMBOLS;
(*6070*)    VAR I:INTEGER; C:CHAR;
(*6071*)    BEGIN
(*6072*)      SSY(.'+'.) := ADDOP; SSY(.'-'.) := ADDOP; SSY(.'*'.) := MULOP;
(*6073*)      SSY(.'/'.):=MULOP; SSY(.')'.):=RPARENT;
(*6074*)      SSY(.'='.):=RELOP; SSY(.','.):=COMMA;
(*6075*)      SSY(.'%'.):=RBRACK; SSY(.'@'.):=ARROW; SSY(.';'.):=SEMICOLON;
(*6076*)      FOR I := 1 TO RESWORDS DO ROP(.I.) := NOOP;
(*6077*)      ROP(.5.) := INOP; ROP(.10.) := IDIV; ROP(.11.) := IMOD;
(*6078*)      ROP(.6.) := OROP; ROP(.13.) := ANDOP;
(*6079*)      FOR I:=64 TO 127 DO SOP(.CHR(I).):=NOOP;
(*6080*)     SSY(.CHR(173).):=LBRACK;                                                   
(*6081*)     SSY(.CHR(189).):=RBRACK;                                                   
(*6082*)     SSY(.'&'.):=MULOP;                                                         
(*6083*)     SSY(.CHR(79).):=ADDOP;                                                     
(*6084*)     SOP(.'&'.):=ANDOP;                                                         
(*6085*)     SOP(.CHR(79).):=OROP;                                                      
(*6086*)      SOP(.'+'.) := PLUS;SOP(.'-'.) := MINUS;SOP(.'*'.) := MUL;SOP(.'/'.) := RDIV;
(*6087*)      SOP(.'='.):=EQOP;
(*6088*)      FOR I:=0 TO 255 DO CHTYPE(.CHR(I).):=SPCHAR;
(*6089*)      FOR C:='A' TO 'I' DO CHTYPE(.C.):=LETTER;
(*6090*)      FOR C:='J' TO 'R' DO CHTYPE(.C.):=LETTER;
(*6091*)      FOR C:='S' TO 'Z' DO CHTYPE(.C.):=LETTER;
(*6092*)      CHTYPE(.'$'.):=LETTER;
(*6093*) CHTYPE(.'_'.) := LETTER;                                                       
(*6094*)      FOR C:='0' TO '9' DO CHTYPE(.C.):=DIGIT;
(*6095*)    END;
(*6096*) 
$TITLE  ENDING PROCEDURES (FINAL)
(*6097*)  PROCEDURE FINAL;
(*6098*)    VAR I:INTEGER;
(*6099*)        CONV:RECORD CASE BOOLEAN OF
(*6100*)               TRUE:  (I1,I2:INTEGER);
(*6101*)               FALSE: (STR:ALFA)
(*6102*)             END;
(*6103*)    BEGIN OBCLEAR;
(*6104*)      FOR I:=1 TO PCNT DO
(*6105*)        BEGIN $PASOBJ1@(.OBPOINTER.):=PROCADDRESS(.I.);
(*6106*)          OBPOINTER:=OBPOINTER+1;
(*6107*)        END;
(*6108*)     OBCLEAR;
(*6109*)     CONV.I1:=INITNUMBER;
(*6110*)     CONV.I2:=PROGCOUNT;
(*6111*)     $PASOBJ2@:=CONV.STR;
(*6112*)     PUT($PASOBJ2);
(*6113*)     CONV.I1:=PCNT;
(*6114*)     CONV.I2:=ORD(ERRORS);
(*6115*)     $PASOBJ2@:=CONV.STR;
(*6116*)     PUT($PASOBJ2);
(*6117*)    END;
(*6118*) 
$TITLE  PASCAL COMPILER - (BODY)
(*6119*)BEGIN
(*6120*) 
(*6121*) INITSCALARS; INITSETS; SYMBOLS;
(*6122*) 
(*6123*) 
(*6124*) LEVEL := 0; TOP := 0;
(*6125*) WITH DISPLAY(.0.) DO
(*6126*)  BEGIN FNAME := NIL; OCCUR := BLCK END;
(*6127*) STDTYPENTRIES; STDNAMENTRIES; ENTERUNDECL;
(*6128*) TOP := 1; LEVEL := 1;
(*6129*) 
(*6130*) 
(*6131*) REWRITE($PASOBJ1); REWRITE($PASOBJ2);
(*6132*) INSYMBOL;
(*6133*) PROGRAMME(BLOCKBEGSYS+STATBEGSYS-(.CASESY.));
(*6134*) 
(*6135*)9999: ENDOFLINE; FINAL;
(*6136*)WRITELN; ENDOFLINE;
(*6137*)WRITELN(' *AAEC PASCAL COMPILATION CONCLUDED *');
(*6138*)ENDOFLINE;
(*6139*)WRITELN; ENDOFLINE;
(*6140*)      IF NOT ERRORS THEN WRITE('0*NO')ELSE WRITE('0*');
(*6141*)      WRITELN(' ERRORS DETECTED IN PASCAL PROGRAM *');
(*6142*)      WRITELN; WRITELN;
(*6143*)      SWEOL:=FALSE;
(*6144*)      IF ERRORTOT  <> 0 THEN
(*6145*)      BEGIN
(*6146*)        WRITELN(' *',ERRORTOT:4,' LINE(S) FLAGGED IN PASCAL PROGRAM*');
(*6147*)        WRITELN;
(*6148*)        IF ERRORS THEN
(*6149*)        BEGIN
(*6150*)         WRITELN; WRITELN(' ERROR LOG : ');
(*6151*)                  WRITELN(' *********** ');
(*6152*)        END;
(*6153*)        WRITELN; RESET($PASMSGS);
(*6154*)        LOCATION := -1;
(*6155*)        FOR ERRORTOT := 0 TO MAXMSGSDIV64 DO
(*6156*)          FOR ZLEV := 0 TO SETMAX DO
(*6157*)          IF ZLEV IN ERRMSGS(.ERRORTOT.) THEN
(*6158*)          BEGIN
(*6159*)            PRINTED := 64*ERRORTOT + ZLEV;
(*6160*)             ERRORS := LOCATION >= PRINTED;
(*6161*)             WHILE (  NOT ERRORS) AND (NOT EOF($PASMSGS)) DO
(*6162*)             BEGIN
(*6163*)               IF SWEOL THEN BEGIN SWEOL:=FALSE; READLN($PASMSGS); END;
(*6164*)               READ($PASMSGS,LOCATION);
(*6165*)               IF LOCATION >= PRINTED THEN ERRORS := TRUE ELSE
(*6166*)                   READLN($PASMSGS);
(*6167*)             END;
(*6168*)             IF LOCATION = PRINTED THEN
(*6169*)             BEGIN
(*6170*)               WRITE(LOCATION:4);
(*6171*)             WHILE NOT EOLN($PASMSGS) DO
(*6172*)             BEGIN
(*6173*)               READ($PASMSGS,CH);
(*6174*)               WRITE(CH);
(*6175*)             END;
(*6176*)             READLN($PASMSGS); WRITELN;
(*6177*)            END ELSE
(*6178*)            BEGIN
(*6179*)              SWEOL :=TRUE;
(*6180*)               WRITELN(PRINTED:4,': MESSAGE NOT IMPLEMENTED');
(*6181*)             END
(*6182*)             END
(*6183*)           END;
(*6184*)(*$L+*)
(*6185*)END .
