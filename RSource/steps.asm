         MACRO
         STEPS
STLINK   DC    A(0)
STDDN    DC    CL8' '
STTIME   DC    A(0)                    TIME TAKEN
STPGM    DC    A(0)                    CORE FOR PGM
STSTAK   DC    A(0)                    CORE FOR STAK
STFREE   DC    A(0)                    CORE NOT USED
STHEAP   DC    A(0)                    CORE FOR HEAP
STEPSL   EQU   *-STEPS
         MEND
