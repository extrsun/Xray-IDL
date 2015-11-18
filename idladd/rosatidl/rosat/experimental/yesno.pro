;*********************************************************************
;+
;*NAME:
;    YESNO     (General IDL Library 01) FEBRUARY 23, 1981
; 
;*CLASS:
;    i/o
;
;*CATEGORY:
;
;*PURPOSE:  
;    To convert an answer to a yes/no question into an integer ( 1 OR 0).
; 
;*CALLING SEQUENCE: 
;    YESNO,IN
; 
;*PARAMETERS:
;    IN   (REQ) (I/O) (0) (S I)
;         The required scalar response to a question. YESNO accepts YES, Y, 1,
;         <return>, NO, N, 0, SD, and STOP. The program will also accept, and
;         return unaltered 1-10. 
;
;*EXAMPLES:
;    To convert from a string response to a question:
;    READ,'WOULD YOU LIKE A RESPONSE?',YN
;    YESNO,YN
;    IF YN EQ 1 THEN BEGIN......
;
;*SYSTEM VARIABLES USED:
;    None
;
;*INTERACTIVE INPUT:
;    None
;
;*SUBROUTINES CALLED:
;    PARCHECK
;
;*FILES USED:
;    None   
;
;*SIDE EFFECTS:
;    The original input variable is converted from whatever input type
;    to an integer.
;
;*RESTRICTIONS:
;    None
;
;*NOTES:
;    None
;
;*PROCEDURE: 
;        YES, Y, 1, y, <CR>      are converted to IN = 1
;        NO, N, 0, n             are converted to IN = 0
;        SD (selective delete)    is converted to IN = 2
;        STOP stops the execution of the program.
;        1-10                    are converted to IN = 1-10
;
;*MODIFICATION HISTORY:
;     Feb. 23, 1981  MEV CURDAF  initial program
;     May  27, 1981  MEV CURDAF  modifications (unspecified)
;     Oct. 22, 1982  MEV CURDAF  support lower case values
;     Dec. 22, 1982  MEV CURDAF  support numbers 0-9 (CR#052)
;     May  31, 1983  MEV CURDAF  to change the phrasing of the reprimand
;     Mar. 21, 1988  CAG GSFC    add VAX RDAF-style prolog, PARCHECK,
;                                and printing of the calling sequence.
;     Apr. 22, 1988  RWT GSFC    add support for #10 as in CURDAF version
;     Jul. 08, 1988  RWT GSFC    fix error when IN = '' (i.e. <CR> )
;     Jul. 17, 1989  RWT GSFC    mod. for SUN IDL
;-
;***********************************************************************
PRO YESNO,IN
;
IF N_PARAMS() EQ 0 THEN BEGIN
   PRINT,'YESNO,IN'
   RETALL & ENDIF
PARCHECK,N_PARAMS(),1,'YESNO'
;
;    find out what input parameter is 
;    type=7 for string, 1-6,8 for number 
;
      TYPE=SIZE(IN)
      IF TYPE(0) EQ 0 THEN TYPE=TYPE(1) ELSE BEGIN
         IN=FIX(IN(0))
         TYPE=4
      END
;
;
      IF TYPE EQ 7 THEN BEGIN     ; if input is string
ENT:     A=FIX(BYTE(IN))
         SA = SIZE(A)
         IF SA(0) EQ 0 THEN A = INTARR(1) + A
         A = (A GT 96)*(A - 32) + (A LT 97)*A
         L=STRLEN(IN)
         INN=-999
         IF (A(0) EQ 89) OR (A(0) EQ 0 ) THEN INN = 1         ; Y or '' 
         IF (A(0) EQ 78) THEN INN = 0                         ; N 
         IF (L EQ 1)*(A(0) GE 48)*(A(0) LE 57) THEN INN = A(0)-48  ; 0-9 
         IF L GT 1 THEN BEGIN                        ; SD or ST 
            IF (L EQ 2)*(A(0) EQ 49)*(A(1) EQ 48) THEN INN = 10  ; 10
            IF (A(0) EQ 83) AND (A(1) EQ 68) THEN INN=2
            IF (A(0) EQ 83) AND (A(1) EQ 84) THEN STOP
            END
         IF INN EQ -999 THEN BEGIN
             PRINT,IN,' is NOT a valid response!',STRING(7B) ;BELL
             IN=' '
             READ,'Please answer with a valid response.',IN
             GOTO,ENT
             END
         IN=INN
      END ELSE BEGIN                ; if input is a number 
         IF (IN LT 0) OR (IN GT 10) THEN BEGIN
            PRINT,IN,' is NOT a valid response!',STRING(7B) ;BELL
            IN=' '
            READ,'Would you please answer with a valid response?',IN
            GOTO,ENT
            END
         IN= FIX(IN)
         END
;
RETURN
END ; YESNO 
