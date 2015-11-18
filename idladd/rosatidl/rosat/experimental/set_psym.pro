PRO SET_PSYM,SYM,SIZ
;
;  Select plotting symbol and size using the USERSYM function.
;
;  INPUT:    SYM = 1   open circle
;                  2   filled circle
;                  3   open square
;                  4   filled square
;                  5   open diamond
;                  6   filled diamond
;                  7   open star
;                  8   filled star
;
;            SIZ = multiplicative factor for symbol size.  Default = 2
;
;   DOES NOT SET !PSYM=8.  This should be done in plotting program to utilize
;   plot symbol defined in this program.
;
;   Written:  G. Sonneborn       9 May 1990
;   Modifid for IDL V2           21 May 1991
;
NPARM=N_PARAMS(0)
IF ((NPARM EQ 0) OR (NPARM GT 2)) THEN BEGIN
   PRINT,'SET_PSYM,SYM,SIZ'
   PRINT,'    SYM = 1,2,3,4 = open/filled circle, open/filled square'
   PRINT,'        = 5,6,7,8   = open/filled diamond, open/filled star'
   PRINT,'    SIZ = multiplicative factor for symbol size.  Default=2'
   RETALL
   END
SYMB=FIX(SYM)
;
IF NPARM EQ 2 THEN SIZE=SIZ ELSE SIZE=2
;
CASE 1 OF
(SYMB EQ 1):  BEGIN
        ANG=FINDGEN(41)*9/!RADEG
        USERSYM,COS(ANG)*SIZE,SIN(ANG)*SIZE
        END
(SYMB EQ 2):  BEGIN
        ANG=FINDGEN(41)*9./!RADEG
        USERSYM,COS(ANG)*SIZE,SIN(ANG)*SIZE, /FILL
        END
(SYMB EQ 3):  BEGIN
        XS=[-1,-1,1,1,-1]
        YS=[-1,1,1,-1,-1]
        USERSYM,XS*SIZE,YS*SIZE
        END
(SYMB EQ 4):  BEGIN
        XS=[-1,-1,1,1,-1]
        YS=[-1,1,1,-1,-1]
        USERSYM,XS*SIZE,YS*SIZE, /FILL
        END
(SYMB EQ 5):  BEGIN
        XS=[0,-1,0,1,0]
        YS=[-1,0,1,0,-1]
        USERSYM,XS*SIZE,YS*SIZE
        END
(SYMB EQ 6):  BEGIN
        XS=[0,-1,0,1,0]
        YS=[-1,0,1,0,-1]
        USERSYM,XS*SIZE,YS*SIZE, /FILL
        END
(SYMB EQ 7):  BEGIN
        ANG=(36*FINDGEN(11)+18)/!RADEG
        X=COS(ANG) & Y=SIN(ANG)
        FOR I=1,9,2 DO X(I)=X(I)*0.38197
        FOR I=1,9,2 DO Y(I)=Y(I)*0.38197
        USERSYM,X*SIZE,Y*SIZE
        END
(SYMB EQ 8):  BEGIN
        ANG=(36*FINDGEN(11)+18)/!RADEG
        X=COS(ANG) & Y=SIN(ANG)
        FOR I=1,9,2 DO X(I)=X(I)*0.38197
        FOR I=1,9,2 DO Y(I)=Y(I)*0.38197
        USERSYM,X*SIZE,Y*SIZE, /FILL
        END
ELSE:  BEGIN
    PRINT,'Enter SYM = 1 - 8'
    END
ENDCASE
;
RETURN
END
