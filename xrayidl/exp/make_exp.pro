;	PROGRAM	: CONST_EXP_MAPS
;	AUTHOR	: STEVE SNOWDEN MPE
;	DATE	: APRIL, 1992
;
;	** THIS PROGRAM IS AN IDL TRANSLATION OF THE ORIGINAL 
;	VAX FORTRAN PROGRAM CREATED BY STEVE SNOWDEN OF 
;	MPE. **
;		- jeffrey a. mendenhall
;
;	THIS PROGRAM GENERATES ENERGY DEPENDENT EXPOSURE MAPS 
;	FOR ROSAT POINTED OBSERVATIONS. INITIALLY, ENERGY
;	DEPENDENT INSTRUMENT MAPS ARE READ AS WELL AS INFORMATION
;	RELATED TO THE PARTICULAR POINTED OBSERVATION IN 
;	QUESTION. LIVE TIME FRACTIONS ARE THEN CALCULATED BASED
;	ON EVENT RATES. FINALLY, THE TOTAL LIVE TIME EXPOSURE
;	IS FOLDED ACROSS THE INSTRUMENT MAP ACCORDING TO THE
;	VARIOUS ASPECT SOLUTIONS CORRESPONDING TO THE OBSERVATION.
;	
;	INPUTS:	
;                       1. *.CAS FILENAME (ASPECT INFO)
;                       2. *.EVR FILENAME (EVENT RATE INFO)
;                       3. NUMBER OF OBI's
;                       4. ENERGY BAND SELECTION
;                       5. GOOD TIMES FILENAME
;       OUTPUTS:
;                       1. ENERGY DEPENDENT EXPOSURE MAP
;                       2. ASPECT DATA FILE
;                       3. EVENTRATE DATA FILE
;                       4. LIVETIME DATA FILE
;
; modified to use default filenames for inputs
; Aug 31 1992 (WQD)
;
;PRO	CONST_EXP_MAPS
pro make_exp

;	Set up arrays

	INSTMP=STRARR(7)
        SCALE =DBLARR(7)

;	Initialization
	
	DEADTP=234.
;        INFILE=''
	ACTIME=''
	EXPMAP=''
	REFILE=''

;	Instrument map information

        INSTMP(0)='/home/casa/wqd/rosat/expmaps/exposure_8_19.fits'
        INSTMP(1)='/home/casa/wqd/rosat/expmaps/exposure_20_41.fits'
        INSTMP(2)='/home/casa/wqd/rosat/expmaps/exposure_42_51.fits'
        INSTMP(3)='/home/casa/wqd/rosat/expmaps/exposure_52_69.fits'
        INSTMP(4)='/home/casa/wqd/rosat/expmaps/exposure_70_90.fits'
        INSTMP(5)='/home/casa/wqd/rosat/expmaps/exposure_91_131.fits'
        INSTMP(6)='/home/casa/wqd/rosat/expmaps/exposure_132_201.fits' 
        SCALE(0)=1.450173E-4
	SCALE(1)=4.841082E-5 
	SCALE(2)=6.500746E-5 
	SCALE(3)=6.1726414E-5
	SCALE(4)=6.04025954E-5 
	SCALE(5)=6.1157329E-5 
	SCALE(6)=6.1680003E-5


;	Get intruductory info

;	Get reference exposure maps so that the header information may be used 
;	when creating the new, energy dependent map.

;	PRINT,'Enter name of reference exposure map /w fits extension: '
;	READ,REFILE
        refile=!data_dir+strtrim(!seq_no)+'_mex.fits'

;	Open file containing times the user has identified as valid and 
;	uncontaminated. These may be the original obi intervals or subsets
;	thereof. See intro documentation for format of this file.
	
;	PRINT,'Enter filename containing accepted time intervals:'
;       READ,ACTIME
	actime=!data_dir+!seq_no+'_actime.dat'

        OPENR,un, ACTIME,/get_lun
;        PRINT,'ACTIME filesize:'
        READf,un,SACT
        ACTDATA=FLTARR(2,SACT)
        READF,un,ACTDATA	
	num=0
	readf,un,num ;obis
print,'number of obis =',num
	free_lun,un

;	Generate Attitude and Livtime data sets

	PRINT,'Creating input data sets...'
	EXP_DATA,ATTDATA,LIVDATA,num
;	CONST_EXP_DATA,ATTDATA,LIVDATA

	S=SIZE(ATTDATA)
	SATT=S(2)
	
	S=SIZE(LIVDATA)
	SLIV=S(2)


; 	Read a sample of the roll angles and take an average, the processing
; 	will offset from this angle
	DTEMP=0
;	DTEMP=TOTAL(ATTDATA(4,0:S(2)))
	DTEMP=TOTAL(ATTDATA(4,0:(SATT-1)))
	DTEMP=DTEMP/SATT
	THETA=DTEMP/7200.
	THETOF=DTEMP

; 	Start the loop over the attitude file

;       Initialize looping parameters

        IOS =0
        NG=0
        NB1=0
        NB2=0
        NB3=0
        NB4=0
        ISCSO=1.0
        IACTBE=1.0
        IACTEN=1.0
        ILIVEN=1.0
        ILIVEN=1.0
        IAEXE=1.0
        IAXE=1.0
        IA1LL=1.0
        NUM=0
        TOTIME=0
        TOTEXP=0
        DONE=0
        rec=0

	
	PRINT,'Looping over attitude file to find good times...'



	ASPECT=FLTARR(51,51,1501)
	I=0
	I=LONG(I)
	J=0
	J=LONG(J)
	K=0
	K=LONG(K)
        WHILE (I LT SATT AND DONE EQ 0) DO BEGIN
            ISCS = ATTDATA(1,I)
	    IX   = ATTDATA(2,I)
            IY   = ATTDATA(3,I)
	    IROLL= ATTDATA(4,I)
	    IY   = -IY
	    DELT = ISCS - ISCSO
            ISCSO= ISCS

; 	Process the attitude step, first check the accepted time file	
; 	to see if the attitude step is in an accepted time period

                WHILE (J LT SACT AND ISCS GT IACTEN) DO BEGIN
		    IACTBE=ACTDATA(0,J)
		    IACTEN=ACTDATA(1,J)
		    J=J+1
                END
		IF J EQ SACT THEN DONE=1

                IF((ISCS GT IACTBE) AND (DONE EQ 0)) THEN BEGIN
                    NUM = NUM + 1
; 	Accepted time, now find the live time

;                    WHILE (K LT SLIV-2 AND ISCS GT ILIVEN) DO BEGIN ;-2 doesn't make sense Aug 1992 (wqd)
                    WHILE (K LT SLIV AND ISCS GT ILIVEN) DO BEGIN
			ILIVEN=LIVDATA(1,K)
                        AEXE  =LIVDATA(2,K)
                        AXE   =LIVDATA(3,K)
			A1LL  =LIVDATA(4,K)
			K=K+1
                    END
		    IF K EQ SLIV THEN DONE=1

; 	Calculate the live time fraction

                    LIVTIM,A1LL,DEADTP,AXE,AEXE,FLIVE1,FLIVE2,FLIVE

; 	The attitude steps are on 1-second intervals, calculate the exposure

                    EXP = DELT*FLIVE
                    TOTIME = TOTIME + DELT
                    TOTEXP = TOTEXP + EXP

;  	Set the X, Y, and ROLL values for the aspect array.  Steps are 
;  	in units of 14.94733 arc seconds (historical reasons).

                    IF(A1LL GT 30.) THEN BEGIN
                        X = 26. + FLOAT(IX)/29.894656
                        IX = FIX(X)
                        IF((IX GE 1) AND (IX LE 51)) THEN BEGIN
                            Y = 26. + FLOAT(IY)/29.894656
                            IY = FIX(Y)
                            IF((IY GE 1) AND (IY LE 51)) THEN BEGIN
                                ROLL = 600. + (FLOAT(IROLL)-THETOF)/149.4733 
                                IR = FIX(ROLL)
                                IF((IR GE 1) AND (IR LE 1500)) THEN BEGIN

; 	Add to the aspect array

                                    ASPECT(IX,IY,IR) =  ASPECT(IX,IY,IR) + EXP
                                    NG = NG + 1
                                END ELSE NB4 = NB4 + 1
                            END ELSE NB3 = NB3 + 1
                        END ELSE  NB2 = NB2 + 1
                    END ELSE NB1 = NB1 + 1
                END
            I=I+1
        END
;
	livdata=0 ;no longer useful
	attdata=0 
        PRINT, 'GOOD STEPS =', NG
        PRINT, 'BAD STEPS = ', NB1, NB2, NB3, NB4
        PRINT, 'NUM, TOTIME, TOTEXP = ',NUM, TOTIME, TOTEXP 
;
expra=total(total(aspect,1),1) ;one dimension (roll angle) exposure distrib
irvalue=where(expra ne 0.,nir)
aspect_c=fltarr(51,51,nir)
for k=0,(nir-1) do aspect_c(*,*,k)=aspect(*,*,irvalue(k))
if !debug eq 1 then stop,'after irvalue'
aspect=0 ; save memory
;
; calculate the rms of the roll angle wobble
;
rarms=(irvalue-600.)*expra(irvalue)*(149.4733/7200.)
rarms=total(rarms*rarms)
if rarms eq 0. then print,'no roll angle wobble in the observation' else begin
rarms=sqrt(rarms)/totexp
print,'The rms of the roll angle wobble is ',rarms
endelse
;
; calculate the rms of the RA wobble
;
expxa=total(total(aspect_c,3),2)
value=where(expxa ne 0.)
rarms=(value-26.)*expxa(value)*(29.894656/7200.)
xarms=total(xarms*xarms)
if xarms eq 0. then print,'no X angle wobble in the observation' else begin
xarms=sqrt(xarms)/totexp
print,'The rms of the X angle wobble is ',xarms
endelse
;
; calculate the rms of the DEC wobble
;
expya=total(total(aspect_c,3),1)
value=where(expya ne 0.)
rarms=(value-26.)*expya(value)*(29.894656/7200.)
yarms=total(yarms*yarms)
if yarms eq 0. then print,'no Y angle wobble in the observation' else begin
yarms=sqrt(yarms)/totexp
print,'The rms of the Y angle wobble is ',yarms
endelse
;


;
;	Loop through and create exposure maps

	ANSWER='Y'
        YESNO,ANSWER
 
        WHILE ANSWER EQ 1 DO BEGIN
        ANSWER=''

;        PRINT, 'Enter name of output exposure map /w fits extension:'
;        READ,EXPMAP
;        PRINT,''
	expmap=!dir+!seq_no+'_exp.fits'


;       Select and read instrument map
        
        PRINT, 'Enter input band'
        PRINT, '1 => CH 8-19'
        PRINT, '2 => CH 20-41'
        PRINT, '3 => CH 42-51'
        PRINT, '4 => CH 52-69'
        PRINT, '5 => CH 70-90'
        PRINT, '6 => CH 91-131'
        PRINT, '7 => CH 132-201'
        READ, IB
        PRINT,''

 	MAP=FLTARR(512,512)
        PRINT, 'READING INSTRUMENT MAP...'
        MAP=READFITS(INSTMP(IB-1),H,/NOSCALE)
        
 
;       Center the instrument map, invert the Y-axis, and turn it real

        PRINT,'Centering instrument map...'
;	I=INDGEN(463) ;compared With the original fortran program, 463 is not right
	I=INDGEN(464)
	I=I+24
	RMAP=FLTARR(512,512)
       	RMAP(*,512-I)=SCALE(IB-1)*MAP(*,I-12)
	i=0 ;no longer useful
;
	map=0 ;no longer useful

;	Now cast the exposure

	PRINT,'Casting exposure...'
	TEMPAR=FLTARR(512,512)
	EXPARR=FLTARR(512,512)
        NB1 = 0
;        FOR IR=0,1500 DO BEGIN
        FOR k=0,(nir-1) DO BEGIN
	   ir=irvalue(k)
            PRINT, IR
            NG = 0
            FOR IX=0,50 DO BEGIN
                FOR IY=0,50 DO BEGIN
;                    IF(ASPECT_C(IX,IY,IR) GT 0) THEN BEGIN
                    IF(ASPECT_C(IX,IY,k) GT 0) THEN BEGIN
                        IF(NG EQ 0) THEN BEGIN

; 	First nonzero aspect point with this roll angle, make a rotated map

                            NG = 1
                            ANGLE  = THETA + (IR-600.)*0.02076017
			    ANGLE  = 3.1415927*ANGLE/180.
                            COSROL = COS(ANGLE)
                            SINROL = SIN(ANGLE)

; 	Calculate the rotated array
	
;	Make this part more efficient
			   
			   FOR I=24,487 DO BEGIN
                                FOR II=24,487 DO BEGIN
                                    IF(RMAP(I,II) NE 0.00000) THEN BEGIN
                                        X = (I - 256.)
                                        Y = (II - 256.)
                                        XX = COSROL*X + SINROL*Y
                                        YY = COSROL*Y - SINROL*X
                                        IXX = FIX(XX + 256.5)
                                        IYY = FIX(YY + 256.5)
                          TEMPAR(IXX,IYY) = TEMPAR(IXX,IYY)+RMAP(I,II)
                                    END
                                ENDFOR
                            ENDFOR

                        END

; 	Nonzero element, cast it
	
;		TEMP=ASPECT_C(IX,IY,IR)*TEMPAR
		EXPARR=EXPARR+SHIFT((ASPECT_C(IX,IY,k)*TEMPAR),IX-26,IY-26)
; 	Zero the temp array
	
			  TEMPAR(*)=0

		END
	    ENDFOR
        ENDFOR
	ENDFOR
if !debug eq 1 then stop,'before fits'
	tempar=0
	rmap=0
	aspec=0

;	WRITE EXPOSURE MAP TO FITS FILE.


;	GET OUTPUT INFO AND OPEN OUTPUT FILES.

	PRINT,'Transferring file to fits format...'
	PRINT,''
        PRINT,'Opening reference exposure map to get header info...'
 	DATA=READFITS(REFILE,REFHEAD,/NOSCALE)

;
;       Write data to fits fomat.
;
 
        PRINT,''
        PRINT,'Writing idl array back into fits format: ',EXPMAP
	SXADDPAR,REFHEAD,'BSCALE',1.0
	SXADDPAR,REFHEAD,'BZERO',0.0
;
	SXADDPAR,REFHEAD,'XS-DANG'',theta
	SXADDPAR,REFHEAD,'XS-ONTI',totime
	SXADDPAR,REFHEAD,'XS-LIVTI',totexp
	SXADDPAR,REFHEAD,'XS-XARMS',xarms
	SXADDPAR,REFHEAD,'XS-YARMS',yarms
	SXADDPAR,REFHEAD,'XS-RARMS',rarms
;
print,'the live time in the reference header ',sxpara(refhead,'XS-LIVTI')
print,' has been replaced by ',totexp
print,'The detetor roll angle ', theta,' degree has been included in the header'
print,'avg x, y, and rotation rms are included in the header'
;
	WRITEFITS,EXPMAP,EXPARR,REFHEAD
	
	PRINT,'Do you wish to create another exposure map? ;
	READ,ANSWER
	YESNO,ANSWER

	END
        
        print,'Normal Termination.'
        close,92
        
        END
