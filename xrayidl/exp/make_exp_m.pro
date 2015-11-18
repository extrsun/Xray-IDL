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
; Major modifications include:
; (1) The setting of the X, Y, and ROLL values for the aspect array is 
; moved out from a loop. The TOTEXP now represents the true exposure of
; the output exposure map.
; (2) The calculation of the rotated intrument map (TEMPAR) is modified to
; use function ROT
; (3) A bug in calculating the mean ROLL value is fixed
; (4) Using procedure GET_POSI to compress the array ASPECT so that only the 
; ROLL values with none zero exposure are included.
; Sept 1 1992 (WQD)
;
;PRO	CONST_EXP_MAPS
pro make_exp_m,quite=quite
;
if n_elements(quite) eq 0 then quite=0

;	Set up arrays

	INSTMP=STRARR(7)
        SCALE =DBLARR(7)

;	Initialization
	
	DEADTP=234.

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
	readf,un,num ;obis are now included in the file rp*_actime.dat
	print,'number of obis =',num
	free_lun,un

;	Generate Attitude and Livtime data sets

	PRINT,'Creating input data sets...'
	EXP_DATA,ATTDATA,LIVDATA,num ;modified version of const_exp_data
;	CONST_EXP_DATA,ATTDATA,LIVDATA

	S=SIZE(ATTDATA)
	SATT=S(2)
	
	S=SIZE(LIVDATA)
	SLIV=S(2)


; 	Calculate the mean  roll angles, the processing
; 	will offset from this angle
	DTEMP=0
;	DTEMP=TOTAL(ATTDATA(4,0:S(2))) ;S(2) is not the dimension of attdata
	DTEMP=TOTAL(ATTDATA(4,0:(SATT-1)))
	DTEMP=DTEMP/SATT
	THETA=DTEMP/7200.
	THETOF=DTEMP

; 	Start the loop over the attitude file

;       Initialize looping parameters

        IOS =0
        NG=0
        NB1=0
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
;
;
;  	Set the X, Y, and ROLL values for the aspect array.  Steps are 
;  	in units of 14.94733 arc seconds (historical reasons).

        IX = FLOAT(ATTDATA(2,*))/29.894656
        IY = FLOAT(ATTDATA(3,*))/29.894656
	minix=min(ix)
	maxix=max(ix)
	miniy=min(iy)
	maxiy=max(iy)

print,'minix,maxix,miniy,maxiy = ',minix,maxix,miniy,maxiy,' in units of 15 arcsec'
print,'The defaults include all the data within these limits'
if(abs(minix) ge 24 or (abs(maxix) ge 24) or (abs(miniy) ge 24) $
or (abs(maxiy) ge 24)) then begin
print,'The amplitute of the wobble is to large. The shift command in this'
print,'program will move some of the exposure to other side of the output'
print,' image. You need to set some limits or you know what to do'
endif 
yn='no'
if quite eq 0 then read,'Do you want to make some change? yes or no?',yn
yesno,yn
if yn eq 1 then read,'minix,maxix,miniy,maxiy = ',minix,maxix,miniy,maxiy
;
	limitx=fix(maxix-minix+1.)
	limity=fix(maxiy-miniy+1.)
	ix=fix(ix+(0.5-minix))
	iy=fix(iy+(0.5-miniy))

	limitra=180 ;all roll angles are selected
	limitra=limitra*7200./149.4733
        IR = FIX((limitra+0.5) + (FLOAT(ATTDATA(4,*))-THETOF)/149.4733) 
;
        ind=where(((IX GE 0) AND (IX LE limitx)) and ((IY GE 0)  $
  AND (IY LE limity)),satt) ; and ((IR GE 0) AND (IR LE 2*limitra)),satt)

	ix=ix(ind)
	iy=iy(ind)
	ir=ir(ind)
	attdata=attdata(*,ind)
	ind=0
;
	get_posi,ir,loci,kdup
	nraloci=n_elements(loci)
;
;	ASPECT=FLTARR(51,51,1501)
	ASPECT=FLTARR(limitx,limity,nraloci)
if !debug eq 1 then stop,'after'

	I=0L
	J=0L
	K=0L
        WHILE (I LT SATT AND DONE EQ 0) DO BEGIN
            ISCS = ATTDATA(1,I)
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

                    IF(A1LL GT 30.) THEN BEGIN

; 	Calculate the live time fraction

                     LIVTIM,A1LL,DEADTP,AXE,AEXE,FLIVE1,FLIVE2,FLIVE

; 	The attitude steps are on 1-second intervals, calculate the exposure

                     EXP = DELT*FLIVE
                     TOTIME = TOTIME + DELT
                     TOTEXP = TOTEXP + EXP

; 	Add to the aspect array
 		     irloci=where(loci eq ir(I))
                     ASPECT(IX(I),IY(I),IRloci(0)) =  $
                        ASPECT(IX(I),IY(I),IRloci(0)) + EXP

                    END ELSE NB1 = NB1 + 1
                END
            I=I+1
        END
;
	livdata=0 ;no longer useful
	attdata=0 
	ix=0
	iy=0
	ir=0
        PRINT, 'GOOD STEPS =', NG
        PRINT, 'BAD STEPS = ', NB1 ;, NB2, NB3, NB4
        PRINT, 'NUM, TOTIME, TOTEXP = ',NUM, TOTIME, TOTEXP 

;
; calculate the rms of the wobble parameters
;
expra=total(total(aspect,1),1) ;one dimension (roll angle) exposure distrib
value=loci
rarms=(value-limitra)*expra(value)*(149.4733/7200.)
rarms=total(rarms*rarms)
if rarms eq 0. then print,'no roll angle wobble in the observation' else begin
rarms=sqrt(rarms)/totexp
print,'The rms of the roll angle wobble is ',rarms
endelse
if !debug eq 1 then stop,'after rarms'
;
; calculate the rms of the RA wobble
;
expxa=total(total(aspect,3),2)
value=where(expxa ne 0.)
xarms=(value+minix)*expxa(value)*(29.894656/120.)
xarms=total(xarms*xarms)
if xarms eq 0. then print,'no X angle wobble in the observation' else begin
xarms=sqrt(xarms)/totexp
print,'The rms of the X angle wobble is ',xarms,' arcmin'
endelse
;
; calculate the rms of the DEC wobble
;
expya=total(total(aspect,3),1)
value=where(expya ne 0.)
yarms=(value+miniy)*expya(value)*(29.894656/120.)
yarms=total(yarms*yarms)
if yarms eq 0. then print,'no Y angle wobble in the observation' else begin
yarms=sqrt(yarms)/totexp
print,'The rms of the Y angle wobble is ',yarms,' arcmin'
endelse
;
;
;	Loop through and create exposure maps

	ANSWER='Y'
        YESNO,ANSWER
 
        WHILE (ANSWER EQ 1) DO BEGIN
        ANSWER=''

	expmap=!data_dir+!seq_no+'_exp.fits'

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
;	I=INDGEN(463) ;compared with the original fortran program, 463 is not right
	I=INDGEN(464)
	I=I+24
	RMAP=FLTARR(512,512)
       	RMAP(*,512-I)=SCALE(IB-1)*MAP(*,I-12)
	i=0 ;no longer useful
;
	map=0 ;no longer useful

;	Now cast the exposure

	PRINT,'Casting exposure...'
	print,'The number of loops is ',nraloci
	TEMPAR=FLTARR(512,512)
	EXPARR=FLTARR(512,512)
        NB1 = 0
        FOR k=0,(nraloci-1) DO BEGIN
	   ir=loci(k)
            PRINT, k
            NG = 0
            FOR IY=0,(limity-1) DO BEGIN
                FOR IX=0,(limitx-1) DO BEGIN

                    IF(ASPECT(IX,IY,k) GT 0) THEN BEGIN
                        IF(NG EQ 0) THEN BEGIN

; 	First nonzero aspect point with this roll angle, make a rotated map

                            NG = 1
                            ANGLE  = THETA + (IR-limitra)*0.02076017
; 	Calculate the rotated array
	
                            TEMPAR = rot(RMAP,angle,1.0,256,256,missing=0.0)

                        END

; 	Nonzero element, cast it
	
		print,'ix+minix,iy+miniy = ',ix+minix,iy+miniy

		EXPARR=EXPARR+SHIFT((ASPECT(IX,IY,k)*TEMPAR),IX+minix,IY+miniy)

		END
	    ENDFOR
        ENDFOR
	ENDFOR
if !debug eq 1 then stop,'before fits'
;
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
print,'the live time in the reference header ',sxpar(refhead,'XS-LIVTI')
print,' has been replaced by ',totexp

	SXADDPAR,REFHEAD,'XS-DANG'',theta
	SXADDPAR,REFHEAD,'XS-ONTI',totime
	SXADDPAR,REFHEAD,'XS-LIVTI',totexp
	SXADDPAR,REFHEAD,'XS-XARMS',xarms
	SXADDPAR,REFHEAD,'XS-YARMS',yarms
	SXADDPAR,REFHEAD,'XS-RARMS',rarms
;
print,'The detetor roll angle ', theta,' degree has been included in the header'
print,'Average x, y, and rotation rms are included in the header'
;
	WRITEFITS,EXPMAP,EXPARR,REFHEAD
	
	if quite eq 0 then begin
	PRINT,'Do you wish to create another exposure map? ;
	READ,ANSWER
	YESNO,ANSWER
	endif

	END
        
        print,'Normal Termination.'
        
        END
