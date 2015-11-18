;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME:
; Make_expmap
;
;*PURPOSE:
; GENERATES ENERGY DEPENDENT EXPOSURE MAPS FOR ROSAT POINTED OBSERVATIONS.
; This procedure may be called by the main program EXPMAP.PRO or can be
; run separately.
;
;*CALLING SEQUENCE:
; make_expmap,blow,bhigh,goodtime,exparr,expfile=expfile,quite=quite
; tfile=tfile,/gtionly,gtifile=gtifile,exptail=exptail
;
;*PARAMETERS:
; INPUTS:
; blow,bhigh - The lower and upper limits of the energy band range 
;		     selected from the following numbers:
;              1 = CH 8-19   
;	       2 = CH 20-41  
;	       3 = CH 42-51  
;	       4 = CH 52-69  
;	       5 = CH 70-90  
;	       6 = CH 91-131 
;	       7 = CH 132-201
;
; expfile - optional input of the file name into which the exposure map is 
;		going to be stored. 
; quite - if set, no question will be asked
; other required input files should have their default names as the follows:
; (* represents a sequece number, i.e., rp123456 as is defined in !seq_no)  
;              1. *_CAS.FITS (ASPECT INFO)                        
;              2. *_EVR.FITS (EVENT RATE INFO) 
;	       3. *_MEX.FITS (REFERENCE EXPOSURE MAP)                   
;              4. *_ACTIME.DAT (GOOD TIME FILENAME produced by actime.pro or 
;           	    use tfile - the time interval file for the data selection
; exptail - use to provide an extra identification in the output exposure file
; if blow = bhigh  - will produce a mask with regions covered by the ribs excluded
; 	in the detector coordinate
;*OUTPUTS
; goodtime - an array containing the selected time intervals which will be
; 		used by MAKE_LIST in selecting photons. This array is also
; 		writen into a file *_gti.dat.
; exparr - the constructed exposure map
; *_gti.dat - A free format data file containing the time intervals within
;		which the exposure map is constructed
; *_exp.fits - fits file containing the constructed exposure map if expfile
;		is not provided
;
;*EXAMPLES:
; make_expmap,1,7
;
;*RESTRICTIONS:
; The available instrument maps were produced with the sky survey data 
; collected with PSPC B. Since there could be significant differences in 
; performance between PSPC B and PSPC C, users should be very careful in 
; using the instrument maps for observations made with  PSPC C
;
; The RA amd DEC amplitudes of the wobble of a pointed observation should
; be within 6 arcminutes. Otherwise, the exposure map may have its actual
; half size larger than the 512/2. 
;
;*NOTES:
; All the input and output files are supposed to be in the default data 
; directory as is defined in !data_dir.
; The output time intervals match the totime, not the totexp. 
;
; Possible problems in the current version of the procedure:
; 1. Some of the detector maps have maximum values = 2^15, why?
; 2. the shift of the detector maps used in the procedure appears
; producing best fit to real count images, but should be shifted using
; shift(*,-1,-1) to match the standard *_mex.fits.
; 3. Whether are the orientations of the detector maps used correctly?
; (in the translation from Fortran code to the original IDL procedure).
;
;*SUBROUTINES CALLED:
; exp_data
; get_posi
; input_int
; output_int
;
;*MODIFICATION HISTORY:
;
; Translated by Jeffrey a. mendenhall (APRIL, 1992) to IDL program from VMS 
; fortran code writen by  STEVE SNOWDEN (MPE) 
; The original program name CONST_EXP_MAP is renamed as MAKE_EXPMAP after 
; substantial modifications. The major modifications include (Sept 1 1992 WQD):
; (0) using default filenames for inputs;
; (1)  moving the setting of the X, Y, and ROLL values for the aspect array  
;  out from a loop. The TOTEXP now represents the true live time of
; the output exposure map;
; (2) using function ROT to calculate the rotated intrument map (TEMPAR);
; (3) fixing a  bug in calculating the mean ROLL value;
; (4) Using procedure GET_POSI to compress the array ASPECT to include all  
; ROLL values with none zero exposure;
; (5) allowing a band range selected .
; Nov 18, 1992 (WQD) add the function for automatically determing the 
; device type the primary (C) or the secondary (B) PSPC. 
;
; July 23, 1993 (wqd) New detector maps are used, including the maps
; in the 11_19 channels. For an observation taken after 10/11/1991,
; the exposure map in the 11_19 band  will be constructed using the 
; 8-19 band detector map.
;
; Aug 21, 1993 (wqd) include bandcr containing diffuse background count
; rates in band 1-7, derived from rp900017 within 15'. This bandcr is
; used as weight in constructing exposure maps of broader energy ranges.
; Aug 26, New scales for the detector maps (steve.scales) are included.
; 
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pro make_expmap,blow,bhigh,goodtime,exparr,expfile=expfile,quite=quite $
,tfile=tfile,gtionly=gtionly,gtifile=gtifile,exptail=exptail
;
if N_params() eq 0 then begin
print,'CALLING SEQUENCE - MAKE_EXPMAP,BLOW,BHIGH,GOODTIME,EXPARR'
print,'EXPFILE=,QUITE=TFILE=,/gtionly,gtifile=gtifile,exptail='
print,'make blow=0 to make a mask image'
retall
endif
if n_elements(exptail) eq 0 then exptail=''
;
if n_elements(quite) eq 0 then quite=0

;	Set up arrays

;	Initialization
	
	DEADTP=234. ;How is this value derived?

;	Get reference header information 

if !proc eq 'MPE' then extyp='_mexmap.ifits' else extyp='_mex.fits'
infile=!data_dir+!seq_no+extyp

	sxhread,infile,refhead
	get_obdate,refhead,julian,/pri
	if julian le 2448281.5 then $ 
	;jan 25, 1991 when the primary PSPC was burned
		device='c' else device='b'
	print,'The device type is = ',device
;this is the inverse of the actual order, but is correct anyway
	
;	Instrument map information
	
	fhead='/data1/wqd/detmap/det_'
	fhead='/data3/detmap/det_'
	fext='.fits'
; ftail(0) (and corresponding scales) is changed to '11_19' from '8_19' 
; on July 26, 1993
	ftail=['11_19','20_41','42_51','52_69','70_90','91_131','132_201', $
	'8_19','8_41','52_90','91_201','42_131','42_201']
if device eq 'c' then ftail(0)='8_19'
ftail(7)='8

        INSTMP=fhead+ftail+'_'+strtrim(device,2)+fext
	mapnorm_b=[17.7733E-5,  6.8581E-5, 6.3094E-5, 5.1019E-5, 5.1628E-5, $
		    5.0908E-5, 5.5935E-5, 16.8681E-5, 9.1572E-5, 4.7012E-5, $
		    4.8574E-5, 4.6476E-5, 4.4918E-5]

	mapnorm_c=[14.535e-5, 4.5975E-5, 5.9797E-5, 4.9065E-5, 5.5607E-5, $
		    5.3753E-5,5.8466E-5,13.7951E-5,7.6664E-5,4.5958E-5, $
		    4.7877E-5,4.6530E-5,4.6415E-5]
; the scales are from the file steve.scales (Aug 16, 1993)
;11_19 for _c is still my best guess and needs to be supplied.

	bandcr=[0.927122,1.39611,0.119149,0.197304,0.240584,0.263655,0.170624]
	if device eq 'b' then begin
		scale=mapnorm_b 
		if julian gt 2448541.0 then begin
			;the date when the PSPC B gain changed
			ftail(0)=ftail(7)
			mapnorm_b(0)=mapnorm_b(7)
			print,'8_19 channel is replaced with 11_19 to account
			print,'for the gain change'
		endif
	endif else scale=mapnorm_c
;	Get intruductory info
;	Open file containing times the user has identified as valid and 
;	uncontaminated. These may be the original obi intervals or subsets
;	thereof.
	
;	PRINT,'Enter filename containing accepted time intervals:'
;       READ,ACTIME

	if keyword_set(tfile) ne 0 then actime=tfile else $
		actime=!seq_no+'_actime.dat'
	print,'The time interval file ',actime,' is used'
input_int,actdata,sact,infile=actime
;fname=!data_dir+!seq_no+'.so'
;sxhread,fname,hdr
;rsgetseqinfo,hdr,num
;	print,'number of obis =',num

;	Generate Attitude and Livtime data sets

	PRINT,'Creating input data sets...'
;	EXP_DATA,ATTDATA,LIVDATA,num ;modified version of const_exp_data
	EXP_DATA,ATTDATA,LIVDATA ;modified version of const_exp_data
;	CONST_EXP_DATA,ATTDATA,LIVDATA

	S=SIZE(ATTDATA)
	SATT=S(2)
	
	S=SIZE(LIVDATA)
	SLIV=S(2)


; 	Calculate the mean  roll angles, the processing
; 	will offset from this angle

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

;
;  	Set the X, Y, and ROLL values for the aspect array.  Steps are 
;  	in units of 14.94733 arc seconds (historical reasons).
;; But the output exposure map is supposed to have a scale of 15 arcsec/pixel
;; Though the error may be small, I still need to understand this Sept 2 (wqd)

        IX = FLOAT(ATTDATA(2,*))/29.894656
        IY = FLOAT(ATTDATA(3,*))/29.894656
	minix=nint((min(ix)+24) > 0) -24 ; limit the amplidute of wobbling to 6'
	maxix=nint(max(ix)) <  24
	miniy=fix((min(iy)+24) > 0) -24
	maxiy=nint(max(iy)) <  24

print,'minix,maxix,miniy,maxiy = ',minix,maxix,miniy,maxiy,' in units of 15 arcsec'
print,'The defaults include only the data with X and Y wobble < 6 arcmin'
;print,'The defaults include all the data within these limits'
;if(abs(minix) ge 24 or (abs(maxix) ge 24) or (abs(miniy) ge 24) $
;or (abs(maxiy) ge 24)) then begin
;print,'The amplitute of the wobble is too large. The shift command in this'
;print,'program may shift one side of the map to its other side'
;print,'You need to change limits or you know what to do with the map'
;endif 
;yn='no'
;if quite eq 0 then read,'Do you want to make some change? yes or no?',yn
;yesno,yn
;if yn eq 1 then read,'minix,maxix,miniy,maxiy = ',minix,maxix,miniy,maxiy
;
	limitx=fix(maxix-minix+1.) ;the X dimension of the aspect array
	limity=fix(maxiy-miniy+1.) ;the Y dimension of the aspect array
	ix=fix(ix+(0.5-minix))     ;shift the X minimum to position 0
	iy=fix(iy+(0.5-miniy))     ;shift the Y minimum to position 0

	minra=-180 ;all roll angles are selected
	minra=minra*7200./149.4733 ;now in its historical unit ? =1'.2456
        IR = FIX((0.5-minra) + (double(ATTDATA(4,*))-THETOF)/149.4733) 
;
; in case of any change in the limits
;
print,'find the points within the limits'

        ind=where(((IX GE 0) AND (IX LE limitx-1)) and ((IY GE 0)  $
  AND (IY LE limity-1)),satt) ; and ((IR GE 0) AND (IR LE 2*minra)),satt)

	ix=ix(ind)
	iy=iy(ind)
	ir=ir(ind)
;
	time=attdata(1,ind)
;
print,'find ROLL values with none zero exposure'
;
	get_posi,ir,loci ;,kdup 
	nraloci=n_elements(loci)
;
;	ASPECT=FLTARR(51,51,1501)
	ASPECT=FLTARR(limitx,limity,nraloci)
	
	PRINT,'Looping over attitude file to find good times...'
;
	attdata(0,*)=0 ;will be used to select good time
	I=0L
	J=0L
	K=0L
        WHILE (I LT SATT AND DONE EQ 0) DO BEGIN
            ISCS = time(I)
;	    DELT = ISCS - ISCSO
            ISCSO= ISCS

; 	Process the attitude step, first check the accepted time file	
; 	to see if the attitude step is in an accepted time period

                WHILE (J LT SACT AND ISCS GT IACTEN) DO BEGIN
		    IACTBE=ACTDATA(0,J)
		    IACTEN=ACTDATA(1,J)
		    J=J+1 
                END
		IF (J EQ SACT and iscs gt iacten) THEN DONE=1 ;ISCS is beyond the limit of accepted time

                IF((ISCS GT IACTBE) AND (DONE EQ 0)) THEN BEGIN
                    NUM = NUM + 1
; 	Accepted time, now find the live time

;                    WHILE (K LT SLIV-2 AND ISCS GT ILIVEN) DO BEGIN 
;-2 doesn't make sense Aug 1992 (wqd)

                    WHILE (K LT SLIV AND ISCS GT ILIVEN) DO BEGIN
			ILIVEN=LIVDATA(1,K)
                        AEXE  =LIVDATA(2,K)
                        AXE   =LIVDATA(3,K)
			A1LL  =LIVDATA(4,K)
			K=K+1
                    END
		    IF (K EQ SLIV and iscs gt iliven) THEN DONE=1 
;ISCS is beyond the limit of live time

                    IF(A1LL GT 30. and done eq 0) THEN BEGIN
;       Mark the good time 
	
		     attdata(0,ind(I)) =1

; 	Calculate the live time fraction

                     LIVTIM,A1LL,DEADTP,AXE,AEXE,FLIVE1,FLIVE2,FLIVE

; 	The attitude steps are on 1-second intervals, calculate the exposure

;                     EXP = DELT*FLIVE
                     EXP = FLIVE
;                     TOTIME = TOTIME + DELT
                     TOTIME = TOTIME + 1
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
	ind=0
	time=0
;
; 	Get the output intervals of the good time
;
;if keyword_set(gtionly) ne 0 then begin
	loc=attdata(0,*)
	loc=[0,loc(0:*)]
	loc=where (([loc(1:*),0]-loc) ne 0,nloc) ;get the start and end locations
	goodtime=lonarr(2,nloc/2) ;output good time intervals
	for k=0,(nloc/2-1) do begin
		goodtime(0,k) = attdata(1,loc(2*k))-0.5 ;to match the totime
		goodtime(1,k) = attdata(1,loc(2*k+1)-1)+0.5
	endfor
	if n_elements(gtifile) eq 0 then gtifile=!seq_no+'_gti'+exptail+'.dat'
	output_int,goodtime,outfile=gtifile
if !debug eq 1 then stop,'after good time'
;endif
;
	livdata=0 ;no longer useful
	attdata=0 
	ix=0
	iy=0
	ir=0
        PRINT, 'BAD STEPS = ', NB1 ;, NB2, NB3, NB4
        PRINT, 'NUM, TOTIME, TOTEXP = ',NUM, TOTIME, TOTEXP 
if keyword_set(gtionly) ne 0 then return
;
; calculate the rms of the wobble parameters
;
expra=total(total(aspect,1),1) ;one dimension (roll angle) exposure distrib
rarms=(loci+minra)*(149.4733/7200.) ;weighted by exposure
rarms=total(rarms*rarms*expra)
if rarms eq 0. then print,'no roll angle wobble in the observation' else begin
rarms=sqrt(rarms/totexp)
print,'The rms of the roll angle wobble is ',rarms,' degrees'
endelse
if !debug eq 1 then stop,'after rarms'
;
; calculate the rms of the RA wobble
;
expxa=total(total(aspect,3),2)
value=where(expxa ne 0.)
xarms=(value+minix)*(29.894656*0.5)
xarms=total(xarms*xarms*expxa(value))
if xarms eq 0. then print,'no X angle wobble in the observation' else begin
xarms=sqrt(xarms/totexp)
print,'The rms of the X angle wobble is ',xarms,' arcsec'
endelse
;
; calculate the rms of the DEC wobble
;
expya=total(total(aspect,3),1)
value=where(expya ne 0.)
yarms=(value+miniy)*(29.894656*0.5)
yarms=total(yarms*yarms*expxa(value))
if yarms eq 0. then print,'no Y angle wobble in the observation' else begin
yarms=sqrt(yarms/totexp)
print,'The rms of the Y angle wobble is ',yarms,' arcsec'
endelse
;
if keyword_set(gtionly) ne 0 then return
;
	if n_elements(expfile) eq 0 then begin
	  expfile=!data_dir+!seq_no+'_exp' $
          +strtrim(blow,2)+strtrim(bhigh,2)+exptail+'.fits'
	endif else  expfile=!data_dir+strtrim(expfile,2)

;	Loop through and create exposure maps

	ANSWER='Y'
        YESNO,ANSWER
 
        WHILE (ANSWER EQ 1) DO BEGIN
        ANSWER=''


;       Read instrument map(s)
        
 	MAP=FLTARR(512,512)
        PRINT, 'READING INSTRUMENT MAP(S)...'

;
; The broad band exposure map should be a function of the spectrum of the 
; incident X-rays and may be calculated by adding the spectral weight.
; The following is only good for the incident X-ray spectrum similar to that
; of the average sky
;
if blow eq 0 then begin
; map=readfits('~/rosat/expmaps/mask.fits',hdr) ;from get_mask.pro 
 map=readfits('/data3/expmaps/mask.fits',hdr) ;from get_mask.pro 
 tscale=1.
endif else begin
	tscale=0.
	for band=blow,bhigh do begin
	 MAP=MAP+READFITS(INSTMP(BAND-1),H,/NOSCALE)*SCALE(BAND-1)*bandcr(band-1)
;	 tscale=tscale+ 1./SCALE(BAND-1)
 	tscale=tscale+bandcr(band-1)
	endfor
	tscale=1./tscale
endelse

; negative values in the INSTMP are found near the edge of the image boundaries
; caused by the subtraction of expected particle fluxes in the image and the
; lower statistics there.
;       Center the instrument map, invert the Y-axis, and turn it real

        PRINT,'Centering instrument map...'
;	I=INDGEN(463) ;compared with the original fortran program, 463 is not right
	I=INDGEN(464)
	I=I+24
	RMAP=FLTARR(512,512)
       	RMAP(*,512-I)=tscale*MAP(*,I-12) ;Is the orientation right?
;	rmap=shift(rmap,1,-1) ;providing the best fit to one image
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
	    angle = theta + (loci(k)+minra)*0.02076017 ;in degrees
            PRINT,'ROLL wobble No. and angle = ', k , angle
            NG = 0
            FOR IY=0,(limity-1) DO BEGIN
                FOR IX=0,(limitx-1) DO BEGIN

                    IF(ASPECT(IX,IY,k) GT 0) THEN BEGIN
                        IF(NG EQ 0) THEN BEGIN

                            NG = 1
 
; 	Calculate the rotated array
	
                            TEMPAR = rot(RMAP,angle,1.0,256,256,missing=0.0)

                        END

; 	Nonzero element, cast it
	
;		print,'Wobbling (X, Y)  = ',ix+minix,iy+miniy

;		EXPARR=EXPARR+SHIFT((ASPECT(IX,IY,k)*TEMPAR),IX+minix,IY+miniy)
		EXPARR=EXPARR+SHIFT((ASPECT(IX,IY,k)*TEMPAR),IX+minix,-IY-miniy)

		    END
	        ENDFOR
            ENDFOR
	ENDFOR
;
	tempar=0
	rmap=0
	aspec=0

;	WRITE EXPOSURE MAP TO FITS FILE.


;	GET OUTPUT INFO AND OPEN OUTPUT FILES.

	PRINT,'Transferring file to fits format...'
	PRINT,''
;
;       Write data to fits fomat.
;
 
        PRINT,''
         PRINT,'Writing idl array back into fits format: ',EXPFILE
	SXADDPAR,REFHEAD,'BSCALE',1.0
	SXADDPAR,REFHEAD,'BZERO',0.0
;
print,'the live time in the reference header ',sxpar(refhead,'XS-LIVTI')
print,' has been replaced by ',totexp

	SXADDPAR,REFHEAD,'XS-DANG',theta
	SXADDPAR,REFHEAD,'XS-ONTI',totime
	SXADDPAR,REFHEAD,'XS-LIVTI',totexp
	SXADDPAR,REFHEAD,'XS-XARMS',xarms
	SXADDPAR,REFHEAD,'XS-YARMS',yarms
	SXADDPAR,REFHEAD,'XS-RARMS',rarms
;
print,'The deettor roll angle ', theta,' degree has been included in the header'
print,'Average rms values of the X, Y, and ROLL are included in the header'
;
	WRITEFITS,EXPFILE,EXPARR,REFHEAD

	if quite eq 0 then begin
	 	PRINT,'Do you wish to create another exposure map? ;
	 	READ,ANSWER
		YESNO,ANSWER
		if answer eq 1 then  $
		 read,'Please give new band interval: ',blow, bhigh
	  	 expfile=!data_dir+!seq_no+'_exp' $
          	  +strtrim(blow,2)+strtrim(bhigh,2)+exptail+'.fits'
		if blow eq 0 then begin
			print,'Mask file will be produced'
			mask=1
		endif
	endif
if !debug eq 1 then stop,'before a new round'
	END
     	
        print,'Normal Termination.'
        
        END
