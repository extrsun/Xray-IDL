pro get_instrmap,bandlow,bandhigh,imap,iscale
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - get_instrmap,bandlow,bandhigh,imap,iscale'
return
endif
;
;	Set up arrays

	INSTMP=STRARR(7)
        SCALE =DBLARR(7)

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


;       Read instrument map(s)
        
 	MAP=FLTARR(512,512)
        PRINT, 'READING INSTRUMENT MAP(S)...'

;
; The broad band exposure map should be a function of the spectrum of the 
; incident X-rays and may be calculated by adding the spectral weight.
; The following is only good for the incident X-ray spectrum similar to that
; of the average sky
;
	iscale=0.
	for band=bandlow,bandhigh do begin
	 MAP=MAP+READFITS(INSTMP(BAND-1),H,/NOSCALE)
	 iscale=iscale+ 1./SCALE(BAND-1)
	endfor
	iscale=1./iscale

;       Center the instrument map, invert the Y-axis, and turn it real

        PRINT,'Centering instrument map...'
;	I=INDGEN(463) ;compared with the original fortran program, 463 is not right
	I=INDGEN(464)
	I=I+24
	IMAP=FLTARR(512,512)
       	IMAP(*,512-I)=MAP(*,I-12)
end