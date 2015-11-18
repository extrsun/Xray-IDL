pro file_params,fname,dhdr,cra,cdec,expt,nra,ndec,xoff,yoff,roll,hdr=hdr,aimoff=aimoff
;+
; get basic data file information and create a simple fits header 
;
;*INPUTS:
; fname - event file name (e.g., 'acisf00945N001_evt2.fits')
;*OUTPUTS:
; dhdr - fits header of the data
; cra, cdec - RA and Dec of the reference (aiming) point (deg)
; expt - exposure time (s)
; nra, ndec - RA and Dec of the centered image (deg)
; xoff, yoff - the X and Y axis offset of the centered image from the 
;		reference point (data pixel)
; roll - pointing roll angle of the observation (deg)
; hdr - original fits header of the event file
; aimoff - two element vector containing the x and y offset of the image
; 	center from the norminal aiming point
;
;written by wqd, 6/4/2001
;-
npara=n_params()
if npara eq 0 then begin
print,'CALLING SEQUENCE - file_params,fname,dhdr,cra,cdec,expt,nra,ndec'
print,',xoff,yoff,roll,hdr=hdr,aimoff=aimoff'
return
endif
;On_error,2    
if n_elements(fdir) eq 0 then fdir=!data_dir
if n_elements(tail) eq 0 then tail=''
if n_elements(fname) eq 0 then fname=!seq_no+tail+'_evt2.fits'
hdr=headfits(fname,ext=1)
if !err eq -7 then stop,'there is a problem in reading the fits header!'

;----------------------------------------------------------------
;XMM block:
if !instr eq 'epic' then begin
;find the reference coordinates:
    cra=sxpar(hdr,'REFXCRVL')
    cdec=sxpar(hdr,'REFYCRVL')
    crval=[cra,cdec]
    if cra eq 0 and cdec eq 0 then crval=sxpar(hdr,'TCRVL*') ;for my event file
    ddim=long(!pref)*2
    get_fitshead,0,dhdr,equi=2000,crval=crval,del=!size_pixel/3600. $
      ,cpx=!pref,cpy=!pref,dim=ddim,type=2

    if npara gt 2 then begin
;exposure and roll angle:
        expt=sxpar(hdr,'LIVETIME',count=nc)
        if nc eq 0 then stop,'there is no fits key word: '+'LIVETIMEEXPOSURE'
        roll=sxpar(hdr,'PA_PNT',count=nc)
        if nc eq 0 then roll=sxpar(hdr,'ROLL_NOM',count=nc)
    endif 
endif else begin
;----------------------------------------------------------------
;find the reference coordinates:
    ctype=sxpar(hdr,'TCTYP*',count=nc)
    if nc eq 0 then stop,'there is no fits key word: '+'TCTYP*'
    sel=where(ctype eq 'RA---TAN',nsel)
    if nsel eq 0 then stop,'there is no fits key word: '+'RA---TAN'
    cra=sxpar(hdr,'TCRVL'+strtrim(sel(nsel-1)+1,2))
    cdec=sxpar(hdr,'TCRVL'+strtrim(sel(nsel-1)+2,2))
    ddim=long(!pref)*2
    get_fitshead,0,dhdr,equi=2000,crval=[cra,cdec],del=!size_pixel/3600.,cpx=!pref,cpy=!pref,dim=ddim,type=2
    if npara eq 2 then return    ;if npara gt 2 then begin
;exposure and roll angle:
        expt=sxpar(hdr,'EXPOSURE',count=nc)
        ;if nc eq 0 then stop,'there is no fits key word: '+'EXPOSURE'
        roll=sxpar(hdr,'ROLL_PNT',count=nc)
        ;if nc eq 0 then roll=sxpar(hdr,'ROLL_NOM',count=nc)
;    endif 
;----------------------------------------------------------------
endelse
;----------------------------------------------------------------
if nc eq 0 then begin
	print,'there is no fits key word: '+'ROLL_PNT'
	roll=0.
endif
rroll=roll*!pi/180

;------------------------------------
; find the offsets of the centered image
if n_elements(aimoff) eq 0 then begin
 case !instr of 
  'epic': begin
	aimoffx=0
	aimoffy=0
	end
  'acisi': begin
	;don't ask me why we need 40 and 10 to center the image. I guess that 
	; some information is still missing
	aimoffx=-73+40 
	aimoffy=-71+10 
	end
  'acisi_low': begin
	;don't ask me why we need 40 and 10 to center the image. I guess that 
	; some information is still missing
	aimoffx=-73+40 
	aimoffy=-71+10 
	end
  'aciss': begin
	aimoffx=137/!size_pixel
	aimoffy=0
	end
 endcase
endif else begin
	aimoffx=aimoff(0)
	aimoffy=aimoff(1)
endelse
xoff=aimoffx*cos(rroll)+aimoffy*sin(rroll)
yoff=-aimoffx*sin(rroll)+aimoffy*cos(rroll)
trans_loct,xoff,yoff,cra,cdec,nra,ndec,/deg,pixsize=!size_pixel
if !debug eq 2 then stop
return
end
