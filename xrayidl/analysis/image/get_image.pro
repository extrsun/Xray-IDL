pro get_image,image_t,image_c,image_tsub,image_ff,blow=blow,bhigh=bhigh, $
block=block,dim=dim,factor=factor,sidexcl=sidexcl,tonly=tonly $
,exptail=exptail,hdr=hdr,tmin=tmin,slow=slow,flow=flow,soufile=soufile $
,mask=mask,timfile=timfile,image_f=image_f,rec=rec,subvalue=subvalue $
,xshift=xshift,yshift=yshift,ashift=ashift,angle=angle,cntrlimit=cntrlimit $
,perclimit=perclimit,psffile=psffile,cra=cra,cdec=cdec,back=back
;+
; Master program for reading or producing images
;*inputs:
; blow, bhigh - the lower and upper limits of the board bands (1-7)
; dim - the dimension of the image (shoudl be even value since the assumed
;   exposure map has a dimension of an even number of pixels
; factor - the radius factor of the source subtractions
; sidexcl - the ids of the sources to be deleted completely (image values set
;	 to zero so that they are not distiguishable from unexposed regions
; tonly - if set, only the exposure images will be produced
; exptail - the identification of the exposure file
; tmin - def = 0. to use *_gtiall.dat for time filtering
;            negative to use *_gti.dat (if exptail is not set)
; soufile - source file to be used for source subtractions
; slow - the minmum signal-to-noise ratio for a source to be included in
; the source subtraction
; flow - the minmum source count rate for a source to be included in
; the source subtraction
; timfile - the exposure image file name (e.g., rp*_mex.fits)
; xshift,yshift,ashift - X, Y (pixel (0".5)), and anti-clockwise 
;			angular (degree) shifts 
; angle - additional rotation (can be any value) in units of degree 
;	(anti-clockwise)
; cra, cdec - RA and Dec of the image center
;*outputs: 
; image_c,image_t - the count and exposure images used in the calculation
; image_tsub - exposure image with sources subtracted
; image_ff - flat fielding image = image_tsub/exptime
; mask - mask of the observations with ribs removed in detector coordinates
;	produced by make_expmap.pro. To subtract source regions, the parameter
;	image_tsub should be given in the call.
;
; Note: When X and Y shifts are not zero, the edge of the images
;	with the width of max(xshift) > max(yshift) should be deleted later 
;	wqd, Jun 9, 1994
;
; writen by wqd, Oct 1992
; corrected the error in the rot 
; (dim*0.5, dim*0.5 ==>(sz(1)-1)*0.5,(sz(2)-1)*0.5); wqd, 7/5/96
;-
if N_params() eq 0 then begin
	print,'CALLING SEQUENCE - get_image,image_t,image_c,image_tsub,image_ff'
	print,',blow=blow,bhigh=bhgih,block=block,dim=dim,factor=factor'
	print,',sidexcl,tonly=,hdr=,tmin=tmin,slow=slow,flow=flow'
	print,',exptail=, soufile=,mask=,timfile=timfile,image_f=image_f'
	print,',rec=rec,subvalue=subvalue,xshift=xshift,yshift=yshift'
	print,',ashift=ashift,cntrlimit=cntrlimit,perclimit=perclimit'
	print,',psffile=psffile,cra=cra,cdec=cdec,back=back'
	return
endif
if n_elements(xshift) eq 0 then xshift=0
if n_elements(yshift) eq 0 then yshift=0
if n_elements(ashift) ne 0 then anglet=ashift
if n_elements(angle) ne 0 then anglet=anglet+angle
if n_elements(slow) eq 0 then slow=3.5
if n_elements(block) eq 0 then block=!block
if n_elements(dim) eq 0 then dim=512
if n_elements(bhigh) eq 0 then bhigh=5
if n_elements(blow) eq 0 then blow=4
if n_elements(subvalue) eq 0 then subvalue=0.
;
if n_elements(factor) eq 0 then begin
factor=1.
print,'factor = ',factor
endif
if n_elements(flow) eq 0 then begin
flow=0.0
print,'flow =',flow
endif
print,'get exposure map'
if n_elements(exptail) eq 0 then exptail=''
if n_elements(timfile) eq 0 then begin
	bandtail=strtrim(blow,2)+strtrim(bhigh,2)+strtrim(exptail,2)
	timfile=!data_dir+!seq_no+'_exp'+bandtail+'.fits'
endif
tfile=!seq_no+'_gti'+strtrim(exptail,2)+'.dat'
image_t = readfits(timfile,hdr) > 0. 
	;regions in the SLS's exposure maps could be negative
image_t=shift(image_t,nint(xshift/float(block)),nint(yshift/float(block)))
if n_elements(anglet) ne 0 then begin
	  sz=size(image_t)
	  image_t=rot(image_t,-anglet,1.,(sz(1)-1)*0.5,(sz(2)-1)*0.5,/inter)
endif
image_t=image_cut(image_t,dim*block/30.*0.5,/pixel,rec=rec)
if block ne 30 then image_t=rebin(image_t,dim,dim)
;----------------------
;define the back to get a background map, copied from image_t
if n_elements(back) gt 0 then begin
back = readfits(!seq_no+'_bac'+bandtail+'.fits') > 0. 
	;regions in the SLS's exposure maps could be negative
back=shift(back,nint(xshift/float(block)),nint(yshift/float(block)))
if n_elements(anglet) ne 0 then begin
	  sz=size(back)
	  back=rot(back,-anglet,1.,(sz(1)-1)*0.5,(sz(2)-1)*0.5,/inter)
endif
back=image_cut(back,dim*block/30.*0.5,/pixel,rec=rec)
if block ne 30 then back=rebin(back,dim,dim)
endif
;=====================================
if n_params() gt 2  then begin
  ;have sources subtracted and suppose the source list is 
  ;from images before the shifts
  source_info,sid,sra,sdec,sigma,cntr,slow=slow,flow=flow $
	,ns=ns,soufile=soufile,/deg
  if n_elements(cra) eq 0 then image_center,cra,cdec,/deg
  if n_elements(sidexcl) ne 0 then begin
	;separate the two source populations
	source_excl,sid,sra,sdec,sidexcl,sraexcl,sdecexcl
	image_t=source_sub_v(image_t,cra,cdec,sraexcl,sdecexcl,cntr $
	,factor=factor,blow=blow,bhigh=bhigh,block=block,subvalue=0. $
	,cntrlimit=cntrlimit,perclimit=perclimit,psffile=psffile,/deg) 
		;the regions covered by these sources 
		;will be considered as having no exposure at all
  endif 
;  print,'subtracting sources'
  image_tsub=source_sub_v(image_t,cra,cdec,sra,sdec,cntr, $
	factor=factor,blow=blow,bhigh=bhigh,block=block,subvalue=subvalue $
	,cntrlimit=cntrlimit,perclimit=perclimit,psffile=psffile,/deg)
;  image_tsub=shift(image_tsub $
;		,nint(xshift/float(block)),nint(yshift/float(block)))
;  if n_elements(anglet) ne 0 then $
;	  image_tsub=rot(image_tsub,-anglet,1.,dim*0.5,dim*0.5,/inter)

  if n_params() gt 3 then begin
	; calculate  <v> and export image_tsub
	time=sxpar(hdr,'LIVETIME')
	image_ff=image_tsub/time
	c=where(image_ff gt 0.,nc) 
	vmean=total(image_ff(c))/nc
	print,'vmean = ',vmean
  endif
endif
;==================================
;image_t=shift(image_t,nint(xshift/float(block)),nint(yshift/float(block)))
;if n_elements(anglet) ne 0 then $
;	  image_t=rot(image_t,-anglet,1.,dim*0.5,dim*0.5,/inter)

if N_elements(tonly) ne 0 and n_elements(mask) eq 0 then return
;
if n_elements(mask) ne 0 then begin
	get_mask_ob,mask,dim=dim
	if n_elements(image_tsub) ne 0 then begin
		c=where(image_tsub le 0.,nc) 
		if nc ne 0 then mask(c)=0.
	endif
endif
;
if keyword_set(tonly) ne 0 then return else begin
	if block eq 30 then begin ;this case the count image is probably there
		filen=!data_dir+!seq_no+'_im'+bandtail+'.fits'
		name=findfile(filen,count=nfile)
	endif else nfile=0

	if nfile ne 0 then begin
		print,'file: ',filen, ' is directly read from the directory'
		image_c=readfits(filen)
		if dim ne 512 then image_c= $
			image_cut(image_c,dim/2.,/pixel,rec=rec)
	endif else begin
		print,'producing the count image'
		chlow=!bandch(*,0)
		chhigh=!bandch(*,1)
		emin=chlow(blow-1)
		emax=chhigh(bhigh-1)
		print,'The energy channel interval selected is defined as:'
		print,'emin = ',emin,'; emax = ',emax
		make_image,image_c,dim=dim,emin=emin,emax=emax,block=block $
			,tfile=tfile,tmin=tmin $
			,xshift=xshift,yshift=yshift,ashift=ashift,angle=angle
		image_f=imdiv(image_c,image_t) ;a flux image is produced
	endelse
endelse
end
