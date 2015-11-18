pro spec_get_dif,source_no,mask,block=block,syserr=syserr,tfile=tfile $
  ,sb,sbe,ss,sse,dtype=dtype,group=group,mask_d=mask_d,corfile=corfile $
	,nofile=nofile,etime=etime,sppr=sppr,sppe=sppe,nobnorm=nobnorm
;-
; master program to produce a diffuse x-ray spectrum in a selected region
; and a mean spectrum of discrete sources in the selected field.
;
;*INPUTS:
; source_no - an extension of the output spectral file. The output file
;		name will be in the form of rp*_source_no.pha
; mask - a spatial mask used in selecting the spectral data. Regions
;		to be used to calculate the diffuse x-ray spectrum must
;		have a positive value while regions to be used to 
;		obtain the average spectrum of sources needs to have
;		a negative value. Regions with mask value equal to zero 
;		will not be used. The mask can be obtained by get_image
;		using the keyword mask=mask (where mask should be defined,
;		e.g., mask=1), which reads from the fits file 
;		rp*_00+exptail.fits produced with make_expmap.pro
; mask_d - the mask in the detector's coordinate for removing ribs
;		def = ~/rosatdata/expmaps/mask.fits
; corfile - correction spectrum (without its own background file) to be 
;	subtracted from the background spectrum.
; dtype - type of the PSPC detector (i.e., 'B' or 'C'). default = 'B'
; block - block of the mask
; syserr - systematical errors expressed as the fraction of the spectral fluxes
;	(e.g., syserr=0.03 --- 3 percent of the fluxes to be added in
;	quadrature to the statistical errors.
; tfile - the time interval file selected for calculating the spectra.
;	default = rp*_gti.dat
; nofile - if set, will produce no file output 
; group - the group of PSPC channels
; etime - the output exposure time from the previous run. This input
;	is necessary for inputting sppr and sppe 
; sppr, sppe - the particle spectrum and its error (or any spectrum to be
; 		subtracted from the background spectrum.
; nobnorm - if set, the background spectrum will not be normalized from
;	    the units of cts/s to cts/s deg^2
;*OUTPUTS:
; The spectra files are written into disk files
; sb,sbe,ss,sse - the calculated spectra of the background and sources and
;	their errors (all in units of cts/s channel)
; writen by WQD, Aug, 1992.
; including the option to input sppr and sppe. wqd, Aug 31, 1993
;+
if n_params() eq 0 then begin
print,'CALL SEQUENCE - spec_get_dif,source_no,mask,block=block,syserr=syserr'
print,',tfile=tfile,sb,sbe,ss,sse,group=group,mask_d=mask_d'
print,',corfile=corfile,nofile=nofile,etime=etime,sppr=sppr,sppe=sppe,nobnorm=nobnorm'
return
endif
if n_elements(block) eq 0 then block= !block    ; blocking factor
if n_elements(exptail) eq 0 then exptail=''
;
;
print,'get particle spectrumn'
;
tsz=size(mask)
if n_elements(etime) ne 0 and n_elements(sppr) ne 0 then begin 
	;when pspectrum is an input
	pspectrum=sppr & pspectrume=sppe
endif else begin
	pspectrum,mask,pspectrum,etime,dtype=dtype,tfile=tfile,/vig 
	pspectrume=pspectrum*0.1 ; assuming 10\% systematic error in the 
			; particle spectrum
endelse	
;vig is set to correct the similar vig correction applied to particle events
; in make_spec
print,'etime = ',etime
;
;calculate the exposure correction for the background spectrum 
;due to the mask of ribs
;filen=!data_dir+strtrim(!seq_no,2)+'_exp00.fits' ;masked exposure map
;maskexp=readfits(filen)
;maskexp=image_cut(maskexp,tsz(1)/2,/pixel) ;assuming the mask is a circle 
					; around the image center
c=where(mask gt 0.) ;only for the background region
maskexp=mask(c)
maskcor=total(maskexp)/(n_elements(maskexp)*max(maskexp))
print,'mask correction is ',maskcor
;
;--------------------------
inputs='obseq='+strtrim(!seq_no,2)+',dir='+!data_dir
;
if tsz(0) ne 0 then begin ;redefine the region
	xc=256.*30. ;the image_t is assumed centering at the reference center
	yc=xc
	hxdim=tsz(1)*block/2 ; now in units of 0".5	
	hydim=tsz(2)*block/2 ; now in units of 0".5
	xmin=nint(xc-hxdim) 
	xmax=nint(xc+hxdim)
	ymin=nint(yc-hydim)
	ymax=nint(yc+hydim)
endif else begin
	print,'the image_t is required for masking the data'
	return
endelse
;
inputs=inputs+',xmin='+strtrim(xmin,2)+',xmax='+strtrim(xmax,2) $
+',ymin='+strtrim(ymin,2)+',ymax='+strtrim(ymax,2)+',tmin=-1'
print,'get the count list from ',inputs
;
make_list,inputs,list,numpix,tfile=tfile
;----------------------------
;
;if !debug eq 1 then stop
print,'masking the list with the image mask
if n_elements(mask_d) eq 0 then $
	mask_d=readfits('~/rosatdata/expmaps/mask.fits') ;get a mask in the 
				;detector coordinate to get ribs
				;the mask.fits is produced assuming block=30
;	if block ne 30 then mask_d=image_comp(mask_d,30/block)
	; assuming the mask_d has a block =30
filter_spatial,list,mask_d,/det
list_b=list
filter_spatial,list_b,mask,xmin=xmin,ymin=ymin,block=block
numpix_b=n_elements(where(mask gt 0.))
list_s=list
filter_spatial,list_s,mask,xmin=xmin,ymin=ymin,block=block,/neg
numpix_s=n_elements(where(mask lt 0.))
;spec_data_dif,list,mask,xmin,ymin,list_s,list_b,numpix_s,numpix_b,block=block
;
print,'numpix_s,numpix_b = ',numpix_s,numpix_b
print,'First, get the count rate spectrum for the source'
fxbopen,5,!data_dir+strtrim(!seq_no,2)+'.fits',3,hdr
;fxbclose,5
make_spec,list_s,etime,ss,sse,mtime_s,syserr=syserr,group=group $
,hdr=hdr
print,'Now, get the count  rate spectrum for the background region'
make_spec,list_b,etime*maskcor,sb,sbe,mtime_b,syserr=syserr,group=group,hdr=hdr
if !debug eq 1 then stop
;
; convert to the unit of counts /s deg^2
trans=(block*!size_pixel/3600.)^2
area_s=numpix_s*trans
area_b=numpix_b*trans
print,'area_s,area_b = ',area_s,area_b, ' deg^2'
;ss=ss/(area_s+area_b)  ;the area of the whole field
;sse=sse/(area_s+area_b)
;
if keyword_set(nobnorm) eq 0 then begin
	sb=sb/area_b
	sbe=sbe/area_b
	;area_b=area_s+area_b
	;the background contribution area_s/area_b*sb will
	;be used in XSPEC for subtraction in the source spectrum
	area_b=1.   ;so that spectrum at different fields can be compared
endif 
if keyword_set(nofile) then return

if strtrim(strupcase(source_no),2) eq 'NO' then return
sfname=!data_dir+strtrim(!seq_no,2)+'_'+strtrim(source_no,2)+'_s'
bfname=!data_dir+strtrim(!seq_no,2)+'_'+strtrim(source_no,2)+'_b'
pfname=!data_dir+strtrim(!seq_no,2)+'_'+strtrim(source_no,2)+'_p'
if n_elements(corfile) ne 0 then $
	cfname=!data_dir+strtrim(!seq_no,2)+'_'+strtrim(source_no,2)+'_c'
print,'output the spectra into the files: '
print,sfname
print,bfname
print,pfname
if n_elements(corfile) ne 0 then print,cfname
;
mtime_s=1. ;What is the use of the exposure in xspec? 
mtime_b=1. 
inputs='FNAME='+sfname+',BKFIL='+bfname
make_pha,inputs,ss,sse,mtime_s,area_s,group=group
;
inputs='FNAME='+bfname+',BKFIL='+pfname
if n_elements(corfile) ne 0 then inputs=inputs+',CORFIL='+cfname
make_pha,inputs,sb,sbe,mtime_b,area_b,group=group
;
inputs='FNAME='+pfname+',BKFIL=none'

make_pha,inputs,pspectrum,pspectrume,1.,1.,group=group ;10\% - arbitrary value
;pspectrum is produced in units of cts/s deg^2
if n_elements(corfile) ne 0 then begin
	inputs='FNAME='+cfname+',BKFIL=none'
	openr,un,corfile,/get_lun
	corv=fltarr(n_elements(sb))
	readf,un,corv
	free_lun,un
	make_pha,inputs,corv,corv,mtime_b,area_b,group=group
endif

;
stop,'stop at the end of the program'
end
