pro spec_get,source_no,xs_pix,ys_pix,filter=filter,etime=etime,ss,sse,sb,sbe $
    ,exptail=exptail,list=list,backfile=backfile,rs=rs,rb1=rb1,rb2=rb2 $
	,dim=dim,block=block,souexc=souexc $
	,xshift=xshift,yshift=yshift,ashift=ashift,xmin=xmin,ymin=ymin,ls=list_s
;-
; Given the source pixel position, extract a spectrum of the source
;*INPUTS:
; source_no - a given name of the source to be used as part of the spectrum
;	file name
; xs_pix,us_pix - the x and y pixel position (in standard SASS 0.5" pixels)
; 		(0,0) corresponding to the center at (7679.5,7679.5) in
;		the SASS image
; filter - an image filter (e.g., image_tsub) used to remove unwanted parts
;	of the image (i.e., source subtracted area)
; etime - the exposure time of the image. Def = the live time as in the
;		corresponding exposure image
; list - structure containing a list of counts of the image which is centered
;	on the observation axis.
; exptail - the tail of the good time interval file name, i.e. rp*+'_gti'+
;		exptail+'.dat'
; backfile - if set, a background file is produced separately
; rs,rb1,rb2 - radius of the source area and  inner radius and outer radius of 
;		the background annulus (in units of arcmin)
; dim - the dimension of the filter 
; block - the block size of the filter
; xc, yc - x and y axis position of the filter center in units of 0.5"
; souexc - if set, zero pixels in the core region will remain to be zero
;		i.e., the data at these pixels will not be used
;*OUTPUTS:
; spectral files in the form of rp*_source_no_s(b).pha
; ss, sse - the spectrum of the source in units of cts/s/cha
; sb, sbe - the spectrum of the local backg in units of cts/s/cha
; list - the list of the photon which can be used in the next run to save time.
; writen by WQD, 1992
; add filter_sp subroutine to account for source subtracted regions in the
;	background annulus
; add the keyword backfile so that source spectrum can be derived with
;	the background subtracted with no background spectrum file needed
; spec_data is replaced by spec_data_dif to use filter_s
; wqd, May 5, 1994
;-
if n_params() eq 0 then begin
print,'CALL SEQUENCE - spec_get,source_no,xs_pix,ys_pix'
print,',filter=filter,etime=etime,ss,sse,sb,sbe,exptail=exptail,list=list'
print,',backfile=backfile,rs=rs,rb1=rb1,rb2=rb2,dim=dim,block=block'
print,',souexc=souexc,xshift=xshift,yshift=yshift,ashift=ashift'
print,',xmin=xmin,ymin=ymin'
return
endif
if n_elements(exptail) eq 0 then exptail='all'
if n_elements(rs) eq 0 then rs=!radius_s
if n_elements(rb1) eq 0 then rb1=!radius_b1
if n_elements(rb2) eq 0 then rb2=!radius_b2
print,'Extract the source and background spectra using rs = ',rs, $
'rb1 = ',rb1,' and rb2 = ',rb2, ' arcminutes'
;
; get the exposure of the image
if n_elements(etime) eq 0 then begin
	if n_elements(filter) ne 0 and max(filter) ne 1 then etime=max(filter)$
	else read,'give etime :',etime
endif
;
	if n_elements(dim) eq 0 then begin
		if n_elements(filter) ne 0 then begin
			sz=size(filter)
			dim=sz(1) > sz(2)
		endif else dim=400 ;50 arcmin radius
	endif
	if n_elements(block) eq 0 then block=!block
if n_elements(list) le 1 then begin
	tfile=!seq_no+'_gti'+exptail+'.dat'
	getlistimage,list,dim=dim*block,xc=7679.5,yc=7679.5,emin=7,emax=247 $
	,xshift=xshift,yshift=yshift,ashift=ashift,xmin=xmin,ymin=ymin $
	,tfile=tfile
endif else print,'the old photon list is used!!!'

if n_elements(filter) eq 0 then filter=replicate(1.,dim,dim)
;hdim=dim*block*0.5
filter_sp,xs_pix,ys_pix,filter,filter_s,block=block,rs=rs,rb1=rb1,rb2=rb2 $
	,souexc=souexc,/pixcenter
; extract spectral data in the source and background areas
spec_data_dif,list,filter_s,xmin,ymin,list_s,list_b,numpix_s,numpix_b,block=block 
if !debug eq 1 then stop
print,'First, get the count rate spectrum for the source'
make_spec,list_s,etime,ss,sse,mtime_s
print,'Now, get the count rate spectrum for the background region'
make_spec,list_b,etime,sb,sbe,mtime_b

; output the spectral files
inputs='FNAME='+strtrim(!seq_no,2)+'_'+strtrim(source_no,2)+'_s,'
if keyword_set(backfile) ne 0 then begin 
	inputs=inputs+ $
 'BKFIL='+strtrim(!data_dir,2)+strtrim(!seq_no,2)+'_'+strtrim(source_no,2)+'_b'
 print,'output the spectra into the files ',inputs
 make_pha,inputs,ss,sse,1.,numpix_s
 inputs='FNAME='+strtrim(!seq_no,2)+'_'+strtrim(source_no,2)+'_b,BKFIL=none'
 make_pha,inputs,sb,sbe,1.,numpix_b
endif else begin
 sb=sb*float(numpix_s)/float(numpix_b)
 ss=ss-sb
 sbe=sbe*float(numpix_s)/float(numpix_b)
 sse=sqrt(sse*sse+sbe*sbe)
 inputs=inputs+'BKFIL=none'
 print,'output the spectra into the files ',inputs
 make_pha,inputs,ss,sse,1.,numpix_s
endelse
if !debug eq 1 then stop

end
