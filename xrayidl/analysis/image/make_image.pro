;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME:
; make_image
;
;*PURPOSE:
; make an image from an observation with the selections of dimension, center
; location, energy range, and block. Such an image is particularly useful
; in the detailed analysis of a source
;
;*CALLING SEQUENCE:
;  make_image,image,seq_no=seq_no,dir=dir,dim=dim,xc=xc,yc=yc,filter=filter,
;  emin=emin,emax=emax,tmin=tmin,tmax=tmax,block=block,list=list,tfile=tfile
; ,det=det,xshift=xshift,yshift=yshift,ashift=ashift,angle=angle
;
;*PARAMETERS:
; INPUTS:
; none
;
;*OPTIONAL INPUT PARAMETERS:
; dir - directory of the observation files. def = !data_dir
; seq_no - sequence No. i.e., rp123456. def=seq_no
; dim - dimension of the output image in units of 0.5 arcsec. (def=7680)
; xc, yc - respectively, the x and y pixel positions in units of 
;          the  normal blocked bins as appear in hardcopies. def=7680
; emin,emax - respectively, the lower and upper limits of the energy (PI)
;             channel range.  defaults = defined in make_list.def (8-201)
;             Note: psf calibration is available only for channels (~14-201 -
;             corresponding to grouped channel range 3-29) and the instrument
;	      maps are available for channels 8-201.
; tmin,tmax - respectively, the lower and upper limits of the observing time
;             def: tmin=-1 to allow the program make_list to read a file 
;	      *_gti.dat (see make_list_new_m.pro). tmin=0 to use all GTI time
;	      intervals
; block - the block size of the output image in terms of the 0".5 pixels
;         def = !block (30)
; det - if set, an image in detector coordinate will be produced
;*OUTPUTS:
; image - a count array
;
;*PROCEDURE:
; first get a list of photons within the selected ranges and then form an image
;
;*EXAMPLES:
;
;*RESTRICTIONS:
; none
;
;*NOTES:
;
;*SUBROUTINES CALLED:
; make_list
;
;
;*MODIFICATION HISTORY:
; writen Aug 29 1992 (WQD)
;
;-
;---------------------------------------------------------------------------
pro make_image,image,seq_no=seq_no,dir=dir,dim=dim,xc=xc $
,yc=yc,tmin=tmin,tmax=tmax,emin=emin,emax=emax,block=block,filter=filter $
,list=list,tfile=tfile,det=det,xshift=xshift,yshift=yshift,ashift=ashift,angle=angle
;
if n_params() eq 0 then begin
print,'CALL SEQUENCE -  make_image,image,seq_no=seq_no,dir=dir,dim=dim,'
print,'xc=xc,yc=yc,emin=emin,emax=emax,tmin=tmim,tmax=tmax,filter=filter'
print,'tfile=tfile,block=block,list=list,det=det,xshift=xshift,yshift=yshift'
print,',ashift=ashift,angle=angle'
print,'xc,yc are the pixel position as appears in the 512*30*521*30 image'
return
endif
;
if n_elements(dim) eq 0 then dim=512
if n_elements(block) eq 0 then block= !block    ; blocking factor
if n_elements(dir) eq 0 then dir=!data_dir else !data_dir=dir
if n_elements(seq_no) eq 0 then seq_no=!seq_no else !seq_no=seq_no
inputs='obseq='+strtrim(seq_no,2)+',dir='+strtrim(dir,2)
print,'get the count list from ',inputs
;
; set up the inputs for MAKE_LIST
;
if n_elements(xc) eq 0 then xc=7680 ; in units of 0.5"; wqd, Aug. 30, 94
if n_elements(yc) eq 0 then yc=7680
	hdim=dim*block/2. ; now in units of 0".5
	xmin=nint(xc-hdim)+1 ;because list.x >=1
;	xmin=nint(xc*block-hdim)+1 ;because list.x >=1
; this correction has very small effect (0.5") wqd, Dec 1, 1993
	xmax=nint(xc+hdim)
	ymin=nint(yc-hdim)+1 
	ymax=nint(yc+hdim)
;
; the position selection is needed in case where the image does not cover
; the whole observation. This could happen when block, or x(y)center, or dim
; has been set.
;
inputs=inputs+',xmin='+strtrim(xmin,2)+',xmax='+strtrim(xmax,2) $
+',ymin='+strtrim(ymin,2)+',ymax='+strtrim(ymax,2)
;
if n_elements(emin) ne 0 then inputs=inputs+',emin='+strtrim(emin,2)
if n_elements(emax) ne 0 then inputs=inputs+',emax='+strtrim(emax,2)
if n_elements(tmin) eq 0 then tmin=-1.
 inputs=inputs+',tmin='+strtrim(tmin,2)
if n_elements(tmax) ne 0 then inputs=inputs+',tmax='+strtrim(tmax,2)
if n_elements(filter) ne 0 then begin
	inputs=inputs+',pixel=BL'
	inputs=inputs+',zoom='+strtrim(block,2)
endif
if n_elements(tfile) eq 0 and tmin eq 0 then begin
	tfile=!seq_no+'_gtiall.dat'
	print,'use the default tfile; ',tfile 
	tmin=-1
endif
if n_elements(det) ne 0 then $
	make_list,inputs,list,numpix,tfile=tfile else $
	make_list,inputs,list,numpix,tfile=tfile,/xytpionly $
		,xshift=xshift,yshift=yshift,ashift=ashift 
if n_elements(list) eq 0 then stop,'The list contains no count'
;
print,' construct image'
;
image=intarr(dim,dim)
;
if n_elements(det) ne 0 then begin
	x=list.dx & y=list.dy 
endif else begin
	x=list.x & y=list.y 
endelse
;rot_xy,x,y,angle,xshift=xshift,yshift=yshift
;c=where(x ge xmin and x le xmax and y ge ymin and y le ymax)
;x=(x(c)-xmin)/block & y=(y(c)-ymin)/block
x=(x-xmin)/fix(block) & y=(y-ymin)/fix(block)
elements=long(dim)*y+x
print,'Creating image'
sz=size(x)  ; check to see if x is a scalar
case 1 of
  (sz(0) ne 0): begin        ; x is a vector
     h=histogram(elements)
     bin=lindgen(n_elements(h))+min(elements)
     image(bin)=h
     end
  else:    image = 0
endcase
end 
