pro mkmap,cra,cdec,list_image,blow,bhigh,array_size_x,array_size_y,bin_size $
,array_c,array_b,array_t,array_tsub,radius=radius,exptail=exptail $
,soufile=soufile,index=indx,slow=slow,flow=flow,factor=factor,array_co=array_co,datadir=datadir
;-----------------------------------------------------------------
; merge images (including count, background, exposure and 
; source-subtracted exposure images) into corresponding arraies.
;*INPUT:
; list_image - list of standard 6 digit file names containing the images
; blow, bhigh - lower and upper limits of the rosat pspc band 
; array_size_x,array_size_y - x and y dimension (number of bins) 
;				of the arrays
; bin_size - size of each array bin  (in units of image pixels)
; radius - the chosen  radius of the image (def = 60 arcmin)
; exptail - the exposure file specification (def = 'all')
; soufile - the source file (def uses individual source files)
;*OUTPUT:
; array_c, array_b, array_t, array_tsub -
; sources are not subtracted in array_b and array_t. To get a source-subtracted
; 	array_b use array_bsub=array_b*imdiv(array_tsub,array_t)
; array_co - set array_co not equal to sero at the input to get a
;		array with the sources in it

; 
; writen by WQD, 3/18/93
; -----------------------------------------------------------------
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - mkmap,cra,cdec,list_image,blow,bhigh,array_size_x'
print,',array_size_y,bin_size,array_c,array_b,array_t,array_tsub,radius=radius'
print,',exptail=exptail,soufile=soufile,index=index,slow=slow,flow=flow,factor=factor,array_co=array_co,datadir=datadir'
return
endif
;
if n_elements(slow) eq 0 then slow=3.5
if n_elements(exptail) eq 0 then exptail=''
;	trans_radian,6,0,0,-71,2,0,cra,cdec ;front center
;trans_radian,6,0,0,-71,2,0,cra,cdec
;trans_radian,5,52,0,-70,15,0,cra,cdec
trans_radian,19,1,0,-36,55,0,cra,cdec
if n_elements(radius) eq 0 then  radius=60.
	image_size=(radius*2.)*(60./!size_pixel/!block)
;
array_c=lonarr(array_size_x,array_size_y)
array_co=lonarr(array_size_x,array_size_y)
array_b=fltarr(array_size_x,array_size_y)
array_t=fltarr(array_size_x,array_size_y)
array_tsub=fltarr(array_size_x,array_size_y)
if keyword_set(index) ne 0 then index=lindgen(array_size_x,array_size_y)
;
; reads image sequence numbers and get the corresponding images
	str=''
	solarf=0.0
	openr,unit,list_image,/get_lun
;;
readf,unit,nfile
subradius=2.
for k=0,nfile-1 do begin
;now read images from LIST_IMAGE
	readf,unit,str
	seq_no=gettok(str,' ')
	solarf=gettok(str,' ')
	form=strmid(seq_no,0,1)
	if strupcase(form) eq 'W' then 	envset,strtrim(seq_no,2),/mpe,datadir=datadir else $
		envset,strtrim(seq_no,2),datadir=datadir
;	factor=subradius*(1.+0.05*blow) ;to accormodate the PSF change 
;					; wqd, July 29, 1993
	get_image,image_t,image_c,image_tsub,blow=blow,bhigh=bhigh $
	   ,dim=image_size,factor=factor,slow=slow,flow=flow,exptail=exptail,soufile=soufile

	image_center,image_ra,image_dec
;
	image_tsub=image_tsub > 0.
	image_t=image_t > 0.
	if n_elements(array_co) ne 0 then image_co=image_c
	c=where(image_tsub eq 0.,nc)
	if nc ne 0 then image_c(c)=0 ;neccessary for merging images
	; get a background image
	tfile=!seq_no+'_gti'+strtrim(exptail,2)+'.dat'
	get_imageb,image_t,blow,bhigh,image_b,tfile=tfile
;	get_imageb,image_tsub,blow,bhigh,image_b,tfile=tfile
; image_tsub needs to be used for source subtractions in overlapping regions

	image_b=image_b+solarf*image_t
;	image_b=image_b+solarf*image_tsub

;	image_b=(image_tsub > 0.)*((15./60.)^2*0.00001) ;now just an arbitrary value
;
if !debug eq 1 then stop,'before merge'
  merge,cra,cdec,image_ra,image_dec,array_size_x,array_size_y $
         ,bin_size,array_c,image_c,array_b,image_b,array_t,image_t $
         ,array_tsub,image_tsub,edge_side=edge_side $
	,array_co=array_co,image_co=image_co
;
    if keyword_set(index) ne 0 then begin
	; to get the index where overlapping happens
	if k gt 0 then begin
		sel=where((array_tsub(index) - array_old(index) gt 1.e3 $
		and array_old(index) ne 0.) ,nsel)
		; 1.e3 is only an arbitrarily chosen value  here
		if nsel ne 0 then remove,sel,index
	endif
	array_old=array_tsub
    endif
endfor
;
; add velues to the imageb in  source subtracted regions so that the regions
; can be recognized in the program image_av.pro
;
sel=where(array_t ne 0. and array_b eq 0.,nsel)
if nsel ne 0 then array_b(sel)=array_t(sel)*1.e-10 ;arbitrary value
free_lun,unit
if !debug eq 1 then stop
stop
free_unit
return
end





