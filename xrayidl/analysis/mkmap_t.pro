pro mkmap_t,center_ra,center_dec,list_image,blow,bhigh,array_size_x,array_size_y,bin_size,array_tsub,radius=radius,exptail=exptail $
,soufile=soufile,index=index,exptlimit=exptlimit,factor=factor,slow=slow,flow=flow
;-----------------------------------------------------------------
; merge a list of images (including count, background, exposure and 
;source-subtracted exposure images) into corresponding arraies.
;*INPUT:
; list_image - list of standard 6 digit file names containing the images
; blow, bhigh - lower and upper limits of the rosat pspc band 
; array_size_x,array_size_y - x and y dimension (number of bins) 
;				of the arrays
; bin_size - size of each array bin  (in units of image pixels)
; radius - the chosen  radius of the image (def = 60 arcmin)
; exptail - the exposure file specification (def = 'all')
; soufile - the source file (def uses individual source files)
; exptlimit - the limiting exposure of a bin to be considered useful
;*OUTPUT:
; array_tsub
; index - index in the array_tsub with no overlaping
; writen by WQD, 3/18/93
; -----------------------------------------------------------------
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - mkmap_t,center_ra,center_dec,list_image,blow,bhigh,array_size_x'
print,',array_size_y,bin_size,array_tsub,radius=radius'
print,',exptail=exptail,soufile=soufile,index=index,exptlimit=exptlimit,factor=factor,slow=slow,flow=flow'
return
endif
;
if n_elements(factor) eq 0. then factor=1.
if n_elements(slow) eq 0 then slow=3.5
if n_elements(flow) eq 0 then flow=0.
if n_elements(exptlimit) eq 0 then exptlimit=(bin_size/4.)^2*1.e4 
; 10^4 s arcmin^2 is only an arbitrarily chosen value for an effective expo
if n_elements(radius) eq 0 then  radius=60.
image_size=(radius*2.)*(60./!size_pixel/!block)

;array_c=lonarr(array_size_x,array_size_y)
;array_b=fltarr(array_size_x,array_size_y)
array_t=fltarr(array_size_x,array_size_y)
array_tsub=fltarr(array_size_x,array_size_y)
;
; reads image sequence numbers and get the corresponding images
	tail=''

	openr,unit,list_image,/get_lun
;;
readf,unit,nfile
if n_elements(exptail) eq 0 then exptail=strarr(nfile)+'all'
for k=0,nfile-1 do begin
;now read images from LIST_IMAGE
	readf,unit,tail
	seqno=gettok(tail,' ')
	if tail eq '0' then tail=''
	form=strmid(seqno,0,1)
	form=gettok(seqno,'p')
	if strupcase(form) eq 'W' then  $
		envset,seqno,tail=tail,/mpe else $
		envset,seqno,tail=tail
	get_image,image_t,image_c,image_tsub,blow=blow,bhigh=bhigh $
	,dim=image_size,factor=factor,slow=slow,flow=flow,exptail=exptail(k),soufile=soufile,/tonly

	image_center,image_ra,image_dec
;
  merge,center_ra,center_dec,image_ra,image_dec,array_size_x,array_size_y $
         ,bin_size,array_tsub,image_tsub
;
    if keyword_set(index) ne 0 then begin
	; to get the index where overlapping happens
	if k gt 0 then begin
		sel=where((array_tsub(index) - array_old(index) gt exptlimit $
		and array_old(index) ne 0.) ,nsel)
		if nsel ne 0 then remove,sel,index
	endif
	array_old=array_tsub
    endif
endfor
;
close,unit
if !debug eq 1 then stop
return
end





