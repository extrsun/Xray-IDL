;main procedure solar_mult
;-
; main program to calculate solar fluxes in overlapping images
; writen by wqd
; Note: Before the procedure is run for the first for a field, valuable
; noindex=0 should be set for excluding regions where no overlapping occurs.
;+
; setup the parameters
;
; the center of the array covering the whole field
;trans_radian,6,0,0,-71,2,0,center_ra,center_dec
trans_radian,19,1,0,-36,55,0,center_ra,center_dec ;R CrA field
blow=6
bhigh=7
array_size_x=35
;array_size_y=24
array_size_y=35
bin_size=16 ;4 arcmin
radius=60 ;radius used for individual images (in arcmin)
;exptail='all'
;soufile='/data1/wqd/archives/shadow/front_sou.dat'
;list_image='/data1/wqd/archives/shadow/frontfile'
exptail=''
soufile='/data1/wqd/archives/rcra/rcra_sou_b.dat'
list_image='/data1/wqd/archives/rcra/image_list'
;
; find the index where no overlapping happens
;
if noindex ne 1 then begin
index=lindgen(array_size_x,array_size_y)
mkmap_t,center_ra,center_dec,list_image,blow,bhigh,array_size_x,array_size_y $
,bin_size,ts,radius=radius,exptail=exptail,soufile=soufile,index=index
noindex=1
endif
;
; exclude these index
;
binmult=lindgen(array_size_x,array_size_y)
remove,index,binmult
nmult=n_elements(binmult)
;
; reads image sequence numbers and get the corresponding images
;
seq_no=''
openr,unit,list_image,/get_lun
readf,unit,nimage
;
image_size=radius*2.*(60./(!block*!size_pixel))
count=intarr(nmult,nimage)
back=fltarr(nmult,nimage)
expt=fltarr(nmult,nimage)
;
for k=0,nimage-1 do begin ;now read images from LIST_IMAGE
	readf,unit,seq_no
	form=strmid(seq_no,0,1)
	if strupcase(form) eq 'W' then 	env,strmid(seq_no,2,6),/mpe else $
		env,strmid(seq_no,2,6)
	get_image,image_t,image_c,image_tsub,blow=blow,bhigh=bhigh $
	   ,dim=image_size,factor=1.5,slow=3.5,exptail=exptail,soufile=soufile

	image_center,image_ra,image_dec

	c=where(image_tsub le 0.,nc)
	if nc ne 0 then image_c(c)=0 ;neccessary for merging images

	; get a particle background image
	tfile=!seq_no+'_gti'+strtrim(exptail,2)+'.dat'
	get_imageb,image_tsub,blow,bhigh,image_b,tfile=tfile

  	array_c=lonarr(array_size_x,array_size_y)
  	array_b=fltarr(array_size_x,array_size_y)
  	array_t=fltarr(array_size_x,array_size_y)
  	array_tsub=fltarr(array_size_x,array_size_y)

  	merge,center_ra,center_dec,image_ra,image_dec,array_size_x $
	,array_size_y,bin_size,array_c,image_c,array_b,image_b $
	,array_t,image_t,array_tsub,image_tsub

	count(0:nmult-1,k)=array_c(binmult)
	back(0:nmult-1,k)=array_b(binmult)
	expt(0:nmult-1,k)=array_tsub(binmult)
endfor
free_lun,unit
;
; least square fit to get solar fluxes
;
get_msolar,count,back,expt,solarf,solarferr,countmin=countmin
;
; add the solar fluxes into the image files (list_image+'_solar')
;
outfile=list_image+'_s'+strtrim(blow,2)+strtrim(bhigh,2)
solar_add,list_image,solarf,solarferr,outfile=outfile
;
free_unit
end