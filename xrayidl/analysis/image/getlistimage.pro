;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME:
; GETLISTIMAGE
;
;*PURPOSE:
; make a photon list, spatially filter it (if chosen) with a filter, and 
; create an image (if chosen) ;
;
;*CALLING SEQUENCE:
; getlistimage,list,image,dim=dim,xcenter=xcenter
; ,ycenter=ycenter,emin=emin,emax=emax,tmin=tmin,tmax=tmax,filter=filter
; ,selneg=selneg,block=block,xmin=xmin,ymin=ymin,tfile=tfile,det=det
; ,xytpionly=xytpionly,listinfo=listinfo
;
;*PARAMETERS:
; INPUTS:
; dim - dimension of the intended image. SHOULD BE IN THE UNBLOCKED UNIT (0".5)
; xcenter, ycenter - the center position of the image (0".5 per pixel)
;			default = (7679.5,7679.5)
; emin,emax - the lower and upper limits of the ADC channels 
;		(default uses make_list.def: 11-247)
; tmin,tmax - the lower and upper limits of the observing time
;		default: tmin=-1 for time filtering with time intervals 
;		contained in *_gti.dat
; filter - the filter used to select the counts. Only counts are selected
;	in regions with filter  values (or negative values if /selneg is set)
; block - the block size of the filter image.
; det - if set, the image will be in the detector coordinate
;
;*OUTPUTS:
; list - the count list
; image - the image created with the list
;
;*PROCEDURE:
; obvious
;
;*EXAMPLES:
; getlistimage,list,image,dim=4200,emin=132,emax=201,filter=image_t
;
;*RESTRICTIONS:
;
;
;*NOTES:
;
;
;*SUBROUTINES CALLED:
; make_list
; filter_spatial
; list_image
;
;*MODIFICATION HISTORY:
; writen by WQD, Nov 20, 1992
;
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro getlistimage,list,image,dim=dim,xcenter=xcenter,ycenter=ycenter,emin=emin,emax=emax,tmin=tmin,tmax=tmax,filter=filter,selneg=selneg,block=block,xmin=xmin,ymin=ymin,tfile=tfile,det=det,xytpionly=xytpionly,xshift=xshift,yshift=yshift,ashift=ashift,listinfo=listinfo,phaorpi=phaorpi,extyp=extyp,refpixel=refpixel
;
if n_params() eq 0 then begin
print,'CALL SEQUENCE - getlistimage,list,image'
print,',[,dim (units=0".5)=,xcenter=,ycenter=,emin=,emax=,tmin=tmin,tmax=tmax'
print,',filter=filter,selneg=selneg,block=block,xmin=,ymin=,tfile=,xytpionly='
print,',xshift=xshift,yshift=yshift,ashift=ashift,listinfo=listinfo,extyp=extyp,refpixel=refpixel]'
return
endif
;
if n_elements(block) eq 0 then block=!block
inputs='obseq='+strtrim(!seq_no,2)+',dir='+!data_dir+',proc='+strtrim(!proc,2)
print,'get the count list from ',inputs
;
if !instr eq 'p' then refpixel=7679.5 else refpixel=4095.5
if n_elements(xcenter) eq 0 then xcenter=refpixel
if n_elements(ycenter) eq 0 then ycenter=refpixel

if n_elements(dim) ne 0 then begin 
	hdim=dim/2			 
	xmin=nint(xcenter-hdim) > 0
	xmax=fix(xcenter+hdim)
	ymin=nint(ycenter-hdim) > 0
	ymax=fix(ycenter+hdim)

	inputs=inputs+',xmin='+strtrim(xmin)+',xmax='+strtrim(xmax) $
        +',ymin='+strtrim(ymin)+',ymax='+strtrim(ymax)
endif else begin
	dim=512*30
	xmin=0
	ymin=0
endelse

if n_elements(emin) eq 0 then begin
	if !instr eq 'h' then emin=0 else emin=20
endif 
inputs=inputs+',emin='+strtrim(emin)
if n_elements(emax) eq 0 then begin
	if !instr eq 'h' then emax=0 else emax=201
endif 
 inputs=inputs+',emax='+strtrim(emax)

if n_elements(tmin) eq 0 then tmin=-1
inputs=inputs+',tmin='+strtrim(tmin,2)
if n_elements(tmax) ne 0 then inputs=inputs+',tmax='+strtrim(tmax,2)
;
if n_elements(phaorpi) ne 0 then inputs=inputs+',phaorpi='+strtrim(phaorpi,2)
make_list,inputs,list,listinfo,tfile=tfile,xytpionly=xytpionly $
	,xshift=xshift,yshift=yshift,ashift=ashift,extyp=extyp
if !debug eq 1 then stop
if n_elements(filter) ne 0 then $
	filter_spatial,list,filter,xmin=xmin,ymin=ymin,block=block
if n_params() gt 1 then $
	list_image,list,xmin,ymin,image,fix(dim/block),block=block,det=det
;
end
