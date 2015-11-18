pro cast_r,outhead,outarr,inhead,inarr,mtype=mtype,offset=offset,infile=infile,nozero=nozero,append=append,avgkey=avgkey,roll=roll
;+
; Cast an image into a new image or append to an existing image
; characterized by the header outhead
;
; note: This program diffs from cast, reversing the casting: e.g., by
; first calculating positions of inarr in outarr and then 
; accumulating/avaraging the values.
;
;*IMPORTANT NOTE:*
; Thus, cast_r.pro should be used for the case in which the pixel size of the 
; new image is greater than that of the input image, while cast.pro should
; be used for the opposite situation. Specifically, the input image needs to
; have a pixel size that is smaller than that of the output image by a factor
; of at least sqrt(2). Otherwise, there will be pixels with no value!!! 
;
;*Inputs:
; inhead, inarr - input fits header and image (assuming the IDL format; i.e.,
;			refp=dim/2 if at the center of the array)
; infile - fits file name of the input image (used only when
;		inhead is not provided.
; mtype - if = 1, RA and Dec of the events are converted into 
;		Galactic coordinates
; offset - if the reference pixel of inhead is not in FORTRAN formation,
;	offset needs to be set (e.g., (e.g., my old IDL format; 
;	1024 for xdim=2048; offset =0)
;	default=1 (FORTRAN format)
; avgkey - if =1, average will be assigned to the output value, even for
;		integer input. (def: avgkey=1 for float or double input images
;		(type=4 or 5) 
; nozero - if =1, pixels with value=0 will not be casted 
; 		(def: nozero=0)
; roll - roll angle of the input images 
;
;*Outputs: 
; outhead, outarr - output fits header and image. The former can be produced 
;		by get_fitshead. The outarr will have the same type as the
;		input image, if avgkey is not set; othwerwise, type=1 (float).
;
; written by wqd 7/25,2001
; added a roll angle for conversion from the ccd frame to the sky frame
; to be called by streak_removal_main.pro, wqd, 12/31/2002
;-
if n_params() eq 0 then begin
print,'CALL SEQUENCE - cast_r,outhead,outarr,inhead,inarr,avgkey=avgkey'
print,',mtype=mtype,offset=offset,infile=infile,nozero=nozero,append=append,roll=roll'
print,'*IMPORTANT*:'
print,'Input pixel size has to be smaller than the output pixel by a factor of at least sqrt(2)!!!'
return
endif
if n_elements(append) eq 0 then append=0 
if n_elements(nozero) eq 0 then nozero=0 ;including zero value in the average
if n_elements(mtype) eq 0 then mtype=0
if n_elements(offset) eq 0 then offset=1
	;IDL image starts at 0 at a distance of 0.5 pixel

get_headinfo,outhead,xsize,ysize,cra,cdec,cpx,cpy,delx,dely,equi=equi
if n_elements(equi) eq 0 then begin
    print,'no EQUINOX info in the header, assuming J2000!!!'
    equi=2000 
endif
if abs(delx+dely) lt 1.e-6*dely then $
	pixelout=dely*3600. else stop,'delx not = dely'

if n_elements(inhead) eq 0 then inarr=readfits(infile,inhead)
sz=size(inarr)
if sz(0) ne 2 then begin
	print,'2-D array is needed!'
	return
endif
get_headinfo,inhead,xsin,ysin,crain,cdecin,cpxin,cpyin,delxin,delyin $
	,equi=equiin,blank=blank
if abs(delxin+delyin) lt 1.e-6*delyin then $
	pixelin=delyin*3600. else stop,'delxin not = delyin'
if pixelout lt (sqrt(2.)*pixelin) then $
	print,'***WARNING***: pixelout lt sqrt(2.)*pixelin'
if equiin eq 0 then equiin=2000 
;
inloc=lindgen(sz(4))
value=inarr
if nozero eq 1 then begin
	sel=where(inarr ne 0,nsel)
	if nsel eq 0 then begin
	 if append eq 1 then return else begin
		print,'no nozero value!'
		return
	 endelse
	endif else begin
		inloc=inloc(sel)
		value=value(sel)
	endelse
endif 
xp=(inloc mod xsin)-(cpxin-offset)
yp=inloc/xsin-(cpyin-offset)
if n_elements(roll) ne 0 then rot_xy,xp,yp,roll,block=1,xpref=0,ypref=0
trans_loct,xp,yp,crain,cdecin,crap,cdecp,/deg,pixsize=pixelin

if mtype eq 1 then glactc,crap,cdecp,equiin,crap,cdecp,1,/deg
trans_dist,cra,cdec,crap,cdecp,xp,yp,/deg,pixsize=pixelout
xp=nint(xp+(cpx-offset)) ;in the IDL coordinates
yp=nint(yp+(cpy-offset))
sel=where(xp ge 1 and xp le xsize and yp ge 1 and yp le ysize,nsel)
if nsel ne 0 then begin
	xp=xp(sel)
	yp=yp(sel)
	value=value(sel)
endif else begin
	 if append eq 1 then return else begin
		print,'no overlapping!'
		return
	endelse
endelse
vmerge,value,yp*xsize+xp,mvalue,mindex,mdup
type=sz(3)
if n_elements(avgkey) eq 0 then begin
	match,[0,type],[4,5],sel,count=nsel ;[0, is to make sure a vector
	if nsel ne 0  then avgkey=1 $ ;for real or double array, do the average
		else avgkey=0 		; no average otherwise
endif else type=4  ;float
if avgkey eq 1 then mvalue=mvalue/float(mdup)

if append eq 1 then outarr(mindex)=outarr(mindex)+mvalue $
else begin
	outarr=make_array(xsize,ysize,type=type)
	outarr(mindex)=mvalue
endelse
return
end
