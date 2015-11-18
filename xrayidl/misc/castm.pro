pro cast,infile,outhead,outfile,outarr=outarr,inarr=inarr,inhead=inhead $
,nointerp=nointerp,galcoor=galcoor
;+
; program to cast an image into a new image characterized by the header 
;	outhead
; note: The pixel size of the new image should be equal to, or smaller than,
; that of the input image so that the interpretation will be meaningful
;
;    Inputs:   infile - input image.
;              outhead - output file containg specifics about the 
;		new image, i.e., x and y size, reference pixel, pixel size
;		center coordinates, and equinox, 
;		which can be produced by get_fitshead
;	       outfile - selected output of the output filename
;
;    Keywords: outarr  - output image array 
;		inarr - input image array (if set, infile will not
;			be used)
;              inhead - FITS header for input image (if set, infile will not
;			be used)
;		galcoor - if set, the input file in an Aitoff projection 
;			centered at (l, b)=(0, 0)
;
; writen by wqd, May 31, 1994
; the aitoff projection is added, 6/18/97
;-
;
if n_params() eq 0 then begin
	print,'CALLING SEQUENCE - cast,infile,outhead,outfile'
	print,',outarr=outarr,inarr=inarr,inhead=inhead'
	return
endif
;
get_headinfo,outhead,xsize,ysize,cra,cdec,cpx,cpy,delx,dely,equi=equi
if equi eq 0 then equi=2000 
;read,'please give the equinox of the output file',equi
if abs(delx) ne dely then print,'delx ne dely'
pixel=dely
if n_elements(inhead) eq 0 then inhead=headfits(infile)
get_headinfo,inhead,xsin,ysin,crain,cdecin,cpxin,cpyin,delxin,delyin $
	,equi=equiin,blank=blank
if abs(delxin) ne delyin then print,'delxin ne delyin'
pixelin=delyin
if equiin eq 0 then equiin=2000 
;read,'please give the equinox of the intput file',equiin
outarr = fltarr(xsize,ysize)
;
num=xsize*ysize
index_conv,lindgen(num),[xsize,ysize],indexv
xx = indexv(0,*) - (cpx-0.5) ;IDL image starts at 0 at a distance of 0.5 pixel
yy = indexv(1,*) - (cpy-0.5) ;from the array edge.
trans_loct,xx,yy,cra,cdec,ra,dec,pixsize=pixel*3600.,/deg
;

if equiin ne equi then begin
	sprecess,ra,dec,equi,equiin
endif
;
; determine the locations in the input image
if keyword_set(galcoor) ne 0 then $
	galtoait,ra,dec,xx,yy,xlon0=crain,pixsize=pixelin else $
	trans_dist,crain,cdecin,ra,dec,xx,yy,pixsize=pixelin*3600.,/deg
;
xx=xx+(cpxin-0.5)
yy=yy+(cpyin-0.5)
;
if n_elements(inarr) eq 0 then inarr = readfits(infile,silent=1) 
mm  = where( (xx le xsin-1) and (xx ge 0) and $
             (yy le ysin-1) and (yy ge 0) and inarr(xx,yy) ne blank, count)
if count ne 0 then begin
	if keyword_set(nointerp) then $
		outarr(mm)=inarr(nint(xx(mm)),nint(yy(mm))) else $
      		outarr(mm) = interpolate(inarr,xx(mm),yy(mm))
endif else begin
   stop,'!!! No correspondence found between the images !!!'
endelse
;
index = where( finite(outarr) eq 0 , count)
if count ne 0 then outarr(index)=blank
mx = max(outarr(where(outarr ne blank)), min = mn)
;
;
sxaddpar,outhead,'datamax',mx
sxaddpar,outhead,'datamin',mn
if n_elements(outfile) ne 0 then writefits,outfile,outarr,outhead
;
end
