pro cast_dssimage,infile,outhead,outfile,outarr=outarr,inarr=inarr,inhead=subhead
;+
; program to cast a DSS image into a new image characterized by the header 
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
;              subhead - FITS header for input dss subimage (if set, infile will not
;			be used)
;
; writen by wqd, May 31, 1994
;-
;
if n_params() eq 0 then begin
	print,'CALLING SEQUENCE - cast_dssimage,infile,outhead,outfile'
	print,',outarr=outarr,inarr=inarr,inhead=subhead'
	return
endif
;
get_headinfo,outhead,xsize,ysize,cra,cdec,cpx,cpy,delx,dely,equi=equi
if equi eq 0 then read,'please give the equinox of the output file',equi
if abs(delx) ne dely then print,'delx ne dely'
pixel=dely
;
if n_elements(subhead) eq 0 then subhead=headfits(infile)
if n_elements(inarr) eq 0 then inarr = readfits(infile,silent=1) 
get_fitsheaddss,inarr,inhead,subhead,/dss
get_headinfo,inhead,xsin,ysin,crain,cdecin,cpxin,cpyin,delxin,delyin $
	,equi=equiin

blank=sxpar(inhead,'blank',count=count)
if n_elements(count) eq 0 then blank=-1. ;blank pixels will have values of -1.
outarr = replicate(blank,xsize,ysize) 
;
num=xsize*ysize
index_conv,lindgen(num),[xsize,ysize],indexv
xx = indexv(0,*) - (cpx-1.) ;IDL image starts at 0
yy = indexv(1,*) - (cpy-1.)
trans_loct,xx,yy,cra,cdec,ra,dec,pixsize=pixel*3600.,/deg
;
if equiin ne equi then sprecess,ra,dec,equi,equiin
;
; determine the locations in the input image
;
trans_dist,crain,cdecin,ra,dec,xxs,yys,/das,/deg
;
dss_platepix,xxs,yys,subhead,xx,yy
;
mm  = where( (xx le xsin-1) and (xx ge 0) and $
             (yy le ysin-1) and (yy ge 0), count)
if count ne 0 then begin
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
stop
end
