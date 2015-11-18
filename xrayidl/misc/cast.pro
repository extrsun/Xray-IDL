pro cast,infile,outhead,outfile,outarr=outarr,inarr=inarr,inhead=inhead $
,interp=interp,gc=gc,offset=offset,roll=roll,inroll=inroll,psum=psum,nointerp=nointerp
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
;		gc - if =1, the input file in an Aitoff projection 
;			centered at (l, b)=(0, 0) to the Ra and Dec projection
;			if = 2, the input file in the regular galactic 
;			projection, similar to that of Ra and Dec, to the
;			same Ra and Dec  coordinates
;			if =3, the input file in the Ra and Dec projection
;			into Galactic project.
; roll - angle (deg, anti-clockwise) to be rotated after the cast 
; psum - if set, pixel size area will be normalized as in a
;        backgrround count image
;
; offset - if the reference pixel of inhead is not in FORTRAN formation,
;	offset needs to be set (e.g., (e.g., my old IDL format; 
;	1024 for xdim=2048; offset =0)
;	default=1 (FORTRAN format)
;		nointerp - if set, no interplation will be performed.
;
; writen by wqd, May 31, 1994
; the aitoff projection is added, 6/18/97
; regular galactic projection is added. 7/21/98
;-
;
if n_params() eq 0 then begin
	print,'CALLING SEQUENCE - cast,infile,outhead,outfile'
	print,',outarr=outarr,inarr=inarr,inhead=inhead,offset=offset'
	print,',nointerp=nointerp,gc=gc,roll=roll,inroll=inroll,psum=psum'
	return
endif
;
if n_elements(gc) eq 0 then gc=0
get_headinfo,outhead,xsize,ysize,cra,cdec,cpx,cpy,delx,dely,equi=equi,n3d=3 
		;force the input image to be a 2-D image 
if equi eq 0 then equi=2000
pixelas=[-delx,dely]*3600.0d
;if n_elements(inhead) eq 0 then inhead=headfits(infile)
if n_elements(inarr) eq 0 then inhead=headfits(infile)
get_headinfo,inhead,xsin,ysin,crain,cdecin,cpxin,cpyin,delxin,delyin $
	,equi=equiin,blank=blank,n3d=3 
pixelinas=[-delxin,delyin]*3600.0d
if equiin eq 0 then equiin=2000 
;----------------------------------------------------
; If the images are center-referenced, it is easy to exclude images
; that are surelly not overlap:
if (abs(xsin*0.5-cpxin+0.5)+abs(ysin*0.5-cpyin+0.5) $
    +abs(xsize*0.5-cpx+0.5)+abs(ysize*0.5-cpy+0.5))lt 1 then begin
 case gc of 
 1: galtoait,cra,cdec,xx,yy,xlon0=crain,pixsize=3600.0d ;in units of arcsec
 2: begin 
	glactc,cra,cdec,equi,ra2,dec2,1,/deg
	trans_dist,crain,cdecin,ra2,dec2,xx,yy,/das,/deg
	;the central coodinate of the existing image (crain,cdecin) is 
	; in galactic coordinates.
    end
 3: begin 
	glactc,ra2,dec2,equiin,cra,cdec,2,/deg
	trans_dist,crain,cdecin,ra2,dec2,xx,yy,/das,/deg
	;the central coodinate of the existing image (crain,cdecin) is 
	;in Ra and Dec
    end
 else: begin
	if equiin ne equi then begin
            ;the error in PRECESS for converting between
            ;       B1950 and J1950 can be up to 1.5"
            if equiin eq 1950 and equi eq 2000 then begin
                print,'using bprecess for a better accuracy'
              bprecess,cra,cdec,cdra,cdec
          endif else sprecess,cra,cdec,equi,equiin
        endif 
	trans_dist,crain,cdecin,cra,cdec,xx,yy,/das,/deg
	end
 endcase
 if (xx^2+yy^2) gt ((cpxin*pixelinas(0)+cpx*pixelas(0))^2 $
        +(cpyin*pixelinas(1)+cpy*pixelas(1))^2) then return
endif 
;-----------------------------------------------------
num=xsize*ysize
index_conv,lindgen(num),[xsize,ysize],indexv
if n_elements(offset) eq 0 then offset=1
xx = (indexv(0,*) - (cpx-offset))*pixelas(0)
yy = (indexv(1,*) - (cpy-offset))*pixelas(1) ;from the array edge.
if n_elements(roll) ne 0 then rot_xy,xx,yy,-roll,block=1,xpref=0,ypref=0,/xyreal
trans_loct,xx,yy,cra,cdec,ra,dec,/das,/deg
;
; determine the locations in the input image
case gc of 
 1: galtoait,ra,dec,xx,yy,xlon0=crain,pixsize=3600.0d
 2: begin 
	if equiin eq 1950 then glactc,ra,dec,equi,ra2,dec2,1,/deg,/fk4 $
          else glactc,ra,dec,equi,ra2,dec2,1,/deg,/fk4 
	trans_dist,crain,cdecin,ra2,dec2,xx,yy,/das,/deg
	;the central coodinate of the existing image (crain,cdecin) is 
	; supposed to be in galactic coordinates.
    end
 3: begin 
	if equiin eq 1950 then glactc,ra2,dec2,equiin,ra,dec,2,/deg,/fk4 $
          else glactc,ra2,dec2,equiin,ra,dec,2,/deg
	trans_dist,crain,cdecin,ra2,dec2,xx,yy,/das,/deg
	;the central coodinate of the existing image (crain,cdecin) is 
	; supposed to be in Ra and Dec
    end
 else: begin
	if equiin ne equi then begin
            ;the error in PRECESS for converting between
            ;       B1950 and J1950 can be up to 1.5"
            if equiin eq 1950 and equi eq 2000 then begin
                print,'using bprecess for a better accuracy'
              bprecess,ra,dec,ra,dec
          endif else sprecess,ra,dec,equi,equiin
        endif 
	trans_dist,crain,cdecin,ra,dec,xx,yy,/das,/deg
	end
endcase
;
if n_elements(inroll) ne 0 then rot_xy,xx,yy,inroll,block=1,xpref=0,ypref=0,/xyreal
xx=xx/pixelinas(0)+(cpxin-offset)
yy=yy/pixelinas(1)+(cpyin-offset)
mm  = where( (xx lt xsin) and (xx ge 0) and $
             (yy lt ysin) and (yy ge 0), count)
;and inarr(xx,yy) ne blank, count)
if n_elements(nointerp) eq 0 then interp=1 else interp=0
if count ne 0 then begin
    outarr = fltarr(xsize,ysize)
    if n_elements(inarr) eq 0 then inarr = readfits(infile,silent=1) 
;    if keyword_set(nointerp) ne 0 then begin
    case interp of 
        0: begin
            xx=nint(xx)
            yy=nint(yy)
            outarr(mm)=inarr(xx(mm),yy(mm)) 
        end
        1: outarr(mm) = interpolate(inarr,xx(mm),yy(mm))
        else: outarr(mm) = interpolate(inarr,xx(mm),yy(mm),cubic=-0.5)
    endcase
endif else begin
   stop,'!!! No correspondence found between the images !!!'
   if !debug eq 1 then stop
   return
endelse
;
index = where( finite(outarr) eq 0 , count)
if count ne 0 then outarr(index)=blank
ss=where(outarr ne blank,nss)
if nss ne 0 then mx = max(outarr(ss), min = mn) else begin 
    mx=blank
    mn=blank
endelse
;
sxaddpar,outhead,'datamax',mx
sxaddpar,outhead,'datamin',mn
if keyword_set(psum) then $
  outarr=outarr*(pixelas(0)*pixelas(1)/(pixelinas(0)*pixelinas(1)))
if n_elements(outfile) ne 0 then writefits,outfile,outarr,outhead
return
end
