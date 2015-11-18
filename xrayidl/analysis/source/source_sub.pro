;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME
;	source_sub
;
;*PURPOSE:
;	Function source_sub sets values of pixels contaminated
; by sources into zero. An exposure image should be used. 
;
;*CALLING SEQUENCE:
; image_new=source_sub(image,image_ra,image_dec,source_ra,source_dec, 
;                  block=block,edge_side=edge_side
;		   ,factor=frac,subvalue)
;
;*PARAMETERS:
; INPUTS:
;       image - a ROSAT exposure image
;	image_ra,image_dec - RA and DEC of the image (radian)
;	source_ra,source_dec - RA and DEC of the sources (radian)
;    	block - the size of the image pixel (in units of 0''.5 ). default=30
;	edge_side - the one-side size of the region (in the image )
;		    which is not used. default=0 pixels
;	factor - the fraction of the default radii in source subtractions
;	subvalue - the value used to give in the image regions where sources
;		are subtracted (def = 0.). A non-zero value may be used
;		to identify regions of the sources.
;
; OUTPUTS
;	image
;
;*PROCEDURE:
;	finds the locations of the sources in the image in pixels and
;	the regions which are contaminated by the sources, and sets
; 	the values of the regions into zero.
;
;*EXAMPLES:
;
;*RESTRICTIONS:
;
;
;*NOTES:
;	Source subtractions in other images such as count and background
;	images can be easily done with the output of the exposure image
; 	from this function
;
;*SUBROUTINES CALLED:
;	trans_loct
;
;
;*MODIFICATION HISTORY:
; writen 3 AUG 1992 (WQD)
;
;-
;---------------------------------------------------------------------------
function source_sub,image,image_ra,image_dec,source_ra,source_dec $
          ,block=block,edge_side=edge_side $
          ,factor=frac,subvalue=subvalue
;-----------------------------------------------------------------------------
if n_params() EQ 0 then begin
	print,'CALLING SEQUENCE - source_sub (image,image_ra,image_dec,'
	print,'                   source_ra,source_dec,[blcok,'
	print,'                   edge_side,factor,subvalue=])'
	retall
endif
;
sz=size(image)
if n_elements(block) EQ 0 then block=!block
xdim=sz(1)
ydim=sz(2)
if n_elements(edge_side) eq 0 then edge_side=0
if n_elements(frac) eq 0 then frac=1.
if n_elements(subvalue) eq 0 then subvalue=0.
;
nbin_o=n_elements(where(image NE 0))
image_sub=image
;
; gets the locations of the sources in the image in pixels
;
trans_dist,image_ra,image_dec,source_ra,source_dec,xp,yp
;
xcpix=(xdim-1.)/2.
ycpix=(ydim-1.)/2.
xp=xp/block+xcpix
yp=yp/block+ycpix
;
; finds the size of the regions which are contaminated by the sources
;
dis=sqrt((xp-xcpix)*(xp-xcpix)+(yp-xcpix)*(yp-ycpix))*block/!ampix
;boxhalf=xp*0.+90./(block*!size_pixel)*frac ;the coefficients selected here are
boxhalf=72.*(1.+dis/20.)/(block*!size_pixel)*frac
;c=where(dis gt 20.,count)            ;quite arbitrary
;if count ne 0 then boxhalf(c)=(90.+12.*(dis(c)-20.))/(block*!size_pixel)*frac
;
; defines the boundaries of the the regions
;
imin=fix(xp-boxhalf+1.0) > edge_side
imax=fix(xp+boxhalf) < (xdim-1-edge_side) 
jmin=fix(yp-boxhalf+1.0) > edge_side
jmax=fix(yp+boxhalf) < (ydim-1-edge_side)
;
; if no source is in the image, return
;
c=where (imin LE imax AND jmin LE jmax, count)
if count eq 0 then begin
print,'No sources are covered in the image.'  & goto,out 
endif
;
print,'Number of sources effecting the image is ',count
;
for k=0,(count-1) do begin
n=c(k)
 for j=jmin(n),jmax(n) do begin
	ys=(j-yp(n))*(j-yp(n))
 for i=imin(n),imax(n) do begin
	dis=sqrt((i-xp(n))*(i-xp(n))+ys)
	if dis le boxhalf(n) and image_sub(i,j) ne 0. then $
	image_sub(i,j)=subvalue ;for identifying
						     ;source regions
 endfor & endfor
endfor
;
print,'number of bins set to zero = ',nbin_o - n_elements(where(image_sub ne 0))
out:
if !debug eq 2 then stop
;
return,image_sub
end