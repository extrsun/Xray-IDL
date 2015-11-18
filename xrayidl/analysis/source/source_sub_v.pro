;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME
;	source_sub_v
;
;*PURPOSE:
;	Function source_sub sets values of pixels contaminated
; by sources into zero. An exposure image should be used. 
;
;*CALLING SEQUENCE:
; image_new=source_sub_v(image,image_ra,image_dec,sra,sdec,cntr
;                  ,blow=blow,bhigh=bhigh,block=block,edge_side=edge_side
;		   ,factor=factor,subvalue=subvalue,cntrlimit=cntrlimit
;		,xcpix=xcpix,ycpix=ycpix,sradius=sradius,deg=deg
;		,psffile=psffile,cra=cra,cdec=cdec)
;
;*PARAMETERS:
; INPUTS:
;       image - any image (e.g., ROSAT exposure image). If not centered
;		on the norminal pointing position, xcpix and ycpix should
;		be specified.
;	xcpix,ycpix - the x and y coordinate of the norminal pointing position
;		relative to the lower left corner pixel in the image 
;		pixel coordinate
;		(e.g., xcpix=-(xp-45.5) where 45.5=(100-1)*0.5 and xp
;		is from trans_dist,cra,cdec,ra,dec,xp,yp,/deg,pix=?)
;	sradius - source subtraction radius in units of original pixels
;		(changed from image bins on 7/26/03)
;	image_ra,image_dec - RA and DEC of the image (radian)
;	sra,sdec - RA and DEC of the sources (radian)
;	cntr - count rates of the sources used for estimating the subtraction
;		radius
; 	blow, bhigh - lower and upper boundaries of the PSPC bands used
;		to construct the images (def = 4, 7)
;    	block - the size of the image pixel (in units of 0''.5 ). default=30
;	edge_side - the one-side size of the region (in the image )
;		    which is not used. default=0 pixels
;	factor - the fraction of the default radii in source subtractions
;	subvalue - the value used to give in the image regions where sources
;		are subtracted (def = 0.). A non-zero value may be used
;		to identify regions of the sources.
;	deg - if set, the input sra and sdec etc are assumed to be in units
;		of degrees
; 	cntrlimit - the maximum cntr allowed to be outside the subtracting
;		radius of a point source (def = 5e-3). Thus, for a 
;		bright source, a large radius is used.
; 	perclimit - the percentage limit (def = 0.9) to calculate the smallest
;		radii for source subtraction
;	outr - source radius in units of the image bin size
;	cra,cdec - the axis RA and Dec (deg)
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
;	detect_params
;
;
;*MODIFICATION HISTORY:
; writen 3 AUG 1992 (WQD)
; using detection parameters and cntr to estimate the subtraction radius
; Nov 30, 1992 WQD
; add blow and bhigh parameters to be passed to detect_params. 
; WQD Sept 11, 1993
; include the keywords: xcpix,ycpix, and sradius, so that the image does
; not have to be centered on the pointing position. wqd, Dec. 16, 1994
; Source subtraction radii are now calculated to not allow more than
; a limiting cntr (set by cntrlimit). wqd, April 18, 1996
;-
;---------------------------------------------------------------------------
function source_sub_v,image,image_ra,image_dec,sra,sdec,cntr $
          ,block=block,edge_side=edge_side $
          ,factor=factor,subvalue=subvalue,xcpix=xcpix,ycpix=ycpix $
	  ,sradius=sradius,deg=deg,cntrlimit=cntrlimit,perclimit=perclimit $
	,psffile=psffile,outr=outr,cra=cra,cdec=cdec,blow=blow,bhigh=bhigh,spfile=spfile,instr=instr,roll=roll,sel=sel
;-----------------------------------------------------------------------------
sz=size(image)
if n_elements(block) EQ 0 then block=!block
xdim=sz(1)
ydim=sz(2)
;if n_elements(cntrlimit) eq 0 then cntrlimit=1.e-2
if n_elements(edge_side) eq 0 then edge_side=0
if n_elements(subvalue) eq 0 then subvalue=0.
;
nbin_o=n_elements(where(image NE 0))
image_sub=image
;
; gets the locations of the sources in the image in pixels
;
trans_dist,image_ra,image_dec,sra,sdec,xp,yp,deg=deg
if n_elements(roll) ne 0 then rot_xy,xp,yp,roll,block=1,xpref=0,ypref=0
;
if n_elements(xcpix) eq 0 then xcpix=(xdim-1.)/2.
if n_elements(ycpix) eq 0 then ycpix=(ydim-1.)/2.
xp=xp/block+xcpix
yp=yp/block+ycpix


;
; finds the size of the regions which are contaminated by the sources
;
if n_elements(sradius) eq 0 then begin
	if n_elements(perclimit) eq 0 then perclimit=0.85
;	print,'perclimit, cntrlimit = ',perclimit, cntrlimit
	if n_elements(cra) eq 0 then cra=image_ra
	if n_elements(cdec) eq 0 then cdec=image_dec
	trans_dist,cra,cdec,sra,sdec,cxp,cyp,/das,/deg
	dis=sqrt(cxp^2+cyp^2)/60. ;off-axis angle in units of arcmin
endif else rso=sradius
psf_params,dis,sradius,cntr,perclimit=perclimit,psffile=psffile $
 ,cntrth=cntrlimit,blow=blow,bhigh=bhigh,spfile=spfile,instr=instr,rso=rso
sradius=sradius/block 
if n_elements(factor) ne 0 then sradius=sradius*factor
sradius=sradius+0.5 > 1
	;including bins with center just outside the source radius
outr=sradius
;
imin=nint(xp-sradius) > edge_side
imax=nint(xp+sradius) < (xdim-1-edge_side) 
jmin=nint(yp-sradius) > edge_side
jmax=nint(yp+sradius) < (ydim-1-edge_side)
;
; if no source is in the image, return
;
sel=where (imin LE imax AND jmin LE jmax, count)
if count eq 0 then begin

print,'No sources are covered in the image.'  & goto,out 
endif
;
print,'Number of sources effecting the image is ',count
;
ssradius=sra*0.+sradius^2 ; to make sure that ssradius is a vector
subvalue=sra*0.+subvalue
for k=0,(count-1) do begin
n=sel(k)
 for j=jmin(n),jmax(n) do begin
	ys=(j-yp(n))^2
 for i=imin(n),imax(n) do begin
	dis=(i-xp(n))^2+ys
	if dis le ssradius(n) and image_sub(i,j) ne 0. then $
	image_sub(i,j)=subvalue(n) ;for identifying
						     ;source regions
 endfor & endfor
endfor
;
print,'number of bins changed = ',n_elements(where((image_sub-image) ne 0))
out:
;
return,image_sub
end
