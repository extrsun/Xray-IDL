;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME:
; dis_flux_1d
;
;*PURPOSE:
; Obtain a 1-D surface brightness profile in an image
;
;*CALLING SEQUENCE:
; dis_flux_1d,image_c,dist,flux,image_t=image_t,xpref=xpref,ypref=ypref
; ,xmin=xminy,xmax=xmax,ymin=ymin,ymax=ymax,plotoff=plotoff,climit=climit
; ,filter=filter,xdivs=xdivs,xabs=xabs,angle=angle,rim=rim
;
;*PARAMETERS:
; INPUTS:
; image_c -  image 
; image_t - effective exposure image or or its equivelent
; filter - If provided, pixels with value = 0 will
;         not be included in the calculation. 
; xmin, xmax - the lower and upper boundaries of the strip of the
;              image (def =0,sz(1))
; ymin, ymax - the lower and upper pixel limits after the rotation
;         (def =0,sz(1))
; angle - rotaion angle (deg, anti-clockwise), if provided, the image 
;         will be rotated first
; xpref, ypref - respectively,  the x, y reference pixel positions of 
;   the rotation center. xpref is also used for the distance reference in
;    the profile (def=(sz(1)-1)/2., (sz(2)-1)/2.)
; xdivs - the number of bins that the x-axis is going to be divided (def=30)
; plotoff - if set, the program will not creat the rbp plot on the screen
; climit - minimum accumulated value per profile bin for the
;          calculation. (def=0)
;
;*OPTIONAL OUTPUTS:
; dist - the vector contains the distance from xref
; flux - surface brightness profile
; rim - output image for viewing image bins that are used in the
;       calculation (after the rotation)
;*PROCEDURE:
; obvious
;
;*NOTES:
; The default xmax,ymin,xmax,ymax takes care of the image rotation
; effects at image edges
;
;
;*MODIFICATION HISTORY:
; writen wqd, May 18, 2004
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro dis_flux_1d,image_c,dist,flux,image_t=image_t,xpref=xpref,ypref=ypref,xmin=xminy,xmax=xmax,ymin=ymin,ymax=ymax,plotoff=plotoff,climit=climit,filter=filter,xdivs=xdivs,xabs=xabs,angle=angle,rim=rim
;---------------------------------------------------------------
if n_params() EQ 0 then begin
print,' CALL SEQUENCE -  dis_flux_1d,image_c,dist,flux,image_t=image_t'
print,',xpref=xpref,ypref=ypref,xmin=xminy,xmax=xmax,ymin=ymin,ymax=ymax'
print,',plotoff=plotoff,climit=climit,filter=filter,xdivs=xdivs,xabs=xabs'
print,',angle=angle,rim=rim'
return
endif
;---------------------------------------------------------------
sz=size(image_c) 
szmin=(sz(1) < sz(2))
if n_elements(xmin) eq 0 then xmin=0. 
if n_elements(xmax) eq 0 then xmax=sz(1)
if n_elements(ymin) eq 0 then ymin=0. 
if n_elements(ymax) eq 0 then ymax=sz(2)
if n_elements(xpref) EQ 0 then xpref=(sz(1)-1)/2.
if n_elements(ypref) EQ 0 then ypref=(sz(2)-1)/2.
if n_elements(climit) eq 0 then climit=0
if n_elements(image_t) eq 0  then image_t=fltarr(sz(1),sz(2))+1. 
if n_elements(filter) eq 0 then filter=image_t 
if n_elements(xdivs) eq 0 then begin
    xdivs=30.
    print,'assuming xdivs = ',xdivs
endif
ringwidth=(xmax-xmin)/xdivs
count=fltarr(xdivs)
dist=fltarr(xdivs)
expt=fltarr(xdivs)

loc=where(filter gt 0., n_bin)
xx=loc mod sz(1)
yy=loc/long(sz(1))
if n_elements(angle) ne 0 then begin
  rot_xy,xx,yy,angle,xpref=xpref,ypref=ypref
endif
sel=where(xx ge xmin and xx lt xmax and yy ge ymin and yy lt ymax,nsel)
;this is needed to avoid the aliases from the image rotation
  if nsel eq 0 then stop,'No data covered'
xx=xx(sel)
yy=yy(sel)
  rloc=yy*long(sz(1))+xx
  rim=image_c*0
  ss=loc(sel)
  rim(rloc)=image_c(ss)
if !debug eq 3 then stop
if keyword_set(xabs) ne 0 then xx=abs(xx-xpref)
  dis=xx
  rad=fix((dis-xmin)/ringwidth)
  vec_c=image_c(ss)
  time=image_t(ss)

  for n=0L,nsel-1 do begin
	nn=rad(n)
	count(nn)=count(nn)+vec_c(n)
	dist(nn)=dist(nn)+dis(n)*vec_c(n) 
	expt(nn)=expt(nn)+time(n)
  endfor

  good=where(expt GT 0 and count gt climit)
  flux=(count(good))/expt(good)
  dist=dist(good)/count(good)
  expt=expt(good)
;
  if keyword_set(plotoff) eq 0 then plot,dist,flux
return
end 
