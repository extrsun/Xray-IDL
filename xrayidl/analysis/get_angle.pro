pro get_angle,dis,angle,dimx,dimy,cpx,cpy,disonly=disonly
;-
; calculate angles relative to a user specified pixel center and the norminal
; x axis (anti-clockwise). 
; dimx, dimy - the x and y dimesions of an image
; cpx, cpy - the pixel center of the image
; angle - the output image with values equal to the calculated angles
; dis - the distances of pixels from the center
;
; writen by wqd Sept 29, 1993
;+
if n_params() eq 0 then begin
print,' CALLING SEQUENCE - get_angle,dis,angle,dimx,dimy,cpx,cpy,disonly='
return
endif
dimx=long(dimx)
dimy=long(dimy)
if n_elements(cpx) eq 0 then cpx=(dimx-1.)*0.5 else cpx=float(cpx)
if n_elements(cpy) eq 0 then cpy=(dimy-1.)*0.5 else cpy=float(cpy)
c=lindgen(dimx*dimy)
y=c/dimx & x=c mod dimx	
dis=fltarr(dimx,dimy)
dis(*)=sqrt((x-cpx)*(x-cpx)+(y-cpy)*(y-cpy)) > 1.e-10
if keyword_set(disonly) ne 0 then return
angle=fltarr(dimx,dimy)
sa=(cpx-x)/dis 
ca=(y-cpy)/dis 
angle(*)=asin(sa)*(180./!pi)
sel=where(ca lt 0.,nsel)
if nsel ne 0 then  angle(sel)=180-angle(sel)
sel=where(sa lt 0. and ca ge 0.,nsel)
if nsel ne 0 then  angle(sel)=360+angle(sel)
end