pro rot_xy,x,y,angle,xshift=xshift,yshift=yshift,block=block,absshift=absshift,xpref=xpref,ypref=ypref,xyreal=xyreal
;-
; making X, Y, and angular shifts of individual counts. 
; xshift, yshift, ashift - X, Y (pixels) and angle (anti-clockwise, degree) 
;			shifts 
; xyreal - if set, output x and y are not converted into integer, used
;          by cast
;
; writen by wqd, Jun 8, 1994
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -- rot_xy,x,y,angle,xshift=xshift,yshift=yshift,block=block,absshift=absshift,xpref=xpref,ypref=ypref,xyreal=xyreal'
return
end

if n_elements(xshift)+n_elements(yshift)+n_elements(angle) eq 0 then return
if n_elements(angle) eq 0 then return
if n_elements(block) eq 0 then block=1.
if n_elements(ypref) eq 0 then ypref=7679.5
if n_elements(xpref) eq 0 then xpref=7679.5
angler=(angle*!pi/180.)
sa=sin(angler) & ca=cos(angler)
if n_elements(absshift) eq 0 then begin
 if n_elements(xshift) ne 0 then xpixmid=xpref+xshift else xpixmid=xpref
 if n_elements(yshift) ne 0 then ypixmid=ypref+yshift else ypixmid=ypref
endif else begin
 xpixmid=xshift*block
 ypixmid=yshift*block
endelse
xx=(x-xpixmid/block)
yy=(y-ypixmid/block)
x=xx*ca-yy*sa+(xpixmid/block) ;xx and yy cannot be replaced by x and y here
y=yy*ca+xx*sa+(ypixmid/block) ;because there are two equations

;if n_elements(absshift) eq 0 then begin
if n_elements(xyreal) eq 0 then begin
	x=nint(x) & y=nint(y)
endif
if !debug eq 2 then stop
return
end

