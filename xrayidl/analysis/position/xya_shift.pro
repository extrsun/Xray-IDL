pro xya_shift,x,y,xshift,yshift,ashift,xx,yy,refp=refp
;+
; making X, Y, and angular shifts of positions for pixels or sources
; x, y - input x and y positions
; xshift, yshift, ashift - X, Y (in the same units as x and y) and angle 
; (anti-clockwise, degree) shifts 
; xx, yy - output x and y positions
; refp - reference pixel position (def = [0.,0.]), which should be consistent
;	with the calculation of x and y
;
; a simplified version of the original shift_xya, writen by wqd, June 16, 2002
;-
if n_params () eq 0 then begin
print,'CALLING SEQUENCE -- xya_shift,x,y,xshift,yshift,ashift,xx,yy,refp=refp'
return
endif
if n_elements(ashift) ne 0 then begin
 	if n_elements(refp) eq 0 ne 0 then refp=[0.,0.]
 	ash=ashift*!pi/180.
 	xx=refp(0)+(x-refp(0))*cos(ash)-(y-refp(1))*sin(ash)
 	yy=refp(1)+(x-refp(0))*sin(ash)+(y-refp(1))*cos(ash)
endif else begin
     xx=x
     yy=y
endelse
xx=xx+xshift
yy=yy+yshift
return
end

