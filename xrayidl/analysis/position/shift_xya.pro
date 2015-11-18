pro shift_xya,x,y,xshift=xshift,yshift=yshift,ashift=ashift,noit=noit,center=center
;+
; making X, Y, and angular shifts of individual counts. 
; xshift, yshift, ashift - X, Y ( blocked pixels) and angle 
; (anti-clockwise, degree) shifts 
; noit - if set, the output x and y will not be rounded to become integers
; writen by wqd, June 8, 1994
; a small bug fixxed (replacing x and y with xx and yy in the middle of
;	of the calculation. wqd, June 23, 1994
; apply to large angle shift. wqd, Jan. 4, 2000
;-
if n_params () eq 0 then begin
print,'CALLING SEQUENCE -- shift_xya,x,y,xshift=xshift,yshift=yshift,ashift=ashift,noit=noit,center=center'
return
endif
ash=ashift*!pi/180
if n_elements(xshift)+n_elements(yshift)+n_elements(ashift) eq 0 then return
if n_elements(ashift) ne 0 then begin
 if keyword_set(center) ne 0 then begin
	xc=0
	yc=0
 endif else begin
 	case !instr of
   		'h' : begin
			yc=4096.5 & xc=4096.5
			end
   		'p' : begin
			yc=7680.5 & xc=7680.5
			end
   		's' : begin
			yc=4096.5 & xc=4096.5
			end
 	endcase
 endelse
 xx=xc+(x-xc)*cos(ash)-(y-yc)*sin(ash)
 yy=yc+(x-xc)*sin(ash)+(y-yc)*cos(ash)
endif else begin
     xx=x
     yy=y
endelse
if n_elements(xshift) ne 0 then xx=xx+xshift
if n_elements(yshift) ne 0 then yy=yy+yshift ;assuming in units of pixels
if keyword_set(noit) eq 0 then begin
	x=nint(xx) & y=nint(yy)
endif else begin
	x=xx & y=yy
endelse
stop
return
end

