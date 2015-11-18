pro filter_spatial,list,filter,xmin=xmin,ymin=ymin,block=block,det=det $
,val=val
;+
; conduct spatial filtering of the photon list with an input spatial filter
; only photons located in regions with a specified value in the filter are 
; selected.
; writen by wqd, Nov 15, 1992
; change the keyword neg into val to accormodate more choices. wqd, 9/30/99
;-
if N_params() eq 0 then begin
print,'CALLING SEQUENCE --- '
print,'filter_spatial,list,filter,xmin=xmin,ymin=ymin,block=block,det=det,val=val'
return
endif
;
if (n_elements(block) eq 0) then block = !block     ;use !block as default
if n_elements(xmin) eq 0 then xmin=0
if n_elements(ymin) eq 0 then ymin=0
;
if n_elements(val) eq 0 then val=0.
;
if n_elements(xp) eq 0 then begin
 if n_elements(det) ne 0 then begin
	pixconv=2.595021e-4/1.388889e-4
	xp=list.dx*pixconv & yp=list.dy*pixconv
 endif else begin
	xp=list.x & yp=list.y
 endelse
endif
s=size(filter) & nxfilter=s(1)
;compute Location indices.
xp=xp-xmin
yp=yp-ymin
Loc = long(xp/block) + long(yp/block) * long(nxfilter)
;
c=where(filter(loc) eq val,nc) 
if nc ne 0 then list=list(c) else list=-1
return
end