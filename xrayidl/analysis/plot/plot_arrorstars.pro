pro plot_arrorstars,corner,x1o,y1o,x2o,y2o,star_label=star_labelo,color=color,noarror=noarror,cs=cs,yfac=yfaco,line=line,sel=sel
;-
;**
;** Plot arrows and stars in cont_grey image.
;**
;** INPUTS:
;**		corner	 == cont_grey output containing normalized
;**			    coordinates of image corners;
;**		x1,y1,x2,y2 == coordinates of the begining points and
;**			end points obtained using cursor_cor.pro
;**		starlabel == labels for stars.
;**		line - line style
;** Written by wqd, Jan 26, 1995
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - plot_arrorstars,corner,x1,y1,x2,y2,star_label=star_label,color=color,noarror=noarror,cs=cs,sel=sel'
return
endif
x1=x1o & y1=y1o & x2=x2o & y2=y2o
if n_elements(sel) ne 0 then msel,sel,x1,y1,x2,y2
if n_elements(star_labelo) ne 0 then begin
    star_label=star_labelo
    if n_elements(sel) ne 0 then msel,sel,star_label
endif 
if n_elements(yfaco) ne 0 then begin
    yfac=yfaco
    if n_elements(sel) ne 0 then msel,sel,yfac
endif
if n_elements(cs) eq 0 then cs=2
nstar = n_elements(x2)
if n_elements(color) eq 0 then color = replicate(1,nstar) else begin
	if n_elements(color) eq 1 then color=x1*0.+color
endelse
nx1 = corner(0) & nx2 = corner(1)
ny1 = corner(2) & ny2 = corner(3)
dx = nx2 - nx1  & dy = ny2 - ny1
sxn1=x1*(nx2-nx1)+nx1
sxn2=x2*(nx2-nx1)+nx1
syn1=y1*(ny2-ny1)+ny1
syn2=y2*(ny2-ny1)+ny1
if n_elements(yfac) eq 0 then yfac=sxn1*0.+1. else yfac=sxn1*0.+yfac
;!p.thick=2.
;!p.charsize=1.9
;!p.charthick=2.00
if n_elements(star_label) ne 0 then begin
	if keyword_set(noarror) eq 0 then $
  for i=0,nstar-1 do plots,[sxn1(i),sxn2(i)],[syn1(i),syn2(i)],/normal,color=color(i),line=line
  for i=0,nstar-1 do xyouts,sxn2(i),syn2(i)+dy/100.*yfac(i),/normal,star_label(i), $
    alignment=0.5,color=color(i),charsize=cs
endif else for i=0,nstar-1 do plots,[sxn1(i),sxn2(i)],[syn1(i),syn2(i)],/normal,color=color(i),line=line

;!p.thick=1.
;!p.charsize=1.
;!p.charthick=1.
if !debug eq 1 then stop
return
end
