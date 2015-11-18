pro plot_gap,t1,t2,yv,gap=gap,line=line,thick=thick,color=color
;+
; plot vertical lines that separate time bins with large time gaps
;
;*INPUTS:
;
; t1, t2 - vectors containing lower and upper limits of time bins
; yv - two element vector contains that lower and upper limits of the y-axis.
; gap - the time gap in units of seconds (def = 1 month)
; line - the line style
;
; written by wqd, Nov. 20, 2000
;-
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - plot_gap,t1,t2,yv,gap=gap,line=line,thick=thick'
return
endif
if n_elements(line) eq 0 then line=2
if n_elements(thick) eq 0 then thick=1
if n_elements(gap) eq 0 then gap=3600.*24*30
nt=n_elements(t1)
if n_elements(t2) ne nt then stop,'error: t1 not = t2'
td=t1(1:*)-t2(0:nt-2)
sel=where(td gt gap,nsel)
tgap=sel+1.5

for k=0,nsel-1 do oplot,yv*0.+tgap(k),yv,line=line,thick=thick,color=color

return
end
