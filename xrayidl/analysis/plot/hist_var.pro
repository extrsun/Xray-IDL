pro hist_var,xl,xu,y,xh,yh,ymin=ymin,thick=thick,noplot=noplot
;-
; plot histogram in an existing plot or output the two vector for ploting
; outside this procedure
;
; xl, xu - lower and upper limits of individual histogram bins
;	which may be obtained from dist_flux_var.pro
; y - y values of the bins
; ymin - the begining and end values of xh and yh. Setting it to min(!y.crage)
;	or 10.^min(!y.crange) (if in log) for example.
; xh, yh - output vectors for outside ploting (e.g., using plot)
; noplot - if set, no overplot will be made
; written by wqd, March 8, 1996
;+
if n_params() eq 0 then begin
print,'hist_var,xl,xu,y,xh,yh,ymin=ymin,thick=thick'
return
endif
if n_elements(thick) eq 0 then thick=!p.thick
nh=n_elements(y)
xh=xl(0)
if n_elements(ymin) ne 0 then yh=ymin else yh=y(0)
yn=[y,y(nh-1)]
for k = 0, nh-1 do begin
	xh=[xh,[xl(k),xu(k),xu(k)]]
	yh=[yh,[yn(k),yn(k),yn(k+1)]]
endfor
if n_elements(noplot) eq 0 then plots,xh,yh,thick=thick 
return
end
