pro plot_xy,glam,glalo,glahi,colval,colvallo,colvalhi,modv,psym=psym,xtitle=xtitle,ytitle=ytitle,plot_oo=plot_oo,xrange=xrange,yrange=yrange
;+
; plot data points with both x and y error bars
;-
if n_params() eq 0 then begin
print,'Calling Seq - plot_xy,glam,glalo,glahi,colval,colvallo,colvalhi,modv'
print,',psym=psym,xtitle=xtitle,ytitle=ytitle,plot_oo=plot_oo'
print,',xrange=xrange,yrange=yrange'
return
endif

if n_elements(psym) eq 0 then psym=7
if n_elements(xtitle) eq 0 then xtitle='!17  X'
if n_elements(ytitle) eq 0 then ytitle='Y'
ncol=n_elements(glam)
if n_elements(xrange) eq 0 then xrange=[glalo(0),glahi(ncol-1)]
if n_elements(yrange) eq 0 then yrange=[colvallo(0),colvalhi(ncol-1)]
if keyword_set(plot_oo) ne 0 then begin
plot_oo,glam,colval,psym=7,xrange=xrange $
	,yrange=yrange,xtitle=xtitle,ytitle=ytitle
	errplot_y,colval,glalo,glahi,width=0
	errplot_x,glam,colvallo,colvalhi,width=0
endif else begin
	plot,glam,colval,psym=7,xrange=xrange $
	,yrange=yrange,xtitle=xtitle,ytitle=ytitle
	errplot_y,colval,glalo,glahi
	errplot_x,glam,colvallo,colvalhi
endelse
if n_elements(modv) ne 0 then oplot,glam,modv
end