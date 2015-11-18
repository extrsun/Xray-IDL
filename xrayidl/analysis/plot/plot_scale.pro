pro plot_scale,cor,xr,yr,lr,label,capl=capl,charsize=charsize
;+
; plot a scale bar in an existing plot
; cor - cornor coodiantes from cont_grey
; xr, yr - the center position in units of the plot size
; lr - the length of the scale bar in units of the plot size
; label - Lable of the bar (e.g., '10'+string("042B) for 10")
; capl - the end cap length of the bar
; charsize - the lable size
; written by wqd, 3/5/2000
;-
 On_error, 2
 IF N_params() LT 4 THEN BEGIN
	print, "plot_scale,cor,xr,yr,lr,label,capl=capl,charsize=charsize"
	RETURN
 ENDIF
if n_elements(capl) eq 0 then capl=0.005
xcord=cor(1)-cor(0)
ycord=cor(3)-cor(2)
xc=cor(0)+xr*xcord
yc=cor(2)+yr*ycord
xl=lr*xcord
xca=xc+[-1.,1]*(xl*0.5)
plots,xca,yc,/norm
plots,xca(0),yc+capl*[-1,1],/norm
plots,xca(1),yc+capl*[-1,1],/norm
if n_elements(lxc) eq 0 then lxc=xc
if n_elements(lyc) eq 0 then lyc=yc+ycord*0.01
xyouts,lxc,lyc,label,/norm,align=0.5,charsize=charsize
return
end