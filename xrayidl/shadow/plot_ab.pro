pro plot_ab,xflux,mincpara,opacity,count,back,time,yfit
;-
; plot the fit of the model to the observed flux as a fuction of opacity
; do this inside the procedure fit_ab.pro
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - plot_ab,xflux,mincpara,opacity,count,back,time,yfit'
return
endif
flux=(count-back)/time
ww=sqrt(yfit)/time
plot,opacity,flux,psym=7
errplot,opacity,flux-ww,flux+ww
op=findgen(50)*(long(max(opacity)+1.)/49.)
mflux=xflux(0)+xflux(1)*exp(-mincpara*op)
oplot,op,mflux
end