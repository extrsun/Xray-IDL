pro rbp_plot,dist,rbp,rbperr,yfit,xr=xr,yr=yr,xt=xt,yt=yt,psym=psym,distm=distm,type=type,lines=lines
;
if n_params() eq 0 then begin
print,'rbp_plot,dist,rbp,rbperr,yfit,xr=xr,yr=yr,xt=xt,yt=yt,distm=dist,psym=psym,type=type,lines=lines'
return
endif 
if n_elements(lines) eq 0 then lines=0
if n_elements(distm) eq 0 then distm=dist
xstyle=!x.style
!x.style=1
ystyle=!y.style
!y.style=1

if n_elements(xt) eq 0 then xt='!6 Off-source distrance (arcmin)'
if n_elements(yt) eq 0 then yt='Intensity (10!e-3!n counts s!e-1!n arcmin!e-2!n)'
if n_elements(psym) eq 0 then psym=5
;if n_elements(xr) eq 0 then xr=[0,0]
;if n_elements(yr) eq 0 then yr=[0,0]
;
;plot,dist,rbp $
ploterr,dist,rbp,rbperr $
,Xtitle=xt, Ytitle=yt,psym=psym $
,xrange=xr,yrange=yr,type=type
;errplot,dist,rbp-rbperr > yr(0),(rbp+rbperr > yr(0)) <yr(1)
oplot,distm,yfit,line=lines
;
!x.style=xstyle
!y.style=ystyle
end
