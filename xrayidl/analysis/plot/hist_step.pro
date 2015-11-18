pro hist_step,cim,bim,tim,xmin,xmax,ymin,ymax,nstep,angle=angle,stepfac=stepfac,block=block,xsh=xsh,ysh=ysh,cton=cton,xrange=xrange,yrange=yrange,xtitle=xtitle,ytitle=ytitle
if n_params() eq 0 then begin
print,'Calling Sequence - hist_step,cim,bim,tim,xmin,xmax,ymin,ymax'
print,',nstep,angle=angle,stepfac=stepfac,block=block,xsh=xsh,ysh=ysh'
print,',cton=cton,xrange=xrange,yrange=yrange,xtitle=xtitle,ytitle=ytitle'
return
endif
if n_elements(stepfac) eq 0. then stepfac=10.
step=(ymax-ymin)/float(nstep)
yminn=ymin-step
for k=0,nstep-1 do begin
	yminn=yminn+step
	ymaxn=yminn+step
	dis_flux_var,cim,bim,tim,dist,flux,eflux,dl,dh,angle=angle $
	,xmin=xmin,xmax=xmax,ymin=yminn,ymax=ymaxn,block=block,xsh=xsh $
	,ysh=ysh,cton=cton,im=im
	if k eq 0 then begin
		if n_elements(xrange) eq 0 then xrange=[min(dl),max(dh)]
		ploterr,dist,flux,eflux,psym=3,type=1,hat=0.,errthick=0.5 $
		,xrange=xrange,yrange=yrange,xtitle=xtitle,ytitle=ytitle
	endif else begin
		flux=flux*stepfac^k
;		eflux=eflux*stepfac
	endelse
	hist_var,dl,dh,flux,xh,yh,thick=1
endfor
return
end