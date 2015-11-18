pro bts_fit,xbo,xeo,ybo,yeo,pam,yehi=yehio,xehi=xehio,sel=sel $
,xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,xrange=xrange,yrange=yrange $
,corr=corr,pri=pri,inpar=inpar,nsim=nsim,seed=seed,paraa=paraa $
,siglevel=siglevel,yfit=yfit,mpoly=mpoly
;+
; calculate confidence limits using bootstrapping samples of a model rbp.
; writen by wqd, 1/1/1997
; yfit - if predefined, will contain the vector containing the best-fit model
; mpoly - the number of parameters in a fit with a polynormial.
; xbo,ybo - the best estimates of x and y values
; xeo,yeo - the 68\% error bars or the lower bars if the upper bars (xehi, yehi
; 		are given.
; xmin,xmax,ymin,ymax - the lower and upper limits of the random x and y 
;		values; values outside the boundary will set to the boundary.
; xrange, yrange - two parameters vectors containing the plot limits.
; corr - if set, correlation coeficient will be calculated
; pri - if set, the plot will be drawn.
; inpar - if given, the non-linear fit will be conducted.
; nsim - number of simulation (def=1000)
; yfit - fit to the input x and y data
; paraa - output result containing the fit (and the correlation coef)
; writen by wqd, 1/2/97
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - bts_fit,xbo,xeo,ybo,yeo,pam,yehi=yehio,xehi=xehio'
print,',sel=sel,xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,xrange=xrange'
print,',yrange=yrange,corr=corr,pri=pri,inpar=inpar,nsim=nsim,seed=seed'
print,',paraa=paraa,siglevel=siglevel,yfit=yfit,mpoly=mpoly'
return
endif
if n_elements(mpoly) eq 0 then mpoly=2
if n_elements(nsim) eq 0 then nsim=1000
if n_elements(sel) eq 0 then sel=lindgen(n_elements(xbo)) else $
	sel=sel(where(sel lt n_elements(xbo)))
xb=xbo(sel)
xe=xeo(sel)
yb=ybo(sel)
ye=yeo(sel)
s=sort(xb)
xb=xb(s)
xe=xe(s)
yb=yb(s)
ye=ye(s)
if n_elements(xehio) ne 0 then begin
	xchn=2 
	xehi=xehio(sel)
endif else xchn=1
if n_elements(yehio) ne 0 then begin
	ychn=2 
	yehi=yehio(sel)
endif else ychn=1

nbin=n_elements(xb)
for k=0,nsim do begin
 if k eq 0 then begin ;direct fit to the input values
	xx=xb 
	yy=yb
	yfit=xx*0.
	if n_elements(inpar) eq 0 then para=svdfit(xx,yy,mpoly,yfit=yfit) $
		else begin
		  para=inpar
	  	  if n_elements(func) eq 0 then func='model_exp'
		  ww=xx*0.+1.0
		  yfit=curvefit(xx,yy,ww,para,func=func)
		endelse
	sz=size(para)
	if keyword_set(corr) eq 1 then npara=sz(1)+1 else npara=sz(1)
	paraa=fltarr(npara,nsim+1)
	if keyword_set(pri) eq 1 then begin
		if n_elements(xrange) eq 0 then xrange=[0,max(xx)*1.2]
		if n_elements(yrange) eq 0 then yrange=[0,max(yy)*1.2]
		plot,xx,yy,xra=xrange,yra=yrange,psym=3
		oplot,xx,yfit
	endif
 endif else begin
	if n_elements(inpar) eq 0 then para=svdfit(xx,yy,mpoly) else begin
		para=inpar
		y=curvefit(xx,yy,ww,para,func=func)
	endelse
	if N_elements(pri) ne 0 then oplot,xx,yy,psym=3
 	xran=randomn(seed,nbin)
 	if xchn eq 1 then begin
 		xx=xran*xe+xb 
 	endif else begin
		sel=where(xran le 0.,nsel) 
		if nsel ne 0 then xx=xran*xe+xb else $
		if nsel ne nbin then xx=xran*xehi+xb
 	endelse
 	yran=randomn(seed,nbin)
 	if ychn eq 1 then begin
 	yy=yran*ye+yb 
 	endif else begin
		sel=where(yran le 0.,nsel) 
		if nsel ne 0 then yy=yran*ye+yb else $
		if nsel ne nbin then yy=yran*yehi+yb
 	endelse
 
 	if n_elements(xmin) ne 0 then xx=xx > xmin
 	if n_elements(ymin) ne 0 then yy=yy > ymin
 	if n_elements(xmax) ne 0 then xx=xx < xmax
 	if n_elements(ymax) ne 0 then yy=yy < ymax
 endelse
 paraa(0:sz(1)-1,k)=para
 if keyword_set(corr) eq 1 then paraa(npara-1,k)=correlate(xx,yy)
endfor
print,'parameters of the fit to the best values: '
print,paraa(*,0)
pam=fltarr(npara,3)
for i=0,npara-1 do begin
	avg_median,paraa(i,*),pm,plo,phi,siglevel=siglevel
	pam(i,0)=pm
	pam(i,1)=plo
	pam(i,2)=phi
endfor
print,pam
stop
return
end
;===================================================================
pro model_exp,x,para,yfit,pder 
ncomp=n_elements(para)
yfit=para(0)*exp(para(1)*x)
pder=fltarr(n_elements(x),ncomp)
pder(*,0)=exp(para(1)*x)
pder(*,1)=para(0)*exp(para(1)*x)*x
return
end