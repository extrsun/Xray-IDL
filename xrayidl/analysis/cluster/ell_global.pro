pro ell_global,pp,pm,ilow=ilow,ihi=ihi
;+
; Calaculate the global variation of elliptical parameters
; pp - the best fits from the data
; pp - fits from the simulated data
; ilow, ihi - the scale rows to be included in the calculation 
;	(the largest scale = 0)
;-
if n_params() lt 1 then begin
print,'CALLING SEQUENCE - ell_global,pp,pm,ilow=ilow,ihi=ihi'
return
endif
sz=size(pp)
nbin=sz(2)
print,'n,difmax/difmaxe,difmax,difmaxe,imax,jmax'
for n=0,3 do begin
 difmax=0.
 if n_elements(ilow) eq 0 then ilow=0
 if n_elements(ihi) eq 0 then ihi=nbin-1
 for i = ilow,ihi do begin
   diferr=sqrt((pp(n,i)-pm(n,i,1))^2+(pm(n,*,2)-pp(n,*))^2)
   if n eq 3 then dif=asin(sin(pp(n,i)-pp(n,*))) else $
   dif=(pp(n,i)-pp(n,*))
   difm=max(dif/diferr,j)
   if difm gt difmax then begin
	difmax=difm
	difmaxe=diferr(j)
	imax=i
	jmax=j
   endif
 endfor
 difmax=difmax*difmaxe
 if n eq 0 or n eq 1 then begin
	difmax=difmax*0.5 ; in units of arcsec
	difmaxe=difmaxe*0.5
 endif else begin
	if n eq 3 then begin
		difmax=difmax*180./!pi ; in units of deg
		difmaxe=difmaxe*180./!pi
	endif
 endelse
 print,n,difmax/difmaxe,difmax,difmaxe,imax,jmax
endfor
return
end
