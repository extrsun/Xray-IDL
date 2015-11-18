pro vatest_gs,sminao,sfvo,gsplo,gsphi,gsn,dsr=dsr,bsp=bsp,gspmax=gspmax,nperd2=nperd2,cnperd2=cnperd2,block=block,nbs=nbs,cnbs=cnbs,smax=smax
;+
; Maximum likelihood fit to the flux distribution observed in a galaxy field, 
;	using a composite model of background and galaxy source fluxes
;
; smina - vector or array containing minimum flux at each pixel of the field
; sfvo	- vector containing the source fluxes
; gsplo,gsphi - lower and upper bounds of the parameter for the 
; 	galaxy source flux distribution 
; gsn - number of division of the parameter
; dsr - the bin size in units of deg^2 (def dsr=(5./3600.)^2)
; bsp - vector contains the parameters of the background source 
;	flux distribution used by int_bs procedure 
; gspmax - the parameter that gives the best fit to the data
; nperd2 - surface density of sources in units of deg^{-2}, 
;		including background sources
;
; written by wqd, Nov 2, 1998
;-
if n_params() lt 1 then begin
print,'CALLING SEQUENCE - mle_gs,smina,sfv,gsplo,gsphi,gsn'
print,',dsr=dsr,bsp=bsp,gspmax=gspmax,nperd2=nperd2,block=block,nbs=nbs,cnbs=cnbs,smax=smax'
return
endif

sfv=sfvo(sort(sfvo))
ngs=n_elements(sfv)
if n_elements(dsr) eq 0 then begin
	if n_elements(block) eq 0 then block=10	; 5" binsize in units of deg^2
	dsr=(block*!size_pixel/3600.)^2 
endif
gsp=gsplo+(gsphi-gsplo)/gsn*findgen(gsn+1.)

; calculate the number of pixesls with limiting fluxes below each source flux
smina=sminao(sort(sminao))
nsmina=n_elements(smina)
linterp,smina,lindgen(nsmina),sfv,npixlow

int_bs,smina,nbsa
kgn=(ngs-total(nbsa)*dsr)
print,'net number of sources: total and galactic = ',ngs,kgn
cc=fltarr(gsn+1)
kg=fltarr(gsn+1)
gspmax=fltarr(3)+1.e10
get_nbs,sfv,nbs,bsp=bsp
;if n_elements(smax) eq 0 then smax=sfv(ngs-1)
for k=0,gsn do begin
	int_bs,smina,ngsa,bsp=[gsp(k),1.],smax=smax
	kg(k)=kgn/total(ngsa)/dsr ;the coefficient of the galaxy source distr.
		; normalization of the galaxy flux distribution
;	probv=kg(k)*sfv^(-gsp(k))+nbs
;	cc(k)=-2.*(total(alog(probv))) ;*npixlow*dsr)))
	vatest,smina,sfv,[gsp(k),kg(k)],vas,smax=smax
	cc(k)=vas(0)-0.5
	if abs(gspmax(2)) gt abs(cc(k)) then gspmax=[gsp(k),kg(k),cc(k)]
endfor
print,'gspmax = ',gspmax
for k=0,gsn do begin
	print,'gsp, kg, deltacc = ',gsp(k),kg(k),cc(k)
endfor

; get the best fit intrinsic model
nperd2=gspmax(1)*sfv^(-gspmax(0))+nbs

;get the best fit model
cnperd2=nperd2*(npixlow/nsmina) ; proportion to the area of the sensitivity
cnbs=nbs*(npixlow/nsmina)
return
end
