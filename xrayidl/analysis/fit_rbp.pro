pro fit_rbp,dist,rbp,rbperr,block=block,rlimit=rlimit,xc=xc,yc=yc,chlow=chlow,chhigh=chhigh,spfil=spfil,yfit=yfit,psfim=psfim,binexc=binexc
;
;+
; xc, yc - sass pixel position of the source for calculating offaxis angle
; in make_psf.pro
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -  fit_rbp,dist,rbp,rbperr,block=block,rlimit=rlimit'
print,',xc=xc,yc=yc,chlow=chlow,chhigh=chhigh,spfil=spfil,yfit=yfit,psfim=psfim,binexc=binexc''
return
endif
if n_elements(rlimit) eq 0 then rlimit=30
if n_elements(block) eq 0 then block=1
if n_elements(chlow) eq 0 then chlow=12
if n_elements(chhigh) eq 0 then chhigh=29
if n_elements(binrat) eq 0 then binrat=1
dim=2*rlimit/(block*0.5)+1
;
emin=!group(chlow,0) 
emax=!group(chhigh,1)
print,'emin, emax = ', emin, emax
if n_elements(spfil) eq 0 then begin
	spfil='~/spectrum/po17.dat'
	print,' get model spectral rate and group...'
 	modelrate,spfil,chlow,chhigh,rate
	group=!group(chlow:chhigh,*)
endif
;

inputs='instr='+!instr+',sptyp=PHA,imgfil=NONE,proftab=NONE'+ $
	',block='+strtrim(block,2)+',binsiz='+strtrim(block*0.5)+',binrat='+ $
	strtrim(binrat,2)+',emin='+strtrim(emin,2)+',emax='+strtrim(emax,2) + $
	',pixel=BL'+',nbins='+strtrim(dim,2)+',spfil=NONE';+spfil+
if n_elements(xc) ne 0 then begin
 case !instr of
   'h' : if xc gt 4096+2400 or xc lt 1696 or yc gt 4096+2400 or yc lt 1696 $
	then 		stop,'the position is not right'
   'p' : if xc gt 7680+7200 or xc lt 480 or yc gt 7680+7200 or yc lt 480 then $
		stop,'the position is not right'
 endcase
 inputs=inputs+',xcen='+strtrim(xc/block,2)+',ycen='+strtrim(yc/block,2)
endif

make_psf,inputs,offang,prof,psfim,rate=rate,group=group
;
if !debug eq 1 then stop,'before rbp'
rbp,psfim,distp,psf,block=block,radiushi=rlimit,/plotoff
;
; now do the fit
;
distn=dist
rbpn=rbp
rbperrn=rbperr
psfn=psf
if n_elements(binexc) ne 0 then remove,binexc,distn,rbpn,rbperrn,psfn
npoint=n_elements(distn)
func=fltarr(npoint,2)
func(*,0)=1.
func(*,1)=psfn
coef=funcfitw(distn,rbpn,(1./rbperrn^2),func,2,yfit,yband,sigma,inv)
;
if n_elements(binexc) ne 0 then begin
	yfit=coef(0)+coef(1)*psf
endif
;
; plot
;
erase
rbp_plot,dist,rbp,rbperr,yfit
if n_elements(binexc) ne 0 then oplot,dist(binexc),rbp(binexc),psym=4
; print out parameters
;
print,'chi square = ',sigma*(npoint-2),' with No. of degree freedom = ',npoint-2
print,'coefficient of the functions:'
for k=0,1 do begin
print,coef(k), ' +/- ',sqrt(inv(k,k))
endfor
end