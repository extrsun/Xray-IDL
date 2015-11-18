pro get_mflux,nhvc,xflux,mflux,bnlo=bnlo,bnhi=bnhi,gnhvc=gnhvc,opfile=opfile,gopfile=gopfile
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
common shared,timevco,op,gop,ncomp
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --- fit_nlvc2,nhvc,countvc,backvc,timevc,xflux'
print,',bnlo=bnlo,bnhi=bnhi,opfile=opfile,mflux=mflux,chimin=chimin'
print,',pmin=pmin,pmax=pmax,pfix=pfix,gnhvc=gnhvc,gopfile=gopfile,level=level,ww=ww '
return
endif
ncomp = 2 ;two components per band 

if n_elements(bnlo) eq 0 then bnlo = 0
if n_elements(bnhi) eq 0 then bnhi = 0
;print,'bnlo,bnhi = ',bnlo,bnhi
sz=size(nhvc)
ncol=sz(1)
n_dim =sz(0)
if n_dim eq 2 then n_band = sz(2) else n_band = 1
timevc=fltarr(ncol)+1.
op=fltarr(ncol,n_band)
gop=fltarr(ncol,n_band)
for band=bnlo,bnhi do begin
    	sh_opac,nhvc*0.01,opacity,band,quad=quad,opfile=opfile
	op(*,band-bnlo)=opacity
	if n_elements(gnhvc) ne 0 then begin
		if n_elements(gopfile) eq 0 then gopfile=opfile
			; the metallicity for example could be different
		sh_opac,gnhvc*0.01,gopacity,band,quad=quad,opfile=gopfile
		gop(*,band-bnlo)=gopacity
	endif
endfor
x=lindgen(ncol*n_band) ;  only the dimension info is to be used
model_shadow,x,xflux,yfit
mflux=yfit/timevc
stop
return
end
;===================================================================
pro model_shadow,x,para,yfit,pder ;,pvar=pvar
yfit=get_mcount(para)
pder=get_mpder(para)
;if n_elements(pvar) ne 0 then begin
;	pder=pder(*,pvar) ;assuming only one band
;endif
if !debug eq 1 then stop
end
;===========================================
function get_mcount,para
common shared,timevc,op,gop,ncomp

sz=size(timevc)
np=sz(1)
if sz(0) ge 2 then n_band=sz(2) else n_band=1
mcount=timevc
npara=n_elements(para)
;parab=reform(para,ncomp,n_band)
if npara eq n_band*ncomp then adj=1. else $
	adj=para(npara-1)
for band=0,n_band-1 do begin
  mcount(*,band)=timevc(*,band)*(para(ncomp*band) $
	+para(1+ncomp*band)*exp(adj*op(*,band)+gop(*,band)))
endfor
mcount=reform(mcount,np*n_band) ;the data in different bands are put in one
				; dimension
return,mcount
end;===========================================
function get_mpder,para
common shared,timevc,op,gop,ncomp ;all these values are inputed in the form of
					  ; fltarr(np,n_band)
sz=size(timevc)
np=sz(1)
if sz(0) ge 2 then n_band=sz(2) else n_band=1
npara=n_elements(para)
mpder=fltarr(np*n_band,npara) ;the data in different bands are put in one
				; dimension
;parab=reform(para(0:ncomp*n_band-1),ncomp,n_band)
if npara eq n_band*ncomp then adj=1. else $
	adj=para(npara-1)
for band=0,n_band-1 do begin
 mpder(band*np:(band+1)*np-1,band*ncomp)=timevc(*,band)
 mpder(band*np:(band+1)*np-1,band*ncomp+1)=timevc(*,band) $
		*exp(adj*op(*,band)+gop(*,band))
endfor
if npara ne n_band*ncomp then begin
 for band=0,n_band-1 do begin
  mpder(band*np:(band+1)*np-1,npara-1)=timevc(*,band)*para(1+ncomp*band) $
		*exp(adj*op(*,band)+gop(*,band))*op(*,band)
 endfor
endif 
;mcount=reform(mcount,n_elements(mcount))
;if !debug eq 1 then stop
return,mpder
end
