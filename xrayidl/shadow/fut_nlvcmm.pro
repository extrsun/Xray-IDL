pro fit_nlvc2,nhvc,countvc,backvc,timevc,xflux,band=band $
,opfile=opfile,mflux=mflux,chimin=chimin,pmin=pmin,pmax=pmax,pfix=pfix,gnhvc=gnhvc,noconf=noconf,plotoff=plotoff,xflux_ini=xflux_ini,gopfile=gopfile,level=level,ww=ww 
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;**NAME:
; fit_nlvc
;
;**PURPOSE:
; A nonlinear fit of a two-component flux model to X-ray data to get 
; foreground and background fluxes as well as an absorption normalization 
;
;**CALLING SEQUENCE:
; fit_nlvc,nhvc,countvc,backvc,timevc,xflux,bnlo=bnlo,bnhi=bnhi $
; ,opfile=opfile,mflux=mflux,mimage=mimage,chimin=chimin
;
;**PARAMETERS:
;**INPUTS:
; nhvc,countvc,backvc,timevc - vectors containing column density, count, backg,
;		and exposure, obtained with get_vec_area.pro
;
;**OPTIONAL INPUTS:
; 
; band - array containing individual bands of the data (def:[0])
;	The definition of the bands are 0,1,2,3,4,5,or 6, 7 = 
;	'1_2','4_5','6_7','1_1','2_2','3_3','4_4',[5-5' respectively.
; chimin - minimum chi^2 value
; xflux_ini - initial values for fitting parameters xflux; sometimes needed
;		for curverging to right minimum
;**OPTIONAL INPUTS or OUTPUTS:
; pmin,pmax - vectors containing user input boundaries for the parameters
; opfile - the opacity file containing the broad band opacity as a
;	function of column density]
; pfix - vector contains the index(es) of the parameter(s) to be fixed during
;	the fit. The value(s) of the parameter(s) should be given in the 
;	vectors, pmin and pmax. For example, the fix the third parameter equal
;	to 1 (i.e. the normalization of the opacity), you need to have pfix=2
;	and pmin(2)=1 and pmax(2)=1.
; gnhvc - foreground absorption column density (in units of 10^20); e.g.,
; 	galactic absorption in an extragalactic shadowing study
;	
; mflux - vectors containing the best-fit model flux
;
;**OUTPUTS:
; xflux = Two component X-ray fluxes in individual energy bands and
;   a opacity normalization value, array(3,n_band) in units of (count-back)/et.
;
;**PROCEDURE:
;
;**EXAMPLES:
; 
;**RESTRICTIONS:
;
;**NOTES:
;**SUBROUTINES CALLED:
; sh_opac
; curvefit_mm
;**MODIFICATION HISTORY:
; written by WQD, Aug 19, 1993
;
; add the keyword pfix so that a parameter can be fixed in the fit
; wqd, Nov 6, 1992
;
; a serious bug is fixed. wqd, Sept 7, 1994. The input parameter, chisqrmin,
; of conf_nl.pro should not be a reduced chisqr. Tests have been done on
; two band fits with the normalization to be same in the two bands.
;-
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
common shared,timevco,op,gop,ncomp
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --- fit_nlvc2,nhvc,countvc,backvc,timevc,xflux'
print,',band=band,opfile=opfile,mflux=mflux,chimin=chimin'
print,',pmin=pmin,pmax=pmax,pfix=pfix,gnhvc=gnhvc,gopfile=gopfile,level=level,ww=ww '
return
endif
timevco=timevc
;*parameter values used
ncomp = 2 ;two components per band 

wclimit=backvc > 10
sz=size(countvc)
ncol=sz(1)
n_dim =sz(0)
if n_elements(band) eq 0 then begin
	n_band = 1
	band=[0]
endif else n_band = n_elements(band)

if n_elements(pmin) eq 0 then pmin=[replicate(1.e-10,ncomp*n_band)]
if n_elements(pmax) eq 0 then pmax=[replicate(1.,ncomp*n_band)]
;print,'pmin=',pmin
;print,'pmax=',pmax

xflux = fltarr(ncomp,n_band)

op=fltarr(ncol,n_band)
gop=fltarr(ncol,n_band)

for nb=0,n_band-1 do begin
    	sh_opac,nhvc*0.01,opacity,band(nb),quad=quad,opfile=opfile
	op(*,nb)=opacity
	if n_elements(gnhvc) ne 0 then begin
		if n_elements(gopfile) eq 0 then gopfile=opfile
			; the metallicity for example could be different
		sh_opac,gnhvc*0.01,gopacity,nb,quad=quad,opfile=gopfile
		gop(*,nb)=gopacity
	endif
endfor
x=lindgen(ncol*n_band) ;  only the dimension info is to be used
xflux=dblarr(ncomp*n_band)

; set initial parameters for the nonlinear fit to arbitrary nonzero values
if n_dim gt 1 then mf=total((countvc-backvc),1)/total(timevc,1) else $
	mf=total(countvc-backvc)/total(timevc) 
;average fluxes in individual bands
if n_elements(xflux_ini) eq 0 then begin
 for band=0,n_band-1 do begin
	xflux(0+band*ncomp)=mf(band)*0.6 ;these are arbitrarily chosen values
	xflux(1+band*ncomp)=mf(band)*0.4
 endfor
endif else xflux=xflux_ini

xflux=(xflux < pmax) > pmin
;print,'Initial parameter values: ',xflux

vcountvc=reform(countvc,ncol*n_band)
vbackvc=reform(backvc,ncol*n_band) ;one dimension data points for
			;multi-band data into curvefit_mm
if n_elements(ww) ne 0 then w=ww else $
	w = 1./(vcountvc > wclimit)
; the weight will be changed once a model is obtained, but is fixed when
;	the confidence intervals are calculated
;chi=1.e22
;repeat begin
;	chio=chi
	yfit=curvefit_mm(x,vcountvc-vbackvc,w,xflux,sigma,function_name $
	 ='model_shadow',chi=chi,pmin=pmin,pmax=pmax,pfix=pfix)
;	w = 1./(yfit+vbackvc)
;endrep until abs(chio-chi)/chi lt 0.01
yfit=reform(yfit,ncol,n_band)

ndf=n_elements(countvc)-n_elements(xflux)+n_elements(pfix)
chimin=chi*ndf
if keyword_set(plotoff) eq 0 then begin
	print,'min chisqr = ',chimin,ndf
	print,'xflux = ',xflux
endif

;if n_band eq 1 then begin
	flux=(countvc-backvc)/timevc
	if n_elements(ww) ne 0. then eflux=1./sqrt(ww) else $
		eflux=sqrt(countvc > 10.)/timevc
	mflux=yfit/timevc
	if keyword_set(plotoff) eq 0 then begin
;		ploterr,nhvc,flux,eflux
		ploterr,nhvc,flux(*,0),eflux(*,0),psym=5
		oplot,nhvc,mflux(*,0)
	endif
if n_band gt 1 then begin
	if keyword_set(plotoff) eq 0 then begin
		oploterr,nhvc,flux(*,1),eflux(*,1),psym=4
		oplot,nhvc,mflux(*,1)
	endif
endif
	sigma=xflux
	if n_elements(pfix) ne 0 then remove,pfix,sigma
;endif
if keyword_set(noconf) eq 0 then begin
conf_nl,vcountvc-vbackvc,w,xflux,chimin,conf,pmin=pmin,pmax=pmax, $
	level=level,sigma=sigma,pfix=pfix 
;nl=n_elements(level)
print,'68% confidence intervals of the parameter: '
for k=0,1 do print,conf(*,k*2)
print,'90% confidence intervals of the parameter: '
for k=0,1 do print,conf(*,k*2+1)
endif
if !debug eq 1 then stop
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
end