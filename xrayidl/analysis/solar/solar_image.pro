pro solar_image,blow,bhigh,solar_f,solar_fe,frac=frac,dim=dim,exptail=exptail,filter=filter,tail2=tail2,f2=f2,etmin=etmin,ind=ind 
;,perc=perc,factor=factor,slow=slow,soufile=soufile,flow=flow
;+
; calculate excess (solar) flux in an uncleaned ("all") image by comparing
; the flux in the image with the flux in the corresponding cleaned image.
; writen by WQD, Aug. 3, 1993
;*INPUTS:
; blow, bhigh - the lower and higher boundaries of the ROSAT energy bands
; filter - if provided, only regions with filter value greater than 0 are used
;*OUTPUTS:
; solar_f, solar_fe - the caculated (and normalized to the total exposure
;	time in the "all" image) excess flux and it error 
;*Limitation:
;	The exposure images in the desired energy band should be produced
;	before hand.
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - solar_image,blow,bhigh,solar_f,solar_fe,dim=dim,factor=factor,slow=slow,frac=frac,soufile=soufile,exptail=exptail'
return
endif
;
if n_elements(etmin) eq 0. then etmin=3000.
if n_elements(dim) eq 0 then dim=128 ; the central part of the image
if n_elements(slow) eq 0 then slow=3.5
if n_elements(frac) eq 0 then frac=1./16.
if n_elements(exptail) eq 0 then exptail='all'
get_image,tall,call,blow=blow,bhigh=bhigh,dim=dim,exptail=exptail ;,factor=factor,slow=slow,soufile=soufile,flow=flow,perc=perc
tsall=tall
if n_elements(filter) ne 0 then tsall(where(image_cut(filter,dim/2.,/pix) le 0.))=0.
if n_elements(tail2) eq 0 then exptail='' else exptail=tail2
get_image,t,c,blow=blow,bhigh=bhigh,dim=dim,exptail=exptail ;,factor=factor,slow=slow,soufile=soufile,flow=flow,perc=perc
;get_image,t,c,ts,blow=blow,bhigh=bhigh,dim=dim,exptail='',factor=factor,slow=slow,soufile=soufile,flow=flow
; make sure that the same area is used in two images
ts=t
if n_elements(f2) ne 0 then ts(where(image_cut(f2,dim/2.,/pix) le 0.))=0.
sel=where(tsall le 0. or ts le 0.,nsel)
	if nsel ne 0 then begin
	tsall(sel)=0.
	ts(sel)=0.
endif
image_comp3,call,tall,tsall,frac=frac
image_comp3,c,t,ts,frac=frac
; convert to vectors and keep only those selected data
sel=where(ts gt etmin and tsall gt etmin,nsel)
if nsel eq 0 then stop,'No non zero exposure included, stop in solar_image'
call=call(sel)
c=c(sel)
tsall=tsall(sel)
ts=ts(sel)
; it is assumed that the "all" image includes the data in the "" image
; now subtract "" image from "all" image
trans=(60./15.*frac)^2
if n_elements(ind) ne 0 then begin
	cdif=call
	tsdif=tsall
endif else begin
	cdif=call-c
	tsdif=tsall-ts
endelse
tratio=total(tsdif)/total(tsall)
; get the excess flux in "dif" image
fdif=total(cdif)/total(tsdif)
f=total(c)/total(ts)
print,'mean flux: fdif and f = ',fdif*trans,f*trans
fexc=cdif/tsdif-c/ts
efexc=sqrt((cdif > (0.5*fdif*tsdif))/(tsdif*tsdif)+(c > (0.5*f*ts))/(ts*ts))
; 0.5 is an arbitrary value chosen here
mc=avg(c) 
print,'mean counts per bin = ', mc
if mc lt 8 then stop,'stop: the average number of counts less than 8 and the least square fit should not be used'
avg_least,fexc,efexc,solar_f,solar_fe
trans=trans*tratio
solar_f=solar_f*trans
solar_fe=solar_fe*trans
print,'normalized solar_f,solar_fe = ', solar_f,solar_fe
end