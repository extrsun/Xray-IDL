pro bs_mc,tb,scth,sel=sel,choice=choice,smin=smin,smax=smax,block=block,ni=ni,y=asfrac
;+
; Program:
; estimate the expected background sources, accounting for the
; Eddington bias 
; tb, scth - exposure, net count threshold maps
; sel - index of selected bins in the maps
; smax - upper limit to the flux (in units of 10^-14 ergs/s cm^2)
; smin - lower limit
; block - the block factor of the maps (def=!block)
; asfrac - energy-encircled radius for the source detection
; ni - the number of the expected sources in each selected bin
;
; written by wqd, Sept, 13, 2003
;-
if n_params() lt 2 then begin
   print,' syntax: '
   return
endif
if n_elements(smin) eq 0 then smin=0
if n_elements(smax) eq 0 then smax=20
if n_elements(block) eq 0 then block=!block
if n_elements(asfrac) eq 0 then asfrac=0.7
if n_elements(sel) eq 0 then begin
    expt=tb & scthv=scth
    endif else begin
        expt=tb(sel) & scthv=scth(sel)
    endelse 
;background source model parameters:
aa=122.
alpha=0.6
conv=1.e-3 ;conversion from flux (in units of 10^-14} to cntr)

nmin=nint(asfrac*smin*conv*expt > scthv)
nmax=fix(asfrac*smax*conv*expt) > nmin
nmaxmax=max(nmax)
nminmin=min(nmin)

nv=findgen(nmaxmax-nminmin+1)+nminmin
ratio=exp(lngamma(nv-alpha)-lngamma(nv))
accum,ratio,aratio
aratio_low=[0,aratio]
nio=aa*asfrac^alpha*(block*!size_pixel/3600.)^2
ni=nio*(aratio(nmax-nminmin)-aratio_low(nmin-nminmin))
nit=total(ni)
print,'total number of the expected sources is ',nit
return
end
