pro int_bs,smin,nbs,bsp=bsp,smax=smax
;+
; integrate over the flux distribution
; smin - the minimum source flux (in units of 10^{-14} ergs/s/cm^2)
; nbs - the integrated surface density (number of sources per deg^2)
; bsp - vector contains the parameters of the background source 
;	flux distribution used by int_bs procedure 
; smax - the maximum flux cutoff
; written by wqd, Nov 2, 1998
;-
if n_params() lt 1 then begin
print,'CALLING SEQUENCE - int_bs,smin,nbs,bsp=bsp,smax=smax'
return
endif
if n_elements(bsp) eq 0 then bsp=[2.72,238.1,2.66,1.94,111.0]
if bsp(0) ne 1 then begin 
	nbs=bsp(1)/(bsp(0)-1.)*smin^(1.-bsp(0))
	if n_elements(smax) ne 0 then $
		nbs=nbs-bsp(1)/(bsp(0)-1.)*smax^(1.-bsp(0)) else $
		if bsp(0) lt 1 then $
		stop,'smax should be given, since the power index bsp(0) < 1'
endif else nbs=bsp(1)*alog(smax/smin)

if n_elements(bsp) eq 2 then return
sel=where(smin lt bsp(2),nsel)
if nsel ne 0 then nbs(sel)=bsp(1)/(bsp(0)-1.)*bsp(2)^(1.-bsp(0))+ $
 bsp(4)/(1.-bsp(3))*(bsp(2)^(1.-bsp(3))-smin(sel)^(1.-bsp(3)))
return
end

