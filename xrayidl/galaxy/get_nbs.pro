pro get_nbs,ss,nbs,bsp=bsp
;+
; calculate the surface density of background sources in units of deg^{-2}
; ss - input source flux
; nbs - output source density
; bsp - vector contains the parameters of the background source 
;	flux distribution used by int_bs procedure 
; written by wqd, Nov 2, 1998
;-
if n_params() lt 1 then begin
print,'CALLING SEQUENCE - get_nbs,ss,nbs,bsp=bsp'
return
endif
if n_elements(bsp) eq 0 then bsp=[2.72,238.1,2.66,1.94,111.0]
nbs=bsp(1)*ss^(-bsp(0))
if n_elements(bsp) eq 2 then return
sel=where(ss lt bsp(2),nsel)
if nsel ne 0 then nbs(sel)=bsp(4)*ss(sel)^(-bsp(3))
return
end