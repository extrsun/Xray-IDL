pro int_bs_new,smin,nbs,bsp=bsp,smax=smax,choice=choice
;+
; integrated number of background X-ray sources based on 
; Moretti, A. et al. 2003, ApJ, 588, 696
;
; smin - the minimum source flux (in units of 10^{-14} ergs/s/cm^2)
; nbs - the integrated surface density (number of sources per deg^2)
; bsp - vector contains the parameters of the background source 
;	flux distribution used by int_bs procedure 
; smax - the maximum flux cutoff
;choice =1 ; def, use 0.5-2 keV band for the background AGN number estimate
;        = 2: 2-10 keV band
;
; written by wqd, July 30, 2003
;-
if n_params() lt 1 then begin
print,'CALLING SEQUENCE - int_bs,smin,nbs,bsp=bsp,smax=smax,choice=choice'
return
endif
if n_elements(bsp) eq 0 then begin
    if n_elements(choice) eq 0 then begin
        print,'Moretti et al soft band assumed!'
        choice=1
    endif 
    case choice of 
        1: bsp=[1.82, 0.60,1.48,6150] ;0.5-2 keV band
        2: bsp=[1.57, 0.44,0.45,5300] ;2-10 keV band
        else: begin
            print,'not impremented!'
            nbs=-1
            return
        end 
    end
endif
nbs=0.2^bsp(0)/(smin^bsp(0)+bsp(2)^(bsp(0)-bsp(1))*smin^bsp(1))
if n_elements(smax) ne 0 then $
      nbs=nbs-0.2^bsp(0)/(smax^bsp(0)+bsp(2)^(bsp(0)-bsp(1))*smax^bsp(1))
nbs=nbs*bsp(3)
;print,'Total accumulated number is ',nbs,' sources per square deg.'
return
end

