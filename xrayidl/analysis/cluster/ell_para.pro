pro ell_para,xx,yy,para,mub=mub,bf=bf,ww=ww,status=status
;+
; calculate the centroid, ellipicity, and orientation with the momentum method
; see Buate and Canizares, ApJ, 427, 86
; written by wqd, 9/25/96
; xx, yy - the coordinates of photons
; para - output vector containing the x/y center position, ellipicity, 
;	and orientation (north through east)
; ww - weights of individual counts (if provided) will be included in
; 	the momentum calculation.
; mub, bf - uniform background moment and flux.
; status - if status = -1, the calculation does not valid. When the counting
;	statistics is too poor, the moment from the data can be smaller 
;	than that from the EXPECTED background contribution.
; written by wqd, 9/25/96
;-
if n_params() lt 1 then begin
print,'CALLING SEQUENCE - ell_para,xx,yy,para,mub=mub,bf=bf,status=status'
return
endif
moment_2d,xx,yy,xm,ym,mu,ww=ww

para=fltarr(5)
para(0:1)=[xm,ym]
if n_elements(bf) ne 0 then mu=(mu-bf*mub)/(1.-bf)
if mu(0,0) le 0 or mu(1,1) le 0 then begin
	; the moment from the data is smaller than that from the EXPECTED
	;	background contribution
	para(2)=1. 
	status=-1
endif else begin
	b=-(mu(0,0)+mu(1,1))
	c=mu(0,0)*mu(1,1)-mu(0,1)^2
	lap=sqrt(0.5*(-b+sqrt(b^2-4*c)))
	lan=sqrt(0.5*(-b-sqrt(b^2-4*c)))
	para(2)=1.-lan/lap
	para(3)=atan(-mu(0,1)/(lap^2-mu(0,0))) ;+0.5*!pi
	status=0
	para(4)=lap
endelse
return
end