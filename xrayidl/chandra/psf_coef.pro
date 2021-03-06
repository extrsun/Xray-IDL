pro psf_coef,blo,bhi,coef,spfile=spfile,instr=instr,perclimit=perclimit
;+
; calculate the off-source angular radius as the function of off-axis angle 
;	only for the 90% radius this time, using the information in the figure
; of the Observatory guide: http://asc.harvard.edu/udocs/docs/POG/MPOG/node9.html#subsec:hrma_ea
; This reference can nolonger be found. But the results seem to agree
; well with those in Jerius, D., et al.  2000, SPIE, 4012, 17 
;
; blo, bhi - lower and higher boundaries of the energy band
; spfile - file contains the assumed source spectrum
; coef - coefficients for caculating the energy-encircled fractions
; 	to be used psf_params.pro
; instr - instrument (one of 'acisi', 'aciss', 'hrci', 'hrcs'; def=!instr)
; written by wqd, 4/7/2002
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -  psf_coef,blo,bhi,coef,spfile=spfile,instr=instr,perclimit=perclimit'
return
endif
if n_elements(instr) eq 0 then instr=!instr
if n_elements(spfile) eq 0 then begin
	spfile=!cdir+strtrim(instr,2)+'_po0.7.dat'
	print,'spfile is assumed to be ',spfile
endif
sp_accum,spfile,ee,cc,spv
esel=where(ee ge blo and ee le bhi,nesel)
if nesel eq 0 then stop,'the energy interval seems too narrow'

coef=fltarr(2)
ww=1./total(spv(esel))
if perclimit eq 0.5 then begin
	ev=[0.277,9.18]
	cv0=[0.34,0.55]
	cv1=[5.8,9]
	rf=cv0(0)+(cv0(1)-cv0(0))*(ee(esel)-ev(0))/(ev(1)-ev(0))
	coef(0)=total(spv(esel)*rf)*ww ;weighted
	rf=cv1(0)+(cv1(1)-cv1(0))*(ee(esel)-ev(0))/(ev(1)-ev(0))
	coef(1)=total(spv(esel)*rf)*ww ;weighted
endif else begin
 if perclimit eq 0.9 then begin
	ev=[1.49,6.4]
	cv0=[1.1,1.8]
	cv1=[9,12.]
	rf=cv0(0)+(cv0(1)-cv0(0))*(ee(esel)-ev(0))/(ev(1)-ev(0))
;	rf=1.1+(1.8-1.1)*(ee(esel)-1.49)/(6.4-1.49)
	coef(0)=total(spv(esel)*rf)*ww ;weighted
	rf=cv1(0)+(cv1(1)-cv1(0))*(ee(esel)-ev(0))/(ev(1)-ev(0))
;	rf=9+(12-9)*(ee(esel)-1.49)/(6.4-1.49)
	coef(1)=total(spv(esel)*rf)*ww ;weighted
 endif else stop,'right now only supports perclimit=0.5 or 0.9'
endelse
return
end
