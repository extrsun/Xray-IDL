pro psf_coef,blo,bhi,coef,spfile=spfile,instr=instr
;+
; calculate the off-source angular radius as the function of off-axis angle 
;	only for the 90% radius this time, using the information in the figure
; of the Observatory guide: http://asc.harvard.edu/udocs/docs/POG/MPOG/node9.html#subsec:hrma_ea
; 
;
; blo, bhi - lower and higher boundaries of the energy band
; spfile - file contains the assumed source spectrum
; coef - coefficients for caculating the energy-encircled fractions
; 	to be used psf_params.pro
; instr - instrument (one of 'acisi', 'aciss', 'hrci', 'hrcs'; def=!instr)
; written by wqd, 4/7/2002
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - psf_bb,blo,bhi,av,afr,psffile=psffile,spfile=spfile,outfile=outfile,frac=frac,instr=instr'
return
endif
if n_elements(spfile) eq 0 then begin
	spfile=!cdir+strtrim(instr,2)+'_po0.7.dat'
	print,'spfile is assumed to be ',spfile
endif
sp_accum,spfile,ee,cc,spv
esel=where(ee ge blo and ee le bhi,nesel)
if nesel eq 0 then stop,'the energy interval seems too narrow'

coef=fltarr(2)
ww=1./total(spv(esel))
rf=1.1+(1.8-1.1)*(ee-1.49)/(6.4-1.49)
coef(0)=total(spv(esel)*rf)*ww ;weighted
rf=9+(12-9)*(ee-1.49)/(6.4-1.49)
coef(1)=total(spv(esel)*rf)*ww ;weighted

print,'coef1,coef2 = ',coef
return
end