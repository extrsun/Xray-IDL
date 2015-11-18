pro psf_bb,blo,bhi,av,afr,psffile=psffile,spfile=spfile,outfile=outfile,frac=frac,instr=instr
;+
; calculate the off-source angular radius as the function of off-axis angle 
;	and energy-encircled radius
;
; blo, bhi - lower and higher boundaries of the energy band
; av - off-axis angle vector 
; afr - off-source angle as a function of off-axis angle and energy-encircled
;	radius
; psffile - input psf file
; spfile - file contains the assumed source spectrum
; frac - energy-encircled fractions to be calculated (def = [0.5,0.7,0.8,0.9])
; instr - instrument (one of 'acisi', 'aciss', 'hrci', 'hrcs'; def=!instr)
; outfile - output file name (def = 'psf_bb.dat')
;
; written by wqd, 6/6/2001
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - psf_bb,blo,bhi,av,afr,psffile=psffile,spfile=spfile,outfile=outfile,frac=frac,instr=instr'
return
endif
if n_elements(outfile) eq 0 then outfile='psf_bb.dat'
if n_elements(frac) eq 0 then frac=[0.5,0.7,0.8,0.9]
if n_elements(psffile) eq 0 then begin
	psffile='/net/xray/usr/local/ciao_2.1/data/psfsize_20000830.fits'
	print,'psffile is assumed to be ',psffile
endif
read_psf,psffile,frac,psf2,ev,av,instr=instr

if blo lt min(ev) or blo gt max(ev) then begin
	print,'Sorry. The energy has to be between ',minmax(ev)
	return
endif
if n_elements(spfile) eq 0 then begin
	spfile='/net/xray/data1/cal_chandra/'+strtrim(instr,2)+'_po0.7.dat'
	print,'spfile is assumed to be ',spfile
endif
sp_accum,spfile,ee,cc,spv
esel=where(ee ge blo and ee le bhi,nesel)
if nesel eq 0 then stop,'the energy interval seems too narrow'

sz=size(psf2)
nfrac=n_elements(frac)
afr=fltarr(sz(1),nfrac)
afrac=fltarr(sz(2))
for n=0,nfrac-1 do begin
 for k=0,sz(1)-1 do begin
	afrac(*)=psf2(k,*,n)
	linterp,ev,afrac,ee(esel),rf
	afr(k,n)=total(spv(esel)*rf)/total(spv(esel)) ;weighted 
 endfor
endfor

openw,un,outfile,/get
printf,un,sz(1),nfrac
printf,un,av
printf,un,frac
printf,un,afr
free_lun,un
return
end