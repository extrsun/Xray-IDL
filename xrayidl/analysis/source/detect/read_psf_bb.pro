pro read_psf_bb,dis,core_size,psffile=psffile,perclimit=perclimit
;-
; get encircled energy radii of the PSF.
;
; dis - off-axis distances (arcmin) of the sources
; core_size - the output radii (arcmin)
; psffile -  the file name of the PSF file (including the directory)
; perclimit - scalar or vector containing the fraction(s) of the PSF energy 
; 		(def = 0.8) for calculating core_size
;
; The PSF file can be produced by psf_bb.pro 
; 
; writen by wqd, 6/7/2001
;+
if n_params() eq 0 then begin
print,'read_psf_bb,dis,core_size,psffile=psffile,perclimit=perclimit'
return
endif
if n_elements(perclimit) eq 0 then perclimit=0.8
if n_elements(psffile) eq 0 then $
	psffile=!cdir+'psf_po0.7_'+strtrim(!instr,2)+'_0.dat'
openr,unin,psffile,/get_lun
readf,unin,nv,nl
perc=fltarr(nl)
offang=findgen(nv)
radius=findgen(nv,nl)
readf,unin,offang,perc,radius
free_lun,unin

acore_size=radius
binterp,offang,perc,radius,dis,dis*0.+perclimit,core_size
;The cubic interplation produce negantive values near the edge of the indeces.
;tabinv_m,offang,dis,xind
;tabinv_m,perc,dis*0.+perclimit,yind
;core_size=rinter(radius,xind,yind)+0.1 ;0.1" error due to the aspect and acis
; binterp automatically set outliers into the boundary values 
;	(ie., 90%-99%)
return
end