pro sou_fits_mult,flist,ext=ext,fdir=fdir
;+
; convert multiple ascii source files into fits files
;*Example:
; sou_fits_mult,'../imaging/flist.dat'
; written by wqd, 12/29/2001
;-
if n_params() eq 0 then begin
print,'CALLING Seq. - sou_fits_mult,flist,ext=ext,fdir=fdir'
return
endif
if n_elements(fdir) eq 0 then fdir='../xdata/'
if n_elements(ext) eq 0 then ext='_map70_mlm_ratio'
openr,un,flist,/get
fname=''
while not eof(un) do begin
	readf,un,fname
;	hdrref=headfits(fdir+fname+'.fits')
;	sxaddpar,hdrref,'CREATOR','sou_fits.pro, '+systime()
;	sxaddpar,hdrref,'Comment: ','Except for CREATOR, the rest from '+fname+'.fits'
	fname=fname+ext ;source file
	sou_fits,fname,fname+'.fits',hdrref=hdrref
endwhile
free_lun,un
return
end