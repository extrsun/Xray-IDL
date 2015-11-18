pro read_acf,filename,angle,acf,acferr,fluxmean,fluxerr,nbin
;+
; read auto-correlation function data from the file created by get_acf.pro
;-
if n_params() eq 0 then begin
print,' CALLING SEQUENCE - read_acf,filename,angle,acf,acferr,fluxmean,fluxerr'
print, '  ,nbin'
return
endif
;
openr,un,filename,/get_lun
n_angle=0
fluxmean=0.
fluxerr=0.
nbin=0
readf,un,n_angle,fluxmean,fluxerr,nbin
a=fltarr(3,n_angle)
readf,un,a
a=transpose(a)
free_lun,un
angle=a(*,0)
acf=a(*,1)
acferr=a(*,2)
;if !debug eq 1 then stop
end