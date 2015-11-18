pro fft_mult_in,dt,tintv,nfrlowr,fname=fname
;-
; This program was origninally intended for fft_mult.pro.
; but it does not work because the fft_mult requires
; an equal length for tintv
;+
if n_elements(fname) eq 0 then fname=!seq_no+'_obstime.dat'

openr,un,fname,/get
readf,un,nint
a=lonarr(5,nint)
readf,un,a
close,un
tintv=lonarr(2,nint)
tintv(0,*)=a(1,*)
tintv(1,*)=a(2,*)
tintv=tintv-tintv(0,0)
tdif=tintv(1,*)-tintv(0,*)
nbin=long(tdif/dt+1)/2
nfrlowr=nbin-min(nbin)
stop
return
end
