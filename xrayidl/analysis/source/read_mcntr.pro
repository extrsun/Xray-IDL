pro read_mcntr,temp,nh,cntr,blow,bhigh,fname=fname,fhead=fhead,flux=flux
;-
; read the cntr file as a function of temperature and absorption
; created in ~/rosatshell/galaxies/. see memo in the directory.
; temp - vector containing the temperature values
; nh - vector containing the column density values
; cntr - count rate as a function of temperature (x-axis) and column density 
;		(y-axis)
; fhead - input file name head
; blow, bhigh - lower and upper band limits (i.e., 2,2; 4,5; 6,7)
; fname - input file name overruling fhead and blow and bhigh
;
; writen by wqd, April 24, 1996
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - read_mcntr,temp,nh,cntr,fhead,blow,bhigh,fname=fname=flux=flux
return
endif
if n_elements(fname) eq 0 then begin
	if n_elements(blow) eq 0 then blow=4
	if n_elements(bhigh) eq 0 then bhigh=5
	if n_elements(fhead) eq 0 then fhead='~/rosatshell/galaxies/pspc_vr_a1.0e_'
	fname=fhead + strtrim(blow,2)+strtrim(bhigh,2) ;+'.dat'
	; for source spectrum with a raymond model and absorption of 
	; both abudances = 50% solar
endif
openr,unin,fname,/get_lun
readf,unin,nl,nv
temp=fltarr(nl)
readf,unin,temp
cntr=fltarr(1+nl,nv)
readf,unin,cntr
if not eof(unin) then begin
	flux=fltarr(nl)
	readf,unin,flux
endif
free_lun,unin
nh=fltarr(nv)
nh(*)=cntr(0,*)
cntr=cntr(1:*,*)
return
end