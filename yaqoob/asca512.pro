pro asca512,cnts,detid=detid,fname=fn,tm=tm,npx=npx,back=back,resp=resp,cor=cor
;modified by T. Yaqoob - March 1993->**
;+
;NAME:
;asca512
;
;PURPOSE:
;to put spectral data into XSPEC format using 512 channels
;
;CALLING SEQUENCE:
;INPUTS:
; cnts = floating point array containing extracted spectrum (counts) 
;KEYWORDS:
; fname = name of output PHA file (without extension)
; tm = exposure time in sec
; npx = # of pixels containing data
; back = name of associated background file
; resp = name of associated response file
; cor = name of associated correction file
;OUTPUTS:
; none
;
;COMMON BLOCKS
;none
;SIDE EFFECTS:
;writes an XSPEC-readable (SF) .pha file 
;RESTRICTIONS:
;none
;
;PROCEDURE:
;writes file in standard format readable by XSPEC
;
;MODIFICATION HISTORY
;written 12-7-91 by M.F. Corcoran
;modified 02/11/93 by PJS
;modified 04/16/93 by PJS
if n_params(0) eq 0 then begin
print,'ASCA512,cnts,detid=detid,fname=fname,tm=tm,npx=npx,back=back,resp=resp,cor=cor
print,'Put spectral data into XSPEC format using 512 channels
retall
end
if n_elements(fname) eq 0 then begin
    fname=''
    read,'Enter filename (w/o extension): ',fname
endif
if n_elements(tm) eq 0 then read,'Enter integr time: ',tm
tm=float(tm)
if n_elements(npx) eq 0 then read,'Enter pixel number: ',npx
npx=float(npx)
openw,lun,fname+'.pha',/get_lun,/f77
;               1         2         3         4         5         6         7
;      1234567890123456789012345678901234567890123456789012345678901234567890
hdr=  'SF01XSPEC data            '+!stime
hdr=hdr+padder(78-strlen(hdr))
writeu,lun,hdr
;
;  write ass. package
;
len=-28L
pktyp='ass. files'
pktyp=pktyp+padder(12-strlen(pktyp))
writeu,lun,len,pktyp,long(1),long(3),padder(8)
if n_elements(back) eq 0 then back='none'
if n_elements(resp) eq 0 then resp='/home1/pjs/asca/sis/sis_s0_ch0.rsp'
if n_elements(cor)  eq 0 then cor='none'
writeu,lun,strlen(back),back
writeu,lun,strlen(resp),resp
writeu,lun,strlen(cor),cor
;
; write file info package
;
len=-128L
;detid='SIS_S0_CH0'
detid=detid+padder(16-strlen(detid))
nbin=long(n_elements(cnts)) & nchan=512L
A=1.0
cor=0.0
cnts=float(cnts)
indices=fltarr(3)*0.
spare=intarr(48)*0
pktyp='file info'
pktyp=pktyp+padder(12-strlen(pktyp))
if !debug eq 1 then stop
writeu,lun,len,pktyp,long(1),long(0),padder(8), $
       detid,nbin,nchan,tm,a,npx,cor,indices,spare
;
; write grouping package
;
len=-28L-512L
pktyp='grouping'
pktyp=pktyp+padder(12-strlen(pktyp))
grp=strarr(513,1)
grp(1:*)='+'
;grp(1:6)='*'
grp(505:*)='*'
writeu,lun,len,pktyp,long(1),long(0),padder(8),grp(1:*)
;
; write pha package 
;
len=-4L*n_elements(cnts)-28L
pktyp='pha data'
pktyp=pktyp+padder(12-strlen(pktyp))
writeu,lun,len,pktyp,long(1),long(0),padder(8),float(cnts)
;
; write pha errors package
;
;pktyp='pha errors'
;pktyp=pktyp+padder(12-strlen(pktyp))
;writeu,lun,len,pktyp,long(1),long(0),padder(8),float(sigrate)
close,lun ; all data written
return
end
