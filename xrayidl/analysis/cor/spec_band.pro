pro spec_band,file,frac,fileout=fileout,rate=rate,sigrate=sigrate,group=group $
,syserr=syserr
;+ 
; Calculate broad band source spectrum from the results of ACF analysis
; file - the data file from ACF analysis
; frac - The X-ray fraction of the total observed counts as source counts
;	in the band 7
; writen by WQD, Nov 1992
;-
if n_params() eq 0 then begin
	print,'CALLING SEQUENCE - '
	print,' spec_band,file,frac,fileout=fileout,rate=rate,sigrate=sigrate'
	print,',group=group,syserr=syserr'
	return
endif
if n_elements(frac) eq 0 then frac=1.
;
openr,un,file,/get
readf,un,blow,bhigh,nbin,etime,npar
data=fltarr(npar,bhigh-blow+1)
readf,un,data
close,un
b2=data(0,*)
b2err=data(1,*)
flux=data(2,*)
meanv=data(3,*)
;
if !debug eq 1 then stop
area=nbin*(15./3600.)^2 ; in units of degree square
counts=flux*nbin
rate=fltarr(8)
sigrate=rate
norm=frac/sqrt(abs(b2(bhigh-blow))) ;surposed to be the channel 7
;
ratio=sign(b2)*sqrt(abs(b2))*norm
ratio_err=ratio*(b2err*0.5)/sqrt(abs(b2)*abs(b2))
count_c=counts/meanv
;
rate(blow-1:bhigh-1)=count_c*ratio/area/etime
sigrate(blow-1:bhigh-1)=abs(rate(blow-1:bhigh-1))* $
	sqrt((b2err*0.5)^2/(abs(b2)*abs(b2)) + 1./counts + syserr*syserr)

print,'Vignetting corrected counts,particle ratios, rateo_err, rate, rate_err'
for k=0,bhigh-blow do begin
	print,count_c(k),ratio(k),ratio_err(k),rate(blow-1+k),sigrate(blow-1+k)
endfor
;
;
if n_elements(fileout) eq 0 then fileout=file
print,'output spectral file is ',fileout
inputs='FNAME='+fileout+',BKFIL=none'
if n_elements(group) eq 0 then begin
	print,'!bandch is used as group'
	group=!bandch
	print,group
endif 
etime=1.
area=1.
make_pha,inputs,rate,sigrate,etime,area,group=group
if !debug eq 1 then stop
end