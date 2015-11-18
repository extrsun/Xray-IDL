pro cl_acf_plot,bandlow,bandhigh,area
;+

;*INPUTS:

; bandlow, bandhigh - the band interval for the calculation of the acf (1-7)
; 
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - prf_cal_plot,bandlow,bandhigh,area'
return
endif
;
dir='/home/casa/wqd/rosat/analysis/cor/'
;
file=['cl5','cl8','cl10','cl20','cl40']
tail='_acf'+strtrim(bandlow,2)+strtrim(bandhigh,2)+'.dat'
;
fname=dir+file(0)+tail
read_acf,fname,angle,acf,acferr,fluxmean,fluxerr,nbin
acf=(acf+1.)*(area/(nbin*0.0625))-1. ;area in units of arcmin
plot,angle/4.,acf,Ytitle='CLUSTER ACF',Xtitle='Angle (arcmin)'
;
for k=1,n_elements(file)-1  do begin
fname=dir+file(k)+tail
read_acf,fname,angle,acf,acferr,fluxmean,fluxerr,nbin
acf=(acf+1.)*(area/(nbin*0.0625))-1.
if k gt 4 then oplot,angle/4.,acf,line=k-4,thick=2. $
 else oplot,angle/4.,acf,line=k
endfor
;
if !debug eq 2 then stop
end