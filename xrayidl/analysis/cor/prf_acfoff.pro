pro prf_acfoff,offangle,tail,area,angle,acf,dir=dir,file=file $
,cal_angle=cal_angle
;+
; Calculate an off-axis ACF by interplating the ACFs of calibration sources
;*INPUTS:
; offangle - the off-axis angle in units of arcminutes
; tail - the band interval for the calculation of the acf (1-7)
; area - the area with which the acf is to be renormalized to
; 
;*OUTPUTS:
; angle - containing the angles in which the acf is calculated in units of 15"
; acf -  the acf at the angles
;-
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - prf_acfoff,offangle,tail,area,angle,acf'
return
endif
;
if n_elements(dir) eq 0 then dir='/home/casa/wqd/rosat/prf/'
;
if n_elements(file) eq 0 then $
	file=['prf','rp110595','rp110594','rp110602','rp110586','rp110590', $
   'rp110599','rp110591','rp110598']
newtail='_acf'+strtrim(tail,2)+'.dat'
if n_elements(cal_angle) eq 0 then $
	cal_angle=[0.,10.70,11.86,13.38,14.96,16.23,17.41,18.16,19.40] 
;
;the last value is the limit of the calibration range
;
find=where( cal_angle gt offangle)
if find(0) gt 0 then k=find(0) else $
print,'the off-axis angle is outside the calibration range'
;
;print,'read prf acf files'
fname=dir+file(k-1)+newtail
read_acf,fname,angle,cal_acf1,acferr,fluxmean,fluxerr,nbin
cal_acf1=(cal_acf1+1.)*(area/(nbin*0.0625))-1. ;area in units of arcmin
fname=dir+file(k)+newtail
read_acf,fname,angle,cal_acf2,acferr,fluxmean,fluxerr,nbin
cal_acf2=(cal_acf2+1.)*(area/(nbin*0.0625))-1.
;
acf=cal_acf1+(cal_acf2-cal_acf1)*((offangle-cal_angle(k-1)) $
/(cal_angle(k)-cal_angle(k-1)))

if !debug eq 2 then stop
end