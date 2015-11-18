pro rdgisgain,gainfile,gain
if n_params(0) eq 0 then begin
 print,' RDGISGAIN, gainfile,gain '
; print,' new = 0 - OLD KEYWORD conventions '
; print,' new = 1 - NEW KEYWORD conventions '
 print,' makes a structure variable with the gain history of GIS2&3 on ASCA'
 retall
end
new = 0
;need to structure 14 columns:
; OLD format
; 1   START TIME ASCA_TIME(s)   1D            label for field 1
; 2     END TIME ASCA_TIME(s)   1D            label for field 2
; 3 S2 HVL LEVEL     none       1I            label for field 3
; 4 S2 HVH LEVEL     none       1I            label for field 4
; 5     S2 TEMP CENTIGRADE   1E            label for field 5
; 6    S2 CAL PH     channel    1E            label for field 6
; 7    S2 CAL RT     channel    1E            label for field 7
; redundant S2 NOF PHOTO     counts     1E            label for field 8
; 8 S3 HVL LEVEL     none       1I            label for field 9
; 9 S3 HVH LEVEL     none       1I            label for field 10
;10     S3 TEMP    CENTIGRADE   1E            label for field 11
;11    S3 CAL PH     channel    1E            label for field 12
;12    S3 CAL RT     channel    1E            label for field 13
;redundant S3 NOF PHOTO     counts     1E            label for field 14
;NEW format
;1 CAL_START == START TIME
;2 CAL_STOP == END TIME
;3 HV_LOW_S2 == S2 HVL LEVEL
;4 HV_HIGH_S2 == S3 HVH LEVEL
;5 TEMP_S2 == S2 TEMP
;6 FE55_PEAK_S2 == S2 CAL PH
;7 RT_PEAK_S2 == S2 CAL RT
; ETC
tab=readfits(gainfile,h,ext=1) & nval=(size(tab))(2)
row={gaintab,tbeg:0.0D0,tend:0.0D0,s2hvl:0l,s2hvh:0l,s2tem:0.0,s2calph:0.0,$
s2calrt:0.0,s3hvl:0l,s3hvh:0l,s3tem:0.0,s3calph:0.0,s3calrt:0.0,vers:0}
gain=replicate(row,nval)
version=sxpar(h,'version') & gain.vers=version
print,' Gain History File Version (i.e. type ) ',version
if version eq 0 then print,' *WARNING* VERSION Keyowrd may not exist '
if version lt 2 then begin
tbeg=tbget(h,tab,'START TIME') & gain.tbeg=tbeg
tend=tbget(h,tab,'END TIME') & gain.tend=tend
s2hvl=tbget(h,tab,'S2 HVL LEVEL') & gain.s2hvl=s2hvl
s2hvh=tbget(h,tab,'S2 HVH LEVEL') & gain.s2hvh=s2hvh
s2tem=tbget(h,tab,'S2 TEMP') & gain.s2tem=s2tem
s2calph=tbget(h,tab,'S2 CAL PH') & gain.s2calph=s2calph
s2calrt=tbget(h,tab,'S2 CAL RT') & gain.s2calrt=s2calrt
s3hvl=tbget(h,tab,'S3 HVL LEVEL') & gain.s3hvl=s3hvl
s3hvh=tbget(h,tab,'S3 HVH LEVEL') & gain.s3hvh=s3hvh
s3tem=tbget(h,tab,'S3 TEMP') & gain.s3tem=s3tem
s3calph=tbget(h,tab,'S3 CAL PH') & gain.s3calph=s3calph
s3calrt=tbget(h,tab,'S3 CAL RT') & gain.s3calrt=s3calrt
endif
if version ge 2 then begin
tbeg=tbget(h,tab,'CAL_START')
if nval gt 1 then gain.tbeg=tbeg else gain.tbeg=tbeg(0)
tend=tbget(h,tab,'CAL_STOP')
if nval gt 1 then gain.tend=tend else gain.tend=tend(0)
s2hvl=tbget(h,tab,'HV_LOW_S2')
if nval gt 1 then gain.s2hvl=s2hvl else gain.s2hvl=s2hvl(0)
s2hvh=tbget(h,tab,'HV_HIGH_S2')
if nval gt 1 then gain.s2hvh=s2hvh else gain.s2hvh=s2hvh(0)
s2tem=tbget(h,tab,'TEMP_S2')
if nval gt 1 then gain.s2tem=s2tem else gain.s2tem=s2tem(0)
s2calph=tbget(h,tab,'FE55_PEAK_S2')
if nval gt 1 then gain.s2calph=s2calph else gain.s2calph=s2calph(0)
s2calrt=tbget(h,tab,'RT_PEAK_S2')
if nval gt 1 then gain.s2calrt=s2calrt else gain.s2calrt=s2calrt(0)
s3hvl=tbget(h,tab,'HV_LOW_S3')
if nval gt 1 then gain.s3hvl=s3hvl else gain.s3hvl=s3hvl(0)
s3hvh=tbget(h,tab,'HV_HIGH_S3')
if nval gt 1 then  gain.s3hvh=s3hvh else gain.s3hvh=s3hvh(0)
s3tem=tbget(h,tab,'TEMP_S3')
if nval gt 1 then  gain.s3tem=s3tem else gain.s3tem=s3tem(0)
s3calph=tbget(h,tab,'FE55_PEAK_S3')
if nval gt 1 then gain.s3calph=s3calph else gain.s3calph=s3calph(0)
s3calrt=tbget(h,tab,'RT_PEAK_S3')
if nval gt 1 then gain.s3calrt=s3calrt else gain.s3calrt=s3calrt(0)
endif
return
end
