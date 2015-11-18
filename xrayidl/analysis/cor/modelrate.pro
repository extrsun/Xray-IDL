;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;*NAME
; modelrate
;
;*PURPOSE:
; use a model spectral model from xspec to calculate the inputs to calcpsfp
;
;*CALLING SEQUENCE:
; modelrate,file,grouplow,grouphigh,rate,group,dir=dir
;
;*PARAMETERS:
; INPUTS:
; file - file name containing the model spectrum (e.g. from XSPEC) in units of 
;	 ph/cm^2/s/cm^2, the first line containts a description of the model
; grouplow - the lower limit of the group number as defined in !group
; grouphigh - the upper limit of the group number
;
; OUTPUTS
; rate - the model count rate in units of ph/s
; group - a copy of !group in the range of grouplow - grouphigh
;
;*PROCEDURE:
; obvious
;
;*NOTES:
; The spectral model file should contain a single column of a model spectrum
; which may be produced by using fortran program RDHIS and a history file
; produced in XSPEC using command DUMP.
;
;*SUBROUTINES CALLED:
; none
;
;*MODIFICATION HISTORY:
; writen Aug 26 1992 (WQD)
;
;-
;--------------------------------------------------------------------------
pro modelrate,file,grouplow,grouphigh,rate,group,dir=dir
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - modelrate,file,grouplow,grouphigh,rate,group,dir=dir'
retall
endif
;
if grouphigh gt 33 then begin
print,'The group range should be within the range 0-33'
return
endif
;
if n_elements(dir) eq 0 then dir=!data_dir
;
rate=fltarr(grouphigh-grouplow+1)
group=intarr(grouphigh-grouplow+1,2) ; group number grouping
ebnds=fltarr(grouphigh-grouplow+1,2) ; energy boundaries of the channels
;
filename=strtrim(file,2)
openr,un,filename,/get_lun
;
text=''
for k=0,grouplow do begin ;the first row is a string describing the model
readf,un,text
;print,'this line is excluded',text
endfor
readf,un,rate ;in units of ph/cm^2/s/keV, but no true effective area is devided
if !debug eq 1 then stop
group=!group(grouplow:grouphigh,*)
ebnds=!ebnds(grouplow:grouphigh,*)
rate=rate*(ebnds(*,1)-ebnds(*,0)) ; get real count rate in channels
close,un & free_lun,un
;
if !debug eq 1 then stop
return
end