pro pspectrum,filter,pspectrum,etime,pc,sctime,mvr,dtype=dtype,block=block $
,tfile=tfile,vig=vig,group=group,proc=proc
;+
; Obtain particle spectrum (cts/s/deg^2/chan) and counts (cts/chan; if pc is 
; set) in the area with
; non-zero exposure. The default file *_gti.dat is used for the time filtering
; The filter is assumed to be centered on the axis
; keyword vig is for artificial vignetting correction which should be 
;  subtracted later.
; This is just an approximation. The real filter should be in the detector's
; coordinate with correction for source subtractions WQD 5/16/93
; writen by WQD, Dec 2 , 1992
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --- pspectrum,filter,pspectrum,etime,pc,sctime,mvr'
print,',dtype=dtype,block=block,tfile=tfile,vig=vig,group=group,proc=proc'
return
endif
;
if n_elements(group) eq 0 then group=!group
if n_elements(proc) eq 0 then proc=strtrim(!proc,2)
;
; first obtain the MV rate
;
inputs='obseq='+strtrim(!seq_no,2)+',dir='+strtrim(!data_dir,2)+',extyp=evr' $
	+',instr=p'+',proc='+proc
rsgetevr,inputs,0,sctime,rates
mvr=rates.mv ;rate
;
print,'filter the MV time with the selected time  intervals
;
if n_elements(tfile) eq 0 then tfile=!seq_no+'_gti.dat'
filter_time,!data_dir+tfile,sctime,indsel
sctime=sctime(indsel)
mvr=mvr(indsel) ;for every two seconds
;
print,' get the off-axis area distribution of the filter'
;
hist_area,filter,posi,kdup,nposi,block=block
if !debug eq 1 then stop
;
print,'calculate particle spectral rates in the groups'
;
sz=size(group)
gdim=sz(1)
pc=fltarr(gdim) 
chanlow=group(*,0) & chanhigh=group(*,1) 
chanmid=(chanlow+chanhigh)*0.5
vig=1. ;default

for k=0,33 do begin
	get_particle,mvr,posi,chanlow(k),chanhigh(k),pcount,dtype=dtype
	if keyword_set(vig) ne 0 then begin
		off_ax_a_new,posi,chanmid(k),vig,ierr
		if ierr eq 1 then stop,'ierr = 1 in off_ax_a' 
	endif 
	pc(k)=total(pcount*kdup/vig)
endfor
etime=n_elements(sctime)*2.
pspectrum=pc*(3600./total(kdup)/etime) ;in units of cts/deg^2/s
if !debug eq 2 then stop
end