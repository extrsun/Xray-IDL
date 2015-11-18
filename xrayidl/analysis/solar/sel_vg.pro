pro sel_vg,seslow,seshigh,estlow,esthigh
;+
; select time intervals within which the SES AND EST angles less or equal to
; the input sescode_sel and estcode_sel
; the standard good time intervals are also used
;-
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - sel_vg,seslow,seshigh,estlow,esthigh'
return
endif
;
; get vg code information
;
inputs='obseq='+strtrim(!seq_no,2)+',dir='+strtrim(!data_dir,2) $
	+',proc='+strtrim(!proc,2)
rsgetvg,inputs,0,time,vginfo,obinfo=obinfo,scttime=scttime
timeo=scttime
; output is now the correct space craft time
;sctbe=obinfo.sctbeg
;
;time=time+(sctbe(0)-time(0))    ;converted from time on the day 
				; to the space craft time
sescodeo=vginfo.sscode
estcodeo=vginfo.epcode
if !debug eq 2 then stop
;
;
answer = 'Y'
yesno, answer
;
; Loop through to create selected time intervals
;
while (answer eq 1) do begin
; select those points with vg codes satisfying the selection criteria
;
sel=where((sescodeo ge seslow and sescodeo le seshigh) and $
	(estcodeo ge estlow and estcodeo le esthigh), nsel)
if nsel eq 0 then stop,'no time interval within the selected codes'
time=timeo(sel)
sescode=sescodeo(sel)
estcode=estcodeo(sel)
if !debug eq 2 then stop
;
; filter with the standard good time intervals
;
filter_sgti,time,sel
time=time(sel)
sescode=sescode(sel)
estcode=estcode(sel)
if !debug eq 2 then stop
;
; calculate the time intervals from the selected time sequences
;
get_interval,60,time,timeint,ntimeint  

;find the locations where the adjacent points have the increments greater than
;60 seconds (the sample increment of the data)

; output into a data file to be used to create the exposure map
;
tail=strtrim(seslow,2)+strtrim(seshigh,2)+strtrim(estlow,2)+strtrim(esthigh,2)
fname=!seq_no+'_vg'+tail+'.dat'
output_int,timeint,outfile=fname
;
print,'Do you wish to create VG file with different VG limits: '
answer='no'
read,answer
yesno,answer
if answer eq 1 then $
	read,'Please give new VG code limits: seslow,seshigh,estlow,esthigh: ', $
		seslow,seshigh,estlow,esthigh
endwhile
	
end