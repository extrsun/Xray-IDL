;+
;
;NAME:
; make_fdist
;
;PURPOSE:
; bin a list of sources or photons into predefined defined bins.
;
;INPUTS:
;list = a list of count rate (or energies) of individual sources (photons)
;group - boundaries of the predefined bins. The size = n_elements(list)+1
;
;OUTPUTS:
; fdist - the histogram of the sources or photons
;
;MODIFICATION HISTORY:
;WQD, Oct 22, 2003
;
;-
pro make_fdist,list,group,fdist
;
if n_params(0) eq 0 then begin
print,'CALLING SEQUENCE --- make_fdist,list,group,fdist'
retall
endif
nchan=n_elements(group)-1
slo=group(0:nchan-1)
shi=group(1:nchan-0)
fdist=intarr(nchan)
for j=0,nchan-1 do begin
  indchan = where((list ge slo(j)) and (list lt shi(j)),nc) 
    fdist(j) = nc
endfor
return
end            

