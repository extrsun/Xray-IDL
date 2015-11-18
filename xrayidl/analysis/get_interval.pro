pro get_interval,step,time,timeint,ntimeint,loc=loc
;
; calculate the time intervals from the selected time sequences
;

if n_params() eq 0 then begin
	print,'CALLING SEQUENCE --- get_interval,step,time,timeint,loc=loc'
	return
endif
;
test=[time,-1]
loc=where(test(1:*)-time ne step);find the locations where the adjacent points
				 ;have the increments greater than
				;the normal step of the time sequence 
ntimeint=n_elements(loc)
timeint=lonarr(2,ntimeint)
timeint(1,*)=time(loc)			;ends of the time intervals
if ntimeint eq 1 then timeint(0,0)=time(0) else $
timeint(0,*)=[time(0),time(loc(0:ntimeint-2)+1)] ;beginings of the intervals
loc=where((timeint(1,*)-timeint(0,*)) ne 0,nsel)
if nsel eq 0 then print,'no selection' else begin
	timeint=timeint(*,loc)
	ntimeint=n_elements(loc)
endelse
end