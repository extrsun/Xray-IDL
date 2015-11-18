pro getplotints,time,kbeg,kend
;
; procedure to look for large time gaps in a time array
; and to return indices of intervals between large gaps
;
ntime = n_elements(time)
delt = time(1:*) - time(0:ntime-2)
ind = sort(delt)
delt = delt(ind)                     ;sort in order of increasing time diff
igtz = min(where(delt gt 0))         ;smallest index where delt > 0
iavg = ntime/2                       ;index of median (not average!) delta t
avdel= delt(iavg > igtz)             ;use median delt or first value > 0
;
kbeg = [-999] & kend = kbeg
indgt = where(delt gt 10.*avdel)     ;look for large time gaps
ngt = n_elements(indgt)
;
if ( (indgt(0) ge 0) and (ntime gt 4)) then begin
  kb = 0 & ke = indgt(0)
  if (kb ne ke) then begin      ;first interval starts at kbeg=0
    kbeg = [kbeg,kb]
    kend = [kend,ke]
  endif 
  if ( (kb eq ke) and (ngt eq 1) ) then begin  ;first ends at ntime-1
    kbeg = [kbeg,ke+1]
    kend = [kend,ntime-1]
  endif
;
  if (ngt ge 1) then begin      ;2 or more plot intervals
    for jj=0,ngt-2 do begin
      kb = indgt(jj) + 1
      ke = indgt(jj+1) < (ntime-1)
      kbeg = [kbeg,kb]
      kend = [kend,ke]
    endfor
    kb = indgt(ngt-1) + 1
    if (kb lt (ntime-1)) then begin     ;last plot interval
      kbeg = [kbeg,kb]
      kend = [kend,ntime-1]
    endif 
  endif
;
endif else begin
  kbeg = [kbeg,0]
  kend = [kend,ntime-1]
endelse
kbeg = kbeg(1:*) & kend = kend(1:*)
;
return
end            ;pro getplotints
