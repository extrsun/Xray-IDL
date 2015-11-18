pro interp_aspect,time,asptim,roll,delx,dely
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' INTERP_ASPECT, time, asptim, ROLL, DELX, DELY'
  return
endif
;
if (npar lt 3) then begin
  print,' INTERP_ASPECT needs at least 3 parameters to run. Returning.'
  return
endif
;
ntime = n_elements(time)
nasp = n_elements(asptim)
indsrt = sort(time)
;
div = 2000.
mindiv = 250.
ntry = ntime/div + 1
while ( (ntry lt 2) and (div gt mindiv)) do begin
  div=div/2.
  ntry = ntime/div + 1
endwhile
;  
ibeg = indgen(ntry)*div
if (ntry ge 2) then iend = [ibeg(1:*)-1.,ntime-1.] else $
                    iend = [ntime-1.]
if (!debug gt 1) then begin
  print,' ntime,nasp,div,ntry: ',ntime,nasp,div,ntry
  stop,'Stopping in interp_aspect. before loop.'
endif
;
if (npar ge 5) then begin
  oldy = dely
  dely = time*0.
endif
if (npar ge 4) then begin
  oldx = delx
  delx = time*0.
endif
oldroll = roll
roll = time*0.
;
ib = 0
for ii=0,ntry-1 do begin
  ib = ibeg(ii)
  ie = iend(ii)
  kb = min( where( asptim ge time(indsrt(ib)) ) ) 
  kb = (kb - 1) > 0
  ke = max( where( asptim le time(indsrt(ie)) ) )
  ke = (ke + 1) < (nasp - 1)  
  if (!debug gt 2) then print,'Loop ',ii,div,ib,ie,kb,ke
  if (!debug gt 5) then stop
;
  tev = time(indsrt(ib:ie))
  tasp = asptim(kb:ke)
;
; interpolate aspect info (within that interval) to photon arrival times
;
  vec = 0
  vec = oldroll(kb:ke)
  linterp,tasp,vec*1.,tev,vec
  roll(indsrt(ib:ie)) = vec
;
  if (npar ge 4) then begin
    vec = 0
    vec = oldx(kb:ke)
    linterp,tasp,vec*1.,tev,vec
    delx(indsrt(ib:ie)) = vec
  endif
; 
  if (npar ge 5) then begin
    vec = 0
    vec = oldy(kb:ke)
    linterp,tasp,vec*1.,tev,vec
    dely(indsrt(ib:ie)) = vec
  endif
endfor
;
return
end            ;interp_aspect
