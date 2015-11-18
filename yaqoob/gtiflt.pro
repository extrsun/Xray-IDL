pro gtiflt,plist,ingti,intractive=intractive
;Author T. Yaqoob - March 1993->** 
if n_params(0) eq 0 then begin
print,' GTIFLT, plist, ingti, /intractive'
print,' Filters a photon list (plist) using  a GTI array GTI(*,2)'
print,' Make the GTI array with MK1GTI which operates on ascii gti files'
retall
end
np=(size(plist))(1)
print,'size of photon list on entry ',np
tmask=intarr(np)
ptime=plist.time
;make sure gtis don't overlap 
orgti,ingti,gti,nout
ntimes=(size(gti))(1)
for k=0l,ntimes-1 do begin
tmask = tmask + ((ptime ge gti(k,0)) and (ptime le gti(k,1)))
endfor
ans='y'
wm=where((tmask gt 0),newsiz)
print,'Number of events in new photon list will be ',newsiz
if n_elements(intractive) gt 0 then read,' Modify plist? (y/n)? ',ans
if ans eq 'y' then plist=plist(where(tmask gt 0 ))
print,'size of photon list on exit ',(size(plist))(1)
return
end
