pro tmflt_arr,plist,qlist,ntimes,tint
if n_params(0) eq 0 then begin
print,' TMFLT_ARR, plist, qlist, tint'
print,' Filters a photon list (plist) using  a GTI array tint(2,*)'
print,' Output in qlist , tint has ntimes intervals and offset is tzero'
retall
end
np=(size(plist))(1)
tmask=intarr(np)
ptime=plist.time
for k=0,ntimes-1 do begin
  tmask = tmask + ((ptime gt tint(0,k)) and (ptime lt tint(1,k)))
endfor
qlist=plist(where(tmask gt 0 ))
return
end
