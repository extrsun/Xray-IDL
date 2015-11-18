pro check_kev_to_pha, plist

if n_params(0) eq 0 then begin
  print, 'check_kev_to_pha, plist'
  return
endif

ccds = minmax(plist.ccdid)
for i=ccds(0), ccds(1) do begin
  w = where(plist.ccdid eq i, n)
  if n gt 0 then begin
    s = stdev(plist(w).energy/plist(w).pha, m)
    print, i, n, m, ' +/-', s/sqrt(n)
  endif
endfor
return
end
