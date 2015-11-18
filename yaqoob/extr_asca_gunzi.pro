pro extr_asca_gunzi, flist
    for k=0, n_elements(flist) - 1 do begin
      tail = strmid(flist(k), strlen(flist(k))-2, 2)
      if tail eq '.Z' or tail eq 'gz' then spawn, 'gunzip -v ' + flist(k)
    endfor
return
end
