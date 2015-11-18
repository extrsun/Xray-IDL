pro extr_asca_gunzip, flist
    for k=0, n_elements(flist) - 1 do begin
      tail = strmid(flist(k), strlen(flist(k))-2, 2)
      if tail eq '.Z' or tail eq 'gz' then spawn, 'gunzip -v ' + flist(k)
    endfor
return
end
