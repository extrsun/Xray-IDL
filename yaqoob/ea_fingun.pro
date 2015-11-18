function ea_fingun, srcstr, count=count

if n_params(0) eq 0 then begin
  print, 'ea_fingun, srcstr, count=count
  print, 'Checks to see if files exists and decompresses them if necessary'
  retall
endif

flist = findfile(srcstr, count=count)
if count gt 0 then ea_gunzip, flist
flist = findfile(srcstr, count=count) ; now flist contains decompressed names
return, flist
end
