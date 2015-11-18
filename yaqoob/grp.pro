pro grp,grpstr,srcspec,bgdfile,arfname,respname,outfil
if n_params(0) eq 0 then begin
 print,'grp,grpstr,srcspec,bgdfile,arfname,respname,outfil'
 retall
endif
bad='bad 1-15 361-512'
rebin=20
grpstr= 'grppha ' + srcspec + ' ' + outfil + ' comm = " '+ bad + $
' & group min ' + strn(rebin) + ' & chkey resp ' + respname + $
' & chkey ancrfile ' + arfname + ' & chkey backfile = '$
+strtrim(bgdfile,2) + ' & exit "'
return
end
