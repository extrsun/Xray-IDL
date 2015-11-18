pro galindex,srcnames,source,index
if n_params(0) eq 0 then begin
 print,'galindex,srcnames,source,index'
 print,'Return array INDEX for object name SOURCE '
 retall
end
namel=strlen(source)
index = where((strmid(srcnames,0,namel)) eq source)
print,'INDEX = ',index
return
end
