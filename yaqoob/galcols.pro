pro galcols,colnames,i1,i2
if n_params(0) eq 0 then begin
 print,'galcols,colnames,i1,i2'
 retall
end
 ncol=n_elements(colnames)
 wocc=where((strmid(colnames,0,1) ne ''),nwocc)
 print,nwocc,' columns defined '
if n_elements(i1) gt 0 and n_elements(i2) gt 0 then begin
forprint,colnames(i1:i2)
endif
return
end
