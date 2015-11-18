pro galrows,srcnames,i1,i2
if n_params(0) eq 0 then begin
 print,'galrows,srcnames,i1,i2'
 retall
end
 nrow=n_elements(srcnames)
 wocc=where((strmid(srcnames,0,1) ne ''),nwocc)
 print,nwocc,' columns defined '
if n_elements(i1) gt 0 and n_elements(i2) gt 0 then begin
forprint,srcnames(i1:i2)
endif
return
end
