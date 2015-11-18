pro calcmnsd,x,xmean,xsd
if n_params(0) eq 0 then begin 
 print,' CALCSD, X'
 print,' program to calculate the mean and sample standard deviation of the vector X'
 retall
end
if n_elements(X) le 1 then begin
 print,' There must be more than one value of X '
 print,' ** CALCMNSD ABORTING ** '
 retall
end
np=float((size(x))(1))
xmean=total(x)/np
dev = x-xmean & devsq = dev*dev
totdev = total(devsq)
if totdev gt 0.0 then xsd=sqrt(totdev/(np-1.)) else xsd = 0.0
return
end
