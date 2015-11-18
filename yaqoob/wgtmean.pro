pro wgtmean,x,sigma,xmean,sigout
if n_params(0) eq 0 then begin
 print,'wgtmean,x,sigma,xmean,sigout'
 print,'Compute weighted mean'
 print,'Excludes all points with sigma <= 0 in calculation'
 retall
end
npts=(size(x))(1)
wnz=where((sigma gt 0.),nwnz)
if nwnz gt 0 then begin
 x=x(wnz) & sigma=sigma(wnz)
endif
sigfac=1./sigma/sigma
sigout=sqrt(1./total(sigfac))
xmean=total(x*sigfac)/total(sigfac)
return
end
