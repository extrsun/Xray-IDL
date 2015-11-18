pro pulsar_err,rant,ff,delff,ffd,fbestv,df,filter,efilter,phd=phd,siglevel=siglevel
if n_elements(phd) eq 0 then phd=0.05
;if n_elements(tmin) eq 0 then tmin=0.0d
if n_elements(siglevel) eq 0 then siglevel=0.05
sz=size(rant)
if sz(0) eq 2 then rann=sz(2) else rann=1
fbestv=dblarr(rann)
for k=0,rann-1 do begin
	pfold,rant(*,k),ff-delff,ff+delff,ffd,fbest=fbest,phd=phd,df=df $
		,filter=filter,efilter=efilter,/plotoff
	fbestv(k)=fbest
endfor
avg_median,fbestv-ff,mfbest,mfbestlo,mfbesthi,siglevel=siglevel
print,'div mfbest,mfbestlo,mfbesthi:'
print,mfbest,mfbestlo,mfbesthi,form='(f20.10)'
print,'relative error = ',(mfbesthi-mfbestlo)*0.5d/fbest
print,'max relative error = ',(max(fbestv)-min(fbestv))/ff
return
end