pro emss_cl_agn,fc,fq,disth=disth,crichth=crichth
qvth=18.
tc=readfits(fc,hc,ext=1)
cn=fits_get(hc,tc,'name')
cra=float(fits_get(hc,tc,'ra_deg'))
cdec=float(fits_get(hc,tc,'dec_deg'))
nc=n_elements(cra)

tq=readfits(fq,hq,ext=1)
qn=fits_get(hq,tq,'name')
qra=float(fits_get(hq,tq,'ra_deg'))
qdec=float(fits_get(hq,tq,'dec_deg'))
qv=float(fits_get(hq,tq,'mv'))
s=where(qv lt qvth,ns)
qn=qn(s)
qra=qra(s)
qdec=qdec(s)
qv=qv(s)
dismin=1.e10
for k=0,ns-1 do begin
trans_dist,qra(k),qdec(k),cra,cdec,xp,yp,/deg
dis=sqrt(xp^2+yp^2)/7200.
if min(dis) lt dismin then dismin=min(dis)
s=where(dis lt disth,ns) 
if !debug eq 1 then stop
if ns ne 0 then begin
	print,qn(k),qv(k),cn(s),dis(s)
endif
endfor
print,'dismin = ',dismin
return
end