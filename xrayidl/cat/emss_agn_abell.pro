pro emss_agn_abell,fc,fq,disth=disth,crichth=crichth,qvth=qvth

tc=readfits(fc,hc,ext=1)
cn=fits_get(hc,tc,'abell')
cra=float(fits_get(hc,tc,'dra'))
cdec=float(fits_get(hc,tc,'ddec'))
cr=float(fits_get(hc,tc,'clus_rad'))
crich=float(fits_get(hc,tc,'rich_grp'))
if n_elements(crichth) ne 0 then begin
	s=where(crich ge crichth,nc)
	cn=cn(s)
	cra=cra(s)
	cdec=cdec(s)
	cr=cr(s)
	crich=crich(s)
endif else $
;s=where(cr lt 1.,ns)
nc=n_elements(cra)

tq=readfits(fq,hq,ext=1)
qn=fits_get(hq,tq,'name')
qra=float(fits_get(hq,tq,'ra_deg'))
qdec=float(fits_get(hq,tq,'dec_deg'))
sprecess,qra,qdec,1950.,2000.
qv=float(fits_get(hq,tq,'mv'))
s=where(qv lt qvth,ns)
qn=qn(s)
qra=qra(s)
qdec=qdec(s)
qv=qv(s)
dismin=1.e10

crp=cr*7200.
for k=0,ns-1 do begin
trans_dist,qra(k),qdec(k),cra,cdec,xp,yp,/deg
dis=sqrt(xp^2+yp^2)/crp
if min(dis) lt dismin then dismin=min(dis)
s=where(dis lt disth,ns) 
if !debug eq 1 then stop
if ns ne 0 then begin
	print,qn(k),qv(k),float([dis(s),cr(s)]),fix([cn(s),crich(s)])
endif
endfor
print,'dismin = ',dismin
stop
return
end