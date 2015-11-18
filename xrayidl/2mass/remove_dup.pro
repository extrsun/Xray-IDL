pro remove_dup,ra,dec,locout,radel=radel
;rth - threshold of radius for duplication exclusion in units of pixel size
;pro remove_dup,lin,dx,dy,locout,radel=radel
if n_elements(radel) eq 0 then radel=1.
if n_elements(rth) eq 0 then rth=1.

;dx=long(dx) & dy=long(dy)
;ra=lin.ra
;dec=lin.dec
xx=(ra-ra(0))*(3600.0d/radel)
yy=(dec-min(dec))*(3600.0d/radel)
xl=long(xx)
yl=long(yy)
dx=max(xl)+1
dy=max(yl)+1

ns=n_elements(xl)
locout=lonarr(ns)
loc=yl*dx+xl
remove_sc,loc,xx,yy,rth,locout
ss=where(locout eq 0,nss)
loc=loc(ss)
xx=xx(ss)
yy=yy(ss)
locout_n=lonarr(nss)
stop
remove_nc,loc,xx,yy,dx,dy,rth,locout_n
stop
sss=where(locout_n eq 1,nsss)
if nsss ne 0 then locout(ss(sss))=1
stop
return
end

pro remove_nc,loc,xx,yy,dx,dy,rth,locout
slocp=sort(loc)
sloc=loc(slocp)
get_posi,sloc,loci,kdup,nloci
accum,kdup,akdup
ss=lindgen(nloci)

loc_n=[loc-1,loc-(dx-1),loc-dx,loc-(dx+1)]
xx_n=[xx,xx,xx,xx]
yy_n=[yy,yy,yy,yy]
slocp_n=sort(loc_n)
loc_n=loc_n(slocp_n)
xx_n=xx_n(slocp_n)
yy_n=yy_n(slocp_n)
get_posi,loc_n,loci_n,kdup_n,nloci_n
accum,kdup_n,akdup_n
tabinv_m,loci_n,loci,ssind_n
ss_n=long(ssind_n)
ss_s=where(ssind_n eq ss_n,nss) 
if nss ne 0 then begin
 ss=ss(ss_s)
 ss_n=ss_n(ss_s)
 stop
 for k=0,nss-1 do begin
 	sk=ss(k)
 	indxlo=akdup(sk)-kdup(sk)
 	indxhi=akdup(sk)-1
	slocps=slocp(indxlo:indxhi)
 	xv=xx(slocps)
 	yv=yy(slocps) 

	sk_n=ss_n(k)
 	indxlo_n=akdup_n(sk_n)-kdup_n(sk_n)
 	indxhi_n=akdup_n(sk_n)-1
	slocps_n=slocp_n(indxlo_n:indxhi_n)
 	xv_n=xx_n(slocps_n)
 	yv_n=yy_n(slocps_n)

 	locouts=intarr(kdup(sk))
 	for kk=kdup(sk)-1,1,-1 do begin
 	 if(total(((xv(kk)-xv_n)^2+(yv(kk)-yv_n)^2) lt rth)) $
		then locouts(kk)=1
 	endfor 
 	locout(slocps)=locouts
 endfor
endif else print,'no overlapping stars'
return
end

pro remove_sc,loc,xx,yy,rth,locout
slocp=sort(loc)
sloc=loc(slocp)
get_posi,sloc,loci,kdup,nloci
accum,kdup,akdup
ss=where(kdup gt 1,nss)
if nloci ne 0 then begin
 for k=0,nss-1 do begin
 	sk=ss(k)
 	indxlo=akdup(sk)-kdup(sk)
 	indxhi=akdup(sk)-1
	slocps=slocp(indxlo:indxhi)
 	xv=xx(slocps)
 	yv=yy(slocps) 
 	locouts=intarr(kdup(sk))
 	for kk=kdup(sk)-1,1,-1 do begin
 	 if(max(((xv(kk)-xv(0:kk-1))^2+(yv(kk)-yv(0:kk-1))^2) lt rth)) $
		then locouts(kk)=1
	print,kk,(xv(kk)-xv(0:kk-1))^2+(yv(kk)-yv(0:kk-1))^2
 	endfor 
 	locout(slocps)=locouts
 endfor
endif
stop
return
end