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

stop
return
end

pro remove_sc,loc,xx,yy,rth,locout
slocp=sort(loc)
sloc=loc(slocp)
get_posi,sloc,loci,kdup,nloci
accum,kdup,akdup
ss=where(kdup gt 1,nss)
stop
if nss ne 0 then begin
 for k=0,nss-1 do begin
 	sk=ss(k)
 	indxlo=akdup(sk)-kdup(sk)
 	indxhi=akdup(sk)-1
	slocps=slocp(indxlo:indxhi)
 	xv=xx(slocps)
 	yv=yy(slocps) 
 	locouts=intarr(kdup(sk))
 	for kk=kdup(sk)-1,1,-1 do begin
 	 if(total(((xv(kk)-xv(0:kk-1))^2+(yv(kk)-yv(0:kk-1))^2) gt rth)) $
		then locouts(kk)=1
 	endfor 
 	locout(slocps)=locouts
 endfor
endif
return
end