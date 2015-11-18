pro removerlp,iname=iname,oname=oname,res
if n_params(0) eq 0 then begin
  print,' REMOVERLP, iname, oname, res '
  print,' attempt to fix overlapping GTIs '
 retall
end
if n_elements(iname) eq 0 then begin
 iname=' '
 read,' Input GTI filename ',iname
endif
if n_elements(oname) eq 0 then begin
 oname=' '
 read,' Name of new GTI output file ',oname
endif
openr,1,iname
openw,2,oname
i=0
tint=dblarr(10000,2) & tout=tint
while not eof(1) do begin
 readf,1,t1,t2,d1,d2,ityp
 tint(i,0)=t1 & tint(i,1)=t2
 i=i+1
endwhile
ntim=i
for j=0l,ntim-1 do begin
 print,' interval ',j
 narr=long((tint(j,1)-tint(j,0))/res)+1l
 print,' narr = ',narr
 tim=tint(j,0)+res*dindgen(narr)
 if j eq 0l then tlong=tim else tlong=[tlong,tim]
endfor
ts=tlong(sort(tlong)) & nt=(size(ts))(1)
ttemp=dblarr(nt,2) & ttemp(0,0)=ts(0) 
nint=0l
tlow=dblarr(nt) & tlow(0)=0.d0
tlow(1:nt-1)=ts(0:nt-2) & diff=ts-tlow
loc1=where(float(diff(1:nt-1)) gt float(res))
if (size(loc1))(1) gt 0.0 then begin
 print,'loc ',(size(loc1))(1)
 bl=tlow(loc1+1) & bu=ts(loc1+1)
 ni=(size(bl))(1)+1
 tout=dblarr(ni,2) & tout(0,0)=ts(0) & tout(ni-1,1)=ts(nt-1)
print,' Number of new time intervals : ',ni
 if ni ge 2 then begin
 for ii=0l,ni-2 do begin
	tout(ii,1)=bl(ii) & tout(ii+1,0)=bu(ii)
 endfor
 endif
endif
for i=0l,ni-1 do begin
 d1=tout(i,0)-tout(0,0) & d2=tout(i,1)-tout(0,0) 
 print,format='(4(F15.3,1X),I2)',tout(i,0),tout(i,1),d1,d2,ityp
 printf,2,format='(4(F15.3,1X),I2)',tout(i,0),tout(i,1),d1,d2,ityp
endfor
close,1
close,2
return
end
