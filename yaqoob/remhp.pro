pro remhp,plist,nmx,inst,xhot,yhot,ccdhot,chng,auto=auto
;Author T. Yaqoob - March 1993 -> **
;Remove hot pixels from SIS data
if n_params(0) eq 0 then begin
print,'REMHP,plist,nmx,inst,xhot,yhot,ccdhot,chng'
print,'Remove hot pixels from SIS data'
print,'INST	:=0 for S0, =1 for S1'
print,'OUTPUT: Hot Pixel and CCD lists XHOT, YHOT, CCDHOT'
print,'output units are RAW coords '
print,'CHNG	:Change in the number of events in plist '
print,'AUTO (input) >0 gives automatic hot pixel removal'
print,'	    if AUTO<1 still asks for threshold '
print,'	       AUTO>1 is completely automatic **BUT BE CAREFULL! **'
retall
end
nyd=848l & nxd=864l
ensize=(size(plist))(1)
print,' Number of events on entry ',ensize
x=plist.x & y=plist.y & m=lonarr(nxd,nyd) & n=(size(plist))(1)-1
x = x > 0 & x = x < 883 & y = y > 0 & y = y < 881
for i=0l,n do m(x(i),y(i))=m(x(i),y(i))+1
a=lonarr(nmx) & for i=1,nmx-1 do begin 
a(i) = total(m ge i) 
endfor
aplt=a & for jj=0,nmx-1 do if aplt(jj) eq 0 then aplt(jj)=1
plot,(indgen(nmx)+1),aplt(1:nmx-1),/xst,/yst,ytype=1
print,'N cts per pixel versus No. of pixels with at least N cts'
print,' '
col1=transpose(indgen(nmx-1)+1) & col2=transpose(a(1:*))
print,[col1,col2]
icut=0
while icut lt nmx do begin
bckh: if col2(icut) eq col2(icut+1) then goto,jmp1
icut=icut+1
endwhile
jmp1: print,'Recommended cut-off of n = ',icut+2
;print,[transpose(indgen(nmx-1)+1),transpose(a(1:*))]
if auto gt 1 then begin
 n=icut+2
endif else begin
 read,'Enter desired cutoff: ',n 
endelse
ans=' ' & hpm = m lt n
msk=lonarr((size(x))(1))
for j=0l,(size(x))(1)-1 do msk(j) = m(x(j),y(j)) lt n
wmsk=where(msk,wc)
hmsk=where((msk eq 0),hwc)
print,' Number of events upon removal will be ',wc
if auto eq 0 then begin
 read,'Ready to modify plist? y/(n): ',ans
endif else begin
 ans='y'
endelse
if ans ne 'y' then goto,fhp2
wz=where((hpm eq 0),nwz)
if nwz gt 0 then begin
 print,nwz,' Hot Pixels '
 yin=wz/nxd & xin = wz-nxd*yin 
 ccdhot=intarr((size(yin))(1))
 for k=0l,nwz-1 do begin
ccdhot(k)=max((plist.ccd)(where( plist.x eq xin(k) and plist.y eq $
 yin(k))))
 endfor
 fchp2raw,xin,yin,xhot,yhot,inst,ccdhot 
endif
plist=plist(wmsk)
fhp2: lvsize=(size(plist))(1)
chng=ensize-lvsize
return
end
