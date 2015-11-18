pro mksisim,plist,phlo,phhi,rebin,imout
if n_params(0) eq 0 then begin
 print,'mksisim,plist,phlo,phhi,rebin,im'
 print,'make 864x848 image and fill in the gaps'
 retall
endif
im=fltarr(864,848)
x=plist.x & y=plist.y
locp=where((plist.pha ge phlo and plist.pha le phhi),nlocp)
if nlocp eq 0 then return
x=x(locp) & y=y(locp)
np=(size(x))(1)
for k=0l,np-1 do begin
 im(x(k),y(k))=im(x(k),y(k))+1.
endfor
mnct=(im(418,*)+im(441,*))/2.
for j=419,440 do begin
ct=fix(mnct-0.5*randomu(seed,848))
 im(j,*)=im(j,*)+float(ct)
endfor
mnct2=(im(*,419)+im(*,423))/2.
for j=420,422 do begin
ct=fix(mnct2-0.5*randomu(seed,864))
im(*,j)=im(*,j)+float(ct)
endfor
if rebin gt 0 then imout=rebin(im,108,106) else imout=im
return
end
