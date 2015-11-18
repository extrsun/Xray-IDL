pro data_snr,l,ston,rr,bpix,xc=xc,yc=yc
rr=(l.x-xc)^2+(l.y-yc)^2
ba=!pi*rr*bpix
nc=n_elements(rr)
for k=1,nc do begin
ston=(k-ba)/sqrt(float(k))
endfor
return
end
