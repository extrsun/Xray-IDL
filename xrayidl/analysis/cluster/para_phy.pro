pro para_phy,pin,psm,pm,pem,dd,pout,xysh=xysh
;+
; calculate statistics of the measured ellipse parameters.
; writen by wqd, 10/11, 1996
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - para_phy,pin,psm,pm,pem,dd,xysh=xysh,pout'
return
endif
pout=pin
if n_elements(xysh) ne 0 then pout(1:2,*)=pout(1:2,*)-xysh
pout(1:2,*)=pout(1:2,*)*0.5 ;in units of arcsec
pout(3,*)=pout(3,*)*(180./!pi)+90. ;position angle N-E

sz=size(pin)
pm=fltarr(sz(1))
pem=pm
dd=pm
for k=0,sz(1)-1 do begin
	pem(k)=min(psm(k,*,2)-psm(k,*,1),minloc)
	pm(k)=pin(k,minloc)
	dd(k)=max((pin(k,*)-pm(k))^2/((psm(k,*,2)-psm(k,1))^2+pem(k)^2))
endfor
pem=pem*0.5
dd=dd*4.
print,'pm,pem = ',pm,pem
print,'dd = ',dd
return
end
