pro rdhrefl,npts,hspec,hname=hname
if n_params(0) eq 0 then begin
 print,'rdhrefl,npts,hspec,hname=hname'
 print,'Read output from HREFL program'
 print,'NPTS: number of energy bins '
 retall
endif
if n_elements(hname) eq 0 then begin
 hname=' '
 read,'Input HREFL filename '
endif
hspec=fltarr(npts,3,2)
openr,2,hname
dum=' '
for k=0l,2 do begin
 readf,2,dum
 for j=0l,npts-1 do begin
   readf,2,s1,s2
   hspec(j,k,0)=s1 & hspec(j,k,1)=s2
 endfor
endfor
close,2
return
end
