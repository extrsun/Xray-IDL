pro rdsmonte,npts,nang,spec,fname=fname
if n_params(0) eq 0 then begin
 print,'rdsmonte,npts,nang,spec,fname=fname'
 print,'Read spectral output file from Monte Carlo Code'
 print,'NPTS: number of energy bins '
 print,'NANG: number of angle bins '
 retall 
end
spec=fltarr(npts,nang,6)
if n_elements(fname) eq 0 then begin
 fname=' '
 read,' Input File name ',fname
endif
openr,1,fname
for k=0l,nang-1 do begin
print,k
for j=0l,npts-1 do begin
 readf,1,s0,s1,s2,s3,s4,s5
 spec(j,k,0)=s0&spec(j,k,1)=s1&spec(j,k,2)=s2&spec(j,k,3)=s3 
 spec(j,k,4)=s4&spec(j,k,5)=s5
endfor
endfor
close,1
return
end
