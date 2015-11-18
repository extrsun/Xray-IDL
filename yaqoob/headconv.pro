pro headconv,hdr 
if n_params(0) eq 0 then begin
print,'headconv,hdr'
print,'Convert FITS header from e.g. TFORM = E to TFORM = 1E'
retall
end
nform=where((strmid(hdr,0,5) eq 'TFORM'),n)
for k=0l,n-1 do begin
 a=hdr(nform(k))
 c=strmid(a,11,1)
 if c eq 'P' then c='D'
 cc='1'+c
 strput,a,cc,11
 hdr(nform(k))=a
endfor
return
end
