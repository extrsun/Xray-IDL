pro cmpcoord,xin,yin,cin,xout,yout,cout
if n_params(0) eq 0 then begin
 print,'cmpcoord,xin,yin,cin,xout,yout,cout'
 print,'Take a bunch of coords and ccdid and eliminate repeated '
 print,'x and y values '
 retall
end
limits=minmax(array) 
nout=0l
for j=limits(0),limits(1) do begin
 wel=where((array eq j),nwel)
 if nwel gt 0 then begin
  if nout eq 0 then arrayout=array(wel(0))
  if nout gt 0 then arrayout=[arrayout,array(wel(0))]
  nout=nout+1
 endif
endfor
nout=nout-1
return
end
