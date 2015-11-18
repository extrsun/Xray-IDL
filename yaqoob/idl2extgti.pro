pro idl2extgti,iname,ename
if n_params(0) eq 0 then begin
 print,'idl2extgti,iname,ename'
 print,'Convert IDL ascii GTI file to an EXTRACTOR time intervals file'
 print,'INAME = name of IDL GTI file'
 print,'ENAME = name of EXTRACTOR GTI file '
 retall
endif
readcol,iname,t1,t2,ti1,ti2,itype
npts=0l
npts=(size(t1))(1)
openw,2,ename
for k=0l,npts-1l do begin
 printf,2,format='(2(F15.3,5X))',t1(k),t2(k)
endfor
close,2
return
end
