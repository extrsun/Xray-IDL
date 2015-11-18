pro showhk,hkinfo,hk
;Author T. Yaqoob - March 1993 -> **
if n_params(0) eq 0 then begin
 print,'showhk,hkinfo,hk '
 retall
end
hksiz=(size(hk))(1)
tgt=hkinfo.target
nfiles=hkinfo.nfiles
fnames=hkinfo.fname
gname=hkinfo.gtifil
print,' HK structure for ',tgt
print,' Input MKFILTER files were :'
for k=0,nfiles-1 do print,fnames(k)
print,' Name of current GTI file: ',gname+'.gti'
winsel=hk.wind
nw=hk.nwin
hkname=hk.name
hkflag=hk.flag
for j=0l,hksiz-1 do begin
if hkflag(j) ge -1 then status = '  ACTIVE' else status = '  DISABLED'
print,' status and FLAG for ',hkname(j),status,hkflag(j)
for i=0l,nw(j)-1 do begin
 print,'window ',i+1,winsel(i,0,j),winsel(i,1,j)
endfor
endfor
return
end
