pro splitpgti,plist,nsplit,rootname=rootname
;Author T. Yaqoob - MArch 1993 ->**
if n_params(0) eq 0 then begin
 print,'splitpgti,plist,nsplit,rootname=rootname'
 print,'Create NPLIST GTI files which split a photon list (PLIST) '
 print,'into NSPLIT photon lists each with ~equal photons numbers '
 retall
end
if nsplit le 1 then return
if n_elements(rootname) eq 0 then begin
 rootname=' '
 read,'Enter rootname of output GTI files ',rootname
endif
fnames=strarr(nsplit)
ityp=9
for k=0,nsplit-1 do fnames(k)=rootname+strn(k+1)+'.gti'
;sort in time
tm=plist.time & time = tm(sort(tm))
np=0l+(size(plist))(1)
nphot = 0l+ np/nsplit
nrem = np - nphot*nsplit
;get the time boundaries
tlow=dblarr(nsplit) & thigh=dblarr(nsplit)
for k=0l,nsplit-1 do begin
 tlow(k) = time(k*nphot)
 if k lt (nsplit-1) then thigh(k)=time((k+1)*nphot) else $
  thigh(k)=time(np-1)
 openw,1,fnames(k)
 print,'writing to ',fnames(k)
 printf,1,format='(4(F15.3,2X),I3)',tlow(k),thigh(k),tlow(k)-tlow(0),$
  thigh(k)-tlow(0),ityp  
 print,format='(4(F15.3,2X),I3)',tlow(k),thigh(k),tlow(k)-tlow(0),$
  thigh(k)-tlow(0),ityp
 close,1
endfor
return
end 
