pro hkgtiqdp,plist,sgti,pi,chlo,chhi,maxtime,mkfnames,hkname,qdpname=qdpname
if n_params(0) eq 0 then begin
 print,'hkgtiqdp,plist,sgti,pi,chlo,chhi,maxtime,mkfnames,hkname,qdpname=qdpname'
 retall
endif
if n_elements(qdpname) eq 0 then begin
 qdpname=' '
 read,'Enter output QDP filename ',qdpname
endif
if pi eq 0 then $
qlist=plist(where(plist.pha ge chlo and plist.pha le chhi))
if pi gt 0 then $
qlist=plist(where(plist.pi ge chlo and plist.pi le chhi))
;get the hkparamter 
plothk,mkfnames,'time',hkname,hktime,hkval,rpt=0
ngti=n_elements(sgti)/2
cts=fltarr(ngti) & twid = cts & ctserr = cts & hk=cts
dumerr=0.
openw,1,qdpname
printf,1,'read serr 1 2'
printf,1,'la x ',hkname
printf,1,'la y cts/s '
printf,1,'marker 17 on 2'
for k=0l,ngti-1l do begin
 twid(k)=sgti(k,1)-sgti(k,0)
 whk=where((hktime ge sgti(k,0) and hktime le sgti(k,1)),nwhk)
 if nwhk gt 0 then $ 
  hk(k)=total(hkval(whk))/float(nwhk)         
 wct=where((qlist.time  ge sgti(k,0) and qlist.time le sgti(k,1)),nwct)
 if nwct gt 0 then begin
  cts(k) = float(nwct) & ctserr(k)=sqrt(cts(k))
  cts(k) = cts(k)/twid(k) & ctserr(k)=ctserr(k)/twid(k)
 endif
endfor 
wnz=where((hk gt 0),nwnz)
if nwnz gt 0 then hknz=hk(wnz) else hknz=hk
xmin=0.97*min(hknz) & xmax=1.03*max(hk) 
ymin=0.97*min(cts-ctserr) & ymax=1.03*max(cts+ctserr)
printf,1,'r ',xmin,xmax,ymin,ymax
for k=0l,ngti-1l do begin
 if twid(k) gt maxtime then begin
  printf,1,hk(k),dumerr,cts(k),ctserr(k)
 endif
endfor
close,1
return
end 
