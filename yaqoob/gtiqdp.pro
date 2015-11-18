pro gtiqdp,qlist,gti,pi,chlo,chhi,maxwid,reftime,outname=outname
if n_params(0) eq 0 then begin
 print,'gtiqdp,plist,gti,pi,chlo,chhi,maxwid,reftime,outname=outname'
 retall
end
plist=qlist
if pi eq 0 then $
 plist=plist(where(plist.pha ge chlo and plist.pha le chhi))
if pi eq 1 then $
 plist=plist(where(plist.pi ge chlo and plist.pi le chhi))
if n_elements(reftime) eq 0 then reftime=min(plist.time)
if n_elements(outname) eq 0 then begin
 outname=' '
 read,'Enter output qdp filename ',outname
endif
rf=1 & posdet=0
mxhwid=maxwid/2.
ngti=n_elements(gti)/2
print,ngti,' GTI intervals'
cts=fltarr(ngti) & ctserr=cts & tcen=fltarr(ngti) & thwid=tcen
odist=fltarr(ngti)
for k=0l,ngti-1l do begin
 wkc=where((plist.time ge gti(k,0) and plist.time le gti(k,1)),nwkc)
 cts(k)=float(nwkc)     
 if nwkc gt 0. then ctserr(k) = sqrt(cts(k))
 tcen(k)=(gti(k,0)+gti(k,1))/2. 
 thwid(k)=(gti(k,1)-gti(k,0))/2.
endfor
cts=cts/2./thwid
ctserr=ctserr/2./thwid
ymax=(max(cts)+max(ctserr))*1.2
openw,1,outname
printf,1,'read serr 1 2'
printf,1,'MARKER SIZE 2 ON 2'
printf,1,'MARKER 17 ON 2'
printf,1,'LA X TIME (s) - ',reftime
printf,1,'LA Y cts/s channels ',chlo,' - ',chhi
printf,1,'r y 0 ',ymax
for k=0,ngti-1 do begin
 if thwid(k) gt mxhwid then begin
  printf,1,tcen(k)-reftime,thwid(k),cts(k),ctserr(k)
 endif
endfor
close,1
return
end
