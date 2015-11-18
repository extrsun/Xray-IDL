pro levsel,plist,sgti,pi,phlo,phhi,binsize,byhand=byhand
if n_params(0) eq 0 then begin
 print,'levsel,plist,sgti,pi,phlo,phhi,binsize,byhand=byhand'
 print,'Make GTI files from time selections based on count'
 print,'rates from a light curve between PHA vals phlo and phhi'
 print,'PI = 0 for PHA, 1 for PI '
 retall
end
if n_elements(byhand) eq 0 then byhand=0
time=plist.time 
if pi eq 0 then time=time(where(plist.pha ge phlo and plist.pha le phhi))$
 else time=time(where(plist.pi ge phlo and plist.pi le phhi))
MKCURVE,time,sgti,binsize,nbins,tcen,tbounds,teff,ctspsec,ctserr
;get rid of incomplete exposure bins
wi=where((teff lt 0.5*binsize),nwi)
if nwi gt 0 then ctspsec(wi)=0.0
tmin=min(tbounds) & tmax=max(tbounds)
window,0
ploterr,tcen,ctspsec,ctserr,psym=3
agn: print,' Click cursor or enter lower bound on count rate '
     print,' Negative value to rescale plot '
if byhand eq 0 then begin
 cursor,xc,clo,4 & print,'Lower Bound: ',clo
endif else begin
 read,'Enter lower bound ',clo
endelse
if clo lt 0.0 then begin
 read,'Enter new limits for y axis (ymin, ymax)',ymin,ymax
 plot,tcen,ctspsec,yr=[ymin,ymax],psym=1 & goto, agn
endif
oplot,[tmin,tmax],[clo,clo]
if byhand eq 0 then begin
 print,' Click cursor for upper bound on count rate '
 cursor,xc,chi,4 & print,'Upper Bound: ',chi
endif else begin
 read,'Enter upper bound on count rate ',chi
endelse
oplot,[tmin,tmax],[chi,chi]
levgti,tbounds,ctspsec,clo,chi,gti
ngti=n_elements(gti)/2 &print,'No. of GTIs :',ngti
gname=' '
read,'Enter name of file to write GTIs to ',gname
openw,1,gname
tz=gti(0,0) & ityp=6
for k=0l,ngti-1 do begin
 t1=gti(k,0) & t2=gti(k,1)
 printf,1,format='(4(F15.3,2X),I3)',t1,t2,t1-tz,t2-tz,ityp
 print,format='(4(F15.3,2X),I3)',t1,t2,t1-tz,t2-tz,ityp
endfor
close,1
ans=' '
read,' Do you want to make another selection? ',ans
if strmid(ans,0,1) eq 'y' then goto, agn
return
end
