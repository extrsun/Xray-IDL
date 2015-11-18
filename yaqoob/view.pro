pro view,plist,texp,detid=detid,tname=tname,quick=quick
;Author T. Yaqoob - March 1993->**
if n_params(0) eq 0 then begin
 print,' VIEW, plist,texp,detid=detid,tname=tname,quick=quick' 
 print,' Given a photon list (plist) and GTI ascii file (tname) VIEW the '
 print,' light curve and spectrum. Routine then allows fine adjustment of '
 print,' the GTI file until you are completely satisfied (!) '
 retall
end
gti=1
if n_elements(tname) eq 0 then begin
 if quick lt 1 then begin
 tname=' '
 read,' Enter GTI file name (or none) ',tname
 endif
 if quick eq 1 then tname = 'none'
 if tname eq 'none' then gti = 0
endif
read,' Enter 0 for PHA or 1 for PI ',iph
;read GTIs
if iph eq 0 then begin
print,' PHA values are in the range '
phlo=min(plist.pha) & phhi=max(plist.pha)
print,phlo,phhi
endif
if iph gt 0 then begin
print,' PI values are in the range '
phlo=min(plist.pi) & phhi=max(plist.pi)
print,phlo,phhi
endif
if quick lt 1 then begin
read,' Enter Lower and upper inclusive PHA/PI values for lightcurve ',phlo,phhi
read,' Enter time binsize in seconds ',bin
endif 
if quick eq 1 then bin=16. 
bck1: print,' Enter: 	0 = gis data '
print,'         1 = sis bright mode [or converted faint]'
print,'         2 = sis faint mode ' 
print,'		3 = PSPC data ' & read,imode
if imode gt 3 then goto, bck1
tint=dblarr(2,1000)
i=0
if gti gt 0 then begin
openr,11,tname
while (not(eof(11))) do begin
 readf,11,t1,t2,d1,d2,itype
;print,i+1,t1,t2
 tint(0,i) = t1 & tint(1,i) = t2
 i=i+1
endwhile
close,11
ntimes=i
num=indgen(ntimes)+1
endif
;filter the list
lp1: if gti gt 0 then forprint,num(0:ntimes-1),tint(0,0:ntimes-1),tint(1,0:ntimes-1)
if gti gt 0 then tmflt_arr,plist,qlist,ntimes,tint
if gti eq 0 then qlist=plist
;plot light curve
if iph eq 0 then begin
time=(qlist.time)(where((qlist.pha ge phlo) and (qlist.pha le phhi)))
endif
if iph gt 0 then begin
time=(qlist.time)(where((qlist.pi ge phlo) and (qlist.pi le phhi)))
endif
torder=sort(time)
tzero=time(torder(0))
timesec=time-tzero
cts=histogram(timesec,binsize=bin)/float(bin)
mxcts=max(cts)
mrklev=1.2*mxcts
yymax=1.4*mxcts
nbins=(size(cts))(1) & print,' Number of bins = ',nbins
taxis=findgen(nbins)*float(bin)+0.5*bin
window,1,xsize=1000,ysize=600
plot,taxis,cts,/xst,/yst,yrange=[0,yymax]
spec_t=dblarr(2,1)
if quick lt 1 then begin
print,' Click with cursor to choose time intervals for spectrum '
cursor, xc, yc, 3 & spec_t(0,0)=tzero+xc
cursor, xc, yc, 3 & spec_t(1,0)=tzero+xc
endif
if quick eq 1 then begin
spec_t(0,0) = tzero-10.
spec_t(1,0) = tzero+max(taxis)+10.
endif
print,' Selected times: '
print,spec_t(0,0),spec_t(1,0),spec_t(0,0)-tzero,spec_t(1,0)-tzero
tmflt_arr,qlist,rlist,1,spec_t
print,' Original photon list size ',(size(qlist))(1)
print,' photon list size for spectrum ',(size(rlist))(1)
;now the spectrum
nsbin = 512
read,' Number of bins in spectrum (power of 2 =< 1024) ',nsbin
sbsiz=1024/nsbin & print,' sbsiz = ',sbsiz
ens=indgen(nsbin)
if imode eq 0 and iph eq 0 then $
spec=histogram([0,(rlist.pha)],max=1023,binsize=sbsiz)
if imode eq 0 and iph gt 0 then $
spec=histogram([0,(rlist.pi)],max=1023,binsize=sbsiz)
if imode eq 1 and iph eq 0 then sisbr,(rlist.pha),spec,bsz=sbsiz
if imode eq 1 and iph gt 0 then sisbr,(rlist.pi),spec,bsz=sbsiz
if imode eq 2 and iph eq 0 then $
 spec=histogram([0,(rlist.pha)],max=4095,binsize=4*sbsiz)
if imode eq 2 and iph gt 0 then $
 spec=histogram([0,(rlist.pi)],max=4095,binsize=4*sbsiz)
if imode eq 3 and iph eq 0 then $
spec=histogram([0,rlist.pha],max=255,binsize=(sbsiz/4))
if imode eq 3 and iph gt 0 then $
spec=histogram([0,rlist.pi],max=255,binsize=(sbsiz/4))
;window,2
window,2,xsize=512,ysize=512
plot,ens,spec,/xst,/yst
if quick lt 1 then begin
ans=' ' & read,' Plot image (y/n) ? ',ans
if ans eq 'y' then begin
 mkimage,qlist,img,pimg
 window,3
 shade_surf,img
endif
endif
if gti eq 0 then goto, lp2
nreg=0
read,' Enter the number of the time region to adjust (0 to stop) ',nreg
if nreg eq 0 then goto, fin
nreg=nreg-1 & print,' Current start and stop time is ',tint(0,nreg),tint(1,nreg)
read,' Enter new start and stop times (0 for same) ',anew,bnew
if anew gt 0.0 then tint(0,nreg) = anew
if bnew gt 0.0 then tint(1,nreg) = bnew 
goto, lp1
fin: print,' Finished fudging - rewriting GTI file ',tname
openw,11,tname & ityp = 4
for k=0,ntimes-1 do begin
 printf,11,tint(0,k),tint(1,k),tint(0,k)-tint(0,0),tint(1,k)-tint(0,0),ityp
 print,k+1,tint(0,k),tint(1,k)
endfor
close,11
lp2: ans=' '
if quick lt 1 then $  
read,' Make PHA file from spectrum? (must be 256, 512 or 1024) ',ans 
if quick eq 1 then ans = 'y'
if ans eq 'n' then goto, lp3
;Get exposure time
;trcts=histogram((qlist.time),binsize=1.0) & texp=total(trcts(where(trcts gt 0.0)))
print,' You may enter a dummy exposure time (<= 0.0) to save spectral '
print,' GTI file for later computation of the correct exposure time '
if n_elements(texp) eq 0 then read,' Enter exposure time ',texp
if n_elements(detid) eq 0 then begin
detid=' '
read,'Enter DETECTOR ID (e.g. ASCA GIS2)',detid
endif
if texp le 0.0 then begin
  sgtiname=' ' & read,' Name of spectral GTI file ',sgtiname
  openw,12,sgtiname & ityp=4 & texp = 0.0
  printf,12,spec_t(0,0),spec_t(1,0),spec_t(0,0)-tzero,spec_t(1,0)-tzero,ityp
  close,12
endif
rname='none' & bname='none' & cname = 'none' & oname=' '
if nsbin eq 256 then begin
 asca256,spec,detid=detid,fname=oname,tm=texp,npx=1,back=bname,resp=rname,cor=cname
endif
if nsbin eq 512 then begin
 asca512,spec,detid=detid,fname=oname,tm=texp,npx=1,back=bname,resp=rname,cor=cname
endif
if nsbin eq 1024 then begin
 asca1024,spec,detid=detid,fname=oname,tm=texp,npx=1,back=bname,resp=rname,cor=cname
endif
lp3: return
end
