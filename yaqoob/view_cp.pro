pro view,plist,tname=tname
if n_params(0) eq 0 then begin
 print,' VIEW, plist, tname=tname '
 print,' Given a photon list (plist) and GTI ascii file (tname) VIEW the '
 print,' light curve and spectrum. Routine then allows fine adjustment of '
 print,' the GTI file until you are completely satisfied (!) '
 retall
end
gti=1
if n_elements(tname) eq 0 then begin
 tname=' '
 read,' Enter GTI file name (or none) ',tname
 if tname eq 'none' then gti = 0
endif
;read GTIs
print,' PHA values are in the range '
print,min(plist.pha),max(plist.pha)
read,' Enter lower and upper inclusive PHA values for lightcurve ',phlo,phhi
read,' Enter time binsize in seconds ',bin
bck1: print,' Enter: 	0 = gis data '
print,'         1 = sis bright mode '
print,'         2 = sis faint mode ' & read,imode
if imode gt 2 then goto, bck1
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
time=(qlist.time)(where((qlist.pha ge phlo) and (qlist.pha le phhi)))
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
;now the spectrum
nsbin = 512
read,' Number of bins in spectrum (must be power of 2 =< 1024) ',nsbin
sbsiz=1024/nsbin & print,' sbsiz = ',sbsiz
ens=indgen(nsbin)
if imode eq 0 then spec=histogram([0,(qlist.pha)],max=1023,binsize=sbsiz)
if imode eq 1 then sisbr,(qlist.pha),spec,bsz=sbsiz
if imode eq 2 then spec=histogram([0,(qlist.pha)],max=4095,binsize=4*sbsiz)
;window,2
window,2,xsize=512,ysize=512
plot,ens,spec,/xst,/yst
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
read,' Make PHA file from spectrum? (must be 512 or 1024) ',ans 
if ans eq 'n' then goto, lp3
;find exposure time
trcts=histogram((qlist.time),binsize=1.0) & texp=total(trcts(where(trcts gt 0.0)))
print,' exposure time = ',texp
rname='none' & bname='none' & cname = 'none' & oname=' '
if nsbin eq 512 then begin
 asca512,spec,fname=oname,tm=texp,npx=1000,back=bname,resp=rname,cor=cname
endif
if nsbin eq 1024 then begin
 asca1024,spec,fname=oname,tm=texp,npx=1000,back=bname,resp=rname,cor=cname
endif
lp3: return
end
