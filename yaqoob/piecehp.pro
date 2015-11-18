pro piecehp,inst,plist,xcen,ycen,rbnds,auto=auto,hotfile=hotfile
;Author T. Yaqoob - March 1993 ->**
 if n_params(0) eq 0 then begin
  print,'piecehp,inst,plist,xcen,ycen,rbnds,auto=auto,hotfile=hotfile '
  print,'remove hot pixels in annuli centered on xcen,ycen'
  print,'You can use the cursor to define the centre if you omit '
  print,'xcen and ycen in the input '
  print,'INST	:=0 for S0, =1 for S1'
  print,'AUTO (input) > 0 for automatic hot pixel removal'
  print,'     if AUTO<=1 then still asks for cut-off threshold '
  print,'     if AUTO>1 completely automatic **BUT BE CAREFULL!!**'
  print,'Input RBNDS : N+1 radial boundaries for N auto steps'
  retall
end
if n_elements(rbnds) gt 0 then nbnds=(size(rbnds))(1) $
 else nbnds=10000000l
if n_elements(xcen) eq 0 then begin
 window,0,xsize=600,ysize=600
 plot,plist.x,plist.y,psym=3
 print,' Use cursor to define centre of annuli '
 cursor,xcen,ycen,4 & print,xcen,ycen
endif
if n_elements(hotfile) eq 0 then begin
 hotfile=' '
 read,'Enter name of Hot Pixel file ',hotfile
endif
;see if the file exist already
openr,1,hotfile,error=er
close,1
if er ne 0 then begin
 print,'Opening New Hot Pixel File ...'
 openw,1,hotfile
endif
if er eq 0 then begin
 print,'Hot Pixel file already exists '
 print,'Will append any new hot pixels to this file '
 readcol,hotfile,cold,xold,yold
 nold=(size(cold))(1)
 print,nold,' Hot Pixels in Old file '
 openw,1,hotfile
 for k=0l,nold-1 do printf,1,fix(cold(k)),fix(xold(k)),fix(yold(k))
endif
;compute radial distance of each event
np=(size(plist))(1)
print,' Number of events on entry ',np
irad=0l
bck: x=plist.x & y=plist.y
rsq=(x-xcen)*(x-xcen)+(y-ycen)*(y-ycen)
print,' MIN & MAX radius ',minmax(sqrt(rsq))
print,' MIN & MAX rsq ',minmax(rsq)
agan: print,'Radial step ',irad+1
if auto eq 0 then begin
 read,'choose inner and outer radius of annulus ',r1,r2
endif else begin
 r1=rbnds(irad) & r2=rbnds(irad+1)
 print,'Doing Radii ',r1,r2
endelse
r1sq=r1*r1 & r2sq=r2*r2
wsub=where((rsq ge r1sq and rsq le r2sq),nsub)
wrem1=where((rsq lt r1sq),nrem1)
wrem2=where((rsq gt r2sq),nrem2)
nrem=nrem1+nrem2
if nrem1 eq 0 then wrem=wrem2
if nrem2 eq 0 then wrem=wrem1
if nrem1 gt 0 and nrem2 gt 0 then wrem=[wrem1,wrem2]
print,' Events in sublist & remainder ',nsub,nrem,nrem+nsub
;print,'nrem1 nrem2 ',nrem1,nrem2
;print,'wrem1 wrem2 ',total(wrem1),total(wrem2)
if nsub le 0 then goto, agan
if nrem gt 0 then remlist=plist(wrem)
sublist=plist(wsub)
mkimage,sublist,simg
nmx=min([200,max(simg)+1])
remhp,sublist,nmx+2,inst,xhot,yhot,chot,chng,auto=auto
nhot=n_elements(xhot)
if nhot gt 0 and chng gt 0 then begin
 for jh=0l,nhot-1 do printf,1,chot(jh),xhot(jh),yhot(jh)
endif 
;if nrem gt 0 then print,'remlist ',(size(remlist))(1)
;print,'sublist ',(size(sublist))(1)
if nrem gt 0 then plist=[remlist,sublist] else plist=sublist
print,' Number of events now ',(size(plist))(1)
irad=irad+1
if auto eq 0 then begin
 ans=' '
 read,' Do another annulus? (y/n)',ans
endif else begin
 ans='y'
endelse
if ans eq 'y' and irad lt nbnds-1 then goto, bck
close,1
return
end
