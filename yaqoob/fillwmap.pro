pro fillwmap,plist,xcnr,ycnr,rbn,wmap
;**Author T. Yaqoob - Oct 1995
if n_params(0) eq 0 then begin
 print,'fillwmap,plist,xcnr,ycnr,rbn,wmap'
 print,'Fills in a blank WMAP created by BLNKWMAP [with -1s and '
 print,'0s in place] with counts from a photon list (PLIST)'
 print,' ** INTPUTS ** '
 print,'PLIST		- Photon List '
 print,'XCNR, YCNR	- bottom left hand binned coords of WMAP'
 print,'RBN		- rebin factor for WMAP'
 print,'WMAP		- the WMAP from blnkwmap '
 print,' ** OUTPUTS ** '
 print,'WMAP		- modified wmap'
 retall
end
xdim=(size(wmap))(1) & ydim=(size(wmap))(2)
print,'WMAP X and Y size ',xdim,ydim
;generate the x and y boundaries
xmn=float(rbn)*(xcnr-1.)
ymn=float(rbn)*(ycnr-1.)
xl=xmn+findgen(xdim) & xh=xl+rbn & xc=0.5*(xl+xh)
yl=ymn+findgen(ydim) & yh=yl+rbn & yc=0.5*(xl+xh)
x=plist.detx & y=plist.dety
xbn=fix((x-xmn)/float(rbn)) + 0l
ybn=fix((y-ymn)/float(rbn)) + 0l
for i=0l,xdim-1l do begin
 for j=0l,ydim-1l do begin
  if wmap(i,j) ge 0. then begin
   wp=where((xbn eq i and ybn eq j),np)
; wp=where((x gt xl(i) and x le xh(i) and y gt yl(j) and y le yh(j)),np)
  	if np gt 0 then wmap(i,j)=float(np)
  endif
 endfor
endfor
return
end
