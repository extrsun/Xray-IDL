pro blnkwmap,xmn,ymn,xmx,ymx,rbn,xcnr,ycnr,xc,yc,xdim,ydim,wmap
;**Author T. Yaqoob - Oct 1995
if n_params(0) eq 0 then begin
 print,'blnkwmap,xmn,ymn,xmx,ymx,rbn,xcnr,ycnr,xc,yc,xdim,ydim,wmap'
 print,'Make a blank WMAP (filled with -1) given the bottom '
 print,'left hand and top right hand detector coords and the '
 print,'rebinning factor.'
 print,' ** INTPUTS ** '
 print,'XMN, YMN	- original bottom left hand det coords'
 print,'XMX, YMX	- original top right hand det coords'
 print,'RBN		- rebin factor for WMAP'
 print,' ** OUTPUTS ** '
 print,'XCNR, YCNR	- bottom left hand binned coords of WMAP'
 print,'XC, YC	- coords of WMAP pixel centres in original det units'
 print,'XDIM, YDIM	- dimensions of WMAP' 
 print,'WMAP		- The WMAP'
 retall
end
wxmn = float(fix(xmn/rbn)+1)
wxmx = float(fix(xmx/rbn)+1)
wymn = float(fix(ymn/rbn)+1)
wymx = float(fix(ymx/rbn)+1)
xdim=0l & ydim=0l
xdim=fix(wxmx-wxmn + 1.0) 
ydim=fix(wymx-wymn + 1.0) 
wmap=fltarr(xdim,ydim) -1.0
xl=1.+rbn*(float(fix(xmn/rbn))+findgen(xdim))
yl=1.+rbn*(float(fix(ymn/rbn))+findgen(ydim))
xh=rbn+xl-1. & xc=0.5*(xl+xh)
yh=rbn+yl-1. & yc=0.5*(yl+yh)
xcnr=wxmn & ycnr=wymn
return
end
