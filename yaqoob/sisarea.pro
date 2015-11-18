pro sisarea,atype,p,im,area,xmin,ymin,xmax,ymax
if n_params(0) eq 0 then begin
 print,'sisarea,atype,p,sisim,area,xmin,ymin,xmax,ymax'
 print,'Give a geometrical shape specified by ATYPE and an '
 print,'SIS exposure map (IM) compute the effective area '
 print,'exposed in each chip. All units are in DETECTOR coords'
 print,'AREA is in pixel units. ' 
 print,'**INPUTS**'
 print,'ATYPE	: Type of geometrical shape '
 print,'	: Current Options:
 print,'      =1: CIRCLE:center (x,y)=(p(0),p(1)) & radius=p(2)'
 print,'P	: parametrs defining region'
 print,'SISIM	: Exposure map made by SISEXP or MKCHIPMAP'   
 print,'**OUTPUTS**'
 print,'AREA(5)	: Total number of included pixels (each chip) weighted' 
 print,'	: by the pixel exposures. Last is the sum of 4 chips'
 print,'XMIN(4) : Minimum X-coord of enclosed region in each chip ' 
 print,'YMIN(4) : Minimum Y-coord of enclosed region in each chip ' 
 print,'XMAX(4) : Maximum X-coord of enclosed region in each chip ' 
 print,'YMAX(4) : Maximum Y-coord of enclosed region in each chip ' 
 retall
end 
area=fltarr(5)
xmin=fltarr(4)-1. & xmax=xmin & ymin=xmin & ymax=xmin
nx=(size(im))(1) & ny=(size(im))(2)
ix=long(make_array(nx*ny,/index))
if atype eq 1 then begin
;we are doing a circle
if p(2) le 0.0 then goto,fin
x0=p(0) & y0=p(1) & r0=p(2) & r0sq=r0*r0
 xcd=float(ix-nx*(ix/nx))+1.5
 ycd=float(ix/nx)+1.5
; print,'MINMAX XCD ',minmax(xcd)
; print,'MINMAX YCD ',minmax(ycd)
 radsq=(float(ix-nx*(ix/nx))+1.5-x0)*(float(ix-nx*(ix/nx))+1.5-x0)+$
 (float(ix/nx)+1.5-y0)*(float(ix/nx)+1.5-y0)
 for k=1,4 do begin
  wreg=where(((im/100000l) eq k and (radsq le r0sq)),nwreg)
  print,'Chip ',k-1
  print,'Total number of enclosed pixels = ',float(nwreg)
  if nwreg gt 0 then begin
;  print,'minmax im',minmax(im(wreg))
  ewa = float(im(wreg)-100000l*k)/10000. 
;  print,'minmax ewa',minmax(ewa)
  area(k-1) = total(ewa)
   xmin(k-1)=min(xcd(wreg)) & xmax(k-1)=max(xcd(wreg))
   ymin(k-1)=min(ycd(wreg)) & ymax(k-1)=max(ycd(wreg))
  endif
  if nwreg le 0 then area(k-1)=0.0
  print,'Total exposure weighted area = ',area(k-1)
 endfor
 area(4)=total(area(0:3))
 print,'Total area for 4 chips = ',area(4)
 goto,fin 
endif
print,'Unrecognized shape '
fin: return
end
