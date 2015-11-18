pro sisannuli,chip,x0,y0,rad,rtr,im,annuli
if n_params(0) eq 0 then begin
 print,'sisannuli,chip,xcen,ycen,rad,rtr,sisim,annuli'
 print,'Compute areas in pixels of the annuli enclosed by an array'
 print,'of radii (rad) centred on xcen, ycen '
 print,'**INPUTS** '
 print,'CHIP	- SIS chip ID '
 print,'XCEN, YCEN - centre '
 print,'RAD	- Radii '
 print,'RTR	- transition radius: if rad<rtr area is computed '
 print,'	- from pi r^2 '
 print,'SISIM	- Map image from SIS [e.g. use mkchipmap] '   
 print,'**OUTPUTS** '
 print,'ANNULI  - Area in pixels enclosed by rad(i) - rad(i-1) '
 print,'	- The first value is the area of the circle centred '
 print,'	- on xcen, ycen '
 retall
end  
nrad=(size(rad))(1) & chp=chip+1
annuli=fltarr(nrad) & areas=annuli
nx=(size(im))(1) & ny=(size(im))(2)
ix=long(make_array(nx*ny,/index))
r0sq=rad*rad
radsq=(float(ix-nx*(ix/nx))+1.5-x0)*(float(ix-nx*(ix/nx))+1.5-x0)+$
 (float(ix/nx)+1.5-y0)*(float(ix/nx)+1.5-y0)
 for j=0l,nrad-1 do begin
  if rad(j) lt rtr then begin
     areas(j) = !pi*r0sq(j)
  endif
  if rad(j) ge rtr then begin
    wreg=where(((im/100000l) eq chp and (radsq le r0sq)),nwreg) 
    if nwreg gt 0 then begin
	ewa = float(im(wreg)-100000l*k)/10000.
	areas(j) = total(ewa)
    endif
  endif
 endfor
annuli(0)=areas(0)
for i=1,nrad-1 do annuli(i)=areas(i)-areas(i-1)
return
end
