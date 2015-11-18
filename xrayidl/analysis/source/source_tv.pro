pro source_tv,x_image,y_image,xcpix,ycpix,x_source,y_source, $
    block=block,color=color,thick=thick,pixel=pixel
;-
; show a circle around a source on the screen
;+
if n_params() EQ 0 then begin
	print,'CALLING SEQUENCE - source_tv,x_image,y_image,xcpix,ycpix,'
	print,'                   x_source,y_source,'
	print,'                   [block,color,thick,pixel]'
	retall
endif
;
if n_elements(block) EQ 0 then block=!block
if n_elements(color) eq 0 then color=!p.color
if n_elements(thick) eq 0 then color=!p.thick
;
if n_elements(pixel) eq 0 then begin
trans_dist,x_image,y_image,x_source,y_source,xp,yp
;
xp=xp/block+xcpix
yp=yp/block+ycpix
endif else begin
xp=x_source
yp=y_source
endelse
;
dist=(xp-xcpix)*(xp-xcpix)+(yp-ycpix)*(yp-ycpix)
c=where(dist ne 0.,count)
if count ne 0 then dist(c)=sqrt(dist(c))*block/!ampix
boxhalf=xp*0.+90./(block*!size_pixel)
c=where(dist gt 20.,count)
if count ne 0 then boxhalf(c)=(90.+12.*(dist(c)-20.))/(block*!size_pixel)
;
for k=0,n_elements(xp)-1 do begin
tvcircle,boxhalf(k),xp(k),yp(k),color=color,thick=thick
endfor
if !debug eq 1 then stop
end