pro getcntrd,im,sm,xc,yc,xy_peak=xy_peak
if n_params(0) eq 0 then begin
 print,'getcntrd,im,sm,xc,yc,xy_peak=xy_peak'
 print,'Driver for the IDL routine cntrd to get centroid '
 print,'of image.'
 print,'IM	- image'
 print,'SM	- smoothing factor'
 print,'XC YC	- output centroid'
 print,'xypeak(2) 	- peak coord of image'
 retall
end
if sm gt 0. then imsm = gauss_smooth(im, sm) else imsm=im
w = where(imsm eq max(imsm), c)
if c gt 1 then begin
    print, 'Warning... more than one maximum in smoothed image'
endif
sx=(size(im))(1)
w = w(0)
y = w/sx
x = w - y*sx
cntrd, imsm, x, y, xc, yc, 8
; Now put (xc, yc) into fits convention
xc = xc + 1
yc = yc + 1
xy_peak=[x+1.,y+1.]
return
end
