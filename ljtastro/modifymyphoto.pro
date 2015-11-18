function filter,image,image0,c=c,n=n,type=type
;type=1, low pass; type=2, high pass

if keyword_set(c) eq 0 then begin
	c=1.0
endif
if keyword_set(n) eq 0 then begin
	n=1
endif
if keyword_set(type) eq 0 then begin
	type=1
endif

if type eq 1 then begin
	filter=1./(1.+c*(image/image0)^(2*n))
endif 
if type eq 2 then begin
	filter=1./(1.+c*(image0/image)^(2*n))
endif

return,filter
end


pro modifymyphoto, filename, outputname, zoom=zoom, quality=quality, c0factor=c0factor,cflow=cflow,cfhigh=cfhigh

if keyword_set(zoom) eq 0 then begin
	zoom=0.5
endif
if keyword_set(quality) eq 0 then begin
	quality=100
endif
if keyword_set(c0factor) eq 0 then begin
	c0factor=50
endif
if keyword_set(cflow) eq 0 then begin
	cflow=0.8
endif
if keyword_set(cfhigh) eq 0 then begin
	cfhigh=1.2
endif
ruihuakernel=[[1,1,1],[1,-7,1],[1,1,1]]
gausskernel3=[[1,2,1],[2,8,2],[1,2,1]]
gausskernel5=[[1,2,3,2,1],[2,7,11,7,2],[3,11,17,11,3],[2,7,11,7,2],[1,2,3,2,1]]

READ_JPEG, filename,a
nx=n_elements(a[0,*,0])
ny=n_elements(a[0,0,*])
zoomedx=fix(zoom*nx)
zoomedy=fix(zoom*ny)
b=fltarr(3,zoomedx,zoomedy)
image=fltarr(nx,ny)

image[*,*]=a[0,*,*]
b[0,*,*]=congrid(image,zoomedx,zoomedy,/interp)
image[*,*]=a[1,*,*]
b[1,*,*]=congrid(image,zoomedx,zoomedy,/interp)
image[*,*]=a[2,*,*]
b[2,*,*]=congrid(image,zoomedx,zoomedy,/interp)
a=b

avred=total(a[0,*,*])/float(n_elements(a[0,*,*]))
avgreen=total(a[1,*,*])/float(n_elements(a[1,*,*]))
avblue=total(a[2,*,*])/float(n_elements(a[2,*,*]))
meancolorvalue=(avred+avgreen+avblue)/3.
cfm=[avred/meancolorvalue,avgreen/meancolorvalue,avblue/meancolorvalue]
print,cfm

image=fltarr(zoomedx,zoomedy)
fredomain=fltarr(zoomedx,zoomedy)
frefilter=fltarr(zoomedx,zoomedy)
filterfredomain=fltarr(zoomedx,zoomedy)
for i=0,2 do begin
	image[*,*]=a[i,*,*]
	fredomain=fft(image,-1)
	frefilter=filter(abs(fredomain),c0factor,type=1,n=5)
	filterfredomain=fredomain*frefilter
	a[i,*,*]=fft(filterfredomain,1)
	image[*,*]=a[i,*,*]*cfm[i]
	image=bscale(image,min(image)*cflow,max(image)*cfhigh)
	a[i,*,*]=image[*,*]
;smooth
;image=convol(image,gausskernel3,total(gausskernel3),/Edge_Truncate)
;remove noise point
;points=randomu(seed,10000)*zoomedx*zoomedy
;image(points)=255
;points=randomu(seed,10000)*zoomedx*zoomedy
;image(points)=0
;image=median(image,3)
;ruihua edge
;a[i,*,*]=convol(image[*,*],ruihuakernel,total(ruihuakernel),/Edge_Truncate)
endfor

window,1,xs=n_elements(a[0,*,0]),ys=n_elements(a[0,0,*])
tvscl,a[0,*,*]
window,2,xs=n_elements(a[0,*,0]),ys=n_elements(a[0,0,*])
tvscl,a[1,*,*]
window,3,xs=n_elements(a[0,*,0]),ys=n_elements(a[0,0,*])
tvscl,a[2,*,*]
WRITE_JPEG, outputname,a, TRUE=1, QUALITY=quality

end
