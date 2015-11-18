pro testcartoon
files="/home/ljt/ljt/movie/nanjing/*.png"
thesefiles=findfile(files,count=numfiles)
zoomx=800
zoomy=700

if 0 then begin
;for fitsfile
image=mrdfits(thesefiles[0],0,fitshead)
xsize=n_elements(image[*,0])
ysize=n_elements(image[0,*])
cartooninfo=intarr(3)
cartooninfo[0]=xsize*zoomx
cartooninfo[1]=ysize*zoomy
cartooninfo[2]=numfiles

print,cartooninfo

XInterAnimate,Set=cartooninfo,/Showload
for i=0,numfiles-1 do begin
image=mrdfits(thesefiles[i],0,fitshead)
newimage=congrid(image,xsize*zoomx,ysize*zoomy,/INTERP)
tvscl,newimage
XInterAnimate,Frame=i,Window=!D.Window
endfor
XInterAnimate,1
endif



if 0 then begin
;for jpgfile
read_jpeg,thesefiles[0],jpegimg
cartooninfo=intarr(3)
cartooninfo[0]=zoomx
cartooninfo[1]=zoomy
cartooninfo[2]=numfiles

print,cartooninfo

XInterAnimate,Set=cartooninfo,/Showload
for i=0,numfiles-1 do begin
read_jpeg,thesefiles[i],jpegimg
image=jpegimg[0,*,*]
xsize=n_elements(image[0,*,0])
ysize=n_elements(image[0,0,*])
image=fltarr(xsize,ysize)
image[*,*]=jpegimg[0,*,*]
newimage=congrid(image,zoomx,zoomy,/INTERP)
tvscl,newimage
XInterAnimate,Frame=i,Window=!D.Window
endfor
XInterAnimate,1
endif




if 1 then begin
;for pngfile
read_png,thesefiles[0],pngimg
cartooninfo=intarr(3)
cartooninfo[0]=zoomx
cartooninfo[1]=zoomy
cartooninfo[2]=numfiles

print,cartooninfo

XInterAnimate,Set=cartooninfo,/Showload
for i=0,numfiles-1 do begin
read_png,thesefiles[i],pngimg
image=pngimg[0,*,*]
xsize=n_elements(image[0,*,0])
ysize=n_elements(image[0,0,*])
image=fltarr(xsize,ysize)
image[*,*]=pngimg[0,*,*]
newimage=congrid(image,zoomx,zoomy,/INTERP)
tvscl,newimage
XInterAnimate,Frame=i,Window=!D.Window
endfor
XInterAnimate,0.1
endif


end
