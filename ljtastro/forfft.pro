pro forfft
imgxsize=1008
;500
;129
imgysize=1024
;375
;129
image1=fltarr(imgxsize,imgysize)
zoomx=1
zoomy=1
toolandtime='TOOL  :forfft.pro-IDL,  2006-04-19   '
outputfits='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/fft/test.fits'
file='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/fft/Ofluxdivtotalflux_img.fits'
;Ofluxdivtotalflux_img.fits'
;acisf00118_source_bin1.fits'
jpegfile='/home/ljt/ljt/picture/astronomy/best_jupiter_cass.jpg'
jpegfiles='/home/ljt/ljt/picture/landscape9/zzz/*.jpg'
thesefiles=findfile(jpegfiles,count=numfiles)
;jpegfile=thesefiles[120]
image=mrdfits(file,0,fitshead)

read_jpeg,jpegfile,jpegimg
print,jpegfile
help,jpegimg
image1[0:imgxsize-1,0:imgysize-1]=jpegimg[2,0:imgxsize-1,0:imgysize-1]

;image1=image

Fre_image=FFT(image1,-1)

para1=1.0
para2=1
para3=1
filterL=1./(1.+para1*(Fre_image/para2)^para3)
filterH=1./(1.+para1*(para2/Fre_image)^para3)
filtered_imgH=FFT(Fre_image*filterH,1)
filtered_imgL=FFT(Fre_image*filterL,1)
powerH=alog(abs(Fre_image*filterH))
powerL=alog(abs(Fre_image*filterL))

window,1,retain=2,xsize=imgxsize,ysize=imgysize
tvscl,image1

window,2,retain=2
shade_surf,alog(abs(Fre_image))

window,3,retain=2,xsize=imgxsize*zoomx,ysize=imgysize*zoomy
;scaledimage=Bytscl(image2,Min=-4,Max=2,Top=255)
;newimage=congrid(image1,imgxsize*zoomx,imgysize*zoomy,/INTERP)
;tv,newimage
tvscl,filtered_imgH

window,4,retain=2
shade_surf,powerH

window,5,retain=2,xsize=imgxsize*zoomx,ysize=imgysize*zoomy
tvscl,filtered_imgL

window,6,retain=2
shade_surf,powerL

window,7,retain=2,xsize=imgxsize*zoomx,ysize=imgysize*zoomy
tvscl,filtered_imgH+filtered_imgL
;tvscl,powerH/powerL

fitsimg=filtered_imgH[0:imgxsize-1,0:imgysize-1]
help,fitsimg


;writeimgfits,fitsimg,outputfits,fitshead,129,129,toolandtime
end
