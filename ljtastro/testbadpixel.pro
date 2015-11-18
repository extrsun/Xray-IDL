function badpixel,file,xrange,yrange,minvalue,maxvalue

label=1
;if label=1,set the NaN pixel to be 0; if label=0,reset the bad pixel.

stanumber=50
statistic=fltarr(2,stanumber)
stalabel=findgen(stanumber)
statistic[0,*]=minvalue+stalabel[*]*(maxvalue-minvalue)/float(stanumber)
statistic[1,*]=0
;print,statistic[0,*]

data=mrdfits(file,0,fitshead)

if label eq 1 then begin
 data1=data
 data[*,*]=0.
endif

labelimg=intarr(xrange,yrange)

labelimg[*,*]=0

for i=0,xrange-1 do begin
for j=0,yrange-1 do begin
 if label eq 0 then begin
  if data[i,j] lt minvalue then begin
     data[i,j]=0;minvalue;
     labelimg[i,j]=-1
  endif
  if data[i,j] gt maxvalue then begin
     data[i,j]=maxvalue;mean(data[i-1:i+1,j-1:j+1]);0.
     labelimg[i,j]=1
  endif
 endif
 if label eq 1 then begin 
  if data1[i,j] gt minvalue and data1[i,j] lt maxvalue then begin
     data[i,j]=data1[i,j]
     labelimg[i,j]=1
     slabel=fix((data1[i,j]-minvalue)*float(stanumber)/(maxvalue-minvalue))
     statistic[1,slabel]=statistic[1,label]+1
  endif
 endif
endfor
endfor

;structuredata={image:data,labelimage:labelimg,header:fitshead}
structuredata={image:data,labelimage:labelimg,header:fitshead,statisticarray:statistic}

return,structuredata
end

pro testbadpixel

file="/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec/figures/paraimg/MgdivO.fits"
xrange=128
yrange=128
minvalue=0.1
maxvalue=10
savenumber=1
toolandtime='TOOL  :testbadpixel.pro-IDL,  2006-04-22   '
outputfilename="/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec/figures/paraimg/MgdivO_SubBad.fits"

structureimage=badpixel(file,xrange,yrange,minvalue,maxvalue)

image=structureimage.image
labelimage=structureimage.labelimage
fitshead=structureimage.header
statistic=structureimage.statisticarray

if savenumber then begin
 
 fxaddpar, fitshead, 'HISTORY', toolandtime
 mwrfits,image,outputfilename,fitshead

endif

window,1,retain=2
scaledimage=Bytscl(image,Min=minvalue,Max=maxvalue,Top=255)
tv,scaledimage

window,2,retain=2
tvscl,labelimage

window,3,retain=2
plot,statistic[0,*],statistic[1,*],psym=0
end
