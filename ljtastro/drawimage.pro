function writeps1,filename,loadctnumber,Xsizenumber,Ysizenumber,XOffsetnumber,YOffsetnumber
thisDevice=!D.Name
Set_Plot,'PS'
Device,Filename=filename,Xsize=Xsizenumber,Ysize=Ysizenumber,XOffset=XOffsetnumber,YOffset=YOffsetnumber,/Inches,/Encapsulated,/Preview
Device,Color=1,Bits_Per_Pixel=8
loadct,loadctnumber
return,thisDevice
end

pro writeps2,thisDevice
Device,/Close_File
Device,Encapsulated=0,Preview=0
Set_Plot,thisDevice
end




function variablebin,imagedata,mincounts,xrange,yrange,initialbin,backgroundtype,imagetype
;xrange=intarr(2),yrange=intarr(2),imagedata=fltarr(xrange[1]-xrange[0]+1,yrange[1]-yrange[0]+1),mincounts=fix,initialbin=fix,backgroundtype=fix.
;notice that initialbin should be 2^n. xrange[1]-xrange[0]+1,yrange[1]-yrange[0]+1 should be m*initialbin and k*initialbin
;if backgroundtype eq 0, set the background to be 0, else set it to be an average value.
;if imagetype eq 0, give out the binned image, if imagetype eq 1, give out the labelimage, in the labelimage, if label eq 0, the counts
;number in a bin is less than mincounts, if label eq 1, the counts number is greater than mincounts, if label eq 2, the counts number
;is greater than 4*mincounts.

labelimage=intarr(xrange[1]-xrange[0]+1,yrange[1]-yrange[0]+1)
binimage=fltarr(xrange[1]-xrange[0]+1,yrange[1]-yrange[0]+1)
labelimage[*,*]=2

for binlabel=0,100 do begin
 
  binnumber=initialbin/2^binlabel
 
  if binnumber lt 1 then begin
 
    goto,NEXT2

  endif
 
  xnumber=(xrange[1]-xrange[0]+1)/binnumber
  ynumber=(yrange[1]-yrange[0]+1)/binnumber

  for i=0,xnumber-1 do begin
  for j=0,ynumber-1 do begin

    totalcounts=0

    for k=0,binnumber-1 do begin
    for t=0,binnumber-1 do begin

       if  labelimage[i*binnumber+k,j*binnumber+t] eq 0 or labelimage[i*binnumber+k,j*binnumber+t] eq 1 then begin

         goto,NEXT1

       endif

       totalcounts=totalcounts+imagedata[i*binnumber+k,j*binnumber+t]

    endfor
    endfor

    if totalcounts lt mincounts then begin

       labelimage[i*binnumber:(i+1)*binnumber-1,j*binnumber:(j+1)*binnumber-1]=0
;we can select one of the two command lines below.the first line is to set the background to be 0, the second line is to set the background
;to be an average value.
       if backgroundtype eq 0 then begin   
         if binlabel eq 0 then begin 
          binimage[i*binnumber:(i+1)*binnumber-1,j*binnumber:(j+1)*binnumber-1]=0.
         endif
         if binlabel ne 0 then begin
          binimage[i*binnumber:(i+1)*binnumber-1,j*binnumber:(j+1)*binnumber-1]=float(totalcounts)/float(binnumber^2)
         endif
       endif          
       if backgroundtype eq 1 then begin          
          binimage[i*binnumber:(i+1)*binnumber-1,j*binnumber:(j+1)*binnumber-1]=float(totalcounts)/float(binnumber^2)
       endif  

    endif

    if totalcounts lt 4*mincounts and totalcounts ge mincounts then begin

       labelimage[i*binnumber:(i+1)*binnumber-1,j*binnumber:(j+1)*binnumber-1]=1
       binimage[i*binnumber:(i+1)*binnumber-1,j*binnumber:(j+1)*binnumber-1]=float(totalcounts)/float(binnumber^2)

    endif

    if totalcounts ge 4*mincounts then begin

       labelimage[i*binnumber:(i+1)*binnumber-1,j*binnumber:(j+1)*binnumber-1]=2
       binimage[i*binnumber:(i+1)*binnumber-1,j*binnumber:(j+1)*binnumber-1]=float(totalcounts)/float(binnumber^2)

    endif
  
    NEXT1: 

  endfor 
  endfor

endfor

NEXT2:

if imagetype eq 0 then begin
return,binimage
endif
if imagetype eq 1 then begin
return,labelimage
endif
end


function hardnessmap,hardfile,softfile,binpara

xrange=binpara[0:1]
yrange=binpara[2:3]
mincounts=binpara[4]
initialbin=binpara[5]
backgroundtype=binpara[6]
imagetype=binpara[7]

hardimage=mrdfits(hardfile,0,fitsheadh)
softimage=mrdfits(softfile,0,fitsheads)
;help,hardimage
hardimagecut=hardimage[xrange[0]:xrange[1],yrange[0]:yrange[1]]
softimagecut=softimage[xrange[0]:xrange[1],yrange[0]:yrange[1]]

hardimagebin=variablebin(hardimagecut,mincounts,xrange,yrange,initialbin,backgroundtype,imagetype)
softimagebin=variablebin(softimagecut,mincounts,xrange,yrange,initialbin,backgroundtype,imagetype)

hardnessmap=hardimagebin[*,*]/softimagebin[*,*]

return,hardnessmap

end


function EWmap,leftfile,rightfile,linefile,leftenergy,rightenergy,lineenergy,binpara
;,EWmaptype
;if EWmaptype eq 0, return the line intensity map, if EWmaptype eq 1, return the EW map.
xrange=binpara[0:1]
yrange=binpara[2:3]
mincounts=binpara[4]
initialbin=binpara[5]
backgroundtype=binpara[6]
imagetype=binpara[7]

leftimage=mrdfits(leftfile,0,fitsheadl)
rightimage=mrdfits(rightfile,0,fitsheadr)
lineimage=mrdfits(linefile,0,fitsheadline)

help,leftimage,rightimage,lineimage

leftimagecut=leftimage[xrange[0]:xrange[1],yrange[0]:yrange[1]]
rightimagecut=rightimage[xrange[0]:xrange[1],yrange[0]:yrange[1]]
lineimagecut=lineimage[xrange[0]:xrange[1],yrange[0]:yrange[1]]

leftimagebin=variablebin(leftimagecut,mincounts,xrange,yrange,initialbin,backgroundtype,imagetype)
rightimagebin=variablebin(rightimagecut,mincounts,xrange,yrange,initialbin,backgroundtype,imagetype)
lineimagebin=variablebin(lineimagecut,mincounts,xrange,yrange,initialbin,backgroundtype,imagetype)

rightposition=(rightenergy[1]+rightenergy[0])/2.
leftposition=(leftenergy[1]+leftenergy[0])/2.
lineposition=(lineenergy[1]+lineenergy[0])/2.

rightCts=rightimagebin/(rightenergy[1]-rightenergy[0])
leftCts=leftimagebin/(leftenergy[1]-leftenergy[0])
lineCts=lineimagebin/(lineenergy[1]-lineenergy[0])

lineContinuum=(rightposition-lineposition)*(leftCts-rightCts)/(rightposition-leftposition)+rightCts
EWmap=(lineCts-lineContinuum)*(lineenergy[1]-lineenergy[0])
averageEWmap=EWmap/(lineContinuum*(lineenergy[1]-lineenergy[0]))

;if EWmaptype eq 0 then begin
;return,EWmap
;;(lineContinuum*(lineenergy[1]-lineenergy[0]))
;;EWmap
;endif
;if EWmaptype eq 1 then begin
;return,averageEWmap
;endif

returnpara={EW:averageEWmap,line:EWmap,continuum:(lineContinuum*(lineenergy[1]-lineenergy[0]))}

return,returnpara

end



pro drawimage
;tacitly approved parameters:
mincounts=20
initialbin=64
backgroundtype=1
imagetype=0
;EWmaptype=1
;if EWmaptype eq 0, return the line intensity map, if EWmaptype eq 1, return the EW map.
xrange=intarr(2)
yrange=intarr(2)
saveepsnumber=0
;if saveepsnumber=1, save the eps figure, if saveepsnumber=0, don't save the eps figure.
savefitsnumber=0
;if savefitsnumber=1, save the fits figure, if savefitsnumber=0, don't save the fits figure.

;change the tacitly approved parameters or not
mincounts=5
initialbin=8
backgroundtype=0
;imagetype=
;EWmaptype=0
saveepsnumber=0
savefitsnumber=1

;to select which figure you need:
hardnessmaplabel=1
totalbandmaplabel=0 
EWmaplabel=0

;parameters must be given by the users:
xrange[0]=0
yrange[0]=0
xrange[1]=initialbin*21+xrange[0]-1
yrange[1]=initialbin*19+yrange[0]-1
;or simply
xrange[1]=128
yrange[1]=128

toolandtime='TOOL  :drawimage.pro-IDL,  2006-04-12   '
totalbandfile="/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/EWmap/aidfile/source_1.33-1.60keV_bin1.fits"
;these parameters must be given, no matter which map you want to draw.

if hardnessmaplabel then begin
 hardfile="/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/EWmap/aidfile/source_2.25-2.68keV_bin1.fits"
 softfile="/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/EWmap/aidfile/source_bin1.fits"
 outputhardnessmap="/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/EWmap/source_2.25-2.68keV_total_bin1_min5.eps"
 outputhardnessfits="/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/EWmap/source_2.25-2.68keV_total_bin1_min5.fits"
endif

if totalbandmaplabel then begin
 outputtotalbandmap="/home/ljt/ljt/data/AboutSNR/SNR/UNREDUCTED/Kes27/secondary/Kes27_totalbandmap.eps"
 outputtotalbandfits="/home/ljt/ljt/data/AboutSNR/SNR/UNREDUCTED/Kes27/secondary/Kes27_totalbandmap.fits"
endif

if EWmaplabel then begin
 leftfile="/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/EWmap/aidfile/source_1.33-1.60keV_bin1.fits"
 rightfile="/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/EWmap/aidfile/source_2.12-2.25keV_bin1.fits"
 linefile="/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/EWmap/aidfile/source_2.25-2.68keV_bin1.fits"
 outputEWmap="/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/EWmap/S2.47_EW_bin1_min10.eps"
 outputEWfits="/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/EWmap/S2.47_EW_bin1_min10.fits"
 outputlinefits="/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/EWmap/S2.47_line_bin1_min10.fits"
 outputcontinuumfits="/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/EWmap/S2.47_continuum_bin1_min10.fits"
 leftenergy=[1.33,1.60]
 rightenergy=[2.12,2.25]
 lineenergy=[2.25,2.68]
endif

;parameters for image display:
zoomnumber=1
zoomx=xrange[1]-xrange[0]+1
zoomy=yrange[1]-yrange[0]+1
minintensity1=0
maxintensity1=2.5
minEW=0
maxEW=0.1
smoothnumber1=0

;the parameters to be used in variablebin function, this array should be given to all the functions which will use the variablebin function.
binpara=intarr(8)
binpara[0:1]=xrange[0:1]
binpara[2:3]=yrange[0:1]
binpara[4]=mincounts
binpara[5]=initialbin
binpara[6]=backgroundtype
binpara[7]=imagetype

if savefitsnumber then begin
 uselessforhead=mrdfits(totalbandfile,0,totalbandfitshead)
endif


;to draw the hardness map
if hardnessmaplabel then begin

 image1=hardnessmap(hardfile,softfile,binpara)


 if savefitsnumber then begin
  fxaddpar, totalbandfitshead, 'BITPIX', '-32','number of bits per data pixel';, AFTER='SIMPLE'  ; BEFORE='NAXIS1'  
  fxaddpar, totalbandfitshead, 'NAXIS1', string(zoomx),'length of data axis';, AFTER='BITPIX'
  fxaddpar, totalbandfitshead, 'NAXIS2', string(zoomy),'length of data axis';, AFTER='NAXIS1'
  fxaddpar, totalbandfitshead, 'HISTORY', toolandtime
  mwrfits,image1,outputhardnessfits,totalbandfitshead
 endif


 window,1,retain=2,xsize=zoomx*zoomnumber,ysize=zoomy*zoomnumber
 newimage=congrid(image1,zoomx*zoomnumber,zoomy*zoomnumber,/interp)
 scaledimage=Bytscl(newimage,Min=minintensity1,Max=maxintensity1,Top=255)
 smoothimage=smooth(scaledimage,smoothnumber1)
 tv,smoothimage

 if saveepsnumber then begin
  thisDevice=writeps1(outputhardnessmap,0,7,5,0,0)
  tv,smoothimage
  writeps2,thisDevice
 endif

 window,2,retain=2,xsize=zoomx*zoomnumber,ysize=zoomy*zoomnumber
 contour,smoothimage

endif


;to draw the total band map.
if totalbandmaplabel then begin

 totalbandimage=mrdfits(totalbandfile,0,fitsheadt)
 totalbandimagecut=totalbandimage[xrange[0]:xrange[1],yrange[0]:yrange[1]]
 TBimage=variablebin(totalbandimagecut,mincounts,xrange,yrange,initialbin,backgroundtype,imagetype)

 if savefitsnumber then begin
  fxaddpar, totalbandfitshead, 'BITPIX', '-32','number of bits per data pixel'  
  fxaddpar, totalbandfitshead, 'NAXIS1', string(zoomx),'length of data axis'
  fxaddpar, totalbandfitshead, 'NAXIS2', string(zoomy),'length of data axis'
  fxaddpar, totalbandfitshead, 'HISTORY', toolandtime
  mwrfits,TBimage,outputtotalbandfits,totalbandfitshead
 endif

 if imagetype eq 1 then begin
  window,4,retain=2
  shade_surf,TBimage
 endif

 window,3,retain=2,xsize=zoomx*zoomnumber,ysize=zoomy*zoomnumber
 newimage=congrid(TBimage,zoomx*zoomnumber,zoomy*zoomnumber,/interp)
 scaledimage=Bytscl(newimage,Min=0,Max=5,Top=255)
 smoothimage=smooth(scaledimage,smoothnumber1)
 tv,smoothimage

 if saveepsnumber then begin
  thisDevice=writeps1(outputtotalbandmap,0,7,5,0,0)
  tv,smoothimage
  writeps2,thisDevice
 endif

endif


;to draw the EW map.
if EWmaplabel then begin

 image2=EWmap(leftfile,rightfile,linefile,leftenergy,rightenergy,lineenergy,binpara);,EWmaptype)

 EWimage=image2.EW
 lineimage=image2.line
 continuumimage=image2.continuum

 if savefitsnumber then begin
  fxaddpar, totalbandfitshead, 'BITPIX', '-32','number of bits per data pixel'
  fxaddpar, totalbandfitshead, 'NAXIS1', string(zoomx),'length of data axis'
  fxaddpar, totalbandfitshead, 'NAXIS2', string(zoomy),'length of data axis'
  fxaddpar, totalbandfitshead, 'HISTORY', toolandtime
  mwrfits,EWimage,outputEWfits,totalbandfitshead
  mwrfits,lineimage,outputlinefits,totalbandfitshead
  mwrfits,continuumimage,outputcontinuumfits,totalbandfitshead
 endif

 window,5,retain=2,xsize=zoomx*zoomnumber,ysize=zoomy*zoomnumber
 newimage=congrid(EWimage,zoomx*zoomnumber,zoomy*zoomnumber,/interp)
 scaledimage=Bytscl(newimage,Min=minEW,Max=maxEW,Top=255)
 smoothimage=smooth(scaledimage,smoothnumber1)
 tv,smoothimage
 ;tvscl,image2

 if saveepsnumber then begin
  thisDevice=writeps1(outputEWmap,0,7,5,0,0)
  tv,smoothimage
  writeps2,thisDevice
 endif

endif


end
