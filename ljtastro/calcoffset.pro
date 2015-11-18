; $Id: calcoffset.pro,v 1.0 2006/01/14
;
; Copyright (c) 2005-2006, Li, Jiang Tao. All rights reserved.
;       Unauthorized reproduction prohibited.
;===============================================================

function linearcorrelate,x,y
;+
; NAME:
;       linearcorrelate
; PURPOSE:
;       calculate the linear correlation coefficient of two arrays.
; EXPLANATION:
;       This function could be used to n-dimensional array.
; CALLING SEQUENCE:
;       correlationcoefficient=linearcorrelate(x,y)
; INPUT PARAMETERS:
;       x           = the first input array
;       y           = the second input array
; EXAMPLE:
;       x=[1,2,3,4,5]
;       y=[6,7,8,9,10]
;       correlationcoefficient=linearcorrelate(x,y)

return,total(x*y)/sqrt(total(x^2)*total(y^2))
end
;===============================================================

function crosscorrelate,x,y,k
;+
; NAME:
;       crosscorrelate 
; PURPOSE:
;       calculate the cross correlation coefficient of two 1-dimensional arrays.
; EXPLANATION:
;       
; CALLING SEQUENCE:
;       correlationcoefficient=crosscorrelate(x,y,k)
; INPUT PARAMETERS:
;       x           = the first input array
;       y           = the second input array
;       k           = the delay of these two arrays
; EXAMPLE:
;       x=[1,2,3,4,5]
;       y=[6,7,8,9,10]
;       correlationcoefficient=linearcorrelate(x,y,1)

numberx=n_elements(x)
numbery=n_elements(y)
if k ge 0 then begin
  xcut=x[0:numberx-1-k]
  ycut=y[k:numbery-1]
  return,linearcorrelate(xcut,ycut)
endif
if k lt 0 then begin
  xcut=x[-k:numberx-1]
  ycut=y[0:numbery-1+k]
  return,linearcorrelate(xcut,ycut)
endif 
end
;===============================================================

function timedelay,x,y,delaystart,delayend
;+
; NAME:
;       timedelay 
; PURPOSE:
;       calculate the time delay of two 1-dimensional arrays.
; EXPLANATION:
;       
; CALLING SEQUENCE:
;       delay=timedelay(x,y,delaystart,delayend)
; INPUT PARAMETERS:
;       x           = the first input array
;       y           = the second input array
;       delaystart     = the lower limit of the possible time delay given by the user.
;       delayend     = the uper limit of the possible time delay given by the user. 
; EXAMPLE:
;       x=[1,2,3,4,5,4,3,2,1]
;       y=[6,7,8,9,10,11,12,11,10]
;       delaystart=-3
;       delayend=3
;       delay=timedelay(x,y,delaystart,delayend)

number=delayend-delaystart+1
timedelayarray=fltarr(2,number)
for i=0,number-1 do begin
  timedelayarray[0,i]=i+delaystart
  timedelayarray[1,i]=crosscorrelate(x,y,i+delaystart)
endfor
timedelayvalue=where(timedelayarray[1,*] eq max(timedelayarray[1,*]))+delaystart
timedelayresult={delay:timedelayvalue,array:timedelayarray}
return,timedelayresult
end
;===============================================================

function crosscorrelate2D,x,y,m,n
;+
; NAME:
;       crosscorrelate2D
; PURPOSE:
;       calculate the cross correlation coefficient of two 2-dimensional arrays.
; EXPLANATION:
;       
; CALLING SEQUENCE:
;       correlationcoefficient=crosscorrelate2D(x,y,m,n)
; INPUT PARAMETERS:
;       x           = the first input array
;       y           = the second input array
;       m           = the delay of these two arrays in "x" direction.
;       n           = the delay of these two arrays in "y" direction.
; EXAMPLE:
;       x=[[1,2,3,4,5],[4,3,2,1,0]]
;       y=[[6,7,8,9,10],[11,12,11,10,9]]
;       m=-1
;       n=1
;       correlationcoefficient=crosscorrelate2D(x,y,m,n) 

numberx1=n_elements(x[*,0])
numberx2=n_elements(x[0,*])
numbery1=n_elements(y[*,0])
numbery2=n_elements(y[0,*])
if m ge 0 and n ge 0 then begin
  xcut=x[0:numberx1-1-m,0:numberx2-1-n]
  ycut=y[m:numbery1-1,n:numbery2-1]
  return,linearcorrelate(xcut,ycut)
endif
if m ge 0 and n lt 0 then begin
  xcut=x[0:numberx1-1-m,-n:numberx2-1]
  ycut=y[m:numbery1-1,0:numbery2-1+n]
  return,linearcorrelate(xcut,ycut)
endif
if m lt 0 and n ge 0 then begin
  xcut=x[-m:numberx1-1,0:numberx2-1-n]
  ycut=y[0:numbery1-1+m,n:numbery2-1]
  return,linearcorrelate(xcut,ycut)
endif
if m lt 0 and n lt 0 then begin
  xcut=x[-m:numberx1-1,-n:numberx2-1]
  ycut=y[0:numbery1-1+m,0:numbery2-1+n]
  return,linearcorrelate(xcut,ycut)
endif
end
;===============================================================

function timedelay2D,x,y,delaystart,delayend
;+
; NAME:
;       timedelay2D 
; PURPOSE:
;       calculate the delay of two 2-dimensional arrays.
; EXPLANATION:
;       
; CALLING SEQUENCE:
;       delay=timedelay2D(x,y,delaystart,delayend)
; INPUT PARAMETERS:
;       x           = the first input array
;       y           = the second input array
;       delaystart     = the lower limit of the possible time delay in two directions given by the user.
;       delayend     = the uper limit of the possible time delay in two directions given by the user. 

number1=delayend[0]-delaystart[0]+1
number2=delayend[1]-delaystart[1]+1
timedelayarray=fltarr(number1,number2)
timedelayvalue=intarr(2)
maxvalue=0.
for i=0,number1-1 do begin
for j=0,number2-1 do begin
  timedelayarray[i,j]=crosscorrelate2D(x,y,i+delaystart[0],j+delaystart[1])
  if timedelayarray[i,j] gt maxvalue then begin
    maxvalue=timedelayarray[i,j]
    timedelayvalue[0]=i+delaystart[0]
    timedelayvalue[1]=j+delaystart[1]
  endif
endfor
endfor
timedelayresult={delay:timedelayvalue,array:timedelayarray}
return,timedelayresult
end
;===============================================================

pro calcoffset
file1="/home/ljt/ljt/data/AboutSNR/SNR/Kepler/merge/7366_regionforcalcoffset_bin1.fits"
file2="/home/ljt/ljt/data/AboutSNR/SNR/Kepler/merge/6718_regionforcalcoffset_bin1.fits"
image1=mrdfits(file1,0,fitshead)
image2=mrdfits(file2,0,fitshead)
xlong=1007
ylong=1768
delaystart=[-6,-7]
delayend=[4,3]

result=timedelay2D(image1[0:xlong,0:ylong],image2[0:xlong,0:ylong],delaystart,delayend)
print,result.delay

window,1,retain=2,xsize=1024,ysize=1024
image=image1[0:xlong,0:ylong]+image2[0:xlong,0:ylong]
newimage=congrid(image,fix(xlong/2),fix(ylong/2),/interp)
tvscl,alog10(newimage+1)

window,2,retain=2,xsize=1024,ysize=1024
if result.delay[0] ge 0 and result.delay[1] ge 0 then begin
image=image1[0:xlong-result.delay[0],0:ylong-result.delay[1]]+image2[result.delay[0]:xlong,result.delay[1]:ylong]
endif
if result.delay[0] ge 0 and result.delay[1] lt 0 then begin
image=image1[0:xlong-result.delay[0],(-result.delay[1]):ylong]+image2[result.delay[0]:xlong,0:(ylong+result.delay[1])]
endif
if result.delay[0] lt 0 and result.delay[1] ge 0 then begin
image=image1[(-result.delay[0]):xlong,0:ylong-result.delay[1]]+image2[0:(xlong+result.delay[0]),result.delay[1]:ylong]
endif
if result.delay[0] lt 0 and result.delay[1] lt 0 then begin
image=image1[(-result.delay[0]):xlong,(-result.delay[1]):ylong]+image2[0:(xlong+result.delay[0]),0:(ylong+result.delay[1])]
endif
newimage=congrid(image,fix(xlong/2),fix(ylong/2),/interp)
tvscl,alog10(newimage+1)

window,3,retain=2
newimage=congrid(result.array,200,200,/interp)
tvscl,newimage

end
