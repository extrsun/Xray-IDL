; $Id: variablebin.pro,v 1.2 2006/01/13
;
; Copyright (c) 2005-2006, Li, Jiang Tao. All rights reserved.
;       Unauthorized reproduction prohibited.

function variablebin,imagedata,mincounts,xrange,yrange,initialbin,backgroundtype,imagetype
;+
; NAME:
;       variablebin
; PURPOSE:
;       rebin the image by a given minimum counts number.
; EXPLANATION:
;       This function could be used to all image files, especially has good effects on sources
;       whose brightness in different parts are obviously different.
;       The returned file is also an image file.
; CALLING SEQUENCE:
;       outputimage=variablebin(imagedata,mincounts,xrange,yrange,$
;                               initialbin,backgroundtype,imagetype)
; INPUT PARAMETERS:
;       imagedata           = input image data, this parameter should be a 2D float array.
;                             imagedata=fltarr(xrange[1]-xrange[0]+1,yrange[1]-yrange[0]+1)
;       mincounts           = the minimum counts number in every rebinned pixels. Notice that
;                             some of the rebinned pixels may have counts numbers less than
;                             mincounts, but greater than mincounts/4.
;       xrange              = the range of the initial image in the x direction, it is better
;                             to let this array satisfy this relation:
;                             xrange[1]-xrange[0]+1=2^n
;       yrange              = the range of the initial image in the y direction, it is better
;                             to let this array satisfy this relation:
;                             yrange[1]-yrange[0]+1=2^m
;       initialbin          = the initial bin number given by the user, this parameter should be
;                             2^k, and it should be large enough to let every initial binned
;                             pixel to have counts numbers greater than mincounts.
;       backgroundtype      = if the initial binned pixel has counts less than mincounts, this
;                             pixel will be regard as background. If backgroundtype equals 0, set
;                             the background pixels to be 0, if backgroundtype equals 1 set them
;                             to be an average value over the initial binned pixel.
;       imagetype           = if imagetype equals 0, give out the binned image, if imagetype
;                             equals 1, give out the labelimage, in the labelimage, if label
;                             equals 0, the counts number in a binned pixel is less than
;                             mincounts, if label equals 1, the counts number is greater than
;                             mincounts and less than 4*mincounts, if label equals 2, the counts
;                             number is greater than 4*mincounts.
; EXAMPLE:
;       mincounts=20
;       xrange=[30,285]
;       yrange=[0,223]
;       initialbin=32
;       backgroundtype=0
;       imagetype=0
;       zoomx=300
;       zoomy=250
;       image=fltarr(zoomx,zoomy)
;       imagecut=image[xrange[0]:xrange[1],yrange[0]:yrange[1]]
;       image1=variablebin(imagecut,mincounts,xrange,yrange,initialbin,backgroundtype,imagetype)


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
