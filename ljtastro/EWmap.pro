; $Id: EWmap.pro,v 1.0 2006/01/13
;
; Copyright (c) 2005-2006, Li, Jiang Tao. All rights reserved.
;       Unauthorized reproduction prohibited.

function EWmap,leftfile,rightfile,linefile,leftenergy,rightenergy,lineenergy,binpara,EWmaptype
;+
; NAME:
;       EWmap
; PURPOSE:
;       draw the EW map.
; EXPLANATION:
;
; CALLING SEQUENCE:
;       outputEWmap=EWmap(leftfile,rightfile,linefile,leftenergy,rightenergy,lineenergy,$
;                         binpara,EWmaptype)
; INPUT PARAMETERS:
;       leftfile           = the path of the left shoulder band image file.
;       rightfile          = the path of the right shoulder band image file.
;       linefile           = the path of the line band image file.
;       leftenergy         = the range of the left shoulder band.
;       rightenergy        = the range of the right shoulder band.
;       lineenergy         = the range of the line shoulder band.
;       binpara            = the parameters used in the variablebin function, it
;                            contains eight integral parameters:
;                          binpara=intarr(8)
;                          binpara[0:1]=xrange[0:1]
;                          binpara[2:3]=yrange[0:1]
;                          binpara[4]=mincounts
;                          binpara[5]=initialbin
;                          binpara[6]=backgroundtype
;                          binpara[7]=imagetype
;                            users may turn to the explanation of variablebin function to see
;                            what do these parameters mean.
;       EWmaptype          = if EWmaptype eq 0, return the line intensity map, if EWmaptype eq 1,
;                            return the EW map.
; FUNCTION CALLS:
;       variablebin
; EXAMPLE:
;       leftfile="/home/ljt/ljt/data/AboutSNR/SNR/UNREDUCTED/Kes27/secondary/acisf03852_2.05-2.2keV_bin10.fits"
;       rightfile="/home/ljt/ljt/data/AboutSNR/SNR/UNREDUCTED/Kes27/secondary/acisf03852_2.7-3keV_bin10.fits"
;       linefile="/home/ljt/ljt/data/AboutSNR/SNR/UNREDUCTED/Kes27/secondary/acisf03852_2.2-2.7keV_bin10.fits"
;       leftenergy=[2.05,2.2]
;       rightenergy=[2.7,3.0]
;       lineenergy=[2.2,2.7]
;       binpara=intarr(8)
;       binpara[0:1]=xrange[0:1]
;       binpara[2:3]=yrange[0:1]
;       binpara[4]=mincounts
;       binpara[5]=initialbin
;       binpara[6]=backgroundtype
;       binpara[7]=imagetype
;       EWmaptype=1
;       image2=EWmap(leftfile,rightfile,linefile,leftenergy,rightenergy,lineenergy,$
;                    binpara,EWmaptype)

xrange=binpara[0:1]
yrange=binpara[2:3]
mincounts=binpara[4]
initialbin=binpara[5]
backgroundtype=binpara[6]
imagetype=binpara[7]

leftimage=mrdfits(leftfile,0,fitsheadl)
rightimage=mrdfits(rightfile,0,fitsheadr)
lineimage=mrdfits(linefile,0,fitsheadline)

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

returndata={EWmaps:averageEWmap,continuummap:lineContinuum}

if EWmaptype eq 0 then begin
return,EWmap
endif
if EWmaptype eq 1 then begin
return,returndata
endif

end
