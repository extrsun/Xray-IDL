; $Id: hardnessmap.pro,v 1.0 2006/01/13
;
; Copyright (c) 2005-2006, Li, Jiang Tao. All rights reserved.
;       Unauthorized reproduction prohibited.

function hardnessmap,hardfile,softfile,binpara
;+
; NAME:
;       hardnessmap
; PURPOSE:
;       draw the hardness map.
; EXPLANATION:
;
; CALLING SEQUENCE:
;       outputhardnessmap=hardnessmap(hardfile,softfile,binpara)
; INPUT PARAMETERS:
;       hardfile           = the path of the hard band image file.
;       softfile           = the path of the soft band image file.
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
; FUNCTION CALLS:
;       variablebin
; EXAMPLE:
;       hardfile="/home/ljt/ljt/data/AboutSNR/SNR/UNREDUCTED/Kes27/secondary/acisf03852_1.6-5keV_bin10.fits"
;       softfile="/home/ljt/ljt/data/AboutSNR/SNR/UNREDUCTED/Kes27/secondary/acisf03852_0.3-1.6keV_bin10.fits"
;       binpara=intarr(8)
;       binpara[0:1]=xrange[0:1]
;       binpara[2:3]=yrange[0:1]
;       binpara[4]=mincounts
;       binpara[5]=initialbin
;       binpara[6]=backgroundtype
;       binpara[7]=imagetype
;       image1=hardnessmap(hardfile,softfile,binpara)

xrange=binpara[0:1]
yrange=binpara[2:3]
mincounts=binpara[4]
initialbin=binpara[5]
backgroundtype=binpara[6]
imagetype=binpara[7]

hardimage=mrdfits(hardfile,0,fitsheadh)
softimage=mrdfits(softfile,0,fitsheads)

hardimagecut=hardimage[xrange[0]:xrange[1],yrange[0]:yrange[1]]
softimagecut=softimage[xrange[0]:xrange[1],yrange[0]:yrange[1]]

hardimagebin=variablebin(hardimagecut,mincounts,xrange,yrange,initialbin,backgroundtype,imagetype)
softimagebin=variablebin(softimagecut,mincounts,xrange,yrange,initialbin,backgroundtype,imagetype)

hardnessmap=hardimagebin[*,*]/softimagebin[*,*]

return,hardnessmap

end
