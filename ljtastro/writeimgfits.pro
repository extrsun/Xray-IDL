; $Id: writeimgfits.pro,v 1.0 2006/01/13
;
; Copyright (c) 2005-2006, Jiang, Bing and Li, Jiang Tao. All rights reserved.
;       Unauthorized reproduction prohibited.

pro writeimgfits,image,path,fitshead,xlength,ylength,toolandtime
;+
; NAME:
;       writeimgfits
; PURPOSE:
;       write a image file in the fits format.
; EXPLANATION:
;       this procedure may be used to write a fits file from a image file of Chandra data.
; CALLING SEQUENCE:
;       writeimgfits,image,path,fitshead,xlength,ylength,toolandtime
; INPUT PARAMETERS:
;       image           = the image data.
;       path            = the path of the output fits file.
;       fitshead        = the head of a fits file.
;       xlength         = the length of the image in x direction in units of pixel.
;       ylength         = the length of the image in y direction in units of pixel.
;       toolandtime     = the tool used to draw the image and the time you draw it.
; PROCEDURE CALLS:
;       fxaddpar,mwrfits,mrdfits
; EXAMPLE:
;       uselessforhead=mrdfits(totalbandfile,0,totalbandfitshead)
;       outputEWfits="/home/ljt/ljt/data/AboutSNR/SNR/UNREDUCTED/Kes27/secondary/Kes27_S_lineintensitymap.fits"
;       image2=EWmap(leftfile,rightfile,linefile,leftenergy,rightenergy,lineenergy,binpara,EWmaptype)
;       zoomx=xrange[1]-xrange[0]+1
;       zoomy=yrange[1]-yrange[0]+1
;       toolandtime='TOOL  :drawimage.pro-IDL,  2005-12-18   '
;       writeimgfits,image2,outputEWfits,totalbandfitshead,zoomx,zoomy,toolandtime

 fxaddpar, fitshead, 'BITPIX', '-32','number of bits per data pixel'
 fxaddpar, fitshead, 'NAXIS1', string(xlength),'length of data axis'
 fxaddpar, fitshead, 'NAXIS2', string(ylength),'length of data axis'
 fxaddpar, fitshead, 'HISTORY', toolandtime
 mwrfits,image,path,fitshead
end