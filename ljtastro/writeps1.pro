; $Id: writeps1.pro,v 1.0 2006/01/13
;
; Copyright (c) 2005-2006, Li, Jiang Tao. All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       writeps1
; PURPOSE:
;       Simply write a .eps figure.
; EXAMPLE:
;       thisDevice=writeps1(outputhardnessmap,0,7,5,0,0)
;       tv,smoothimage
;       writeps2,thisDevice
function writeps1,filename,loadctnumber,Xsizenumber,Ysizenumber,XOffsetnumber,YOffsetnumber
thisDevice=!D.Name
Set_Plot,'PS'
Device,Filename=filename,Xsize=Xsizenumber,Ysize=Ysizenumber,XOffset=XOffsetnumber,YOffset=YOffsetnumber,/Inches,/Encapsulated,/Preview
Device,Color=1,Bits_Per_Pixel=8
loadct,loadctnumber
return,thisDevice
end

