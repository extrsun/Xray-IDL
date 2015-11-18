; $Id: writeps2.pro,v 1.0 2006/01/13
;
; Copyright (c) 2005-2006, Li, Jiang Tao. All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       writeps2
; PURPOSE:
;       Simply write a .eps figure.
; EXAMPLE:
;       thisDevice=writeps1(outputhardnessmap,0,7,5,0,0)
;       tv,smoothimage
;       writeps2,thisDevice
pro writeps2,thisDevice
Device,/Close_File
Device,Encapsulated=0,Preview=0
Set_Plot,thisDevice
end
