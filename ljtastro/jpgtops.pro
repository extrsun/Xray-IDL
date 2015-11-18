pro jpgtops,jpgfile,psfile,zoomx,zoomy,psxsize,psysize,type
;when type=0,1,2, draw one of the color; when type=3, draw true color image.
;EXAMPLE:
;.compile jpgtops.pro
;jpgtops,'Fig4.jpg','Fig4.ps',3,3,9.31,5.26,0

READ_JPEG, jpgfile, image, true=1
help,image
sizex=n_elements(image[0,*,0])
sizey=n_elements(image[0,0,*])
newimage=congrid(image,3,sizex*zoomx,sizey*zoomy,/INTERP)

thisDevice=!D.Name
Set_Plot,'PS'
Device,Filename=psfile,Xsize=psxsize,Ysize=psysize,XOffset=0,YOffset=0,/COLOR,/Inches,/Encapsulated,/Preview

if type ne 3 then begin
tv,newimage[type,*,*]
endif

if type eq 3 then begin
tv,newimage,true=1
endif

Device,/Close_File
Device,Encapsulated=0,Preview=0
Set_Plot,thisDevice

end
