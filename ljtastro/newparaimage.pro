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


function readpara,filename,paranumber
parameters=fltarr(paranumber)
str1="ljt"

openr,lun,filename,/get_lun
    readf,lun,str1
;useless lines
  readf,lun,str1
  parameters[0]=float(strmid(str1,11,4))
  parameters[1]=float(strmid(str1,17,4))
  parameters[2]=float(strmid(str1,31,5))
   for n1=0,6 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[3]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[4]=float(strmid(str1,0,16))
   for n1=0,3 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[5]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[6]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[7]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[8]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[9]=float(strmid(str1,0,16))
    readf,lun,str1
    readf,lun,str1
;useless lines
  readf,lun,str1
  parameters[10]=float(strmid(str1,0,16))
   for n1=0,1 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[11]=float(strmid(str1,0,16))
    readf,lun,str1
;useless lines  
  readf,lun,str1
  parameters[12]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[13]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[14]=float(strmid(str1,0,16))

close,lun
free_lun,lun

return,parameters

end



pro newparaimage
;0:X
;1:Y
;2:counts
;3:nH
;4:T
;5:O
;6:Ne
;7:Mg
;8:Si
;9:S
;10:Fe
;11:Tau
;12:norm
;13:PwrId
;14:PwrNorm
centralposi=[3935.,4060.]
files='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec/spectra/*_SNR3_0.3-10keV.xcm'
thesefiles=findfile(files,count=numfiles)
outputfits='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec/figures/PwrNormdivbin2_img.fits'
toolandtime='TOOL  :newparaimage.pro-IDL,  2006-04-12   '
fileforhead='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec/aidfile/acisf00118_source_bin1.fits'
paranumber=15
imgparanum1=14
imgparanum2=2
parameters=fltarr(paranumber)
position=intarr(2)
xrange=[3871,3999]
yrange=[3996,4124]
xedge=3871
yedge=3996
paraimg=fltarr(xrange[1]-xrange[0]+1,yrange[1]-yrange[0]+1)
paraimg[*,*]=0.0


print,xedge+(xrange[1]-xrange[0]+1)/2,yedge+(yrange[1]-yrange[0]+1)/2,xrange[1]-xrange[0]+1,yrange[1]-yrange[0]+1
print,numfiles

for i=0,numfiles-1 do begin
  filename=thesefiles[i]
  parameters=readpara(filename,paranumber)
  position[0]=fix(parameters[0])
  position[1]=fix(parameters[1])
  binnumber=fix(strmid(thesefiles[i],80,2))
  for binx=0,binnumber-1 do begin
  for biny=0,binnumber-1 do begin 
    xposi=fix((position[0]-xedge-binnumber/2)+binx)
    yposi=fix((position[1]-yedge-binnumber/2)+biny)
    ;print,xposi,yposi
    paraimg[xposi,yposi]=parameters[imgparanum1]/(binnumber^2);parameters[imgparanum2];parameters[imgparanum2]
    ;paraimg[xposi,yposi]=alog10(parameters[5])/alog10(parameters[8])
    ;if (parameters[5])^(-1.5)*4*10e9 lt parameters[8] then begin
    ; paraimg[xposi,yposi]=1
    ;endif
    ;if (parameters[5])^(-1.5)*4*10e9 ge parameters[8] then begin
    ;  paraimg[xposi,yposi]=2
    ;endif
    ;if parameters[1] lt 0.4 and parameters[2] lt 20 then begin
    ;  paraimg[xposi,yposi]=1
    ;endif
    ;if parameters[1] ge 0.4 and parameters[2] lt 20 then begin
    ;  paraimg[xposi,yposi]=2
    ;endif
    ;if parameters[1] lt 0.4 and parameters[2] ge 20 then begin
    ;  paraimg[xposi,yposi]=3
    ;endif
    ;if parameters[1] ge 0.4 and parameters[2] ge 20 then begin
    ;  paraimg[xposi,yposi]=4
    ;endif
  endfor
  endfor
endfor
;print,parameters
;print,position
;print,binnumber

window,1,retain=2
tvscl,paraimg

nouse=mrdfits(fileforhead,0,fitshead)
writeimgfits,paraimg,outputfits,fitshead,xrange[1]-xrange[0]+1,yrange[1]-yrange[0]+1,toolandtime
end
