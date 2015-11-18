function readpara,filename,paranumber
parameters=fltarr(paranumber)
str1="ljt"

openr,lun,filename,/get_lun
   for n1=0,8 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[0]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[1]=float(strmid(str1,0,16))
   for n1=0,10 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[2]=float(strmid(str1,0,16))
   readf,lun,str1
   readf,lun,str1
;useless lines
  readf,lun,str1
  parameters[3]=float(strmid(str1,0,16))
   readf,lun,str1
;useless lines
  readf,lun,str1
  parameters[4]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[5]=float(strmid(str1,0,16))
   for n1=0,6 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[6]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[7]=float(strmid(str1,0,16))
   for n1=0,4 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[8]=float(strmid(str1,0,16))
   readf,lun,str1
;useless lines
  readf,lun,str1
  parameters[9]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[10]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[11]=float(strmid(str1,0,16))
close,lun
free_lun,lun

return,parameters

end



pro paraimagemodel2

files='/home/ljt/ljt/data/SNR/Kepler/spectra/fitresult/vpshockpower/'
thesefiles=findfile(files,count=numfiles)
paranumber=12
imgparanum=11
parameters=fltarr(paranumber)
position=intarr(2)
xrange=[3712,4448]
yrange=[3914,4522]
xedge=3686.5
yedge=3890.5
paraimg=fltarr(xrange[1]-xrange[0]+1,yrange[1]-yrange[0]+1)
paraimg[*,*]=0.0
outputfits='/home/ljt/ljt/data/SNR/Kepler/spectra/fitresult/image/TLVsFeLlabel_img.fits'
toolandtime='TOOL  :drawimage.pro-IDL,  2006-01-25   '
fileforhead='/home/ljt/ljt/data/SNR/Kepler/merge/merged_cali_imgregion_bin1.fits'

print,xedge+(xrange[1]-xrange[0]+1)/2,yedge+(yrange[1]-yrange[0]+1)/2,xrange[1]-xrange[0]+1,yrange[1]-yrange[0]+1
print,numfiles

for i=0,numfiles-1 do begin
  filename=files+thesefiles[i]
  parameters=readpara(filename,paranumber)
  position[0]=fix(strmid(thesefiles[i],2,4))
  position[1]=fix(strmid(thesefiles[i],8,4))
  binnumber=fix(strmid(thesefiles[i],16,2))
  for binx=0,binnumber-1 do begin
  for biny=0,binnumber-1 do begin 
    xposi=fix((position[0]-xedge-binnumber/2)+binx)
    yposi=fix((position[1]-yedge-binnumber/2)+biny)
    ;print,xposi,yposi
    ;paraimg[xposi,yposi]=parameters[imgparanum]
    ;paraimg[xposi,yposi]=alog10(parameters[5])/alog10(parameters[8])
    ;if (parameters[5])^(-1.5)*4*10e9 lt parameters[8] then begin
    ; paraimg[xposi,yposi]=1
    ;endif
    ;if (parameters[5])^(-1.5)*4*10e9 ge parameters[8] then begin
    ;  paraimg[xposi,yposi]=2
    ;endif
    if parameters[1] lt 0.4 and parameters[2] lt 20 then begin
      paraimg[xposi,yposi]=1
    endif
    if parameters[1] ge 0.4 and parameters[2] lt 20 then begin
      paraimg[xposi,yposi]=2
    endif
    if parameters[1] lt 0.4 and parameters[2] ge 20 then begin
      paraimg[xposi,yposi]=3
    endif
    if parameters[1] ge 0.4 and parameters[2] ge 20 then begin
      paraimg[xposi,yposi]=4
    endif
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
