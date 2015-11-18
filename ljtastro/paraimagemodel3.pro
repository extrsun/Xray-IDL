function readpara,filename,paranumber
parameters=fltarr(paranumber)
str1="ljt"

openr,lun,filename,/get_lun
   for n1=0,11 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[0]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[1]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[2]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[3]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[4]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[5]=float(strmid(str1,0,16))
close,lun
free_lun,lun

return,parameters

end



pro paraimagemodel3

files='/home/ljt/ljt/data/SNR/Kepler/spectra/fitresult/power2gauss/'
thesefiles=findfile(files,count=numfiles)
paranumber=6
imgparanum=5
parameters=fltarr(paranumber)
position=intarr(2)
xrange=[3712,4448]
yrange=[3914,4522]
xedge=3686.5
yedge=3890.5
paraimg=fltarr(xrange[1]-xrange[0]+1,yrange[1]-yrange[0]+1)
paraimg[*,*]=0.0
outputfits='/home/ljt/ljt/data/SNR/Kepler/spectra/fitresult/image/Snorm_img.fits'
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
    paraimg[xposi,yposi]=parameters[imgparanum]
  endfor
  endfor
endfor
print,parameters
;print,position
;print,binnumber

window,1,retain=2
tvscl,paraimg

nouse=mrdfits(fileforhead,0,fitshead)
writeimgfits,paraimg,outputfits,fitshead,xrange[1]-xrange[0]+1,yrange[1]-yrange[0]+1,toolandtime
end
