function readpara,filename,paranumber
parameters=fltarr(3,paranumber)
str1="ljt"

parameters[0,0]=float(strmid(filename,60,4))
parameters[1,0]=float(strmid(filename,66,4))

openr,lun,filename,/get_lun
 for n1=0,9 do begin 
  readf,lun,str1
  parameters[1,1+n1]=float(strmid(str1,6,17))
  parameters[2,1+n1]=float(strmid(str1,24,17))
 endfor
   for n1=0,1 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[2,0]=float(strmid(str1,23,10))
   readf,lun,str1
;useless lines 
  readf,lun,str1
  parameters[0,1]=float(strmid(str1,48,13))
  readf,lun,str1
  parameters[0,2]=float(strmid(str1,48,13))
   for n1=0,3 do begin 
    readf,lun,str1
   endfor
;useless lines
 for n1=0,4 do begin
  readf,lun,str1
  parameters[0,3+n1]=float(strmid(str1,48,13))
 endfor
   for n1=0,1 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[0,8]=float(strmid(str1,48,13))
   for n1=0,1 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[0,9]=float(strmid(str1,48,13))
    readf,lun,str1
;useless lines
  readf,lun,str1
  parameters[0,10]=float(strmid(str1,48,13))
   for n1=0,1 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[0,11]=float(strmid(str1,17,13))
  readf,lun,str1
  parameters[1,11]=float(strmid(str1,23,15))
  parameters[2,11]=float(strmid(str1,46,6))
    readf,lun,str1
;useless lines
  readf,lun,str1
  parameters[0,12]=float(strmid(str1,33,12))
 for i=0,2 do begin
  readf,lun,str1
  parameters[i,13]=float(strmid(str1,33,12))
 endfor
 for i=0,2 do begin
  readf,lun,str1
  parameters[i,14]=float(strmid(str1,33,12))
 endfor

close,lun
free_lun,lun

return,parameters

end



pro fluxparaimage
;0:X; Y; counts(0.3-8keV)
;1:nH(error)
;2:T(error)
;3:O(error)
;4:Ne(error)
;5:Mg(error)
;6:Si(error)
;7:S(error)
;8:Fe(error)
;9:Tau(error)
;10:norm(error)
;11:Chi; Reduced Chi; d.o.f.
;12:total flux(0.3-8keV)
;13:flux-O(0.3-8keV); flux-Ne(0.3-8keV); flux-Mg(0.3-8keV)
;14:flux-Si(0.3-8keV); flux-S(0.3-8keV); flux-Fe(0.3-8keV)

centralposi=[4252.0,4158.0]
files='/home/ljt/ljt/data/AboutSNR/SNR/N23/2762/2Dspec/fitresult/*_SNR3'
thesefiles=findfile(files,count=numfiles)
outputfits='/home/ljt/ljt/data/AboutSNR/SNR/N23/2762/2Dspec/figures/sqrtnormdivbin2_img.fits'
toolandtime='TOOL  :newparaimage.pro-IDL,  2006-05-18   '
fileforhead='/home/ljt/ljt/data/AboutSNR/SNR/N23/2762/2Dspec/aidfile/source_bin1.fits'
paranumber=15
imgparanum1a=0
imgparanum1b=10
imgparanum2a=2
imgparanum2b=14
parametersread=fltarr(3,paranumber)
parameters=fltarr(3,paranumber)
position=intarr(2)
xrange=[4124,4380]
yrange=[4030,4286]
xedge=4124
yedge=4030
paraimg=fltarr(xrange[1]-xrange[0]+1,yrange[1]-yrange[0]+1)
paraimg[*,*]=0.0


print,xedge+(xrange[1]-xrange[0]+1)/2,yedge+(yrange[1]-yrange[0]+1)/2,xrange[1]-xrange[0]+1,yrange[1]-yrange[0]+1
print,numfiles

for i=0,numfiles-1 do begin
  filename=thesefiles[i]
  parametersread=readpara(filename,paranumber)
  parameters[*,0:12]=parametersread[*,0:12]
  parameters[0,13]=parametersread[0,12]-parametersread[0,13]
  parameters[1,13]=parametersread[0,13]-parametersread[1,13]
  parameters[2,13]=parametersread[1,13]-parametersread[2,13]
  parameters[0,14]=parametersread[2,13]-parametersread[0,14]
  parameters[1,14]=parametersread[0,14]-parametersread[1,14]
  parameters[2,14]=parametersread[1,14]-parametersread[2,14]
  position[0]=fix(parameters[0,0])
  position[1]=fix(parameters[1,0])
  binnumber=fix(strmid(filename,74,2))
;print,binnumber,parameters[0],parameters[1]
  for binx=0,binnumber-1 do begin
  for biny=0,binnumber-1 do begin 
    xposi=fix((position[0]-xedge-binnumber/2)+binx)
    yposi=fix((position[1]-yedge-binnumber/2)+biny)
    paraimg[xposi,yposi]=sqrt(parameters[imgparanum1a,imgparanum1b]/(binnumber^2));parameters[imgparanum2a,imgparanum2b]
    ;paraimg[xposi,yposi]=alog10(parameters[5])/alog10(parameters[8])
  endfor
  endfor
endfor
print,parameters
;print,position
print,binnumber

window,1,retain=2
tvscl,paraimg

nouse=mrdfits(fileforhead,0,fitshead)
writeimgfits,paraimg,outputfits,fitshead,xrange[1]-xrange[0]+1,yrange[1]-yrange[0]+1,toolandtime
end
