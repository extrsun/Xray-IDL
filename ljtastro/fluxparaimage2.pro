pro fluxparaimage2
;0:X
;1:Y
;2:counts
;3:total flux(0.3-10keV)
;4:power law flux(0.3-10keV)

centralposi=[3935.,4060.]
file='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec/nonthermalflux'
outputfits='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec/figures/Pwrfluxdivtotalflux_Subbad_img.fits'
toolandtime='TOOL  :newparaimage.pro-IDL,  2006-04-14   '
fileforhead='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec/aidfile/acisf00118_source_bin1.fits'
numfiles=147
paranumber=5
imgparanum1=4
imgparanum2=3
parametersread=fltarr(paranumber)
parameters=fltarr(paranumber)
position=intarr(2)
xrange=[3871,3999]
yrange=[3996,4124]
xedge=3871
yedge=3996
paraimg=fltarr(xrange[1]-xrange[0]+1,yrange[1]-yrange[0]+1)
paraimg[*,*]=0.0

k=0
openr,lun,file,/get_lun
str1='ljt'
for i=0,numfiles-1 do begin
  readf,lun,str1
  parametersread[0]=float(strmid(str1,2,4))
  parametersread[1]=float(strmid(str1,8,4))
 binnumber=fix(strmid(str1,16,2))
  parametersread[2]=float(strmid(str1,22,5))
  readf,lun,str1
  parametersread[3]=float(strmid(str1,20,10))
  readf,lun,str1
  parametersread[4]=float(strmid(str1,20,10))
  readf,lun,str1
  parameters[0:3]=parametersread[0:3]
  parameters[4]=parametersread[3]-parametersread[4]
  position[0]=fix(parameters[0])
  position[1]=fix(parameters[1])
 
if parameters[3] le parameters[2]*1e-16 then begin
  for binx=0,binnumber-1 do begin
  for biny=0,binnumber-1 do begin 
    xposi=fix((position[0]-xedge-binnumber/2)+binx)
    yposi=fix((position[1]-yedge-binnumber/2)+biny)
    paraimg[xposi,yposi]=parameters[imgparanum1]/parameters[imgparanum2];/(binnumber^2);
    ;paraimg[xposi,yposi]=alog10(parameters[5])/alog10(parameters[8])
  endfor
  endfor
  k=k+1
endif

endfor
close,lun
free_lun,lun

print,'used data number=',k
;print,parameters
;print,position
;print,binnumber

window,1,retain=2
tvscl,paraimg

nouse=mrdfits(fileforhead,0,fitshead)
writeimgfits,paraimg,outputfits,fitshead,xrange[1]-xrange[0]+1,yrange[1]-yrange[0]+1,toolandtime
end
