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



pro parafiguremodel2

files='/home/ljt/ljt/data/SNR/Kepler/spectra/fitresult/vpshockpower/'
thesefiles=findfile(files,count=numfiles)
paranumber=12
position=intarr(2)
parafig=fltarr(paranumber,numfiles)
layernumber=75
pixelinonelayer=4
radialprafile=fltarr(paranumber+1,layernumber)
centralx=4080
;4055
;17h30m41s
;4080.
centraly=4230
;4237
;-21d29m23s
;4230.
;central position:(centralx,centraly),radius:layernumber*pixelinonelayer

print,numfiles

for i=0,numfiles-1 do begin  
  filename=files+thesefiles[i]
  parafig[*,i]=readpara(filename,paranumber)
  XCoord=float(strmid(thesefiles[i],2,4))
  YCoord=float(strmid(thesefiles[i],8,4))
  radiuslabel=fix(sqrt((XCoord-centralx)^2+(YCoord-centraly)^2)/float(pixelinonelayer))
  if parafig[0,i] le 1.1 and parafig[0,i] ge 0.3 then begin
  if parafig[1,i] le 1.0 and parafig[1,i] ge 0.1 then begin
  if parafig[2,i] le 500 and parafig[2,i] ge 1 then begin
  if parafig[3,i] le 5e13 and parafig[3,i] ge 1e10 then begin 
  if parafig[5,i] le 80 and parafig[5,i] ge 0.1 then begin 
  if parafig[6,i] le 1000 and parafig[6,i] ge 0 then begin 
  if parafig[7,i] le 1000 and parafig[7,i] ge 0 then begin
  if parafig[8,i] le 1e14 and parafig[8,i] ge 3e9 then begin
  if parafig[10,i] le 5 and parafig[10,i] ge 0.1 then begin 
  if parafig[11,i] le 1e-3 and parafig[11,i] ge 1e-7 then begin 
  for j=0,paranumber-1 do begin
    radialprafile[j,radiuslabel]=radialprafile[j,radiuslabel]+parafig[j,i]
    ;radialprafile[11,radiuslabel]=radialprafile[11,radiuslabel]/(1-radialprafile[10,radiuslabel])
    radialprafile[paranumber,radiuslabel]=radialprafile[paranumber,radiuslabel]+1
    ;this parameter is the number of points in one layer.
  endfor
  endif
  endif
  endif
  endif
  endif
  endif
  endif
  endif
  endif
  endif
endfor
;print,parameters
print,XCoord,YCoord,radiuslabel

for i=0,layernumber-1 do begin
if radialprafile[paranumber,i] gt 0 then begin
  radialprafile[0:paranumber-1,i]=radialprafile[0:paranumber-1,i]/radialprafile[paranumber,i]
endif
endfor
radialprafile[paranumber,*]=findgen(layernumber)*pixelinonelayer
;in units of pixel.
;radialprafile[paranumber,*]=findgen(layernumber)*pixelinonelayer*0.5
;in units of arcsec

for i=0,paranumber-1 do begin
maxvalue=max(radialprafile[i,*])
radialprafile[i,*]=radialprafile[i,*]/maxvalue
endfor

window,1,retain=2
plot,parafig[1,*],parafig[3,*],psym=3,/xlog,/ylog,xrange=[0.1,1],xtitle='Temperature(keV)(low temperature component)',ytitle='upper limit of ionization time scale';,yrange=[0,5e12]
plx=float(findgen(10))/10.+0.1
;print,plx
ply=plx^(-4)*10e9
oplot,plx,ply
;print,plx
;print,ply
;outputjpeg='/home/ljt/ljt/data/SNR/Kepler/spectra/fitresult/image/TLVsTauL.jpeg'
;arr=tvrd(true=1)
;write_jpeg,outputjpeg,arr,quality=100,true=1
;Save the image.

window,2,retain=2
plot,parafig[5,*],parafig[8,*],psym=3,/xlog,/ylog,xtitle='Temperature(keV)(high temperature component)',ytitle='upper limit of ionization time scale';,yrange=[0,5e12]
;,xrange=[0,2],yrange=[0,5e12]
plx=float(findgen(100))/1.+0.01
;print,plx
ply=plx^(-1.5)*4*10e9
oplot,plx,ply
;print,plx
;print,ply
;outputjpeg='/home/ljt/ljt/data/SNR/Kepler/spectra/fitresult/image/THVsTauH.jpeg'
;arr=tvrd(true=1)
;write_jpeg,outputjpeg,arr,quality=100,true=1
;Save the image.

window,3,retain=2
plot,parafig[6,*],parafig[7,*],psym=3,xrange=[1,1000],yrange=[1,1000],/xlog,/ylog,xtitle='Si abundance',ytitle='S abundance';,xrange=[0,1000],yrange=[0,1000]
;outputjpeg='/home/ljt/ljt/data/SNR/Kepler/spectra/fitresult/image/SiVsS.jpeg'
;arr=tvrd(true=1)
;write_jpeg,outputjpeg,arr,quality=100,true=1
;Save the image.

window,4,retain=2
plot,parafig[2,*],parafig[6,*],psym=3,xrange=[1,10000],yrange=[1,10000],/xlog,/ylog;,xrange=[0,1000],yrange=[0,1000]

window,5,retain=2
plot,parafig[1,*],parafig[2,*],psym=3,xrange=[0.1,1],yrange=[1,10000],/xlog,/ylog,xtitle='Temperature(keV)(low temperature component)',ytitle='Fe abundance';,xrange=[0,1000],yrange=[0,1000]
ply=fltarr(100)
plx=float(findgen(100))/100.+0.1
ply[*]=20.
oplot,plx,ply
ply=float(findgen(100))*100.+1.
plx[*]=0.4
oplot,plx,ply
outputjpeg='/home/ljt/ljt/data/SNR/Kepler/spectra/fitresult/image/TLVsFe.jpeg'
arr=tvrd(true=1)
write_jpeg,outputjpeg,arr,quality=100,true=1
;Save the image.

window,6,retain=2
;outputjpeg='/home/ljt/ljt/data/SNR/Kepler/spectra/fitresult/image/radiaprofile1.jpeg'
;plot,radialprafile[paranumber,*],radialprafile[0,*],xtitle='radius from the central point(in units of pixel)'
;oplot,radialprafile[paranumber,*],radialprafile[1,*],color='100000'
;oplot,radialprafile[paranumber,*],radialprafile[2,*],color='111111'
;oplot,radialprafile[paranumber,*],radialprafile[3,*],color='300000'
;plot,radialprafile[paranumber,*],radialprafile[5,*],xtitle='radius from the central point(in units of pixel)'
;oplot,radialprafile[paranumber,*],radialprafile[6,*],color='600000'
;oplot,radialprafile[paranumber,*],radialprafile[7,*],color='111111'
;oplot,radialprafile[paranumber,*],radialprafile[8,*],color='800000'
plot,radialprafile[paranumber,*],radialprafile[10,*],xtitle='radius from the central point(in units of pixel)'
oplot,radialprafile[paranumber,*],radialprafile[11,*],color='110011'
;arr=tvrd(true=1)
;write_jpeg,outputjpeg,arr,quality=100,true=1
;Save the image.

window,7,retain=2
plot,parafig[10,*],parafig[11,*],psym=3,/xlog,/ylog,xtitle='power law index',ytitle='power law norm',xrange=[0.001,10],yrange=[1e-8,1e-3]
;plot,radialprafile[10,*],radialprafile[11,*],psym=1,/xlog,/ylog,xrange=[0.01,1]
;outputjpeg='/home/ljt/ljt/data/SNR/Kepler/spectra/fitresult/image/PwIndexVsnorm.jpeg'
;arr=tvrd(true=1)
;write_jpeg,outputjpeg,arr,quality=100,true=1
;Save the image.
end
