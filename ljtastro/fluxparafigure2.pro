function arrangement,array,number,ndimen,direction

if direction eq 0 then begin
for j=0,number-1 do begin
label=j
minvalue=array[ndimen,j]
for i=j,number-1 do begin
if array[ndimen,i] lt minvalue then begin
 minvalue=array[ndimen,i]
 label=i
endif
endfor
forexchange=array[*,j]
array[*,j]=array[*,label]
array[*,label]=forexchange
endfor
endif

if direction eq 1 then begin
for j=0,number-1 do begin
label=j
maxvalue=array[ndimen,j]
for i=j,number-1 do begin
if array[ndimen,i] gt maxvalue then begin
 maxvalue=array[ndimen,i]
 label=i
endif
endfor
forexchange=array[*,j]
array[*,j]=array[*,label]
array[*,label]=forexchange
endfor
endif

return,array

end



function readdata,filename

thesefiles=findfile(filename,count=numfiles)

data=fltarr(2,numfiles)

str1='ljt'

for i=0,numfiles-1 do begin
 openr,lun,thesefiles[i],/get_lun
  for k=0,9 do begin
    readf,lun,str1
  endfor
  readf,lun,str1
  data[0,i]=float(strmid(str1,0,15))
  for k=0,13 do begin
    readf,lun,str1
  endfor
  readf,lun,str1  
  data[1,i]=float(strmid(str1,0,15))
 close,lun
 free_lun,lun
endfor

return,{data:data,number:numfiles}

end



function readpara,filename,paranumber
parameters=fltarr(paranumber)
str1="ljt"

parameters[0]=float(strmid(filename,68,4))
parameters[1]=float(strmid(filename,74,4))
parameters[2]=float(strmid(filename,88,5))

openr,lun,filename,/get_lun
  readf,lun,str1
  parameters[3]=float(strmid(str1,48,13))
  readf,lun,str1
  parameters[4]=float(strmid(str1,48,13))
   for n1=0,3 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[5]=float(strmid(str1,48,13))
   readf,lun,str1
;useless lines 
  readf,lun,str1
  parameters[6]=float(strmid(str1,48,13))
  readf,lun,str1
  parameters[7]=float(strmid(str1,48,13))
  readf,lun,str1
  parameters[8]=float(strmid(str1,48,13))
   for n1=0,1 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[9]=float(strmid(str1,48,13))
   for n1=0,1 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[10]=float(strmid(str1,48,13))
    readf,lun,str1
;useless lines  
  readf,lun,str1
  parameters[11]=float(strmid(str1,48,13))
   for n1=0,1 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[12]=float(strmid(str1,17,13))
  readf,lun,str1
  parameters[13]=float(strmid(str1,23,15))
  parameters[14]=float(strmid(str1,46,6))
   for n1=0,1 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[15]=float(strmid(str1,33,11))

  for i=0,4 do begin
   for j=0,5 do begin
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[16+i]=float(strmid(str1,33,11))
  endfor

   for n1=0,30 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[21]=float(strmid(str1,33,11))

close,lun
free_lun,lun

return,parameters

end




pro fluxparafigure2
;0:X
;1:Y
;2:counts
;3:nH
;4:T
;5:O
;6:Mg
;7:Si
;8:S
;9:Fe
;10:Tau
;11:norm
;12:Chi
;13:Reduced Chi
;14:d.o.f.
;15:total flux(0.4-2.68keV)
;16:flux-O(0.4-2.68keV)
;17:flux-Mg(0.4-2.68keV)
;18:flux-Si(0.4-2.68keV)
;19:flux-S(0.4-2.68keV)
;20:flux-Fe(0.4-2.68keV)
;21:continuum(0.4-2.68keV)



centralposi=[3935.,4060.]
files='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec/fitresult/*_SNR3_0.4-2.68keV.xcm'
thesefiles=findfile(files,count=numfiles)
paranumber=22
parametersread=fltarr(paranumber)
parameters=fltarr(paranumber)
position=intarr(2)
xrange=[3871,3999]
yrange=[3996,4124]
xedge=3871
yedge=3996
parafig=fltarr(paranumber,numfiles)

print,numfiles

for i=0,numfiles-1 do begin
  filename=thesefiles[i]
  parametersread=readpara(filename,paranumber)
  parameters[0:15]=parametersread[0:15]
  parameters[16]=parametersread[15]-parametersread[16]
  parameters[17]=parametersread[16]-parametersread[17]
  parameters[18]=parametersread[17]-parametersread[18]
  parameters[19]=parametersread[18]-parametersread[19]
  parameters[20]=parametersread[19]-parametersread[20]
  parameters[21]=parametersread[21]
  position[0]=fix(parameters[0])
  position[1]=fix(parameters[1])
parafig[0:21,i]=parameters[0:21]
endfor




window,1,retain=2
outputjpg="/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec/figures/countsvstotalflux.jpg"
label1=2
label2=15
pointsize=0.5
device,decomposed=0
plot,parafig[label1,*],parafig[label2,*],psym=7,SYMSIZE=pointsize,color=0B,background=255B,position=[0.15,0.1,0.96,0.96],charsize=1$
,/xlog,/ylog,xtitle="counts",ytitle="total flux(0.3-10keV)";,yrange=[0,10];,xrange=[1,1e4]
device,decomposed=1

AA=1e-16
kk=1
x=10^(findgen(100)*0.03+1.)
y=AA*x^(kk)
oplot,x,y,color=255B

;imgarr=tvrd(true=1)
;write_jpeg,outputjpg,imgarr,quality=100,true=1

if 1 then begin
files1='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec1/figures/TvsTau/Ok/Ok*.xcm'
files2='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec1/figures/TvsTau/Ne/Ne*.xcm'
files3='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec1/figures/TvsTau/Mg/Mg*.xcm'
files4='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec1/figures/TvsTau/Si/Si*.xcm'
files5='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec1/figures/TvsTau/Sk/Sk*.xcm'
files6='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec1/figures/TvsTau/Fe/Fe*.xcm'

data0=readdata(files1)
data1b=arrangement(data0.data,data0.number,0,0)
data1=data1b[*,6:42]
data0=readdata(files2)
data2b=arrangement(data0.data,data0.number,0,0)
data2=data2b[*,6:42]
data0=readdata(files3)
data3b=arrangement(data0.data,data0.number,0,0)
data3=data3b[*,6:42]
data0=readdata(files4)
data4b=arrangement(data0.data,data0.number,0,0)
data4=data4b[*,6:42]
data0=readdata(files5)
data5b=arrangement(data0.data,data0.number,0,0)
data5=data5b[*,6:42]
data0=readdata(files6)
data6b=arrangement(data0.data,data0.number,0,0)
data6=data6b[*,6:42]
endif

window,2,retain=2,xsize=1200,ysize=900
outputjpg="/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec/figures/TvsTau/TvsTau.jpg"
label1=4
label2=10
pointsize=1.5
device,decomposed=0
plot,parafig[label1,*],parafig[label2,*],psym=7,SYMSIZE=pointsize,color=0B,background=255B,thick=2,position=[0.15,0.1,0.96,0.96],charsize=2$
,/xlog,/ylog,xtitle="T/keV",ytitle="n!de!nt/cm!u-3!ns";,yrange=[1,1e4],xrange=[1,1e4]
device,decomposed=1

if 1 then begin
oplot,data1[0,*],data1[1,*],psym=2,SYMSIZE=pointsize2,thick=2,color='111111'
oplot,data1[0,*],data1[1,*],psym=0,linestyle=2,thick=2,color='111111'
oplot,[5,7],[7e13,7e13],psym=-2,SYMSIZE=pointsize2,linestyle=2,thick=2,color='111111'
XYOutS,0.7,0.929,'O 0.30-1.60keV band',size=charactersize1,/normal,color='111111'

oplot,data2[0,*],data2[1,*],psym=4,SYMSIZE=pointsize2,thick=2,color=255
oplot,data2[0,*],data2[1,*],psym=0,linestyle=2,thick=2,color=255
oplot,[5,7],[5e13,5e13],psym=-4,SYMSIZE=pointsize2,linestyle=2,thick=2,color=255
XYOutS,0.7,0.904,'Ne 0.30-1.60keV band',size=charactersize1,/normal,color=255

oplot,data3[0,*],data3[1,*],psym=5,SYMSIZE=pointsize2,thick=2,color='300000'
oplot,data3[0,*],data3[1,*],psym=0,linestyle=0,thick=1,color='300000'
oplot,[5,7],[3.5e13,3.5e13],psym=-5,SYMSIZE=pointsize2,linestyle=0,thick=2,color='300000'
XYOutS,0.7,0.879,'Mg 0.30-1.60keV band',size=charactersize1,/normal,color='300000'

oplot,data4[0,*],data4[1,*],psym=6,SYMSIZE=pointsize2,thick=2,color='050000'
oplot,data4[0,*],data4[1,*],psym=0,linestyle=0,thick=1,color='050000'
oplot,[5,7],[2.5e13,2.5e13],psym=-6,SYMSIZE=pointsize2,linestyle=0,thick=2,color='050000'
XYOutS,0.7,0.854,'Si 1.30-3.00keV band',size=charactersize1,/normal,color='050000'

oplot,data5[0,*],data5[1,*],psym=7,SYMSIZE=pointsize2,thick=2,color='080805'
oplot,data5[0,*],data5[1,*],psym=0,linestyle=3,thick=2,color='080805'
oplot,[5,7],[1.78e13,1.78e13],psym=-7,SYMSIZE=pointsize2,linestyle=3,thick=2,color='080805'
XYOutS,0.7,0.829,'S 1.30-3.00keV band',size=charactersize1,/normal,color='080805'

oplot,data6[0,*],data6[1,*],psym=2,SYMSIZE=pointsize2,thick=2,color=230
oplot,data6[0,*],data6[1,*],psym=0,linestyle=3,thick=2,color=230
oplot,[5,7],[1.3e13,1.3e13],psym=-2,SYMSIZE=pointsize2,linestyle=3,thick=2,color=230
XYOutS,0.7,0.804,'Fe 0.30-2.00keV band',size=charactersize1,/normal,color=230
endif

;imgarr=tvrd(true=1)
;write_jpeg,outputjpg,imgarr,quality=100,true=1



end
