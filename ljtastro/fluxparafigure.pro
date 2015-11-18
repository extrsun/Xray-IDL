pro errplotx,y,low,high,number,OPL,colorvalue
;when OPL=1 we plot the x errbar first, and use the procedure plot.
;when OPL=0 we do not plot the x errbar first, and use the procedure oplot.
;you'd better use OPL=0.
;"number" is the number of the data point.
;"low" and "high" are the lower and higher edge of the data points.
;"y" is the y value of the data points.
;colorvalue: you can choose to give this value or not.
x1=fltarr(2,number)
x1[0,*]=low
x1[1,*]=high
y1=fltarr(2,number)
y1[0,*]=y
y1[1,*]=y
x2=fltarr(2)
y2=fltarr(2)
width=(x1[1,0]-x1[0,0])/50.

if OPL eq 1 then begin
for i=0,number-1 do begin
plot,x1[*,i],y1[*,i],psym=0,color=colorvalue
x2[*]=x1[0,i]
y2[0]=y1[0,i]+width
y2[1]=y1[0,i]-width
oplot,x2,y2,psym=0,color=colorvalue
x2[*]=x1[1,i]
y2[0]=y1[0,i]+width
y2[1]=y1[0,i]-width
oplot,x2,y2,psym=0,color=colorvalue
endfor
endif

if OPL eq 0 then begin
for i=0,number-1 do begin
oplot,x1[*,i],y1[*,i],psym=0,color=colorvalue
x2[*]=x1[0,i]
y2[0]=y1[0,i]+width
y2[1]=y1[0,i]-width
oplot,x2,y2,psym=0,color=colorvalue
x2[*]=x1[1,i]
y2[0]=y1[0,i]+width
y2[1]=y1[0,i]-width
oplot,x2,y2,psym=0,color=colorvalue
endfor
endif

end



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



pro fluxparafigure
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
paranumber=15
parametersread=fltarr(3,paranumber)
parameters=fltarr(numfiles,3,paranumber)
position=intarr(2)
xrange=[4124,4380]
yrange=[4030,4286]
xedge=4124
yedge=4030


print,xedge+(xrange[1]-xrange[0]+1)/2,yedge+(yrange[1]-yrange[0]+1)/2,xrange[1]-xrange[0]+1,yrange[1]-yrange[0]+1
print,numfiles

for i=0,numfiles-1 do begin
  filename=thesefiles[i]
  parametersread=readpara(filename,paranumber)
  parameters[i,*,0:12]=parametersread[*,0:12]
  parameters[i,0,13]=parametersread[0,12]-parametersread[0,13]
  parameters[i,1,13]=parametersread[0,13]-parametersread[1,13]
  parameters[i,2,13]=parametersread[1,13]-parametersread[2,13]
  parameters[i,0,14]=parametersread[2,13]-parametersread[0,14]
  parameters[i,1,14]=parametersread[0,14]-parametersread[1,14]
  parameters[i,2,14]=parametersread[1,14]-parametersread[2,14]
  position[0]=fix(parameters[i,0,0])
  position[1]=fix(parameters[i,1,0])
  binnumber=fix(strmid(filename,74,2))
endfor

parafig=parameters

window,1,retain=2
outputjpg="/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec/figures/countsvstotalflux.jpg"
label1a=0
label1b=2
label2a=0
label2b=1
pointsize=1
device,decomposed=0
plot,parafig[*,label1a,label1b],parafig[*,label2a,label2b],psym=7,SYMSIZE=pointsize,color=0B,background=255B,position=[0.15,0.1,0.96,0.96],charsize=1$
,/xlog,/ylog,xtitle="counts",ytitle="total flux(0.3-10keV)",yrange=[1e-4,1e0];,xrange=[1,1e4]
;ERRPLOT,parafig[*,label1a,label1b],parafig[*,1,label2b],parafig[*,2,label2b],color=0B
;errplotx,parafig[*,label2a,label2b],parafig[*,1,label1b],parafig[*,2,label1b],numfiles,0,0B

device,decomposed=1

AA=1e-16
kk=1
x=10^(findgen(100)*0.03+1.)
y=AA*x^(kk)
oplot,x,y,color=255B

;imgarr=tvrd(true=1)
;write_jpeg,outputjpg,imgarr,quality=100,true=1


if 0 then begin

files1='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec/figures/TvsTau/Ok/Ok*.xcm'
files2='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec/figures/TvsTau/Ne/Ne*.xcm'
files3='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec/figures/TvsTau/Mg/Mg*.xcm'
files4='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec/figures/TvsTau/Si/Si*.xcm'
files5='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec/figures/TvsTau/Sk/Sk*.xcm'
files6='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec/figures/TvsTau/Fe/Fe*.xcm'

data0=readdata(files1)
data1=arrangement(data0.data,data0.number,0,0)
data0=readdata(files2)
data2=arrangement(data0.data,data0.number,0,0)
data0=readdata(files3)
data3=arrangement(data0.data,data0.number,0,0)
data0=readdata(files4)
data4=arrangement(data0.data,data0.number,0,0)
data0=readdata(files5)
data5=arrangement(data0.data,data0.number,0,0)
data0=readdata(files6)
data6=arrangement(data0.data,data0.number,0,0)

window,2,retain=2,xsize=1200,ysize=900
outputjpg="/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec/figures/TvsTau/TvsTau.jpg"
label1=4
label2=11
pointsize=1.5
device,decomposed=0
plot,parafig[label1,*],parafig[label2,*],psym=7,SYMSIZE=pointsize,color=0B,background=255B,thick=2,position=[0.15,0.1,0.96,0.96],charsize=2$
,/xlog,/ylog,xtitle="T/keV",ytitle="n!de!nt/cm!u-3!ns";,yrange=[1,1e4],xrange=[1,1e4]
device,decomposed=1


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


;imgarr=tvrd(true=1)
;write_jpeg,outputjpg,imgarr,quality=100,true=1

endif

end
