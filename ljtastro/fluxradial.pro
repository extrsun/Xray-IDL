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
parameters[*,*]=0.
str1="ljt"

parameters[0,0]=float(strmid(filename,80,4))
parameters[0,1]=float(strmid(filename,86,4))
parameters[0,2]=float(strmid(filename,94,6))

openr,lun,filename,/get_lun
  readf,lun,str1
  parameters[0,3]=float(strmid(str1,48,13))
  readf,lun,str1
  parameters[0,4]=float(strmid(str1,48,13))
   for n1=0,3 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[0,5]=float(strmid(str1,48,13))
   readf,lun,str1
;useless lines 
  readf,lun,str1
  parameters[0,6]=float(strmid(str1,48,13))
  readf,lun,str1
  parameters[0,7]=float(strmid(str1,48,13))
  readf,lun,str1
  parameters[0,8]=float(strmid(str1,48,13))
   for n1=0,1 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[0,9]=float(strmid(str1,48,13))
   for n1=0,1 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[0,10]=float(strmid(str1,48,13))
    readf,lun,str1
;useless lines  
  readf,lun,str1
  parameters[0,11]=float(strmid(str1,48,13))
   for n1=0,1 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[0,12]=float(strmid(str1,17,13))
  readf,lun,str1
  parameters[0,13]=float(strmid(str1,23,15))
  parameters[0,14]=float(strmid(str1,46,6))
   for n1=0,2 do begin 
    readf,lun,str1
   endfor
;useless lines
 for i=0,8 do begin
  readf,lun,str1
  parameters[1,3+i]=float(strmid(str1,43,16))
  parameters[2,3+i]=float(strmid(str1,61,16))
 endfor
 for i=0,6 do begin
  readf,lun,str1
  parameters[0,15+i]=float(strmid(str1,33,11))
 endfor

close,lun
free_lun,lun

return,parameters

end




pro fluxradial
;0:r low
;1:r high
;2:counts
;3:nH,err low,err high
;4:T,err low,err high
;5:O,err low,err high
;6:Mg,err low,err high
;7:Si,err low,err high
;8:S,err low,err high
;9:Fe,err low,err high
;10:Tau,err low,err high
;11:norm,err low,err high
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
files='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/radial/fitresult/*.txt'
thesefiles=findfile(files,count=numfiles)
paranumber=22
parametersread=fltarr(3,paranumber)
parameters=fltarr(3,paranumber)

parafig=fltarr(3,paranumber,numfiles)

print,numfiles

for i=0,numfiles-1 do begin
  filename=thesefiles[i]
  parametersread=readpara(filename,paranumber)
  parameters=parametersread
  parameters[0,16]=parametersread[0,15]-parametersread[0,16]
  parameters[0,17]=parametersread[0,16]-parametersread[0,17]
  parameters[0,18]=parametersread[0,17]-parametersread[0,18]
  parameters[0,19]=parametersread[0,18]-parametersread[0,19]
  parameters[0,20]=parametersread[0,19]-parametersread[0,20]
  parameters[0,21]=parametersread[0,21]
parafig[*,*,i]=parameters[*,*]
endfor

print,parameters


window,1,retain=2
outputjpg="/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec/figures/countsvstotalflux.jpg"
label1=7
label2=8
pointsize=1
;device,decomposed=0

plot,(parafig[0,0,*]+parafig[0,1,*])/4.,parafig[0,label1,*]/parafig[0,label2,*],psym=7,SYMSIZE=pointsize,position=[0.15,0.1,0.96,0.96],charsize=1.5$
,/ylog,xtitle="radius/arcsec",ytitle="Si abundance/S abundance",yrange=[0.1,10];,xrange=[1,1e4]
low=(parafig[0,label1,*]+parafig[1,label1,*])/(parafig[0,label2,*]-parafig[1,label2,*])
high=(parafig[0,label1,*]-parafig[1,label1,*])/(parafig[0,label2,*]+parafig[1,label2,*])
;low[7]=0.1
;low[9]=0.1
;low[10]=0.1
ERRPLOT,(parafig[0,0,*]+parafig[0,1,*])/4.,low,high
errplotx,parafig[0,label1,*]/parafig[0,label2,*],(parafig[0,0,*]+parafig[0,1,*])/4.+(parafig[0,0,*]-parafig[0,1,*])/4.,(parafig[0,0,*]+parafig[0,1,*])/4.-(parafig[0,0,*]-parafig[0,1,*])/4.,numfiles,0

;"n!de!nt/cm!u-3!ns"
;"nH/10!u22!n"
;device,decomposed=1

;imgarr=tvrd(true=1)
;write_jpeg,outputjpg,imgarr,quality=100,true=1

;file1='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/radial/figures/SidivS_radialprofile.eps'
;thisDevice=writeps1(file1,0,7,6,0,0)
;plot,(parafig[0,0,*]+parafig[0,1,*])/4.,parafig[0,label1,*]/parafig[0,label2,*],psym=7,SYMSIZE=pointsize,position=[0.15,0.1,0.96,0.96],charsize=1.5$
;,/ylog,xtitle="radius/arcsec",ytitle="Si abundance/S abundance",yrange=[0.1,10];,xrange=[1,1e4]
;low=(parafig[0,label1,*]+parafig[1,label1,*])/(parafig[0,label2,*]-parafig[1,label2,*])
;high=(parafig[0,label1,*]-parafig[1,label1,*])/(parafig[0,label2,*]+parafig[1,label2,*])
;low[7]=0.1
;low[9]=0.1
;low[10]=0.1
;ERRPLOT,(parafig[0,0,*]+parafig[0,1,*])/4.,low,high
;errplotx,parafig[0,label1,*]/parafig[0,label2,*],(parafig[0,0,*]+parafig[0,1,*])/4.+(parafig[0,0,*]-parafig[0,1,*])/4.,(parafig[0,0,*]+parafig[0,1,*])/4.-(parafig[0,0,*]-parafig[0,1,*])/4.,numfiles,0
;writeps2,thisDevice



end
