pro tomographyindex

files='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/work/tomography/bin8_4-4.5_4.5-6_index-4.4--2/tom12/Tomography*tom12.fits'
thesefiles=findfile(files,count=numfiles)
outputeps='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/work/tomography/bin8_4-4.5_4.5-6_index-4.4--2/minindex_tom12.eps'

data=fltarr(2,numfiles)
beginnumber=114

;help,imagedata
;print,float(strmid(thesefiles[0],beginnumber,5))
;print,total(imagedata)
print,numfiles

minvalue=1000.
minindex=0.

for i=0,numfiles-1 do begin
 imagedata=mrdfits(thesefiles[i],0,fitsheadl)
 data[0,i]=-float(strmid(thesefiles[i],beginnumber,5))
 data[1,i]=total(imagedata)
 if abs(data[1,i]) lt minvalue then begin
   minvalue=abs(data[1,i])
   minindex=data[0,i]
 endif
endfor

print,'minvalue=',minvalue,' when index=',minindex
;print,data

window,1,retain=2
plot,data[0,*],abs(data[1,*]),psym=2,xtitle='Spectral Index',ytitle='Absolute value of total counts in the image'
oplot,data[0,*],abs(data[1,*]),linestyle=1

;thisDevice=writeps1(outputeps,0,7,5,0,0)
;plot,data[0,*],abs(data[1,*]),psym=2,xtitle='Spectral Index',ytitle='Absolute value of total counts in the image'
;oplot,data[0,*],abs(data[1,*]),linestyle=1
;writeps2,thisDevice



window,2,retain=2
dataarray=[[3.52,903,76030],[3.75,2027,611774],[4.08,1556,629504],[3.48,823,108601],[3.23,652,40265],[3.78,1089,47095],[3.15,654,23579],$
[3.73,1459,348155],[2.93,451,201032],[2.93,162,9048],[3.68,203,93469],[0.68,135,10095]]

plot,dataarray[0,*],dataarray[1,*]/dataarray[2,*],psym=2

end
