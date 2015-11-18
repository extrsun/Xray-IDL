pro encloseradius,radialprofile,number,ratio,type
;when type=0, negative part; when type=1, positive part.
;EXAMPLE:
;encloseradius,'300-700_minor_radialprofile_CNR8b',5,0.75,1

x=fltarr(number)
x1=fltarr(number)
y=fltarr(number)
err=fltarr(number)
openr,lun,radialprofile,/get_lun
readf,lun,x
readf,lun,y
readf,lun,err
close,lun
free_lun,lun

totalflux=mean(y)*number
ratioarr=fltarr(number)

enclosedrlabel=0

if type eq 0 then begin
for i=0,number-1 do begin
sum=0
for j=0,i do begin
sum=sum+y[number-1-j]
endfor
ratioarr[i]=sum/totalflux
x1[i]=abs(x[number-1-i])
if ratioarr[i] ge ratio then begin
if enclosedrlabel eq 0 then begin
enclosedrlabel=i
endif
endif
endfor
window,1
plot,x1,ratioarr,psym=-2
xrange=fltarr(2)
xrange[0]=min(x1)
xrange[1]=max(x1)
yrange=fltarr(2)
yrange[0]=ratio
yrange[1]=ratio
oplot,xrange,yrange
endif

if type eq 1 then begin
for i=0,number-1 do begin
sum=0
for j=0,i do begin
sum=sum+y[j]
endfor
ratioarr[i]=sum/totalflux
x1[i]=abs(x[i])
if ratioarr[i] ge ratio then begin
if enclosedrlabel eq 0 then begin
enclosedrlabel=i
endif
endif
endfor
window,1
plot,x1,ratioarr,psym=-2
xrange=fltarr(2)
xrange[0]=min(x1)
xrange[1]=max(x1)
yrange=fltarr(2)
yrange[0]=ratio
yrange[1]=ratio
oplot,xrange,yrange
endif

if enclosedrlabel eq 0 then begin
print,'enclosedrlabel=',enclosedrlabel,'         Please give more data points'
goto,NEXT
endif

closedr=(ratio-ratioarr[enclosedrlabel-1])*(x1[enclosedrlabel]-x1[enclosedrlabel-1])/(ratioarr[enclosedrlabel]-ratioarr[enclosedrlabel-1])+x1[enclosedrlabel-1]
print,'enclosedrlabel=',enclosedrlabel
print,ratio,'    enclosed radius is',closedr

NEXT:

end
