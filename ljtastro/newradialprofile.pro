function writeps1,filename,loadctnumber,Xsizenumber,Ysizenumber,XOffsetnumber,YOffsetnumber
thisDevice=!D.Name
Set_Plot,'PS'
Device,Filename=filename,Xsize=Xsizenumber,Ysize=Ysizenumber,XOffset=XOffsetnumber,YOffset=YOffsetnumber,/Inches,/Encapsulated,/Preview
Device,Color=1,Bits_Per_Pixel=8
loadct,loadctnumber
return,thisDevice
end

pro writeps2,thisDevice
Device,/Close_File
Device,Encapsulated=0,Preview=0
Set_Plot,thisDevice
end


function readpara,filename,paranumber,number
Pi=3.1415926535
parameters=fltarr(paranumber,number)
str1="ljt"

openr,lun,filename,/get_lun

for i=0,number-1 do begin
  readf,lun,str1
  radiusl=float(strmid(str1,14,4))
  radiush=float(strmid(str1,20,4))
  anglel=float(strmid(str1,3,3))
  angleh=float(strmid(str1,8,3))
  parameters[0,i]=(radiusl+radiush)/2.
  parameters[1,i]=Pi*(radiush^2)*(angleh-anglel)/360.-Pi*(radiusl^2)*(angleh-anglel)/360.
  parameters[2,i]=float(strmid(str1,28,6))
  for j=3,9 do begin
   readf,lun,str1
   parameters[j,i]=float(strmid(str1,0,6))
  endfor
endfor

close,lun
free_lun,lun

return,parameters

end



pro newradialprofile
;0:radius
;1:region area
;2:total counts
;3:nonthermal:         4-6keV
;4:FeK:    left:       3.94-6.19keV
;5:        right:      6.7-7.6keV
;6:        line:       6.24-6.7keV
;7:OFeL:   left:       0.5-0.8keV
;8:        right:      1-1.15keV
;9:        line:       0.8-1keV
file='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/work/CRprecursor/radialregion/radialcts'
paranumber=10
number=19
data=fltarr(paranumber,number)
FeKEW=fltarr(number)
FeKcontinuum=fltarr(number)
FeLEW=fltarr(number)
FeLcontinuum=fltarr(number)
FeKleft=[3.94,6.19]
FeKright=[6.7,7.6]
FeKline=[6.24,6.7]
FeLleft=[0.5,0.8]
FeLright=[1.0,1.15]
FeLline=[0.8,1.0]



data=readpara(file,paranumber,number)

FeKcontinuum=(data[4,*]/(FeKleft[1]-FeKleft[0])-((FeKline[1]+FeKline[0])/2.-(FeKleft[1]+FeKleft[0])/2.)/((FeKright[1]+FeKright[0])/2.-(FeKleft[1]+FeKleft[0])/2.)$
*(data[4,*]/(FeKleft[1]-FeKleft[0])-data[5,*]/(FeKright[1]-FeKright[0])))
FeKEW[*]=(data[6,*]/(FeKline[1]-FeKline[0])-FeKcontinuum)/FeKcontinuum


FeLcontinuum=(data[7,*]/(FeLleft[1]-FeLleft[0])-((FeLline[1]+FeLline[0])/2.-(FeLleft[1]+FeLleft[0])/2.)/((FeLright[1]+FeLright[0])/2.-(FeLleft[1]+FeLleft[0])/2.)$
*(data[7,*]/(FeLleft[1]-FeLleft[0])-data[8,*]/(FeLright[1]-FeLright[0])))
FeLEW[*]=(data[9,*]/(FeLline[1]-FeLline[0])-FeLcontinuum)/FeLcontinuum



window,1,retain=2
;outputeps="/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/radialprofile/figures/THvsPwrId_total.eps"
label1=6
label2=9
radialnumberlow=16
radialnumberhigh=18
;plot,data[label1,radialnumberlow:radialnumberhigh]/data[1,radialnumberlow:radialnumberhigh],FeLEW[radialnumberlow:radialnumberhigh],psym=2,/ynozero;,xrange=[0,50]
plot,data[label1,radialnumberlow:radialnumberhigh],data[label2,radialnumberlow:radialnumberhigh]$
,psym=2,/ynozero,xrange=[0,30];,/xlog;,/ylog

;thisDevice=writeps1(outputeps,0,7,5,0,0)
;plot,parafig2[label1,0:k-1],parafig2[label2,0:k-1],psym=2,xtitle="T(keV)(High Temperature)",ytitle="Power law index",/xlog,/ylog
;writeps2,thisDevice


window,2,retain=2
plot,data[0,radialnumberlow:radialnumberhigh],FeKEW[radialnumberlow:radialnumberhigh],psym=2,yrange=[-1,5]
oplot,data[0,radialnumberlow:radialnumberhigh],FeLEW[radialnumberlow:radialnumberhigh],psym=1
end
