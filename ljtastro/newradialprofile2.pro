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
  for j=3,paranumber-1 do begin
   readf,lun,str1
   parameters[j,i]=float(strmid(str1,0,6))
  endfor
endfor

close,lun
free_lun,lun

return,parameters

end


function calcEW,parameters

returnpara=fltarr(3)
left=fltarr(2)
right=fltarr(2)
line=fltarr(2)
left[0:1]=parameters[0:1]
right[0:1]=parameters[2:3]
line[0:1]=parameters[4:5]
CtsL=parameters[6]
CtsR=parameters[7]
Ctsline=parameters[8]

rightposition=(right[1]+right[0])/2.
leftposition=(left[1]+left[0])/2.
lineposition=(line[1]+line[0])/2.

rightCts=CtsR/(right[1]-right[0])
leftCts=CtsL/(left[1]-left[0])
lineCts=Ctsline/(line[1]-line[0])

continuumvalue=(rightposition-lineposition)*(leftCts-rightCts)/(rightposition-leftposition)+rightCts
lineintensity=(lineCts-continuumvalue)*(line[1]-line[0])
EWvalue=lineintensity/(continuumvalue*(line[1]-line[0]))

;continuumvalue=(CtsL/(left[1]-left[0])-((line[1]+line[0])/2.-(left[1]+left[0])/2.)/((right[1]+right[0])/2.-(left[1]+left[0])/2.)$
;*(CtsL/(left[1]-left[0])-CtsR/(right[1]-right[0])))
;lineintensity=Ctsline/(line[1]-line[0])-continuumvalue
;EWvalue=lineintensity/continuumvalue

returnpara[0]=continuumvalue
returnpara[1]=lineintensity
returnpara[2]=EWvalue

return,returnpara

end





pro newradialprofile2
;0:radius
;1:region area
;2:total counts
;3:FeL:    left:       0.5-0.8keV
;4:        right:      1-1.15keV
;5:        line:       0.8-1keV
;6:SiK:    left:       1410-1680keV
;7:        right:      1950-2100keV
;8:        line:       1710-1950keV

;0:radius
;1:region area
;2:total counts
;3:FeL:    continuum
;4:        line
;5:        EW
;6:SiK:    continuum
;7:        line
;8:        EW

file='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/work/SiKandFeL/radial/counts_regions18'
outputeps1="/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/work/SiKandFeL/radial/regions18_FeLVsSiK.eps"
outputeps2="/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/work/SiKandFeL/radial/regions18_FeLradial.eps"
outputeps3="/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/work/SiKandFeL/radial/regions18_SiKradial.eps"
paranumber=9
number=77
;for,counts_regions1: 67(40-140)
;for,counts_regions2: 31(10-40)
;for,counts_regions3: 70(10-40)
;for,counts_regions4: 56(40-140)
;for,counts_regions5: 35(40-140)
;for,counts_regions6: 60(0-30)
;for,counts_regions7: 62(30-60)
;for,counts_regions8: 66(60-90)
;for,counts_regions9: 55(90-120)
;for,counts_regions10: 41(120-150)
;for,counts_regions11: 25(150-180)
;for,counts_regions12: 13(180-210)
;for,counts_regions13: 20(210-240)
;for,counts_regions14: 13(240-270)
;for,counts_regions15: 16(270-300)
;for,counts_regions16: 26(300-330)
;for,counts_regions17: 39(300-330)
;for,counts_regions18: 77(0-50)
radialnumberlow=0
radialnumberhigh=number-1
data=fltarr(paranumber,number)
FeL=fltarr(3,number)
SiK=fltarr(3,number)
FeLleft=[0.5,0.8]
FeLright=[1.0,1.15]
FeLline=[0.8,1.0]
SiKleft=[1.41,1.68]
SiKright=[1.95,2.1]
SiKline=[1.71,1.95]

data=readpara(file,paranumber,number)

;print,data[*,0]

parameters=fltarr(9)

for i=0,number-1 do begin

 parameters[0:1]=FeLleft[0:1]
 parameters[2:3]=FeLright[0:1]
 parameters[4:5]=FeLline[0:1]
 parameters[6:8]=data[3:5,i]

 FeL[*,i]=calcEW(parameters)

 parameters[0:1]=SiKleft[0:1]
 parameters[2:3]=SiKright[0:1]
 parameters[4:5]=SiKline[0:1]
 parameters[6:8]=data[6:8,i]

 SiK[*,i]=calcEW(parameters)

endfor

;print,FeL[*,0]
;print,SiK[*,0]

data[3:5,*]=FeL[0:2,*]
data[6:8,*]=SiK[0:2,*]




window,1,retain=2
label1=5
label2=8
plot,data[label1,radialnumberlow:radialnumberhigh],data[label2,radialnumberlow:radialnumberhigh],psym=2,/ynozero,xtitle="FeL equivalent width",ytitle="SiK equivalent width"
;oplot,data[label1,radialnumberlow:radialnumberhigh],data[label2,radialnumberlow:radialnumberhigh],linestyle=1

;thisDevice=writeps1(outputeps1,0,7,5,0,0)
;plot,data[label1,radialnumberlow:radialnumberhigh],data[label2,radialnumberlow:radialnumberhigh],psym=2,/ynozero,xtitle="FeL equivalent width",ytitle="SiK equivalent width"
;writeps2,thisDevice


window,2,retain=2
plot,data[0,radialnumberlow:radialnumberhigh],data[5,radialnumberlow:radialnumberhigh],psym=2,/ynozero,xtitle="radius (pixel)",ytitle="FeL equivalent width"
oplot,data[0,radialnumberlow:radialnumberhigh],data[5,radialnumberlow:radialnumberhigh],linestyle=1

;thisDevice=writeps1(outputeps2,0,7,5,0,0)
;plot,data[0,radialnumberlow:radialnumberhigh],data[5,radialnumberlow:radialnumberhigh],psym=2,/ynozero,xtitle="radius (pixel)",ytitle="FeL equivalent width"
;oplot,data[0,radialnumberlow:radialnumberhigh],data[5,radialnumberlow:radialnumberhigh],linestyle=1
;writeps2,thisDevice

window,3,retain=2
plot,data[0,radialnumberlow:radialnumberhigh],data[8,radialnumberlow:radialnumberhigh],psym=2,/ynozero,xtitle="radius (pixel)",ytitle="SiK equivalent width"
oplot,data[0,radialnumberlow:radialnumberhigh],data[8,radialnumberlow:radialnumberhigh],linestyle=1

;thisDevice=writeps1(outputeps3,0,7,5,0,0)
;plot,data[0,radialnumberlow:radialnumberhigh],data[8,radialnumberlow:radialnumberhigh],psym=2,/ynozero,xtitle="radius (pixel)",ytitle="SiK equivalent width"
;oplot,data[0,radialnumberlow:radialnumberhigh],data[8,radialnumberlow:radialnumberhigh],linestyle=1
;writeps2,thisDevice
end
