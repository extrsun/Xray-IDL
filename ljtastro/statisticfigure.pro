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


function readstatisticpara,filename,paranumber
parameters=fltarr(2,paranumber)
str1="ljt"

openr,lun,filename,/get_lun

for i=0,paranumber-1 do begin

  readf,lun,str1
  parameters[0,i]=float(strmid(str1,27,12))
   for n1=0,5 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[1,i]=float(strmid(str1,25,7))
   readf,lun,str1
;useless lines

endfor

close,lun
free_lun,lun

return,parameters

end



pro statisticfigure
file="/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/merge/EWmap/statistics2"
number=678
statisticpara=fltarr(2,number)
statisticpara=readstatisticpara(file,number)
;outputeps="/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/merge/EWmapnew/statisticLowE.eps"

window,1,retain=2

;thisDevice=writeps1(outputeps,0,7,5,0,0)

plot,statisticpara[0,*],statisticpara[1,*],psym=3,xrange=[2.5,8],xtitle="Energy(keV)",ytitle="Reduced statistic";,/ylog,/xlog
;/sqrt(1.7-statisticpara[0,*])
print,statisticpara
px1=[1.41,1.41]
px2=[1.68,1.68]
px3=[1.71,1.71]
px4=[1.95,1.95]
px5=[2.1,2.1]
px6=[2.3,2.3]
px7=[2.52,2.52]
px8=[2.58,2.58]
px9=[2.77,2.77]
px10=[2.99,2.99]
px11=[3.20,3.20]
px12=[3.67,3.67]
px13=[3.94,3.94]
px14=[6.19,6.19]
px15=[6.24,6.24]
px16=[6.7,6.7]
px17=[7.6,7.6]
py=[0.,2000.]
oplot,px1[*],py[*],psym=0
oplot,px2[*],py[*],psym=0
oplot,px3[*],py[*],psym=0
oplot,px4[*],py[*],psym=0
oplot,px5[*],py[*],psym=0
oplot,px6[*],py[*],psym=0
oplot,px7[*],py[*],psym=0
oplot,px8[*],py[*],psym=0
oplot,px9[*],py[*],psym=0
oplot,px10[*],py[*],psym=0
oplot,px11[*],py[*],psym=0
oplot,px12[*],py[*],psym=0
oplot,px13[*],py[*],psym=0
oplot,px14[*],py[*],psym=0
oplot,px15[*],py[*],psym=0
oplot,px16[*],py[*],psym=0
oplot,px17[*],py[*],psym=0

;writeps2,thisDevice

end
