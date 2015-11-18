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
  for k=0,27 do begin
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





function readdata2,filename

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








pro newparafigureaddEW
;0: FeL:        left:       500-800eV
;1:             right:      1000-1150eV
;2:             line:       800-1000eV
;3: SiK1.85:    left:       1410-1680eV
;4:             right:      1950-2100eV
;5:             line:       1710-1950eV
;6: Si2.2:      left:       1950-2100eV
;7:             right:      2580-2770eV
;8:             line:       2100-2300eV
;9: S2.4:       left:       1950-2100eV
;10:            right:      2580-2770eV
;11:            line:       2300-2520eV
;12:S2.88:      left:       2580-2770eV
;13:            right:      3200-3670eV
;14:            line:       2770-2990eV
;15:Ar3.1:      left:       2580-2770eV
;16:            right:      3200-3670eV
;17:            line:       2990-3200eV
;18:Ca3.8:      left:       3200-3670eV
;19:            right:      3940-6190eV
;20:            line:       3670-3940eV
;21:FeK:        left:       3940-6190eV
;22:            right:      6700-7600eV
;23:            line:       6240-6700eV


;0:X                        0:X
;1:Y                        1:Y
;2:bin                      2:bin  
;3:counts                   3:counts   
;4:nH                       4:nH
;5:TL                       5:TL
;6:OL                       6:OL
;7:NeL                      7:NeL
;8:MgL                      8:MgL
;9:SiL                      9:SiL
;10:SL                      10:SL
;11:CaL                     11:CaL
;12:FeL                     12:FeL
;13:Tau_uL                  13:Tau_uL
;14:normL                   14:normL
;15:TH                      15:TH
;16:OH                      16:OH
;17:NeH                     17:NeH
;18:MgH                     18:MgH
;19:SiH                     19:SiH
;20:SH                      20:SH
;21:CaH                     21:CaH
;22:FeH                     22:FeH
;23:Tau_uH                  23:Tau_uH
;24:normH                   24:normH
;25:PwrId                   25:PwrId
;26:PwrNorm                 26:PwrNorm
;                           27:FeLcontinuum
;                           28:FeLline
;                           29:FeLEW
;                           30:SiKcontinuum
;                           31:SiKline
;                           32:SiKEW
;                           33:Si2.2continuum
;                           34:Si2.2line
;                           35:Si2.2EW
;                           36:S2.4continuum
;                           37:S2.4line
;                           38:S2.4EW
;                           39:S2.88continuum
;                           40:S2.88line
;                           41:S2.88EW
;                           42:Ar3.1continuum
;                           43:Ar3.1line
;                           44:Ar3.1EW
;                           45:Ca3.8continuum
;                           46:Ca3.8line
;                           47:Ca3.8EW
;                           48:FeKcontinuum
;                           49:FeKline
;                           50:FeKEW


parafile='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/Kepler_parameters'
outputparanumber=51
totalnumfiles=1483
parameters=fltarr(outputparanumber,totalnumfiles)
parameters2=fltarr(outputparanumber,totalnumfiles)
pointsize=0.5
pointsize2=2
charactersize1=1.5

openr,lun,parafile,/get_lun
readf,lun,parameters
close,lun
free_lun,lun

k=0
for i=0,totalnumfiles-1 do begin
;if parameters[33,i]*0.2 ge 10 and parameters[34,i]*0.2 ge 1 then begin 
parameters2[*,i]=parameters[*,i]
k=k+1
;endif
endfor

print,'Used data number=',k 




files1='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/work2/totalspectra/FeL/FeTh*.xcm'
files2='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/work2/totalspectra/Si1.85/SiTh*.xcm'
files3='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/work2/totalspectra/NeMg/MgTh*.xcm'
files4='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/work2/totalspectra/Ca/CaTh*.xcm'
files5='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/work2/totalspectra/NeFe/NeTh*.xcm'
files6='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/work2/totalspectra/NeMgFe/NeTh*.xcm'
files7='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/work2/totalspectra/FeK/FeTh*.xcm'
files8='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/work2/totalspectra/S2.4c/SKTh*.xcm'
data0=readdata(files1)
data1=arrangement(data0.data,data0.number,0,0)
data0=readdata(files2)
data2=arrangement(data0.data,data0.number,0,0)
data0=readdata(files3)
data3=arrangement(data0.data,data0.number,0,0)
data0=readdata2(files4)
data4=arrangement(data0.data,data0.number,0,0)
data0=readdata2(files5)
data5=arrangement(data0.data,data0.number,0,0)
data0=readdata2(files6)
data6=arrangement(data0.data,data0.number,0,0)
data0=readdata2(files7)
data7=arrangement(data0.data,data0.number,0,0)
data0=readdata2(files8)
data8=arrangement(data0.data,data0.number,0,0)

window,1,retain=2;,xsize=1500,ysize=1200
outputfigure='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/work2/totalspectra/TvsTau.jpg'
label1=15
label2=23
label3=2
label4=3
device,decomposed=0
plot,parameters2[label1,*],parameters2[label2,*],psym=7,SYMSIZE=pointsize,color=0B,background=255B,position=[0.15,0.1,0.96,0.96],charsize=2$
,xtitle='T/keV (High Temperature Component)',ytitle='n!de!nt/cm!u-3!ns (High Temperature Component)',/ylog,/xlog;,yrange=[1e-6,1e-2],xrange=[1e-5,1e-2]
device,decomposed=1
;oplot,parameters2[label3,*],parameters2[label4,*],psym=6,SYMSIZE=pointsize,color=255B

AA1=9.5
BB1=0.1
kk1=-0.25
AA2=10.7
BB2=0.5
kk2=-0.5
;upper curve:AA=10.7,BB=0.5,kk=-0.5
;lower curve:AA=9.5,BB=0.1,kk=-0.25
;n_eTau=10^AA*(T-BB)^(-(T-BB)^kk)
x=double(10^(findgen(1000)*0.004-2.))
y1=10^AA1*(x-BB1)^(-(x-BB1)^kk1)
y2=10^AA2*(x-BB2)^(-(x-BB2)^kk2)
;oplot,x,y1,color=255B
;oplot,x,y2,color=255B

oplot,data1[0,*],data1[1,*],psym=2,SYMSIZE=pointsize2,thick=2,color='111111'
oplot,data1[0,*],data1[1,*],psym=0,linestyle=2,thick=2,color='111111'
oplot,[2,3],[7e13,7e13],psym=-2,SYMSIZE=pointsize2,linestyle=2,thick=2,color='111111'
XYOutS,0.7,0.934,'Fe 0.50-1.15keV band',size=charactersize1,/normal,color='111111'

oplot,data2[0,*],data2[1,*],psym=4,SYMSIZE=pointsize2,thick=2,color=255
oplot,data2[0,*],data2[1,*],psym=0,linestyle=2,thick=2,color=255
oplot,[2,3],[5e13,5e13],psym=-4,SYMSIZE=pointsize2,linestyle=2,thick=2,color=255
XYOutS,0.7,0.913,'Si 1.41-2.10keV band',size=charactersize1,/normal,color=255

oplot,data3[0,*],data3[1,*],psym=5,SYMSIZE=pointsize2,thick=2,color='300000'
oplot,data3[0,*],data3[1,*],psym=0,linestyle=0,thick=1,color='300000'
oplot,[2,3],[3.5e13,3.5e13],psym=-5,SYMSIZE=pointsize2,linestyle=0,thick=2,color='300000'
XYOutS,0.7,0.89,'Ne,Mg 1.15-1.41keV band',size=charactersize1,/normal,color='300000'

oplot,data4[0,*],data4[1,*],psym=6,SYMSIZE=pointsize2,thick=2,color='040909'
oplot,data4[0,*],data4[1,*],psym=0,linestyle=0,thick=1,color='040909'
oplot,[2,3],[2.5e13,2.5e13],psym=-6,SYMSIZE=pointsize2,linestyle=0,thick=2,color='040909'
XYOutS,0.7,0.87,'Ca 3.50-4.00keV band',size=charactersize1,/normal,color='040909'

oplot,data5[0,*],data5[1,*],psym=7,SYMSIZE=pointsize2,thick=2,color='080805'
oplot,data5[0,*],data5[1,*],psym=0,linestyle=3,thick=2,color='080805'
oplot,[2,3],[1.65e13,1.65e13],psym=-7,SYMSIZE=pointsize2,linestyle=3,thick=2,color='080805'
XYOutS,0.7,0.843,'Ne,Fe 0.50-1.15keV band',size=charactersize1,/normal,color='080805'

oplot,data6[0,*],data6[1,*],psym=1,SYMSIZE=pointsize2,thick=2,color='050000'
oplot,data6[0,*],data6[1,*],psym=0,linestyle=0,thick=1,color='050000'
oplot,[2,3],[1.05e13,1.05e13],psym=-1,SYMSIZE=pointsize2,linestyle=0,thick=2,color='050000'
XYOutS,0.7,0.815,'Ne,Mg,Fe 0.30-0.80keV band',size=charactersize1,/normal,color='050000'

;oplot,data7[0,*],data7[1,*],psym=5,SYMSIZE=pointsize2,thick=2,color='090606'
;oplot,data7[0,*],data7[1,*],psym=0,linestyle=3,thick=2,color='090606'
;oplot,[2,3],[7e12,7e12],psym=-5,SYMSIZE=pointsize2,linestyle=3,thick=2,color='090606'
;XYOutS,0.7,0.79,'Fe 4.00-7.00keV band',size=charactersize1,/normal,color='090606'

oplot,data8[0,*],data8[1,*],psym=5,SYMSIZE=pointsize2,thick=2,color='090606'
oplot,data8[0,*],data8[1,*],psym=0,linestyle=2,thick=2,color='090606'
oplot,[2,3],[7e12,7e12],psym=-5,SYMSIZE=pointsize2,linestyle=2,thick=2,color='090606'
XYOutS,0.7,0.79,'S 2.30-2.77keV band',size=charactersize1,/normal,color='090606'

;imgarr=tvrd(true=1)
;write_jpeg,outputfigure,imgarr,quality=100,true=1





window,2,retain=2
;outputfigure2='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/figures/TvsIndex.jpg'
plot,x,(x-BB1)^kk1,position=[0.15,0.1,0.96,0.96],/xlog,/ylog,xtitle='T(keV)(High Temperature Component)'$
,ytitle='Index',yrange=[1e-2,1e2]
oplot,x,(x-BB2)^kk2,color='111111'
XYOutS,0.8,0.8,'Si 1.41-2.10keV band',size=1,/normal

;imgarr=tvrd(true=1)
;write_jpeg,outputfigure2,imgarr,quality=100,true=1



window,3,retain=2
device,decomposed=0
plot,[8,9,10],[8e13,8e13,8e13],psym=-2,linestyle=2,thick=1,color=0B,background=255B,/xlog,/ylog,yrange=[1e12,1e14]
device,decomposed=1
oplot,[8,9,10],[7e13,7e13,7e13],psym=-2,linestyle=2,thick=1,color='030099'
oplot,[8,9,10],[6e13,6e13,6e13],psym=-2,linestyle=2,thick=1,color='060909'
oplot,[8,9,10],[5e13,5e13,5e13],psym=-2,linestyle=2,thick=1,color='030000'
oplot,[8,9,10],[4e13,4e13,4e13],psym=-2,linestyle=2,thick=1,color='040000'
oplot,[8,9,10],[3e13,3e13,3e13],psym=-2,linestyle=2,thick=1,color='050000'
oplot,[8,9,10],[2e13,2e13,2e13],psym=-6,linestyle=0,thick=1,color='040909'
oplot,[8,9,10],[1e13,1e13,1e13],psym=-7,linestyle=3,thick=1,color='080805'
oplot,[8,9,10],[2e12,2e12,2e12],psym=-1,linestyle=1,thick=2,color='090606'



window,4,retain=2,xsize=1000,ysize=800
outputfigure='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/work2/FeLcontinuumvsFeLline.jpg'
label1=27
label2=28
label3=3
label4=3
device,decomposed=0
plot,parameters2[label1,*]*0.2/parameters2[label3,*],parameters2[label2,*]*0.2/parameters2[label4,*],psym=7,SYMSIZE=pointsize,color=0B,background=255B,position=[0.15,0.1,0.96,0.96],charsize=2$
,xtitle='Continuum Counts in Fe L Band/Total Counts',ytitle='Fe L Line Counts/Total Counts',/ylog,/xlog;,xrange=[1e-2,1e2],yrange=[1e-2,1e2]
device,decomposed=1
;oplot,parameters2[label3,*],parameters2[label4,*],psym=7,SYMSIZE=pointsize,color=255B

xco=fltarr(1483)
yco=fltarr(1483)
k=0
for i=0,1482 do begin
;if parameters2[label1,i] lt 1e1 and parameters2[label1,i] gt 1e-2 then begin
;if parameters2[label2,i] lt 1e-3 and parameters2[label2,i] gt 1e-8 then begin
xco[k]=parameters2[label1,i]
yco[k]=parameters2[label2,i]
k=k+1
;endif
;endif
endfor
print,'data used=',k
print,'correlation coefficient=',correlate(xco,yco)

AA=0.8
kk=2
x=10^(findgen(100)*0.04-2.)
y=AA*x^(kk)
oplot,x,y,color=255B

;imgarr=tvrd(true=1)
;write_jpeg,outputfigure,imgarr,quality=100,true=1



end
