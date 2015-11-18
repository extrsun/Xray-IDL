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


function readpara,filename,paranumber
parameters=fltarr(paranumber)
str1="ljt"

openr,lun,filename,/get_lun
    readf,lun,str1
;useless lines
  readf,lun,str1
  parameters[0]=float(strmid(str1,11,4))
  parameters[1]=float(strmid(str1,17,4))
  parameters[2]=float(strmid(str1,31,5))
   for n1=0,6 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[3]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[4]=float(strmid(str1,0,16))
   for n1=0,3 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[5]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[6]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[7]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[8]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[9]=float(strmid(str1,0,16))
    readf,lun,str1
;useless lines
  readf,lun,str1
  parameters[10]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[11]=float(strmid(str1,0,16))
   for n1=0,1 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[12]=float(strmid(str1,0,16))
    readf,lun,str1
;useless lines  
  readf,lun,str1
  parameters[13]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[14]=float(strmid(str1,0,16))
   for n1=0,3 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[15]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[16]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[17]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[18]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[19]=float(strmid(str1,0,16))
    readf,lun,str1
;useless lines
  readf,lun,str1
  parameters[20]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[21]=float(strmid(str1,0,16))
   for n1=0,1 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[22]=float(strmid(str1,0,16))
    readf,lun,str1
;useless lines
  readf,lun,str1
  parameters[23]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[24]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[25]=float(strmid(str1,0,16))

close,lun
free_lun,lun

return,parameters

end



pro newparaspace
;0:X
;1:Y
;2:counts
;3:nH
;4:TL
;5:OL
;6:NeL
;7:MgL
;8:SiL
;9:SL
;10:CaL
;11:FeL
;12:Tau_uL
;13:normL
;14:TH
;15:OH
;16:NeH
;17:MgH
;18:SiH
;19:SH
;20:CaH
;21:FeH
;22:Tau_uH
;23:normH
;24:PwrId
;25:PwrNorm
centralposi=[4070.,4206.]
files1='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/spectra/X13*_SNR3_0.3-10keV.xcm'
thesefiles1=findfile(files1,count=numfiles1)
files2='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/spectra/X14*_SNR3_0.3-10keV.xcm'
thesefiles2=findfile(files2,count=numfiles2)
outputfits='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/figures/XlogTHYPwrId_X-2Y-4_X15Y5_X60Y70_paraspace.fits'
toolandtime='TOOL  :newparaspace.pro-IDL,  2006-03-01   '
fileforhead='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/paraimg/aidfile/Kepler_source_bin1.fits'
numfiles=numfiles1+numfiles2
paranumber=26

xpara=14
ypara=24
xzero=-2
yzero=-4
xzoom=15
yzoom=5
xrange=60
yrange=70
paraspacefig=lonarr(xrange,yrange)


parafig=fltarr(paranumber,numfiles)
parafig2=fltarr(paranumber,numfiles)

print,numfiles

k=0
for i=0,numfiles-1 do begin  
  if i lt numfiles1 then begin
   filename=thesefiles1[i]
  endif
  if i ge numfiles1 then begin
   filename=thesefiles2[i-numfiles1]
  endif
  
  parafig[*,i]=readpara(filename,paranumber)
  
  ;if parafig[0,i] le 9900 then begin 
  ;if parafig[1,i] le 9900 then begin 
  ;if parafig[2,i] le 9900 then begin 
  if parafig[3,i] le 2 and parafig[3,i] gt 0 then begin 
  ;if parafig[4,i] le 10 and parafig[4,i] gt 0.01 then begin
  ;if parafig[5,i] le 9900 then begin 
  ;if parafig[6,i] le 9900 then begin 
  ;if parafig[7,i] le 9900 then begin 
  ;if parafig[8,i] le 250 and parafig[8,i] gt 0 then begin
  ;if parafig[9,i] le 1000 and parafig[9,i] gt 0 then begin 
  ;if parafig[10,i] le 9900 then begin 
  ;if parafig[11,i] le 9900 then begin
  ;if parafig[14,i] le 100 and parafig[4,i] gt 0.01 then begin
  ;if parafig[15,i] le 9900 then begin 
  ;if parafig[16,i] le 9900 then begin 
  ;if parafig[17,i] le 9900 then begin 
  ;if parafig[18,i] le 9900 then begin
  ;if parafig[19,i] le 9900 then begin 
  ;if parafig[20,i] le 9900 then begin 
  ;if parafig[21,i] le 9900 then begin
  ;if parafig[24,i] le 9900 then begin 
  ;if parafig[25,i] le 0.01 and parafig[25,i] gt 1e-16 then begin

    
    radius=sqrt((parafig[0,i]-centralposi[0])^2+(parafig[1,i]-centralposi[1])^2)
   ;if parafig[4,i] le parafig[14,i] then begin
     parafig2[*,k]=parafig[*,i]
     parafig2[0,k]=radius
     k=k+1
   ;endif
   ;if parafig[4,i] gt parafig[14,i] then begin
   ;  parafig2[0:3,k]=parafig[0:3,i]
   ;  parafig2[4:13,k]=parafig[14:23,i]
   ;  parafig2[14:23,k]=parafig[4:13,i]
   ;  parafig2[24:paranumber-1,k]=parafig[24:paranumber-1,i]
   ;  ;print,parafig2[*,k]
   ;  k=k+1
   ;endif

  endif  
  ;endif
  ;endif
  ;endif
  ;endif
  ;endif
  ;endif
  ;endif
  ;endif
  ;endif
  ;endif
  ;endif
  ;endif
  ;endif
  ;endif
  ;endif
  ;endif
  ;endif
  ;endif
  ;endif
  ;endif
endfor

;print,parafig2
print,"data number",n_elements(parafig2[0,*])
datanumber=k
print,"select data number",k

for i=0,datanumber-1 do begin
  xlabel=fix((alog10(parafig2[xpara,i])-xzero)*xzoom)
  ;xlabel=fix((parafig2[xpara,i]-xzero)*xzoom)

  ;ylabel=fix((alog10(parafig2[ypara,i])-yzero)*yzoom)
  ylabel=fix((parafig2[ypara,i]-yzero)*yzoom)

  paraspacefig[xlabel,ylabel]=paraspacefig[xlabel,ylabel]+parafig2[2,i]
endfor

nouse=mrdfits(fileforhead,0,fitshead)
writeimgfits,paraspacefig,outputfits,fitshead,xrange,yrange,toolandtime


window,1,retain=2,xsize=200,ysize=200
tvscl,paraspacefig

;thisDevice=writeps1(outputeps,0,7,5,0,0)
;plot,parafig2[0,0:k-1],parafig2[label,0:k-1],psym=2,xtitle="radius(in units of pixel)",ytitle="Tau(Low Temperature)",/ylog
;writeps2,thisDevice

end
