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
  parameters[0]=float(strmid(str1,24,4))
  parameters[1]=float(strmid(str1,30,4))
  parameters[2]=float(strmid(str1,38,6))
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



pro paraspace
;0:Radius
;1:nH
;2:TL
;3:OL
;4:NeL
;5:MgL
;6:SiL
;7:SL
;8:CaL
;9:FeL
;10:Tau_uL
;11:normL
;12:TH
;13:OH
;14:NeH
;15:MgH
;16:SiH
;17:SH
;18:CaH
;19:FeH
;20:Tau_uH
;21:normH
;22:PwrId
;23:PwrNorm
files='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/radialprofile/spectra/Dl*_SNR5_0.3-10keV.xcm'
thesefiles=findfile(files,count=numfiles)
paranumber=26

xpara=6
ypara=7
xzero=0
yzero=0
xzoom=0.25
yzoom=0.1
xrange=40
yrange=65
paraspacefig=lonarr(xrange,yrange)
outputfits='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/radialprofile/figures/SiLvsSL_paraspacefig.fits'
toolandtime='TOOL  :paraspace.pro-IDL,  2006-02-22   '
fileforhead='/home/ljt/ljt/data/AboutSNR/SNR/Kepler/Kepler/radialprofile/Kepler_source_bin1.fits'

parafig=fltarr(paranumber,numfiles)
parafig2=fltarr(paranumber-1,numfiles)

print,numfiles

k=0
for i=0,numfiles-1 do begin
  filename=thesefiles[i]
  parafig[*,i]=readpara(filename,paranumber)

  if parafig[5,i] le 9900 then begin
  if parafig[6,i] le 9900 then begin
  if parafig[7,i] le 9900 then begin
  if parafig[8,i] le 9900 and parafig[8,i] gt 1 then begin
  if parafig[9,i] le 9900 and parafig[9,i] gt 1 then begin
  if parafig[10,i] le 9900 then begin
  if parafig[11,i] le 9900 then begin
  if parafig[15,i] le 9900 then begin
  if parafig[16,i] le 9900 then begin
  if parafig[17,i] le 9900 then begin
  if parafig[18,i] le 9900 then begin
  if parafig[19,i] le 9900 then begin
  if parafig[20,i] le 9900 then begin
  if parafig[21,i] le 9900 then begin
  ;if parafig[8,i]/parafig[18,i] le 100 then begin
  ;if parafig[8,i]/parafig[9,i] le 100 then begin

   parafig2[0,k]=(parafig[0,i]+parafig[1,i])/2.
   if parafig[4,i] le parafig[14,i] then begin
     parafig2[1:paranumber-3,k]=parafig[3:paranumber-1,i]
     parafig2[paranumber-2,k]=parafig[2,i]
     k=k+1
   endif
   if parafig[4,i] gt parafig[14,i] then begin
     parafig2[1,k]=parafig[3,i]
     parafig2[2:11,k]=parafig[14:23,i]
     parafig2[12:21,k]=parafig[4:13,i]
     parafig2[22:paranumber-3,k]=parafig[24:paranumber-1,i]
     parafig2[paranumber-2,k]=parafig[2,i]
     ;print,parafig2[*,k]
     k=k+1
   endif

  ;endif
  ;endif
  endif
  endif
  endif
  endif
  endif
  endif
  endif
  endif
  endif
  endif
  endif
  endif
  endif
  endif
endfor

;print,parafig2
print,"data number",n_elements(parafig2[0,*])
datanumber=k
print,"select data number",k

for i=0,datanumber-1 do begin
  ;xlabel=fix((alog10(parafig2[xpara,i])-xzero)*xzoom)
  xlabel=fix((parafig2[xpara,i]-xzero)*xzoom)
;0-30
  ;ylabel=fix((alog10(parafig2[ypara,i])-yzero)*yzoom)
  ylabel=fix((parafig2[ypara,i]-yzero)*yzoom)
;0-20
  paraspacefig[xlabel,ylabel]=paraspacefig[xlabel,ylabel]+parafig2[paranumber-2,i]
endfor

nouse=mrdfits(fileforhead,0,fitshead)
writeimgfits,paraspacefig,outputfits,fitshead,xrange,yrange,toolandtime


window,1,retain=2,xsize=xrange,ysize=yrange
tvscl,paraspacefig

;thisDevice=writeps1(outputeps,0,7,5,0,0)
;plot,parafig2[0,0:k-1],parafig2[label,0:k-1],psym=2,xtitle="radius(in units of pixel)",ytitle="Tau(Low Temperature)",/ylog
;writeps2,thisDevice

end
