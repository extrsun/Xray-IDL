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
    readf,lun,str1
;useless lines
  readf,lun,str1
  parameters[10]=float(strmid(str1,0,16))
   for n1=0,1 do begin 
    readf,lun,str1
   endfor
;useless lines
  readf,lun,str1
  parameters[11]=float(strmid(str1,0,16))
    readf,lun,str1
;useless lines  
  readf,lun,str1
  parameters[12]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[13]=float(strmid(str1,0,16))
  readf,lun,str1
  parameters[14]=float(strmid(str1,0,16))

close,lun
free_lun,lun

return,parameters

end



pro newparafigure
;0:X
;1:Y
;2:counts
;3:nH
;4:T
;5:O
;6:Ne
;7:Mg
;8:Si
;9:S
;10:Fe
;11:Tau
;12:norm
;13:PwrId
;14:PwrNorm
centralposi=[3935.,4060.]
files='/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec/spectra/*_SNR3_0.3-10keV.xcm'
thesefiles=findfile(files,count=numfiles)

paranumber=15
totalnumfiles=numfiles

parafig=fltarr(paranumber,totalnumfiles)
parafig2=fltarr(paranumber-2,totalnumfiles)

print,numfiles,totalnumfiles

k=0
for i=0,totalnumfiles-1 do begin  
  filename=thesefiles[i]
  parafig[*,i]=readpara(filename,paranumber)
  k=k+1
endfor




window,1,retain=2
outputjpg="/home/ljt/ljt/data/AboutSNR/SNR/SNR0519-69.0/118/2Dspec/figures/TvsCa.jpg"
label1=3
label2=14
pointsize=0.5
device,decomposed=0
plot,parafig[label1,0:k-1],parafig[label2,0:k-1],psym=7,SYMSIZE=pointsize,color=0B,background=255B,position=[0.15,0.1,0.96,0.96],charsize=1$
,/xlog,/ylog,xtitle="T(keV)",ytitle="Fe Abundence";,yrange=[1,1e4],xrange=[1,1e4]
device,decomposed=1

;imgarr=tvrd(true=1)
;write_jpeg,outputjpg,imgarr,quality=100,true=1


end
