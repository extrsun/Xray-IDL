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




pro writetablefile,data,outputfile
nspec=n_elements(data[0,0,*])
npara=17
;n_elements(data[0,*,0])

openw,lun,outputfile,/get_lun

for i=0,nspec-1 do begin
;printf,lun,'& '+strtrim(string(float(data[0,0,i])),2)
;printf,lun,'& '+strtrim(string(float(data[1,0,i])),2)+'/'+strtrim(string(float(data[2,0,i])),2)
;for j=6,6 do begin
;if data[1,j,i]+data[0,j,i] ne 0 or data[2,j,i]+data[0,j,i] ne 0 then begin
;printf,lun,'& $'+strtrim(string(float(data[0,j,i])),2)+'_{'+strtrim(string(float(data[1,j,i])),2)+'}^{'$
;+strtrim(string(float(data[2,j,i])),2)+'}$ '
;endif
;if data[1,j,i]+data[0,j,i] eq 0 and data[2,j,i]+data[0,j,i] eq 0 then begin
;printf,lun,'& --'
;endif
;endfor
for j=9,9 do begin
if data[1,j,i]+data[0,j,i] ne 0 or data[2,j,i]+data[0,j,i] ne 0 then begin
printf,lun,'& $'+strtrim(string(float(data[0,j,i])),2)+'_{'+strtrim(string(float(data[1,j,i])),2)+'}^{'$
+strtrim(string(float(data[2,j,i])),2)+'}$ '
endif
if data[1,j,i]+data[0,j,i] eq 0 and data[2,j,i]+data[0,j,i] eq 0 then begin
printf,lun,'& --'
endif
endfor
printf,lun,'& '+strtrim(string(float(data[0,10,i])),2)
printf,lun,'& '+strtrim(string(float(data[1,10,i])),2)
printf,lun,'& '+strtrim(string(float(data[2,10,i])),2)
printf,lun,'& '+strtrim(string(float(data[0,11,i])),2)
;printf,lun,'& '+strtrim(string(float(data[1,11,i])),2)
printf,lun,'& '+strtrim(string(float(data[2,11,i])),2)
printf,lun,'\\'
endfor

close,lun
free_lun,lun

end



pro writetablefortex
;0:counts(0.3-8keV), reduced chi, d.o.f.
;1:nH(error)
;2:T(error)
;3:O(error)
;4:Ne(error)
;5:Mg(error)
;6:Si(error)
;7:Fe(error)
;8:Tau(error)
;9:norm(error)
;10:total flux(0.3-8keV), flux-O(0.3-8keV), flux-Ne(0.3-8keV)
;11:flux-Mg(0.3-8keV), flux-Si(0.3-8keV), flux-Fe(0.3-8keV)


centralposi=[4252.0,4158.0]
files='/home/ljt/ljt/data/AboutSNR/SNR/N23/2762/2Dspec/aidfile/parameters'
outputfile='/home/ljt/ljt/data/AboutSNR/SNR/N23/2762/work/fortex3'
;thesefiles=findfile(files,count=numfiles)
paranumber=12
parametersread=fltarr(3,paranumber)
parameters=fltarr(3,paranumber)
numfiles=15
parafig=fltarr(3,paranumber,numfiles)

print,numfiles

openr,lun,files,/get_lun
readf,lun,parafig
close,lun
free_lun,lun

for i=0,numfiles-1 do begin
  parametersread[*,*]=parafig[*,*,i]
  parameters=parametersread
  parameters[1,1:9]=parameters[1,1:9]-parameters[0,1:9]
  parameters[2,1:9]=parameters[2,1:9]-parameters[0,1:9]
  parameters[1,10]=parametersread[0,10]-parametersread[1,10]
  parameters[2,10]=parametersread[1,10]-parametersread[2,10]
  parameters[0,11]=parametersread[2,10]-parametersread[0,11]
  if parametersread[1,11] ne 0 then begin
  parameters[1,11]=parametersread[0,11]-parametersread[1,11]
  parameters[2,11]=parametersread[1,11]-parametersread[2,11]
  endif
  if parametersread[1,11] eq 0 then begin
  parameters[1,11]=0
  parameters[2,11]=parametersread[0,11]-parametersread[2,11]
  endif
parafig[*,*,i]=parameters[*,*]
endfor
parafig[*,1,*]=parafig[*,1,*]/0.01
parafig[*,8,*]=parafig[*,8,*]/1e10
parafig[*,9,*]=parafig[*,9,*]/1e-4
parafig[*,10,*]=parafig[*,10,*]/1e-13
parafig[*,11,*]=parafig[*,11,*]/1e-13

writetablefile,parafig,outputfile

end

