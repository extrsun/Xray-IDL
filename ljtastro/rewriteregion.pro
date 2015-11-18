pro rewriteregion
regionsname='/home/ljt/ljt/data/AboutSNR/SNR/N23/2762/2Dspec/aidfile/regions2.reg'
files='/home/ljt/ljt/data/AboutSNR/SNR/N23/2762/2Dspec/regions2/*.reg'
thesefiles=findfile(files,count=numfiles)
str1="# Region file format: CIAO version 1.0"
str2="ljt"

print,numfiles,files

openw,lun1,regionsname,/get_lun
printf,lun1,str1
for i=0,numfiles-1 do begin

 openr,lun2,thesefiles[i],/get_lun
  readf,lun2,str1
  readf,lun2,str2
 close,lun2
 free_lun,lun2 

printf,lun1,str2
endfor

close,lun1
free_lun,lun1

print,str1
end
