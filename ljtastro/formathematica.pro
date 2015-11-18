pro formathematica

outputfilename='/home/ljt/ljt/lectures/mathematica/filename.dat'
files='/home/ljt/ljt/lectures/mathematica/ngc5548/*.dat'
thesefiles=findfile(files,count=numfiles)

print,numfiles

openw,lun1,outputfilename,/get_lun
for i=0,numfiles-1 do begin
printf,lun1,strmid(thesefiles[i],35,23)
printf,lun1,''
endfor

close,lun1
free_lun,lun1
end
