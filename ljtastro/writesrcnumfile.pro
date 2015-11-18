pro writesrcnumfile, regionfile=regionfile, outputfile=outputfile

string1='a'
openr, lun, regionfile, /get_lun
;readf,lun,string1
;print,string1
openw, lun1, outputfile, /get_lun
for n=1,1000 do begin
readf,lun,string1
print,string1
if strcmp(strmid(string1,0,6),'circle') ne 1 then begin
goto, NEXT
endif
printf,lun1,float(strmid(string1,7,9)),'  ',float(strmid(string1,17,9)),'  ',n
endfor

NEXT:
close,lun
free_lun,lun
close,lun1
free_lun,lun1

end
