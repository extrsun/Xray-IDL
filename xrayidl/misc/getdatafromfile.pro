function RowOfFile,filname
openr,lun,filname,/get_lun
s = ' '
i=0L;
while (eof(lun) ne 1) do begin
        i = i + 1
        readf, lun , s
endwhile
close, lun & free_lun, lun
return, i
end

function ColOfFile, filname
openr, lun, filname, /get_lun
s = ' '
readf, lun, s
close, lun & free_lun, lun
words = strsplit(s,' | ',/extract,/regex)
return, n_elements(words)-1
end

pro getDataFromFile, filname, dt
row=rowoffile(filname)
col=coloffile(filname)
print,row,col
dt = dblarr(col,row)
openr, lun ,filname, /get_lun
readf, lun, dt
close, lun & free_lun, lun
end

