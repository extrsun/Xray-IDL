pro actime,seq_no=seq_no,infile=infile,outfile=outfile
;-
; produce the input data file 'actime.dat' for MAKE_EXP'
; written by WQD. 1993
;+

if n_elements(infile) eq 0 then infile=!seq_no
get_actime,start1,end1,fname=infile,nrow=nrow
;
if n_elements(outfile) eq 0 then outfile=!seq_no+'_actime.dat'
start1=long(start1+0.5)
end1=long(end1+0.5)
openw,unit,outfile,/get_lun

tottime=total(end1-start1)
print,nrow,tottime
printf,unit,nrow,tottime
 for i=0,nrow-1 do begin
	print,start1(i),end1(i)
	printf,unit,start1(i),end1(i)
 endfor
free_lun,unit
return

end

