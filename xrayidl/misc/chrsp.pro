pro chrsp, filein,fileout
openr,unin,filein,/get
openw,unout,fileout,/get
;
a='                                                                                 '
while not eof(unin) do begin
	readf,unin,a
;	print,a
	printf,unout,a
	printf,unout
endwhile
free_lun,unin,unout
end