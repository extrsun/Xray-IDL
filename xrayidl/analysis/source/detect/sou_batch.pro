openr,un,flist,/get
evtfname=''
k=0
while not eof(un) do begin
	k=k+1
	print,'k, evtfname = ',k, evtfname
	readf,un,evtfname
	.run sou_main
endwhile
free_lun,un
end
