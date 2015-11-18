pro wfc_sss,file,f
dir='/data3/wfc/'
openr,un,file,/get
f=0.
fname=''
while not eof(un) do begin
	readf,un,fname
	fname=dir+fname
	c=readfits(fname+'.img',h)
	t=readfits(fname+'x.exp')
a=imdiv(c,t)
tv,bscale(a*1.e4,0,15)
stop
	f=f+a
endwhile
free_lun,un
return
end