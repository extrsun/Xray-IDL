pro irxray,infile,dir=dir
if n_elements(dir) eq 0 then dir='/data1/wqd/archives/cor_iras/'
openr,un,infile,/get_lun
fname=''
dra=0.
ddec=0.
irfname=''
while not eof(un) do begin
	readf,un,fname,dra,ddec,irfname,format='(a7,2f9.3,1x,a3)'
	fname=strtrim(dir,2)+'rp'+strtrim(fname,2)+'_ir4.fit'
	irfname='/cdrom/images/i'+irfname+'b4h0.fit'
if !debug eq 1 then stop
ir2xray,irfname,fname,xsize=512,ysize=512,pixsize=15,xcenter=dra,ycenter=ddec $
,image=image
if !debug eq 1 then stop
endwhile
close,un
end