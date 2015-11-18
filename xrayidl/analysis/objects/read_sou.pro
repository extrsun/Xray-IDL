pro read_sou,fname,sra,sdec,sf
openr,un,fname,/get
str=''
text=strarr(1000)
sra=fltarr(1000)
sdec=fltarr(1000)
sf=fltarr(1000)
k=0
while not eof(un) do begin
	readf,un,str
	sra(k)=float(strmid(str,18,8))
	sdec(k)=float(strmid(str,27,8))
	sf(k)=float(strmid(str,36,11))
	text(k)=str
	k=k+1
endwhile
free_lun,un
sra=sra(0:k-1)
sdec=sdec(0:k-1)
text=text(0:k-1)
sf=sf(0:k-1)
return
end