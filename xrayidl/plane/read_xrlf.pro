pro read_xlf,elow,ehigh,starn,file=file
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - read_xlf,elow,ehigh,starn,file=file'
return
endif

if n_elements(file) eq 0 then file='/home/casa/wqd/rosat/plane/xrlf.dat'
openr,un,file,/get
readf,un,nline
input=fltarr(3,nline)
elow=fltarr(nline) 
ehigh=elow
starn=elow
k=0
while (eof(un) eq 0) do begin
	readf,un,input
	k=k+1
endwhile

kk=k-1
starn=input(2,*)
elow=input(0,*)
ehigh=input(1,*)
free_lun,un


return
end