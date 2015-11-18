pro total_chi,nline,nfile,filehead
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - total_chi,nline,nfile,filehead'
return
endif
;
a=''
tail=indgen(4)+1
data=fltarr(3,nline)
chi=fltarr(nline)
for k=0,nfile-1 do begin
	file=filehead+strtrim(tail(k),2)+'.log'
	openr,un,file,/get
	for n=0,3 do readf,un,a
	readf,un,data
	chi(*)=chi(*)+data(0,*)
	free_lun,un
endfor
minchi=min(chi)
dchi=chi-minchi
print,'min chi =',minchi
for k=0,nline-1 do begin
if dchi(k) lt 7.5 then print,chi(k),dchi(k),data(2,k)
endfor
end