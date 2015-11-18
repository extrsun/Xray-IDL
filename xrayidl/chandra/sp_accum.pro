pro sp_accum,fname,ee,cc,spv
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - sp_accum,fname,ee,cc,spv'
return
endif

openr,un,fname,/get
aa=''
for k=0,2 do readf,un,aa ;useless header
sp=fltarr(5)
nr=1025
ee=fltarr(nr)
cc=fltarr(nr)
spv=cc

cc(0)=0
k=0
while not eof(un) do begin
	readf,un,sp
	k=k+1
	ee(k)=sp(0)
	spv(k)=sp(4)
	cc(k)=cc(k-1)+sp(4)
endwhile
free_lun,un
cc=cc(1:k)
ee=ee(1:k)
spv=spv(1:k)
;print,'spectral min and max energy = ',minmax(ee)
return
end
