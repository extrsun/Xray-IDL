pro datest,ee,spv
openr,lun,'acisi_po0.7.dat',/get
aa=''
for k=0,2 do readf,lun,aa
sp=fltarr(5)
nr=1025
ee=fltarr(nr)
cc=fltarr(nr)
spv=cc

cc[0]=0
k=0
while not eof(lun) do begin
  readf,lun,sp
  k=k+1
  ee[k]=sp[0]
  spv[k]=sp[4]
endwhile
free_lun,lun
ee=ee[1:k]
spv=spv[1:k]
return
end
