pro comb_opac,file1,file2,norm1,norm2,fileout
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - comb_opac,file1,file2,norm1,norm2,fileout'
return
endif
openw,uout,fileout,/get_lun
openr,unit1,file1,/get_lun
openr,unit2,file2,/get_lun
a=''
for i=0,12 do begin
  readf,unit1,a
  printf,uout,a
  readf,unit2,a
endfor
nbin = 134

arr1=fltarr(2,nbin)
readf,unit1,arr1
free_lun,unit1
arr2=fltarr(2,nbin)
readf,unit2,arr2
free_lun,unit2

if total(arr1(0,*)-arr2(0,*)) ne 0. then stop,'too arrays are different'
arr=fltarr(2,nbin)
arr(0,*)=arr1(0,*)
arr(1,*)=arr1(1,*)*norm1+arr2(1,*)*norm2
printf,uout,arr
free_lun,uout
stop
return
end
