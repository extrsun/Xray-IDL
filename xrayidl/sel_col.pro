pro sel_col,filein,fileout,nc,nr,ns
openr,un,filein,/get
arr=fltarr(nc,nr)
readf,un,arr
free_lun,un
arr_s=arr(ns,*)
openw,un,fileout,/get
for k=1,nr do printf,un,arr_s(k-1)
free_lun,un
stop
end