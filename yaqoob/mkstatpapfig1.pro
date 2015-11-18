pro mkstatpapfig1
;set up input file rootnames
rnames=strarr(44)
rnames(0)='bba0fk_m00_t1e1_200a'
rnames(1)='bba0fk_m00_t1e1_800a'
rnames(2)='bba0fk_m00_t1e2_200a'
rnames(3)='bba0fk_m00_t1e2_223a'
rnames(4)='bba0fk_m00_t1e2_577a'
rnames(5)='bba0fk_m00_t1e3_200a'
rnames(6)='bba0fk_m00_t1e3_800a'
rnames(7)='bba0fk_m01_t1e1_200a'
rnames(8)='bba0fk_m01_t1e1_800a'
rnames(9)='bba0fk_m01_t1e2_200a'
rnames(10)='bba0fk_m01_t1e2_143b'
rnames(11)='bba0fk_m01_t1e2_514c'
rnames(12)='bba0fk_m01_t1e2_143d'
rnames(13)='bba0fk_m01_t1e3_111a'
rnames(14)='bba0fk_m01_t1e3_189a'
rnames(15)='bba0fk_m01_t1e3_200a'
rnames(16)='bba0fk_m01_t1e3_500a'
rnames(17)='bba0fk_m02_t1e1_200a'
rnames(18)='bba0fk_m02_t1e1_300a'
rnames(19)='bba0fk_m02_t1e1_500a'
rnames(20)='bba0fk_m02_t1e2_200a'
rnames(21)='bba0fk_m02_t1e2_800a'
rnames(22)='bba0fk_m02_t1e3_200a'
rnames(23)='bba0fk_m02_t1e3_800a'
rnames(24)='bba0fk_m03_t1e1_200a'
rnames(25)='bba0fk_m03_t1e1_800a'
rnames(26)='bba0fk_m03_t1e2_200a'
rnames(27)='bba0fk_m03_t1e2_800a'
rnames(28)='bba0fk_m03_t1e3_200a'
rnames(29)='bba0fk_m03_t1e3_800a'
rnames(30)='bba0fk_m04_t1e1_200a'
rnames(31)='bba0fk_m04_t1e1_800a'
rnames(32)='bba0fk_m04_t1e2_200a'
rnames(33)='bba0fk_m04_t1e2_800a'
rnames(34)='bba0fk_m04_t1e3_200a'
rnames(35)='bba0fk_m04_t1e3_800a'
rnames(36)='bba0fk_m04_t1e4_200a'
rnames(37)='bba0fk_m04_t1e4_800a'
rnames(38)='bba0fk_m05_t1e1_200a'
rnames(39)='bba0fk_m05_t1e1_800a'
rnames(40)='bba0fk_m05_t1e2_200a'
rnames(41)='bba0fk_m05_t1e2_800a'
rnames(42)='bba0fk_m05_t1e3_200a'
rnames(43)='bba0fk_m05_t1e3_800a'
statnames=rnames+'.stat' & parnames=rnames+'.pars'
;now set up vectors to indicate how to group files for each plot group
grpfile=[2,3,2,2,4,4,3,2,2,2,2,2,2,2,2,2,2,2,2]
nfree=[3,3,3,6,6,6,6,6,6,6,6,6,6,6,6,6,5,5,5]
grps= [0,0,1,0,0,1,0,0,1,0,0,1,0,0,0,1,0,0,1]
ngrp=(size(grpfile))(1)
print,'size and total of grpfile ',ngrp,total(grpfile)
;set up qdp file
openw,1,'statpapfig1.qdp'
printf,1,'@statpapfig1.pco'
;loop through each data group
for i=0l,ngrp-1 do begin
 print,'Doing group ',i+1
;and then each file in the group
 for j=0l,grpfile(i)-1 do begin
  if i eq 0 then fnum =j else fnum = fix(total(grpfile(0:i-1)))+j
  print,'reading file ',fnum
  readcol,statnames(fnum),s1,n1,s2,n2
  if j eq 0 then begin
   strue=s1 & smin=s2 
  endif
  if j gt 0 then begin
   strue=[strue,s1] & smin=[smin,s2] 
  endif
  delstat = strue - smin
 endfor
print,'Number of values of stats ',(size(delstat))(1)
;now compute the delta-stat distribution from delstat
ncval=21 & cval=fltarr(ncval) & cumchi=cval & norm=1000.+fltarr(ngrp)
;norm is the number of simulations - 1000 for each group here
cval=min(delstat)+findgen(ncval)*(max(delstat)-min(delstat))/float(ncval-1)
  for k=0l,ncval-1 do begin
    wdum=where((delstat le cval(k)),val) & cumchi(k)=val
    printf,1,cval(k),cumchi(k)/norm(i)
  endfor
  printf,1,'NO  NO'
  if grps(i) gt 0 then begin
;write the theoretical chi-square distribution
  npts = 250l & cmin=0.0 & cmax=1.3*max(delstat) 
  delc=(cmax-cmin)/float(npts-1) & xchi=cmin+findgen(npts)*delc
  prob=fltarr(npts)
  for k=0l,npts-1 do begin 
   prob(k)=chi_sqr1(xchi(k),nfree(i))
   printf,1,xchi(k),prob(k)
  endfor
  printf,1,'NO	NO'
 endif
endfor
close,1
return
end
