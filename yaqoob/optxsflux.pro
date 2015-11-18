pro optxsflux,logname,nx=nx,ny=ny,fluxs0,fluxs1,fluxg2,fluxg3
if n_params(0) eq 0 then begin
 print,'optxsflux,logname,nx=nx,ny=ny,fluxs0,fluxs1,fluxg2,fluxg3'
 print,'Takes an XSPEC log file produced by running a script '
 print,'produced by ARFGRID with ONE SEQUENCE ONLY'
 print,'In other words, the script runs xspec on s0-s3 for '
 print,'one observation and produces 4 sets of fluxes as '
 print,'output. Fluxes 0..3 are 0.5-10,0.5-5.,0.5-2.,2-10 keV resp.'
 print,'nx and ny are the x and y dimensions for the s0..s3 grids'
 print,'Must be consistent with ARFGRID. Default nx=ny=[8,8,10,10]'  
 print,'LOGNAME is the name of the xspec log file'
 retall
end
if n_elements(nx) eq 0 then nx=[8,8,10,10]
if n_elements(ny) eq 0 then ny=[8,8,10,10]
nflx=4
fluxs0=fltarr(nx(0),ny(0),nflx)
fluxs1=fltarr(nx(1),ny(1),nflx)
fluxg2=fltarr(nx(2),ny(2),nflx)
fluxg3=fltarr(nx(3),ny(3),nflx)
dummy=' '
openr,1,logname
openw,3,'dump.dat'
for i=0l,3l do begin
 for j=0l,nx(i)-1l do begin
  for k=0l,ny(i)-1l do begin 
;   printf,3,'I J K',i,j,k
lup: readf,1,dummy
; printf,3,dummy
 if strpos(dummy,'photon') gt 0 then begin
  sep=str_sep(dummy,' ')
  if (size(sep))(1) ge 5 then reads,strmid(sep(5),1,15),fluxdum 
  if i eq 0 then fluxs0(j,k,0)=fluxdum
  if i eq 1 then fluxs1(j,k,0)=fluxdum
  if i eq 2 then fluxg2(j,k,0)=fluxdum
  if i eq 3 then fluxg3(j,k,0)=fluxdum
;  print,'I J K',i,j,k,fluxdum
  for n=1,3 do begin 
   readf,1,dummy
;   printf,3,dummy
   readf,1,dummy
;   printf,3,dummy
   sep=str_sep(dummy,' ')
   if (size(sep))(1) ge 5 then reads,strmid(sep(5),1,15),fluxdum
    if i eq 0 then fluxs0(j,k,n)=fluxdum
    if i eq 1 then fluxs1(j,k,n)=fluxdum
    if i eq 2 then fluxg2(j,k,n)=fluxdum
    if i eq 3 then fluxg3(j,k,n)=fluxdum
  endfor
 endif else begin
  goto,lup
 endelse
  endfor
 endfor
endfor
close,1
close,3
return
end
 
