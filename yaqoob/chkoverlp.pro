pro chkoverlp,fname
if n_params(0) eq 0 then begin
 print,' CHKOVERLP, fname '
 print,' check GTI file for overlapping time intervals '
 retall
end
if n_elements(fname) eq 0 then begin
 fname=' '
 read,' Enter filename to check ',fname
end
i=0
tint=dblarr(10000,2)
openr,1,fname
while not eof(1) do begin
 readf,1,t1,t2,dt1,dt2,ityp
 tint(i,0)=t1 & tint(i,1)=t2
 print,tint(i,0),tint(i,1)
 i=i+1
endwhile
ntim=i 
ifound=0
print,' Read ',ntim,' intervals '
for j=0l,ntim-1 do begin
 print,'checking interval ',j,tint(j,0),tint(j,1)
 for k=0l,ntim-1 do begin
  if j ne k then begin
   if tint(j,0) gt tint(k,0) and tint(j,0) lt tint(k,1) then $
    print,' start of interval ',j,' lies within interval ',k
   if tint(j,0) eq tint(k,0) then $
   print,' start of interval ',j,' = start of interval ',k
   if tint(j,0) eq tint(k,1) then $
   print,' start of interval ',j,' = end of interval ',k
   if tint(j,1) gt tint(k,0) and tint(j,1) lt tint(k,1) then $
    print,' end of interval ',j,' lies within interval ',k
   if tint(j,1) eq tint(k,0) then $
   print,' end of interval ',j,' = start of interval ',k
   if tint(j,1) eq tint(k,1) then $
   print,' end of interval ',j,' = end of interval ',k
  endif
 endfor
endfor
close,1
return
end
