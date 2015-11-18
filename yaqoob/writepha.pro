pro writepha,cts,ctserr,dtype,header,hlines,outname=outname
if n_params(0) eq 0 then begin
 print,'writepha,cts,ctserr,dtype,header,hlines,outname=outname'
 print,' writes a PHA file in XSPEC (old) format given the counts '
 print,' (or count-rate) and optional errors. header is a string '
 print,' which contains the file header right up to the last line '
 print,' before the data. DTYPE = 0 if data is counts and DTYPE = 1 '
 print,' if data is to be written as counts/s '
 print,' Expected that the routine READPHA is normally used to '
 print,' generate cts,ctserr,dtype,header,hlines. HLINES is number '
 print,' of lines in the header array'
 retall
end
if n_elements(outname) eq 0 then begin
 outname=' '
 read,' Enter name of output PHA file '
endif
openw,1,'outtemp.ascii'
;write header
for k=0l,hlines-1 do begin
 printf,1,header(k)
endfor
;determine how many data values will be on last line
ncts=(size(cts))(1)
nlines=ncts/3 & nrem = ncts - 3*nlines
if dtype eq 0 then begin
 for j = 0l,nlines-1 do begin
   i1=3*j & i2=i1+1 & i3=i1+2
   printf,1,format='(3(1X,I4,2X,E9.3,4X))',i1+1,cts(i1),i2+1,cts(i2)$
,i3+1,cts(i3)
 endfor
 if nrem eq 2 then begin
   i1=ncts-2 & i2=ncts-1
   printf,1,format='(2(1X,I4,2X,F9.1,4X))',i1+1,cts(i1),i2+1,cts(i2)  
 endif
 if nrem eq 1 then begin
   i1=ncts-1
   printf,1,format='(1X,I4,2X,E9.3,4X)',i1+1,cts(i1)
 endif
endif
if dtype eq 1 then begin
  for j=0l,nlines-1 do begin
    i1=3*j & i2=i1+1 & i3=i1+2
    printf,1,format='(3(1X,I4,2X,E9.3,3X,E8.2))',i1+1,cts(i1),$
    ctserr(i1),i2+1,cts(i2),ctserr(i2),i3+1,cts(i3),ctserr(i3)
  endfor
  if nrem eq 2 then begin
    i1=ncts-2 & i2=ncts-1
    printf,1,format='(2(1X,I4,2X,E9.3,3X,E8.2))',i1+1,cts(i1),$
    ctserr(i1),i2+1,cts(i2),ctserr(i2)
  endif
  if nrem eq 1 then begin
    i1=ncts-1
    printf,1,format='(1X,I4,2X,E9.3,3X,E8.2)',i1+1,cts(i1),$
    ctserr(i1)
  endif
  tot=total(cts)
  toterr=total(ctserr*ctserr)
  if toterr gt 0. then toterr=sqrt(toterr)
  printf,1,format='(A20,F9.2,1X,A2,1X,F9.2)',' Total counts/sec = ',$
  tot,' +- ',toterr
endif
close,1
com='mkpha outtemp.ascii '+outname
spawn,com
return
end
