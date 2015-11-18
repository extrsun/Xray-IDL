pro divpha,divcts,diverr,fname1=fname1,fname2=fname2,outname=outname
if n_params(0) eq 0 then begin
 print,'divpha,divcts,diverr,fname1=fname1,fname2=fname2,outname=outname'
 print,'Divide two XSPEC PHA files '
 print,' outname = fname2/fname1 '
 retall
endif
if n_elements(fname1) eq 0 then begin
 fname1=' '
 read,' Enter 1st input filename ',fname1
endif
if n_elements(fname2) eq 0 then begin
 fname2='
 read,' Enter 2nd input filename ',fname2
endif
if n_elements(outname) eq 0 then begin
 outname=' '
 read,' Enter output filename ',outname
endif
;read first file
readpha,dtype1,fname=fname1,cts1,ctserr1,pline1,ilines1
;read second file
readpha,dtype2,fname=fname2,cts2,ctserr2,pline2,ilines2
;now do some checks
if dtype1 ne dtype2 then begin
 print,' CANNOT MIX COUNTS & CTS/S - ABORTING '
 goto, fin
endif
head1=strarr(100) & head2=strarr(100)
;find some indices for crucial lines in file1
tline=' '
idet1=where(strmid(pline1,0,5) eq ' Dete')
idet2=where(strmid(pline2,0,5) eq ' Dete')
if (pline1(idet1))(0) ne (pline2(idet2))(0) then begin
 print,pline1(idet1)
 print,pline2(idet2)
 print,' ********* WARNING DIFFERENT DETECTORS *********** '
endif 
indat1=where(strmid(pline1,0,10) eq ' No. of da')
indat2=where(strmid(pline2,0,10) eq ' No. of da')
line=str_sep((pline1(indat1))(0),':')
ndat1=line(1)
line=str_sep((pline2(indat2))(0),':')
ndat2=line(1)
if ndat2 ne ndat1 then begin
 print,'No. of data bins   (file1) ',ndat1
 print,'No. of data bins   (file2) ',ndat2
 print,'Different Number of data bins - ABORTING'
 goto, fin
endif
iudat1=where(strmid(pline1,0,10) eq ' Unbinned ')
iudat2=where(strmid(pline2,0,10) eq ' Unbinned ')
line=str_sep((pline1(iudat1))(0),':')
udat1=line(1)
line=str_sep((pline2(iudat2))(0),':')
udat2=line(1)
if udat1 ne udat2 then begin
 print,' Unbinned data channels (file1) ',udat1
 print,' Unbinned data channels (file2) ',uda21
 print,' Different Nuber of Unbinned channels - ABORTING '
 goto, fin
endif 
print,' ** Make sure the grouping cards of both files are compatible **'
hline=pline1
;reset some stuff
ibfile=where(strmid(pline1,0,12) eq ' Background:')
hline(ibfile)=' Background: none '
icorr=where(strmid(pline1,0,12) eq ' Correction:') 
hline(icorr)=' Correction: none '
itime=where(strmid(pline1,0,12) eq ' Integration') 
hline(itime)=' Integration time(secs) :  1.00 ' 
ibsf=where(strmid(pline1,0,13) eq ' Background s') 
hline(ibsf)=' Background scale factor:       0.000 '
icorf=where(strmid(pline1,0,13) eq ' Correction s')
hline(icorf)=' Correction scale factor:       0.000'
iarea=where(strmid(pline1,0,12) eq ' On source a')
hline(iarea)=' On source area (cm**2) :      1.00'
ipha=where(strmid(pline1,16,3) eq 'pha')
pstr=hline(ipha)  
ptype=strmid(hline(ipha),16,12)
if ptype(0) eq 'pha data    ' then $ 
hline(ilines1-1)=' ***** Package "pha per sec " *****
;now write to the new file
openw,2,'outtemp.ascii'
for k=0l,ilines1-1 do begin
 printf,2,hline(k)
endfor
if ptype(0) eq 'pha data    ' then begin
extline=' chan  pha per sec error    chan  pha per sec error    chan  '
extline=extline+'pha per sec error'
printf,2,extline
endif
divcts=fltarr(ndat1) & diverr=divcts
for j=0l,ndat1-1 do begin
 if cts2(j) gt 0.0 and cts1(j) gt 0.0 then begin
  divcts(j)= cts2(j)/cts1(j)
  diverr(j) = divcts(j)*((ctserr1(j)/cts1(j))+(ctserr2(j)/cts2(j)))
 endif
endfor
ploterr,divcts,diverr,psym=3
;how many lines of data
nlines=ndat1/3 & nrem=ndat1 - 3*nlines
print,' nlines and nrem ',nlines,nrem
for j=0l,nlines-1 do begin
 i1=3*j & i2=i1+1 & i3=i1+2
 printf,2,format='(3(1X,I4,2X,E9.3,3X,E8.2))',i1+1,divcts(i1),$
 diverr(i1),i2+1,divcts(i2),diverr(i2),i3+1,divcts(i3),diverr(i3)
endfor
if nrem eq 2 then begin
 i1=ndat1-2 & i2=ndat1-1
 printf,2,format='(2(1X,I4,2X,E9.3,3X,E8.2))',i1+1,divcts(i1),$
 diverr(i1),i2+1,divcts(i2),diverr(i2)
endif
if nrem eq 1 then begin
 i1=ndat1-1
 printf,2,format='(1X,I4,2X,E9.3,3X,E8.2)',i1+1,divcts(i1),$
 diverr(i1)
endif
tot=total(divcts)
toterr=total(diverr*diverr)
if toterr gt 0. then toterr =sqrt(toterr)
;tot=1.0 & toterr=0.10 
printf,2,format='(A20,F9.2,1X,A2,1X,F9.2)',' Total counts/sec = ',$
tot,' +- ',toterr
close,2
fin: print,' Done '
com='mkpha outtemp.ascii '+outname
spawn,com
;spawn,'rm -f outtemp.ascii'
return
end
