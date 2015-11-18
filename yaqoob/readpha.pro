pro readpha,dtype,fname=fname,cts,ctserr,pline,ilines
if n_params(0) eq 0 then begin
 print,'readpha,dtype,fname=fname,cts,ctserr,pline,ilines'
 print,'Read an XSPEC pha file '
 print,'OUTPUT dtype =0 if COUNTS & dtype = 1 if  CTS/S'
 print,'OUTPUT in cts and ctserr'
 print,'File header in STR array pline (first ilines elements)'
 retall
end 
if n_elements(fname) eq 0 then begin
 fname=' '
 read,'Enter name of XSPEC file to read ',fname
endif
;make template ascii file first
spawn,'rm -f temppha.ascii'
com='rdpha '+fname+' temppha.ascii'
spawn,com
;read each line of the file until we get to the file info package
openr,1,'temppha.ascii'
fline=' '
fpack=0
ilines=0l
nmax=10000l
pline=strarr(nmax)
while fpack eq 0 do begin
 readf,1,fline
 pline(ilines)=fline & ilines=ilines+1
 if strmid(fline,1,1) eq '*' then begin
  sepfline=str_sep(fline,'"')
  if sepfline(1) eq 'file info   ' then fpack=1
 endif
endwhile
print,'reached file info package'
print,fline
;reached file info package. read some info
doneinfo=0
nbins=0 & itime=0.
while doneinfo eq 0 do begin
 readf,1,fline
 pline(ilines)=fline & ilines=ilines+1
 sepfline=str_sep(fline,':')
 if sepfline(0) eq ' No. of data bins       ' then nbins=sepfline(1)
 if sepfline(0) eq ' Integration time(secs) ' then itime=float(sepfline(1))
 if nbins gt 0 and itime gt 0 then doneinfo=1
 print,'nbins and line time ',nbins,itime
endwhile
;how many lines will the data have in the file?
nlines=nbins/3 & nrem=nbins-(nlines*3)
print,'data will have ',nlines,' lines remainder ',nrem
;now find the data package
fdata=0
while fdata eq 0 do begin
readf,1,fline
pline(ilines)=fline & ilines=ilines+1
if strmid(fline,1,1) eq '*' then begin
 sepfline=str_sep(fline,'"')
 if sepfline(1) eq 'pha per sec ' then begin
  dtype = 1 & fdata=1
 endif
 if sepfline(1) eq 'pha data    ' then begin
  dtype = 0 & fdata=1
 endif
 print,sepfline(1)
endif
endwhile
if dtype eq 1 then begin
 readf,1,fline
 pline(ilines)=fline & ilines=ilines+1
endif
;now read the data
cts=fltarr(nbins) & ctserr=cts
for k=0l,nlines-1 do begin
 inx = k*3 
 if dtype eq 0 then begin
  readf,1,chan1,d1,chan2,d2,chan3,d3
  cts(inx)=d1 & cts(inx+1)=d2 & cts(inx+2)=d3
  if d1 gt 0. then ctserr(inx)=sqrt(d1) 
  if d2 gt 0. then ctserr(inx+1)=sqrt(d2)
  if d3 gt 0. then ctserr(inx+2)=sqrt(d3)
  if k eq (nlines-1) then begin
   if nrem eq 2 then begin
    readf,1,c4,d4,c5,d5
    cts(inx+3)=d4 & cts(inx+4)=d5
    if d4 gt 0. then ctserr(inx+3)=sqrt(d4)
    if d5 gt 0. then ctserr(inx+4)=sqrt(d5)
   endif
   if nrem eq 1 then begin
    readf,1,c4,d4
    cts(inx+3)=d4 
    if d3 gt 0. then ctserr(inx+3)=sqrt(d4)
   endif
  endif
 endif
 if dtype eq 1 then begin
  readf,1,c1,d1,e1,c2,d2,e2,c3,d3,e3
  cts(inx)=d1 & cts(inx+1)=d2 & cts(inx+2)=d3
  ctserr(inx)=e1 & ctserr(inx+1)=e2 & ctserr(inx+2)=e3
  if k eq (nlines-1) then begin
   if nrem eq 2 then begin
    readf,1,c4,d4,e4,c5,d5,e5
    cts(inx+3)=d4 & ctserr(inx+3)=e4
    cts(inx+4)=d5 & ctserr(inx+4)=e5
   endif
   if nrem eq 1 then begin
    readf,1,c4,d4,e4
    cts(inx+3)=d4 & ctserr(inx+3)=e4
   endif
  endif
 endif
endfor
print,'No. of lines read before the data: ',ilines
close,1
return
end
