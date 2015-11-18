pro exptime,gtifiles,texp
;compute approximate exposure time from set of GTI files
if n_params(0) eq 0 then begin 
 print,'EXPTIME , GTIFILES, TEXP  '
retall
end
ntmax=1000
;print list of gti files
nfiles=n_elements(gtifiles) & fnum=indgen(nfiles)+1 
dum=strarr(nfiles) & for k=0,nfiles-1 do dum(k)=' '
print,' GTI File List is : '
forprint,fnum(0:nfiles-1),dum(0:nfiles-1),gtifiles(0:nfiles-1)
read,'Enter highest number of file to use in list ',ngti
if ngti le 0 then goto, fin
for i=0,ngti-1 do begin
tstart=dblarr(ntmax) & tstop=dblarr(ntmax) & tint=dblarr(ntmax)
 fname=gtifiles(i)
; read,' Input GTI filename ',fname
 openr,1,fname
j=0l
 while not (eof(1)) do begin
  readf,1,t1,t2,d1,d3,ityp
 tstart(j)=t1 & tstop(j)=t2 & tint(j)=t2-t1 & j=j+1
; print,j,tstart(j-1),tstop(j-1)
 endwhile
 ntimes=j & ntim=ntimes
 tbeg=tstart(0:ntimes-1) & tend=tstop(0:ntimes-1)
 tbeg=tbeg(sort(tbeg)) & tend=tend(sort(tend))
 tsbeg=tbeg-tbeg(0) & tsend=tend-tbeg(0)
; forprint,tbeg(0:ntimes-1),tend(0:ntimes-1),tsbeg(0:ntim-1),tsend(0:ntim-1) 
 if i eq 0 then begin
 stretch=tend(ntimes-1)-tbeg(0) & nbins=stretch+1 
; print,'stretch= ',stretch
 print,' No. of 1 sec bins: ',nbins
 time=tbeg(0)+dindgen(nbins) & print,time(0),time(nbins-1)
  tmask=intarr(nbins)
 endif
  for j=0l,ntimes-1 do begin
	tmask=tmask+(time ge tbeg(j) and (time lt tend(j)))
  endfor
  print,' Total exposure on round ',i,(size(where(tmask eq (i+1))))(1)-1
close,1
endfor
texp=float((size(where(tmask eq ngti)))(1)-1)
if texp le 0.0 then print,' **WARNING** zero or -ve exposure time '
fin: return
end
