pro mk1gti,gtifiles,cgti,texp
;Author T. Yaqoob - March 1993 -> **
;compute approximate exposure time from set of GTI files
if n_params(0) eq 0 then begin 
 print,'MK1GTI , GTIFILES, CGTI, TEXP  '
 print,' *input* GTIFILES = array of GTI ascii filenames '
 print,' *output* CGTI = array of combined GTI start and stop times '
 print,' *output* TEXP = net exposure time '
retall
end
ntmax=10000
t1=0.0d0 & t2=0.0d0
;print list of gti files
nfiles=n_elements(gtifiles) & fnum=indgen(nfiles)+1 
dum=strarr(nfiles) & for k=0,nfiles-1 do dum(k)=' '
print,' GTI File List is : '
forprint,fnum(0:nfiles-1),dum(0:nfiles-1),gtifiles(0:nfiles-1)
aga: read,'Enter highest number of file to use in list ',ngti
if ngti gt nfiles then goto, aga
if ngti le 0 then goto, fin
for i=0,ngti-1 do begin
tstart=dblarr(ntmax) & tstop=dblarr(ntmax) 
 fname=gtifiles(i)
 openr,1,fname
j=0l
 while not (eof(1)) do begin
  readf,1,format='(4(F15.3,2X),I3)',t1,t2,d1,d3,ityp
; print,format='(2(F15.3,1X))',t1,t2
 tstart(j)=t1 & tstop(j)=t2 & j=j+1
; print,j,tstart(j-1),tstop(j-1)
 endwhile
 ntimes=j & ntim=ntimes
 if i eq 0 then begin
	tint_1=dblarr(1,2) & tint_1(0,0)=min(tstart(0:ntimes-1))*0.9
	tint_1(0,1)=1.1*max(tstop(0:ntimes-1))
; print,'start stop ',tint_1(0,0),tint_1(0,1)
 endif
 if i gt 0 then begin
	tint_1=tout
 endif
 tint_2=dblarr(ntimes,2) 
 tint_2(*,0)=tstart(0:ntimes-1) & tint_2(*,1)=tstop(0:ntimes-1)
; SORT AND CHECK GTIs FOR OVERLAPS [i.e. OR the intervals]
orgti,tint_2,tint_3,ntout
 combtint,tint_1,tint_3,tout & tsiz=n_elements(tout)
close,1
 if tsiz le 0 then begin
  print,' NO GTI INTERVALS -> NO DATA CAN BE SELECTED: RETURNING '
  RETURN
 endif
 npt=(size(tout))(1)
 texp=0.0D0 & for ii=0l,npt-1 do texp=texp+(tout(ii,1)-tout(ii,0))
 print,' Exposure time on round ',i+1,' = ',texp,' (secs)'
endfor
cgti=tout
fin: return
end
