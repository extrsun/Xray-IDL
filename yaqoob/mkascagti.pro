pro mkascagti,fname1,fname2,type
;Author T. YAqoob - March 1993->**
if n_params(0) eq 0 then begin
 print,'mkascagti,fname1,fname2,type'
 print,'Convert a 2 column ascii GTI files into a 5 column '
 print,'file which can be read by ASCA IDL routines '
 print,'*FNAME1 - input filename '
 print,'*FNAME2 - new output filename '
 print,'*TYPE   - integer coding the origin of the GTIs '
 retall
endif
if n_elements(fname1) eq 0 then begin
 fname1=' '
 read,'Enter input filename ',fname1
endif
if n_elements(fname2) eq 0 then begin
 fname2=' '
 read,'Enter output filename ',fname2
endif
readcol,fname1,c1,c2
nlines=n_elements(c1)
openw,1,fname2
type=fix(type)
for k=0,nlines-1 do $
printf,1,format='(4(F15.3,2X),I3)',c1(k),c2(k),c1(k)-c1(0),c2(k)-c2(0),type
close,1 
return
end
