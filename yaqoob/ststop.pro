pro ststop,xten,dir=dir,flist=flist,start,stop,xte=xte
if n_params(0) eq 0 then begin
 print,'ststop,xten,dir=dir,flist=flist,start,stop,xte=xte'
 print,'Take a list of FITS filenames and extract the start and stop'
 print,'times.'
 retall
end
if n_elements(flist) eq 0 then begin
 flist=' '
 read,'Enter name of ascii file containing filenames ',flist
endif
if n_elements(dir) eq 0 then begin
 dir=' '
 read,'Enter name of directory containing the data files ',dir
endif
nfiles=0l
openr,1,flist
ntmax=10000l
t1=fltarr(ntmax) & t2=t1
while (not eof(1)) do begin
 fname=' '
 readf,1,fname
 fname=dir+fname
 tab=readfits(fname,h,ext=xten)
 tableget,fname,tt1,'tstart',xten,xte=xte
 tableget,fname,tt2,'tstop',xten,xte=xte 
 t1(nfiles)=tt1 & t2(nfiles)=tt2
 nfiles=nfiles+1l
 print,nfiles,t1,' ',t2
endwhile 
start=t1(0:nfiles-1l)
stop=t2(0:nfiles-1l)
close,1
return
end
