pro readbbxrt,dname=dname,fname=fname,tname=tname,qname=qname,slist
if n_params(0) eq 0 then begin
 print,'readbbxrt,dname=dname,fname=fname,tname=tname,qname=qname,slist'
 print,' dname: name of directory containing event files'
 print,' fname: name of file with list of event files '
 print,' tname: name of file which will contain GTIs '
 print,' qname: qdp rootname for rates '
 retall
end
if n_elements(dname) eq 0 then begin
 dname=' '
 read,' Enter name of directory containing event files ',dname
endif
if n_elements(fname) eq 0 then begin
 fname=' '
 read,' Enter name of events file '
endif
if n_elements(tname) eq 0 then begin
 tname=' '
 read,' Enter name of output GTI file ;
endif
if n_elements(qname) eq 0 then begin
 qname=' '    
 read,' Enter rootname of output QDP lightcurves'
endif
gtimx=1000
allgti_start=dblarr(gtimx)
allgti_stop=dblarr(gtimx)
ngti=0l
;open list file
openr,1,fname
gtiname=tname+'bbxrt_all.gti'
openw,2,gtiname
i=0
while not eof(1) do begin
 evefile=' '
 readf,1,evefile
 i=i+1
 tab1=readfits(evefile,h1,ext=1)
 tab2=readfits(evefile,h2,ext=2)
 bbxrtlist,h1,tab1,plist
 if i eq 1 then slist=plist else slist=[slist,plist]
 start=tbget(h2,tab2,'START') & stop=tbget(h2,tab2,'STOP')
 ns=(size(start))(1)      
 allgti_start(ngti:ngti+ns-1)=start(0:ns-1)
 allgti_stop(ngti:ngti+ns-1)=stop(0:ns-1)
 ngti=ngti+ns
 forprint,format='(4(F15.3,2X),I3)',start,stop
endwhile
close,1
tz=allgti_start(0:0) & ityp=1
for k=0l,ngti-1 do begin
 t1=allgti_start(k) & t2=allgti_stop(k)
 printf,2,format='(4(F15.12,2X),I3)',t1,t2,t1-tz,t2-tz
endfor
close,2
return
end
