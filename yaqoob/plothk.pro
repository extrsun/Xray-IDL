pro plothk,mkfnames,hkx,hkname,time,hkval,rpt=rpt,xte=xte,nanval=nanval,qdpname=qdpname
;Auhtor T. Yaqoob - March 1993->**
;modified for xte August 1996
if n_params(0) eq 0 then begin
 print,'plothk,mkfnames,hkx,hkname,xax,yax,rpt=rpt,xte=xte,nanval=nanval,qdpname=qdpname'
 retall
endif
if n_elements(qdpname) eq 0 then begin
 qdpname=' '
 read,'Enter name of output QDP file ',qdpname
endif
openw,1,qdpname
printf,1,'SKIP SINGLE'
nf=n_elements(mkfnames)
agan: for k=0l,nf-1 do begin
  if xte le 0 then begin
  tab=readfits(mkfnames(k),h,ext=1)
  timtmp=tbget(h,tab,hkx)
  hktmp=tbget(h,tab,hkname)
  endif
  if xte ge 1 then begin
   fxbopen,unit,mkfnames(k),1,h
   fxbread,unit,timtmp,hkx,nanvalue=nanval
   fxbread,unit,hktmp,hkname,nanvalue=nanval
   fxbclose,unit
  endif
  if k eq 0 then begin
    time=timtmp & hkval=hktmp
  endif
  if k gt 0 then begin
    time=[time,timtmp] & hkval=[hkval,hktmp]
  endif
endfor
window,0,xsize=800,ysize=500
ymax=1.2*max(hkval) & ymin=0.8*min(hkval)
plot,xtitle=hkx,ytitle=hkname,yr=[ymin,ymax],time,hkval,psym=3
npts=(size(time))(1)
resc=' '
rscl: read,'Rescale Y axis? (y/n)',resc
if strmid(resc,0,1) ne 'y' then goto, cnt 
 print,'Click cursor defining new Ymin and Ymax '
 cursor,dumx,y1,4
 cursor,dumx,y2,4
 ymin=min([y1,y2]) & ymax=max([y1,y2])
 print,'Ymin and Ymax selected: ',ymin,ymax
; read,'Enter Ymin Ymax ',ymin,ymax
 plot,xtitle=hkx,ytitle=hkname,yr=[ymin,ymax],time,hkval,psym=3
 goto,rscl
cnt: if rpt gt 0 then begin 
 ans=' '
 read,' Plot another one ?(y/n) ',ans
 for j=0l,npts-1l do begin
  printf,1,time(j),hkval(j)
 endfor
 if ans eq 'n' then goto,fin 
 printf,1,'NO NO'
 read,' Name of HK parameter to plot on x axis ',hkx
 read,' Name of HK parameter to plot on y axis ',hkname
 goto, agan
endif
fin: close,1
return
end
