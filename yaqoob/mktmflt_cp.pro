pro mktmflt,tname=tname,qlist
if n_params(0) eq 0 then begin
print,' DATATIM, tname=tname, qlist '
print,' Create GTI ascii file using light curve from qlist '
retall
end
if n_elements(tname) eq 0 then begin
 tname=' '
 read,' Enter name of time intervals output file ',tname
endif
;define time interval arrays
tint=dblarr(2,1000)
ttyp=intarr(1000)
;make light curve
lpp: print,' PHA values are in the range '
print,min(qlist.pha),max(qlist.pha)
read,' Enter lower and upper inclusive PHA values for lightcurve ',phlo,phhi
plist=qlist(where((qlist.pha ge phlo) and (qlist.pha le phhi)))
read,' Enter time binsize in seconds ',bin
time=plist.time
torder=sort(time)
tzero=time(torder(0))
timesec=time-tzero
cts=histogram(timesec,binsize=bin)/float(bin)
mxcts=max(cts)
mrklev=1.2*mxcts
yymax=1.4*mxcts
nbins=(size(cts))(1) & print,' Number of bins = ',nbins
taxis=findgen(nbins)*float(bin)+0.5*bin
window,0,xsize=1000,ysize=600
plot,taxis,cts,/xst,/yst,yrange=[0,yymax]
ans=' '
read,'Try different binning ?',ans
if strmid(ans,0,1) eq 'y' then goto, lpp
i=0
lp1: plot,taxis,cts,/xst,/yst,yrange=[0,yymax]
	for k=0,i do begin
	  xmark = [tint(0,k),tint(1,k)] & ymark=[mrklev,mrklev]
	  oplot,xmark,ymark
	endfor
	read,' Enter new min x and max x (0,0 to stop)',xmin,xmax
	if xmin+xmax eq 0 then goto, opt1
	plot,taxis,cts,xrange=[xmin,xmax],yrange=[0,yymax]
	for k=0,i do begin
	  xmark = [tint(0,k),tint(1,k)] & ymark=[mrklev,mrklev]
	  oplot,xmark,ymark
	endfor
lp2: print,' Use cursor to mark tstart and tend for this interval '
cursor,xc,yc,3
tint(0,i)=xc
cursor,xc,yc,3
tint(1,i)=xc & tint(1,i) = tint(1,i) > 0.0d0 & ttyp(i) = 4
print,tint(0,i),tint(1,i)
print,tzero+tint(0,i),tzero+tint(1,i),ttyp(i)
xmark=[tint(0,i),tint(1,i)] & ymark=[mrklev,mrklev]
oplot,xmark,ymark
i=i+1 
print,' Press r to rescale x-axis q to quit (anything else to cont) ' 
dum=get_kbrd(1)
if dum eq 'r' then goto, lp1
if dum eq 'q' then goto, opt1
goto, lp2
opt1: print,' i = ',i
if i lt 1 then goto, fin
forprint,tint(0,0:i-1),tint(1,0:i-1)
ntimes=i
openw,10,tname
for j=0,ntimes-1 do begin
printf,10,tint(0,j)+tzero,tint(1,j)+tzero,tint(0,j),tint(1,j),ttyp(j)
endfor
close,10
tint=tint+tzero
tmflt_arr,qlist,ulist,ntimes,tint
ucts=histogram((ulist.time),binsize=bin)/float(bin)
fin: plot,taxis,ucts,/xst,/yst,yrange=[0,yymax]
	for k=0,i do begin
	  xmark = [tint(0,k),tint(1,k)] & ymark=[mrklev,mrklev]
	  oplot,xmark,ymark
	endfor
return
end
