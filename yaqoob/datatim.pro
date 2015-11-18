pro datatim,tname=tname,plist
if n_params(0) eq 0 then begin
print,' DATATIM, tname=tname, plist '
print,' Up-date GTI ascii file using light curve from plist '
retall
end
if n_elements(tname) eq 0 then begin
 tname=' '
 read,' Enter name of time intervals file',tname
endif
;define time interval arrays
tint=dblarr(2,1000)
ttyp=intarr(1000)
;open the file
openr,10,tname
i=0
while not(eof(10)) do begin
	readf,10,t1,t2,d1,d2,t3 & tint(0,i)=t1 & tint(1,i)=t2 & ttyp(i)=t3
	print,tint(0,i),tint(1,i),d1,d2,ttyp(i)
	i=i+1
endwhile
ntimes=i
close,10
print,' number of time intervals read in = ',ntimes
$rm -f "tname"
;make light curve
print,' PHA values are in the range '
print,min(plist.pha),max(plist.pha)
read,' Enter lower and upper inclusive PHA values for lightcurve plot',phlo,phhi
plist=plist(where((plist.pha ge phlo) and (plist.pha le phhi)))
read,' Enter time binsize in seconds ',bin
time=plist.time
torder=sort(time)
tzero=time(torder(0))
timesec=time-tzero
cts=histogram(timesec,binsize=bin)/float(bin)
nbins=(size(cts))(1) & print,' Number of bins = ',nbins
taxis=findgen(nbins)*float(bin)+0.5*bin
plot,taxis,cts,/xst,/yst
lp1: plot,taxis,cts,/xst,/yst
	read,' Enter new min x and max x (0,0 to stop)',xmin,xmax
	if xmin+xmax eq 0 then goto, opt1
 	plot,taxis,cts,xrange=[xmin,xmax]
lp2: print,' Use cursor to mark tstart and tend for this interval '
cursor,xc,yc,3
tint(0,i)=xc
cursor,xc,yc,3
tint(1,i)=xc & tint(1,i) = tint(1,i) > 0.0d0 & ttyp(i) = 4
print,tint(0,i),tint(1,i)
print,tzero+tint(0,i),tzero+tint(1,i),ttyp(i)
i=i+1 
print,' Press r to rescale x-axis q to quit' & dum=get_kbrd(1)
if dum eq 'r' then goto, lp1
if dum eq 'q' then goto, opt1
goto, lp2
opt1: print,' i = ',i
forprint,tint(0,0:i-1),tint(1,0:i-1)
ntimes=i
openw,10,tname
for j=0,ntimes-1 do begin
printf,10,tint(0,j)+tzero,tint(1,j)+tzero,tint(0,j),tint(1,j),ttyp(j)
endfor
close,10
end
