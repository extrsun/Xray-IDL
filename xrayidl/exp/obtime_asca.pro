pro obtime_asca,infile,obstart,obend,obtime,gaplimit=gaplimit
;+
; group individual OBs into time segments separated by a gaplimit specified.
; infile - input file name to read individual OBs
; gaplimit - the minimum gas between consective segments in units of sec
; -
if n_elements(gaplimit) eq 0 then gaplimit=1.e3
get_actime,start,end1,fname=infile

tottime=total(end1-start)
n_interval=n_elements(start)
print,'original n_interval,tottime = ', n_interval,tottime
tottime=0
nob=0
obtime=dblarr(1000)
obstart=obtime
obend=obtime
obstart(0)=start(0)
	 print,'start(i),end1(i),nob,time,gap,tottime'
	for i=0,n_interval-1 do begin
		if i eq 0 then gap=0 else gap=start(i)-end1(i-1)
		if gap gt gaplimit then begin
			obend(nob)=end1(i-1)
			nob=nob+1
			obstart(nob)=start(i)
		endif
		time=end1(i)-start(i)
		obtime(nob)=obtime(nob)+time
		tottime=tottime+end1(i)-start(i)
		print,start(i),end1(i),nob,time,gap,tottime
	endfor
obend(nob)=end1(n_interval-1)
obstart=obstart(0:nob)
obend=obend(0:nob)
print,'nob,obstart,obend,obtime,obdif ='
obdif=obend-obstart
for n=0,nob do print,n,obstart(n),obend(n),obtime(n),obdif(n)
return
end
