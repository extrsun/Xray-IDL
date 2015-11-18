pro obtime,infile,outfile,gaplimit=gaplimit
;+
; group individual OBs into time segments separated by a gaplimit specified.
; infile - input file name to read individual OBs
; outfile - output file name to contain the old info, plus both the total 
;	observing time (obtime) and the total duration (obdif) of each segment.
; gaplimit - the minimum gas between consective segments in units of sec
; -
if n_elements(gaplimit) eq 0 then gaplimit=3.6e3

if n_elements(infile) eq 0 then infile=!seq_no+'_actime.dat'
openr,unit,infile,/get_lun
readf,unit,n_interval,tt
time=lonarr(2,n_interval)
 readf,unit,time
start=time(0,*)
end1=time(1,*)
free_lun,unit

tottime=total(end1-start)
if n_elements(outfile) eq 0 then outfile=!seq_no+'_obtime.dat'
openw,unit,outfile,/get_lun
print,n_interval,tottime
tottime=0
nob=0
obtime=lonarr(1000)
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
print,'nob,obstart,obend,obtime,obdif ='
printf,unit,nob+1,tt
obdif=obend-obstart
for n=0,nob do print,n,obstart(n),obend(n),obtime(n),obdif(n)
for n=0,nob do printf,unit,n,obstart(n),obend(n),obtime(n),obdif(n)
free_lun,unit
return
end
