pro list_center,list,xobm,yobm,cob,obfile=obfile,plot=plot,xmv=xmv,xme=xme,ymv=ymv,yme=yme,tint=tint
;-
; calculate centrods of counts listed in an input list in individual
; time segments.
;
; list - list of counts, properly mastered with spec_get_dif.pro
; xobm,yobm,cob - vectors containing x and y positions of the centrods
;		and the total number of counts in individual time segments
; obfile - the name of the file containing the time seqments,
;		def=!seq_no+'_obtime.dat' as may be got from actime.pro
; writen by wqd, Dec 10, 1994
;+
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - list_center,list,xobm,yobm,cob,obfile=obfile'
print,',plot=plot,xmv=xmv,xme=xme,ymv=ymv,yme=yme,tint=tint'
return
endif 

t=list.time
list=list(sort(t))
t=list.time
x=list.x
y=list.y

if n_elements(tint) eq 0 then begin
	if n_elements(obfile) eq 0 then obfile=!seq_no+'_obtime.dat'
	openr,unit,obfile,/get
	readf,unit,nob
	tint=lonarr(2,nob)
	t1=0L
	t2=t1
	for n=0,nob-1 do begin
		readf,unit,nn,t1,t2
		tint(0,n)=t1
		tint(1,n)=t2
	endfor
	free_lun,unit
endif else begin
	sz=size(tint)
	nob=sz(2)
endelse
xobm=fltarr(nob)
yobm=xobm
cob=xobm
;
tabinv_m,t,tint(1,*),indend
indend=nint(indend,/long)
indstart=[0,indend(0:nob-2)+1]
cob=indend-indstart+1
for n=0, nob-1 do begin
	xob=x(indstart(n):indend(n))
	yob=y(indstart(n):indend(n))
;	xobm(n)=avg(xob)
;	yobm(n)=avg(yob)
	avg_median,xob,xobms
	avg_median,yob,yobms
	xobm(n)=xobms & yobm(n)=yobms
endfor
nt=total(cob)
xmv=total(xobm*cob)/nt
ymv=total(yobm*cob)/nt
;avg_median,xobm,xmv
;avg_median,yobm,ymv
xobm=xobm-xmv
yobm=yobm-ymv
xme=sqrt(total((xobm)^2)/(nob-1))
yme=sqrt(total((yobm)^2)/(nob-1))
print,'xmv,xme, ymv,yme = ',xmv,xme,' ',ymv,yme
for n=0,nob-1 do print,indstart(n),indend(n),indend(n)-indstart(n),xobm(n),yobm(n),cob(n)
if keyword_set(plot) ne 0 then begin
	plot,xobm,yrange=[5*xme,5*xme],psym=7
	oplot,yobm,psym=5
endif
end
