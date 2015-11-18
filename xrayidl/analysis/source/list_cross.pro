pro list_cross,infile,soufile,sradius=sradius
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - source_apm,cra,cdec,infile,outfile'
print,',slow=slow,flow=flow,soufile=soufile,sradius=sradius'
print,',self=self,vradius=vradius,radius=radius'
return
endif
if n_elements(sradius) eq 0 then sradius=0.5
if n_elements(self) eq 0 then self=0

fac=(sradius*120.)^2
str=''
nq=1000
raq=fltarr(nq)
text=strarr(nq)
decq=raq
openr,un,soufile,/get
for i=1,6 do readf,un,str
k=0
while not eof(un) do begin
	readf,un,str,format='(a101)'
	stringad,strmid(str,22,22),rascale,decscale
	raq(k)=rascale & decq(k)=decscale
	text(k)=str
	k=k+1
endwhile
free_lun,un
raq=raq(0:k-1)
decq=decq(0:k-1)
text=text(0:k-1)

openr,un,infile,/get
for i=1,6 do readf,un,str
while not eof(un) do begin
	readf,un,str,format='(a104)'
	aa=float(strmid(str,12,4))
	radec=strmid(str,9,5)+string((aa-fix(aa))*60.)+strmid(str,16,10)
	stringad,radec,rascale,decscale
	precess,rascale,decscale,1950,2000
	trans_dist,rascale,decscale,raq,decq,xp,yp,/deg
	diss=(xp^2+yp^2)
	dis2=fac*(float(strmid(str,27,5)))^2
	sel=where(diss lt dis2,nc)
	if (nc ne 0) then begin
		forprint,str,format='(a104)'
		trans_degree,rascale,decscale,ihr,imin,xsec,ideg,imn,xsc,/deg
		print,ihr, imin, xsec, ideg, imn, xsc
		for kk=0,nc-1 do begin
		 forprint,text(sel(kk)),format='(a101)'
		 forprint,sqrt(diss(sel(kk)))/120.,xp(sel(kk))/120.,yp(sel(kk))/120.
if !debug eq 3 then stop
		endfor
	endif
endwhile
free_lun,un
end