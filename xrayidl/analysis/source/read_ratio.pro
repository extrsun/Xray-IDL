pro read_ratio,fname,c1,c2,c3,c1e,c2e,c3e,sn,ston,cntr,cntre,rlo=rlo,rhi=rhi,text=text
;,perr=perr
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - read_ratio,fname,c1,c2,c3,c1e,c2e,c3e,sn,ston
print,',cntr,cntre,rlo=rlo,rhi=rhi,text=text'
return
endif

if n_elements(rlo) eq 0 then rlo=0.
if n_elements(rhi) eq 0 then rhi=60.
ns=1000
text=strarr(ns)
k=0
textc=''
openr,un,fname,/get_lun
while eof(un) ne 1 do begin
	readf,un,textc,format='(a180)'
	dis=strmid(textc,115,8)
	if dis gt rlo and dis le rhi then begin 
		text(k)=textc
		k=k+1
	endif
endwhile
free_lun,un
text=text(0:k-1)
sn=fix(strmid(text,0,3))
clo=35
ston=float(strmid(text,clo,8)) & clo=clo+10
cntr=float(strmid(text,clo,8)) & clo=clo+10
cntre=float(strmid(text,clo,8)) & clo=clo+10
c1=float(strmid(text,clo,8)) & clo=clo+10
c1e=float(strmid(text,clo,8)) & clo=clo+10
c2=float(strmid(text,clo,8)) & clo=clo+10
c2e=float(strmid(text,clo,8)) & clo=clo+10
c3=float(strmid(text,clo,8)) & clo=clo+10
c3e=float(strmid(text,clo,8)) & clo=clo+10
;perr=float(strmid(text,115,8))
return
end