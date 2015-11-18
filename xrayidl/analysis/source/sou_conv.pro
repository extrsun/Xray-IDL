pro sou_conv,infile,outfile,slow=slow,type=type
;-
; convert the GC source file of Galactic coordinates into RA and Dec required
; by Snowden's mask software (type=0 ---def) or into a nice format for final
; output (type=1).
;+
if n_params() eq 0 then begin
print,'Calling Seq = sou_conv,infile,outfile,slow=slow,type=type'
return
endif
if n_elements(slow) eq 0 then slow=4
if n_elements(type) eq 0 then type=0
source_info,sn,gl,gb,ston,cntr,souf=infile,/deg,slow=slow
glactc,sra,sdec,2000.,gl,gb,2
sra=sra*15.
trans_dist,0.,0.,gl,gb,xp,yp,pixsize=30.,/deg
trans_degree,sra,sdec,ih,im,is,jd,jm,js,/deg
js=nint(js)
xp=256.5+xp
yp=256.5+yp
openw,un,outfile,/get
ns=n_elements(sn)
for k=0,ns-1 do begin
if type eq 0 then begin
	printf,un,k+1,xp(k),yp(k),sra(k),sdec(k),cntr(k),ston(k),ih(k),im(k),is(k),jd(k),jm(k),js(k),gl(k),gb(k),sn(k),form='(i5,2i6,2f9.4,f12.7,f13.8,2i3,f5.1,i4,2i3,2f10.4,i4)'
endif else begin
if sign(jd(k)) eq 1 then sig='+' else sig='-'
ss='RXJ'+string(ih(k),'(i2.2)')+string(im(k)+is(k)/60.,'(f4.1)')+sig+string(abs(jd(k)),'(i2.2)')+string(jm(k)+nint(js(k)/60.),'(i2.2)')
iis=fix(is(k))
iisr=nint((is(k)-iis)*10)
if iisr eq 10 then begin
	iis=iis+1
	iisr=0
endif
	printf,un,k+1,ss,ih(k),im(k),iis,'.',iisr,jd(k),jm(k),js(k),gl(k),gb(k),cntr(k),ston(k),form='(i5.3,a15,3i3.2,a1,i1,i4.2,2i3.2,2f8.3,f8.4,f6.1)'
endelse
endfor
free_lun,un
return
end