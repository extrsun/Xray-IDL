pro radec_out,sra,sdec,sradec,ih,im,is,jd,jm,js,dig=dig,iauname=iauname
;+
; Convert the RA and Dec of sources into a nice format plus the IAU name 
; for a final output
;
;*Inputs:
; sra, sdec - RA and Dec in deg
; dig - the number of digits to be kept for the RA output
;	if given (only 1 or 2), the IAUID will be recalculated
;
;*Outputs:
; sradec - output RA and Dec combined string vector
; ih,im,is,jd,jm,js - numerical values of the RA and Dec vector
; iauname - output IAU name vector (works only for dig=1 and 2)
;
; written by wqd, 6/27/2001
; revised to move sra,sdec from the keywords. wqd 12/30/2001
;
;-
if n_params() eq 0 then begin
print,'Calling Seq. - radec_out,sra,sdec,sradec,ih,im,is,jd,jm,js'
print,',dig=dig,iauname=iauname'
return
endif
trans_degree,sra,sdec,ih,im,is,jd,jm,js,/deg
ns=n_elements(ih)
sradec=strarr(ns)
iauname=strarr(ns)
if n_elements(dig) eq 0 then dig=2 
for k=0,ns-1 do begin
 if sign(jd(k)) eq 1 then sig='+' else sig='-'
 iis=fix(is(k))
 iisr=nint((is(k)-iis)*10^(dig+1))
 iisrc=fix((is(k)-iis)*10^dig)
; if iisr eq 10^dig then begin
 if iisr eq 10^(dig+1) then begin
 	iis=iis+1
 	iisr=0
 endif
 jjs=fix(js(k))
 jjsr=nint((js(k)-jjs)*10^dig)
 jjsrc=fix((js(k)-jjs)*10^(dig-1))
; if jjsr eq 10^(dig-1) then begin
 if jjsr eq 10^(dig) then begin
	jjs=jjs+1
	jjsr=0
 endif
 case dig of
  1: begin
 	sradec(k)=string(ih(k),'(i2.2)')+' '+string(im(k),'(i2.2)') $
 	+' '+string(iis,'(i2.2)')+'.'+string(iisr,'(i2.2)')+' & ' $
 	+sig+string(abs(jd(k)),'(i2.2)')+' '+string(jm(k),'(i2.2)') $
	+' '+string(jjs,'(i2.2)')+'.'+string(jjsr,'(i1.1)')

 	iauname(k)='J'+string(ih(k),'(i2.2)')+string(im(k),'(i2.2)') $
 	+string(iis,'(i2.2)')+'.'+string(iisrc,'(i1.1)') $
 	+sig+string(abs(jd(k)),'(i2.2)')+string(jm(k),'(i2.2)') $
	+string(jjs,'(i2.2)')
	end
  2: begin
 	sradec(k)=string(ih(k),'(i2.2)')+' '+string(im(k),'(i2.2)') $
 	+' '+string(iis,'(i2.2)')+'.'+string(iisr,'(i3.3)')+' & ' $
 	+sig+string(abs(jd(k)),'(i2.2)')+' '+string(jm(k),'(i2.2)') $
	+' '+string(jjs,'(i2.2)')+'.'+string(jjsr,'(i2.2)')

 	iauname(k)='J'+string(ih(k),'(i2.2)')+string(im(k),'(i2.2)') $
 	+string(iis,'(i2.2)')+'.'+string(iisrc,'(i2.2)') $
 	+sig+string(abs(jd(k)),'(i2.2)')+string(jm(k),'(i2.2)') $
	+string(jjs,'(i2.2)')+'.'+string(jjsrc,'(i1.1)')
	end
  else: sradec(k)='J'+string(ih(k),'(i2.2)')+' '+string(im(k),'(i2.2)') $
 	+' '+string(iis,'(i2.2)')+'.'+string(iisr,'(i2.2)')+' & ' $
 	+sig+string(abs(jd(k)),'(i2.2)')+' '+string(jm(k),'(i2.2)') $
	+' '+string(jjs,'(i2.2)')+'.'+string(jjsr,'(i1.1)')
 endcase
endfor
return
end
