PRO get_headinfo,header,xsize,ysize,cra,cdec,cpx,cpy,delx,dely,equi=equi,blank=blank,n3d=n3d
;+
; get information from a fits header
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - get_headinfo,header,xsize,ysize,cra,cdec,cpx,cpy'
print,',delx,dely,equi=equi,blank=blank,n3d=n3d'
return
endif 

if sxpar(header,'naxis') gt 2 then begin
	if n_elements(n3d) eq 0 then begin
		print,'The dimesion as indicated in the header is > 2'
		print,'Is it a 3-D image? If yes, specify n3d keyword.'
		return
	endif else begin
		case n3d of
			1: begin
				x3d='2' & y3d='3'
			   end
			2: begin
				x3d='1' & y3d='3'
			   end
			3: begin
				x3d='1' & y3d='2'
			   end
		endcase
	endelse
endif else begin
	x3d='1' & y3d='2'
endelse
xsize=sxpar(header,'naxis'+x3d)
ysize=sxpar(header,'naxis'+y3d)
BLANK  = sxpar(header,'blank')
cra=sxpar(header,'crval'+x3d)
cdec=sxpar(header,'crval'+y3d)
cpx=sxpar(header,'crpix'+x3d)
cpy=sxpar(header,'crpix'+y3d)
CDELTV = [sxpar(header,'cdelt'+x3d,count=count),sxpar(header,'cdelt'+y3d,count=count)]
if  count eq 0 then $
	cdeltv=[sxpar(header,'cd1_1'),sxpar(header,'cd2_2')]	
delx=cdeltv(0)
dely=cdeltv(1)
;if abs(dely+delx) gt 1.e-6  then print,'***WARNING: delx not = dely: ',delx,dely
CROTAV = [sxpar(header,'crota'+x3d),sxpar(header,'crota'+y3d)]
equi=sxpar(header,'equinox',count=count)
if count eq 0 then begin
	equi=sxpar(header,'epoch',count=count)
	if count eq 0 then print,'Warning: No equninox keyword in the hearder!'
    endif
; remove header such as 'J' in "J2000':
equic=strtrim(equi,2)
if strlen(equic) eq 5 then $
  equi=strmid(equic,strlen(equi)-4,4)
sel=where(crotav ne 0,nsel)
if nsel then print,'crotav =',crotav
;
RETURN
END
