pro read_reg_str,regfile,slist,row=row,factor=factor
;+
; read an XMM source region file and convert the RA, Dec, and source
; radius into a structure
;
; regfile - region file name, created by ds9 or sou_final, for
;           example.
; slist - output source lsit
; row - definition of the structure,
;       def={ra:0.0d,dec:0.0d,sradius:0.0}
; factor - conversion of the source radius unit of the region file to
; athe units of pixel; def=1./!size_pixel
;
; written by wqd, March 26, 2005
;-
if N_params() eq 0 then begin
  print,'CALLING SEQUENCE - read_reg_str,regfile,slist,row=row,factor=factor'
 return
endif
if n_elements(factor) eq 0 then $
  factor=1./!size_pixel   ;to convert from arcsec to the units of pixel
openr,un,regfile,/get
text=''
sra=[-999.0d]
sdec=sra
spara=[999.]
while eof(un) eq 0 do begin
    readagain: readf,un,text
    if strmid(text,0,1) eq '#' then goto,readagain
    shape=gettok(text,'(')
    if text eq '' then  goto,readagain
    sra=[sra,gettok(text,',')]
    sdec=[sdec,gettok(text,',')]
    spara=[spara,gettok(text,')')]
endwhile
free_lun,un
sra=sra(1:*)
sdec=sdec(1:*)
spara=spara(1:*)*factor
nct=n_elements(sra)
if n_elements(row) eq 0 then row={ra:0.0d,dec:0.0d,sradius:0.0}
slist = replicate(row,nct)
slist.ra=sra
slist.dec=sdec
slist.sradius=spara
return
end
