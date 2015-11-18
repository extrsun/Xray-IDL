pro fitsdump,name,nrec,h,tab,unit,dumphdrs=dumphdrs
;
; procedure to dump data stored in FITS table file
; nrec = number of records to dump (default = 5)
;
if (n_elements(dumphdrs) eq 0) then dumphdrs=0   ;default is no 
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' FITSDUMP, name, nrec (5), ( H, TAB,) unit (1)'
  print,'         ( dumphdrs = dumphdrs )'
  return
endif
if (npar lt 5) then unit = 1
if (npar lt 2) then nrec = 5
;
; read XTENSION keyword from header to see what kind of file it is
;
print,' '
print,' Reading input file ',name
strd,im,h,name
im = 0
xtens = sxpar(h,'XTENSION','XTENSION parameter not found',count=count)
xtens = strtrim(xtens,2)
;
if ( (xtens eq 'A3DTABLE') or (xtens eq 'BINTABLE') ) then begin
  tbread,name,h,tab,unit
  tbhelp,h
endif 
if (xtens eq 'TABLE') then begin
  ftread,name,h,tab,unit
  fthelp,h
endif
;
if (dumphdrs ne 0) then begin         ;print out the header
  print,' '
  hprint,h
  print,' '
endif           ;dumping headers
;
naxis = sxpar(h,'NAXIS*')
nrow = fix(strtrim(naxis(1),2))
if (nrec gt nrow) then nrec = nrow      ;can't dump more rows than there are
;
; this next part is (mostly) duplicated from TBHELP
;
nf = sxpar(h,'tfields')
ttype = strarr(nf) 
tform = ttype
key = strmid(h,0,5)
for i = 1, n_elements(h)-1 do begin
  if (key(i) eq 'TTYPE') then begin
    j = fix(strtrim(strmid(h(i),5,3),2))
    ttype(j-1) = strmid(h(i),10,20)
  endif
  if (key(i) eq 'TFORM') then $
    tform(fix(strtrim(strmid(h(i),5,3),2))-1) = strtrim(strmid(h(i),10,20),2)
endfor
;
; now dump out the results
;
print,' '
print,nrec,format='(1x,"Field",3x,"Name",5x,"Records 1 to",i3)'
print,' '
for i=0,nf-1 do begin 
  xtype = gettok(ttype(i),"'")
  xform = gettok(tform(i),"'") & xform = strtrim(xform,2)
  num = ' '+string(format='(I3)',i+1)+'  '
;
  if ( (xtens eq 'A3DTABLE') or (xtens eq 'BINTABLE') ) then $
     val = tbget(h,tab,i+1)
  if (xtens eq 'TABLE') then val = ftget(h,tab,i+1)
  print,num,xtype,val(0:nrec-1)
endfor
;
return
end          ;pro fitsdump
