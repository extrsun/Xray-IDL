pro tab_gsel,rac,decc,glr,gbr,infile,outfile,deci=deci,nheadline=nheadline,ntailline=ntailline,equi=equi,tc=tc,tr=tr,excl=excl
;+
; select entries in the chandra archive data file
;
; rac, dec - character numbers in the data file for Ra and Dec
; glr, gbr - galactic coordinate ranges
; excl - if set, the glr and gbr are the ranges to be excluded
; deci - if set, rac and dec are in decimal units of deg
; nheadline, ntailline - begining and ending numbers of lines in the
;                        file
; tc - character numbers in the data file for exposure
; tr - exposure ranges
;
;*examples:
;  tab_gsel,[70,10],[80,8],[0,360],[-5.,5],/deci,nh=11,'public.txt','test.txt',nt=2,tc=[90,9],tr=[40,1000]
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - tab_gsel,rac,decc,glr,gbr,infile,outfile,deci=deci,nheadline=nheadline,ntailline=ntailline,equi=equi,tc=tc,tr=tr,excl=excl'
return
endif
if n_elements(equi) eq 0 then  equi=2000

str=''
k=0
openr,un,infile,/get
if n_elements(nheadline) ne 0 then for i=1,nheadline do readf,un,str
while not eof(un) do begin
    readf,un,str
    if k eq 0 then text=str else text=[text,str]
    k=k+1
endwhile
free_lun,un
nl=k

if n_elements(ntailline) ne 0 then begin
    nl=k-ntailline
    text=text(0:nl-1)
endif
raq=strmid(text,rac(0),rac(1))
decq=strmid(text,decc(0),decc(1))

if keyword_set(deci) eq 0 then $
  stringad,raq+decq,raq,decq  $  ;convert string to deg
else begin 
    raq=double(raq) & decq=double(decq) 
endelse

glactc,raq,decq,equi,gl,gb,1,/deg
sel=where(gl ge glr(0) and gl lt glr(1) and $
            gb ge gbr(0) and gb lt gbr(1),nsel)

if keyword_set(excl) ne 0 then begin
    esel=sel
    sel=lindgen(nl)
    remove,esel,sel
    nl=nl-nsel
endif 
text=text(sel)+' '+strtrim(gl(sel),2)+' '+strtrim(gb(sel),2)

if n_elements(tr) ne 0 then begin
    expt=strmid(text,tc(0),tc(1))
    sel=where(expt ge tr(0) and expt lt tr(1),nl)
    if nl eq 0 then stop,'no items within tr: ',tr
    text=text(sel)+' '+strtrim(expt(sel),2)
endif

nout=n_elements(outfile)
for k=0,nl-1 do print,text(k)
if nout ne 0 then begin
    openw,un,outfile,/get
    for k=0,nl-1 do printf,un,text(k)
    free_lun,un
endif
print,'Total lines selected= ',nl
stop
return
end
