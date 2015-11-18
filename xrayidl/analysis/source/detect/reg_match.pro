pro reg_match,oregfile,nregfile,sel=sel,rsel=rsel
;+
; Compare two regional files to determine which sources have been
; removed, used for source removal between two runs of sou_final
;
; cor - normalized plot corner from cont_grey
; hdr - the header of the plot
; oregfile, nregfile - names of the original and revised region files
;                      name
; sel, rsel - selected and removed source numbers (index+1), rsel can
;             be used as sexcl - the sou_final input for source exclusion. 
;
; written by wqd, July 3, 2005
;-
if N_params() eq 0 then begin
  print,'CALLING SEQUENCE - reg_match,oregfile,nregfile,sel=sel,rsel=rsel'
 return
endif

openr,un,oregfile,/get
text=''
xp=[-999]
yp=[-999]
while eof(un) eq 0 do begin
    oreadagain: readf,un,text
    if strmid(text,0,1) eq '#' then goto,oreadagain
    if strmid(text,0,9) eq 'physical;' then textrem=gettok(text,';')
    shape=gettok(text,'(')
    if strlen(text) ne 0 then begin
        xp=[xp,gettok(text,',')]
        yp=[yp,gettok(text,',')]
    endif 
endwhile
free_lun,un
oxp=xp(1:*)
oyp=yp(1:*)
n_oreg=n_elements(oxp)

openr,un,nregfile,/get
text=''
xp=[-999]
yp=[-999]
while eof(un) eq 0 do begin
    nreadagain: readf,un,text
    if strmid(text,0,1) eq '#' then goto,nreadagain
    if strmid(text,0,9) eq 'physical;' then textrem=gettok(text,';')
    shape=gettok(text,'(')
    if strlen(text) ne 0 then begin
        xp=[xp,gettok(text,',')]
        yp=[yp,gettok(text,',')]
    endif 
endwhile
free_lun,un
xp=xp(1:*)
yp=yp(1:*)
n_nreg=n_elements(xp)
sel=[-1]
k=0
for kn=0,n_nreg-1 do begin
    while oxp(k) ne xp(kn) or oyp(k) ne yp(kn) do begin
        k=k+1
    endwhile
    sel=[sel,k]
    k=k+1
endfor

rsel=lindgen(n_oreg)
if n_elements(sel) gt 1 then begin
    sel=sel(1:*)
    remove,sel,rsel
    rsel=rsel+1 ;required for sou_main
endif
return
end
