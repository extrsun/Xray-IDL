pro exp_cast,flist,hdr,texp,fname=fname
;-
; cast the exposure maps from a list of off-set observations to a norminal 
; pointing direction
; written by wqd Jun/18/98
;+
if n_params() eq 0 then begin
print,'CALL SEQUENCE - exp_cast,flist,hdr,texp,dir=dir'
return
endif
if n_elements(fname) eq 0 then fname=''
obdir=''
openr,un,flist,/get
while not eof(un) do begin
	readf,un,obdir
	cast,obdir+fname,hdr,outa=obexp
if !debug eq 1 then stop
texp=texp+obexp
endwhile
free_lun,un
return
end