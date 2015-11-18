;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
; Procedure to read in and set up the saa vertices for all models in the pdb
;
;-
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pro read_saa,lsaa,bsaa
;
npar = n_params(0)
if (npar eq 0) then begin
  print,'READ_SAA, LSAA, BSAA'
  retall
endif
;
lsaa=fltarr(25,99) & bsaa=lsaa
get_lun,unit
openr,unit,getlog('ZAUX')+'svdf.dat'
x=''
while not eof(unit) do begin 
  readf,unit,x
  if strmid(x,71,2) eq 'VD' then begin
    cur_mn=fix(strmid(x,0,2))
    pt=fix(strmid(x,8,2))-1
    bsaa(pt,cur_mn) = strmid(x,18,6)
    lsaa(pt,cur_mn) = strmid(x,32,6)
  endif
endwhile
free_lun,unit
;
return
end              ;pro read_saa
