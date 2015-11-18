pro getfquals,typcom,filj,extj,quals,selectquals=selectquals
;
; procedure to get vector of qualifiers for an observation
;
q = gettok(extj,';')          ;strip off initial period
quals = strarr(1)
q = gettok(extj,';')
while (q ne '') do begin
  quals = [quals,q+' ']
  q = gettok(extj,';')
endwhile
quals = quals(1:*)            ;strip off initial value
nquals = n_elements(quals)
;
if (nquals eq 1) then print,$
   'Observation ',filj,' has only one filetype ',quals(0)
;
if ((selectquals ne 0) and (nquals gt 1)) then begin
  print,' For observation ',filj,' you can choose the following file types:'
  print,quals
  print,' Enter the types you wish to convert. To end, type XXX.'
  print,' To convert them all, type ALL.'
  ans = ''
  read,'?',ans
  ans = strtrim(ans,2)
  if (strupcase(ans) ne 'ALL') then begin
    quals = strarr(1)
    while (strupcase(ans) ne 'XXX') do begin
      quals = [quals,ans]
      read,'?',ans
      ans = strtrim(ans,2)
    endwhile
    quals = quals(1:*)              ;strip off initial value
  endif
endif          ;selecting qualifiers for this observation
;
return
end            ;pro getfquals
