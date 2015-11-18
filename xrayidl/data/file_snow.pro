pro file_snow,head,ttail=ttail
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -- file_snow,head (ie., rh123456),ttail=ttail'
return
endif
;
head2=strmid(head,0,8)
nf=4
if !proc eq 'RDF' then nf=2
t=strarr(nf)
st=strarr(nf)

case !proc of
'US': begin
	t(0) = '.cas' & st(0) = '_ATTITUDE.FITS'
	t(1) = '.evr' & st(1) = '_EVENTRATES.FITS'
	t(2) = '.fits' & st(2) = '_EVENTS.FITS'
	t(3) = '.so' & st(3) = '_ORBIT.FITS'
      end
'MPE': begin
	t(0) = '_attitude.tfits' & st(0) = '_ATTITUDE.FITS'
	t(1) = '_eventrates.tfits' & st(1) = '_EVENTRATES.FITS'
	t(2) = '_events.tfits' & st(2) = '_EVENTS.FITS'
	t(3) = '_orbit.tfits' & st(3) = '_ORBIT.FITS'
      end
'RDF': begin
	t(0) = '_anc.fits' & st(0) = '_ANC.FITS'
	t(1) = '_bas.fits' & st(1) = '_BAS.FITS'
      end
endcase

for i=0,nf-1 do begin
	spawn, 'cp '+ head+t(i) + ' '+ head2+st(i)
endfor
IF n_elements(ttail) ne 0 then $
	spawn, 'cp '+ head+strtrim(ttail,2) +  ' VALID_TIMES'
end
