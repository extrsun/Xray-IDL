pro hr_out,slist,outfile=outfile,texfile=texfile,tabfile=tabfile,dig=dig,nband=nband,hrth=hrth,npsyserr=npsyserr,silent=silent,dhrch=dhrch,apsyserr=apsyserr
;+
; output hardness ratios in Latex delux table format
; 
; slist - source structure list
; texfile - output file in the Latex format, which should be consistent with
; 	tabfile.
; tabfile - table Latex head and tail
; dig - output digits of the source positions (RA) (def = 2)
; nbands - number of bands used in calculating the hardness ratios
; hrth - hardness error threshold above which no hardness ratio values will
;	be output into the latex file for a source (def=0.2)
; npsyserr - if set, systematic position error will be included in the
;            output
; silent - if set, no printing of individual output on screen
; dhrch - the choice for the HR definition (the same for bands=3): 
;         for dhrch=0 (def), HR=(c(2:3)-c(1))/(c(2:3)+c(1))
; for dhrch=1, HR=(c(2:3)-c(0:1))/(c(2:3)+c(0:1)); output in 3 columns
;; for dhrch=2, HR=(c(2)-c(0:1))/(c(2)+c(0:1))
; apsyserr - additional systematic position error of the sources to be
;            included in the output latex table,
;            (e.g., beyond psyserr; in arcsec)
;
; written by wqd, 6/17/2002
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - hr_out,slist,outfile=outfile'
print,',texfile=texfile,tabfile=tabfile,dig=dig,nband=nband,hrth=hrth'
print,',npsyserr=npsyserr,silent=silent,dhrch=dhrch'
return
endif
if n_elements(dhrch) eq 0 then dhrch=0
if n_elements(hrth) eq 0 then hrth=0.2
if n_elements(nband) eq 0 then nband=4
if n_elements(dig) ne 0 then radec_out,slist.ra,slist.dec,iau=iau else $
	iau=slist.iauid

if n_elements(outfile) ne 0 then sou_struct_fits,slist,outfile

if n_elements(texfile) eq 0 then texfile='hr_out.tex'
openw,un,texfile,/get_lun

if n_elements(tabfile) eq 0 then begin
	case nband of 
		3: tabfile='$PUBDIR/tab_chandra/tab_source_3b.tex'
		4: tabfile='$PUBDIR/tab_chandra/tab_source_4b.tex'
		else: stop,'please give tabfile'
	endcase
endif
openr,untab,tabfile,/get_lun
tabtext=''
while not eof(untab) do begin
 readf,untab,tabtext
 if tabtext ne '' then printf,un,tabtext $
 else begin
  shr='$'+string(slist.hr,'(f5.2)')+'\pm'+string(slist.hre,'(f4.2)')+'$ & '
  sel=where(slist.hre gt hrth,nsel)
  if nsel ne 0 then shr(sel)='--& '
;  if nband eq 4 then begin
 if dhrch eq 0 then begin
	shrs='$'+string(slist.hr1,'(f5.2)')+'\pm'+string(slist.hr1e,'(f4.2)')+'$ & '
	sel=where(slist.hr1e gt hrth,nsel)
	if nsel ne 0 then shrs(sel)='--& '
	shr=shr+shrs
endif
format='(I4,a3,a20,a3,(f4.1,a3),(f9.2,a5,f7.2,a3),a,a,a3)'
;  endif else $
;	format='(I4,a3,a20,a3,(f4.1,a3),(f6.1,a3),(f9.2,a5,f7.2,a3),a34,a8,a3)'
if dhrch eq 2 then begin
  shrs='$'+string(slist.hr2,'(f5.2)')+'\pm'+string(slist.hr2e,'(f4.2)')+'$ &'
  sel=where(slist.hr2e gt hrth,nsel)
  if nsel ne 0 then shrs(sel)='-- & '
  shr=shr+shrs
endif
 if n_elements(apsyserr) eq 0 then apsyserr=0.
 if n_elements(npsyserr) eq 0 then npsyserr=1 
  if npsyserr eq 0 then perr=apsyserr+slist.perr else $
        perr=apsyserr+sqrt((slist.perr)^2+(slist.psyserr)^2) ;including systematic error
  for k=0,(n_elements(slist)-1) do begin
          printf,un,k+1,' & ',iau(k),' &  ',perr(k) $
                 ,'&$',slist(k).cntrb*1.e3,'\pm',slist(k).cntrbe*1.e3 $
                 ,'$& ',shr(k),strtrim(slist(k).sdb,2),' \\',format=format
    endfor
if keyword_set(silent) ne 1 then begin
  for k=0,(n_elements(slist)-1) do begin
 	print,k+1,' & ',iau(k),' &  ',perr(k) $
	,'&$',slist(k).cntrb*1.e3,'\pm',slist(k).cntrbe*1.e3 $
	,'$& ',shr(k),strmid(slist(k).sdb,0,7),' \\',format=format
    endfor
endif 
 endelse
endwhile
free_lun,un
free_lun,untab
if !debug eq 2 then stop
return
end
