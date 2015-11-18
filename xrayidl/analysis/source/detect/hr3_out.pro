pro hr_out,slist,hr,hr2,hre,hr2e,soufile=soufile,outfile=outfile,dig=dig,hrslow=hrslow,probth=probth,tabfile=tabfile
;+
; calculate hardness ratios and output the results in a nice tex format
; 
; slist - source structure list
; soufile - source fits file. If provided, slist will be read out from the file
; hr, hr2 - HR1 and HR2
; hre, hr2e - errors of HR1 and HR2
; outfile - output file name
; dig - output digits of the source positions (RA) (def = 2)
; hrslow - lower limit of the S/N for calculating the hardness ratios
;		(def=4)
; probth - probability threshold for selecting the output sources
; tabfile - table Latex head and tail
; 
; written by wqd, 6/21/2001
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - hr_out,slisto,hr,hr2,hre,hr2e,soufile=soufile,outfile=outfile,dig=dig,hrslow=hrslow,tabfile=tabfile'
return
endif
if n_elements(soufile) ne 0 then $
 	sou_fits_info,soufile,slist,flow=flow,probth=probth,/all,nsel=ns $
else begin
	sel=where(slist.prob lt probth,ns)
	if ns ne 0 then slist=slist(sel) else begin 
		print,'No source above the probth threshold"
		return
	endelse 
endelse

cc1=slist.cntrb1 > 0.
cc2=slist.cntrb2 > 0.
cc3=slist.cntrb3 > 0.
cc1e=slist.cntrb1e
cc2e=slist.cntrb2e
cc3e=slist.cntrb3e
hr=imdiv(cc2+cc3-cc1,cc2+cc3+cc1)
hr2=imdiv(cc3-cc2,cc2+cc3)
hr2e=2/(cc2+cc3 > 0.00001)^2*sqrt((cc2e*cc3)^2+(cc3e*cc2)^2)
hre=2/(cc1+cc2+cc3 > 0.00001)^2*sqrt((cc1e*(cc3+cc2))^2+((cc3e^2+cc2e^2)*cc1^2))
if n_elements(dig) ne 0 then radec_out,slist.ra,slist.dec,iau=iau else $
	iau=slist.iauid

if n_elements(hrslow) eq 0 then hsslow=4
for k=0,(ns-1) do begin
	if slist(k).snr gt hrslow then begin
		shr=string(hr(k),'(f5.2)')+'$\pm$'+string(hre(k),'(f4.2)')
		shr2=string(hr2(k),'(f5.2)')+'$\pm$'+string(hr2e(k),'(f4.2)')
	endif else begin
		shr='$-$'
		shr2='$-$'
	endelse
 	print,k+1,' & ',iau(k),' &  ',slist(k).perr,' & ',slist(k).prob,' & ' $
	,slist(k).cntrb*1.e3,'$\pm$',slist(k).cntrbe*1.e3 $
	,' & ',shr,' & ',shr2,' & ',slist(k).sdb,' \\' $
	,format='(I4,a3,a16,a3,(f4.1,a3),(f6.1,a3),(f9.2,a5,f7.2,a3),2(a14,a3),a10,a3)'
endfor
	
if n_elements(outfile) eq 0 then return
openw,un,outfile,/get_lun

if n_elements(tabfile) eq 0 then tabfile='$PUBDIR/tab_chandra/tab_source.tex'
openr,untab,tabfile,/get_lun
tabtext=''
while not eof(untab) do begin
readf,untab,tabtext
if tabtext ne '' then printf,un,tabtext $
else begin
for k=0,(ns-1) do begin
	if slist(k).snr gt hrslow then begin
		shr=string(hr(k),'(f5.2)')+'$\pm$'+string(hre(k),'(f4.2)')
		shr2=string(hr2(k),'(f5.2)')+'$\pm$'+string(hr2e(k),'(f4.2)')
	endif else begin
		shr='$-$'
		shr2='$-$'
	endelse
  	printf,un,k+1,' & ',iau(k),' &  ',slist(k).perr,' & ',slist(k).prob,' & ' $
	,slist(k).cntrb*1.e3,'$\pm$',slist(k).cntrbe*1.e3 $
	,' & ',shr,' & ',shr2,' & ',slist(k).sdb,' \\' $
	,format='(I4,a3,a16,a3,(f4.1,a3),(f6.1,a3),(f9.2,a5,f7.2,a3),2(a14,a3),a10,a3)'
endfor
endelse
endwhile
free_lun,un
free_lun,untab
return
end