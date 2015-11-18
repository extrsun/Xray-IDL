pro match_out,sn,ora,odec,offset,outv,perr=perr,texfile=texfile,tabfile=tabfile,dig=dig
;+
; output hardness ratios in Latex delux table format
; 
; sn - X-ray source number
; ora,odec - corresponding RA and Dec of the match objects
; offset - the position separation of the counterpart from the X-ray
;          source (arcsec)
; outv - the string to be output (e.g., magnitudes from match_obj)
; perr - position error (arcsec)
; texfile - output file in the Latex format, which should be consistent with
; 	tabfile.
; tabfile - table Latex head and tail
; dig - output digits of the source positions (RA) (def = 2)
;
; written by wqd, 6/4/2005
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - match_out,sn,ora,odec,offset,outv,perr=perr'
print,',texfile=texfile,tabfile=tabfile,dig=dig'
return
endif
;if n_elements(dig) eq 0 then dig=1
radec_out,ora,odec,iau=iau,dig=dig 
text=string(sn,'(i4)')+' & '+iau+' & '+string(offset,'(f4.1)')
if n_elements(perr) ne 0 then $
    text=text+' $\pm$'+string(perr,'(f5.1)')+'&'+outv $
         else text=text+'&'+outv

if n_elements(texfile) eq 0 then texfile='id_out.tex'
openw,un,texfile,/get_lun

if n_elements(tabfile) eq 0 then tabfile='$PUBDIR/tab_chandra/tab_id.tex'
openr,untab,tabfile,/get_lun
tabtext=''
while not eof(untab) do begin
    readf,untab,tabtext
    if tabtext ne '' then printf,un,tabtext $
 else begin
     for k=0,n_elements(text)-1 do begin
         printf,un,text(k)+'\\'
         print,text(k)+'\\'
     endfor 
 endelse
endwhile
free_lun,un
free_lun,untab
if !debug eq 3 then stop
return
end
