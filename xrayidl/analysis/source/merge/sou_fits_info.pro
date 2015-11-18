pro sou_fits_info,soufile,outslist,row=row,all=all,slow=slow,flow=flow,probth=probth,nsel=nsel,radec_off=radec_off
;+
; plot source position in an existing image
; soufile - the name of the source file
; outslist - output source structure
; row - structure format (def row={sn:0,ra:0.0d,dec:0.0d,snr:0.0,cntr:0.0})
; all - if set, all source parameter entries will be included, overriding row
; slow, flow - lower limits for snr and cntr
; probth - upper limit of the probability for a fake source
; nsel - number of selected sources
; radec_off - offset (deg) to applied to the output source RA and Dec
;             (e.g., the output val parameter from get_offset_opt.pro;
;             but only x and y shifts are implemented) 
;
;written by wqd, 2/3/2002
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - sou_fits_info,soufile,slist,row=row,all=all,' 
print,',slow=slow,flow=flow,probth=probth,nsel=nsel,radec_off=radec_off'
return
endif
if exist(soufile) eq 0 then begin
    print,'The file ',soufile,' does not exist!!!'
    return
endif
if keyword_set(all) eq 0 then $
   if n_elements(row) eq 0 then $
	row={sn:0,ra:0.0d,dec:0.0d,snr:0.0,cntrb:0.0,prob:0.0}
list_xray,soufile,outslist,row=row
nsel=n_elements(outslist)
if nsel eq 0 then return
if n_elements(slow) ne 0 then begin
	sel=where(outslist.snr ge slow,nsel) 
	if nsel ne 0 then outslist=outslist(sel) else begin
		print,'no source has slow > ',slow
		return
	endelse
endif
if n_elements(flow) ne 0 then begin
	sel=where(outslist.cntrb ge flow,nsel) 
	if nsel ne 0 then outslist=outslist(sel) else begin
		print,'no source has cntrb > ',flow
		return
	endelse
endif
if n_elements(probth) ne 0 then begin
	sel=where(outslist.prob le probth,nsel) 
	if nsel ne 0 then outslist=outslist(sel) else begin
		print,'no source has prob < ',probth
		return
	endelse
endif
if n_elements(radec_off) eq 2 then begin
    print,'*** Correct for the pointing offset = ',radec_off
    outslist.ra=outslist.ra-radec_off(0)/3600.
    outslist.dec=outslist.dec+radec_off(1)/3600.
endif
return
end
