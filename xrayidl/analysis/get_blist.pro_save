pro get_blist,bfname,tblist,bexptv,imccd=imccd,fidv=fidv
;+
; get a background event list, useful for later removing sources and
; then for constructing background images in various bands, using blist_image
; 
; bfname - vector contains the background event file names (including
;          the path; different pointings or chips)
; tblist - input event list originally from get_blist
; bexptv - vector containing exposure times of the background files
; imccd - chosen ccd of the output image (def = the output from sou_det_params)
; fidv - vector containing the IDs of individual files, used
;        for their specific normalizations
;
; written by wqd, June 7, 2007
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --- get_blist,bfname,tblist,bexptv,imccd=imccd,fidv=fidv'
return
endif
if !instr eq 'epic' then tagin=['X','Y','PI','CCDNR']
nbfname=n_elements(bfname)

 if n_elements(fidv) ne 0 then $
   brow={x:0.,y:0.,energy:0,ccd_id:0,fid:0} else $
   brow={x:0.,y:0.,energy:0,ccd_id:0}
   tagin=tag_names({x:0.,y:0.,energy:0,ccd_id:0}) 
 ncol=lonarr(nbfname)
 bexptv=fltarr(nbfname)
 tblist=brow
 for kk=0,nbfname-1 do begin
     list_xray,bfname(kk),blist,ccd=imccd,row=brow,nct=nct,tagin=tagin
     fhdr=headfits(bfname(kk),ext=1)
     bexpt=sxpar(fhdr,'livetime')
     if bexpt eq 0 then bexpt=sxpar(fhdr,'exposure')
     if nct ne 0 then begin
     	if bexpt eq 0 then stop,'the background file exposure = 0! need a fix.'
     	tblist=[tblist,blist]
     endif
     bexptv(kk)=bexpt
     if n_elements(fidv) ne 0 then tblist.fid=fidv(kk)
 endfor
 tblist=tblist(1:*)
end
;========================================================
