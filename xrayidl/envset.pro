pro envset,seqno,mpe=mpe,hri=hri,tail=tail,rdf=rdf,datadir=datadir
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - envset,seqno,mpe=mpe,hri=hri,tail=tail,rdf=rdf,datadir=datadir'
return
endif
if n_elements(tail) eq 0 then tail=''
On_error,2                                 ;Return to caller
;if keyword_set(hri) ne 0 or keyword_set(mpe) ne 0 then datadir='/data2/' else $
;	datadir='/data2/wqd/archives/'
;	datadir='/data2/'
if n_elements(datadir) eq 0 then datadir='/data2/'
if keyword_set(hri) ne 0  then begin
	!instr='h' 
	defsysv,'!size_pixel',0.5  ;1 pixel=0''.5
	;defsysv,'!size_pixel',0.498  ;1 pixel=0''.498 ;changed on 12/12/94
	!block=16
endif else begin
	 !instr='p'
	defsysv,'!size_pixel',0.5  ;1 pixel=0''.5
	!block=30
endelse

if keyword_set(mpe) ne 0 then begin 
	!seq_no='w'+!instr+strtrim(seqno,2) 
	!proc='MPE'
endif else begin
 	!seq_no='r'+!instr+strtrim(seqno,2)
	!proc='US'
endelse
if keyword_set(rdf) ne 0 then !proc='RDF'
dir=datadir+!seq_no+tail
cd,dir
;!data_dir=dir+'/'
;
;if keyword_set(hri) ne 0 then extyp='_src.fits' else begin
;if !proc eq 'MPE' then extyp='_mexmap.ifits' else extyp='_mex.fits'
;endelse
;infile=!data_dir+!seq_no+extyp


;	sxhread,infile,refhead
;	get_obdate,refhead,julian,/pri
end