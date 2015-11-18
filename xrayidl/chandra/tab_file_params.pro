pro tab_file_params,flist,outfile,tabfile=tabfile,dir=dir,ftail=ftail
;+
; Generate a LaTex table containing key observation parameters for
; multiple files
; 
; flist - name of the file containing a list of observations (events files)
; 	which may include the subdirectory information.
; tabfile - the head and tail of the table
; outfile - output file in the Latex format, which should be consistent with
; 	tabfile.
; dir - master directory of the files
; ftail - if the flist only contains the file name roots, the file
;         tail name should be included
;
; written by wqd, 1/4/2003
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - tab_file_params,flist,outfile,tabfile=tabfile,dir=dir,,ftail=ftail'
return
endif
if n_elements(dir) eq 0 then dir=''
if n_elements(ftail) eq 0 then ftail=''
if n_elements(tabfile) eq 0 then $
	tabfile='$PUBDIR/tab_chandra/tab_file_params.tex'
if n_elements(outfile) eq 0 then outfile='tab_file_params.tex'
openr,untab,tabfile,/get_lun
openw,unout,outfile,/get_lun
tabtext=''
k=0
while not eof(untab) do begin
 readf,untab,tabtext
 if strtrim(tabtext,2) ne '' then begin
	printf,unout,tabtext
	print,tabtext
 endif else begin
	print,'read from flist'
  openr,un,flist,/get
  fname=''
  while not eof(un) do begin
	k=k+1
	text=''
	readf,un,fname
	fname=dir+fname+ftail
	print,fname
	file_params,fname,fhdr,fra,fdec,expt,nra,ndec,xoff,yoff,roll,hdr=hdr
	time_obs=sxpar(hdr,'date-obs')
	date_obs=gettok(time_obs,'T')
	obs_id=sxpar(hdr,'obs_id')
	radec_out,fra,fdec,radec
	text=text+string(obs_id,'(a8)')+' &'+radec+' &'+string(expt,'(i7)') $
		+' &'+string(roll,'(f8.1)')+' &'+date_obs+' \\'
	print,k,text
	printf,unout,text
  endwhile
  free_lun,un
endelse
endwhile
free_lun,untab
free_lun,unout
if !debug eq 2 then stop
return
end
