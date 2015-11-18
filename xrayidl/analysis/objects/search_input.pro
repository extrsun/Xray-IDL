pro search_input,ra,dec,error,tempfile=tempfile,searchfile=searchfile,choice=choice
;+
; prepare a file for a batch NED or SIMBAD search based on positions
;  for details see http://nedwww.ipac.caltech.edu/help/batch_sim.txt
;
; ra, dec - positions (deg).
; error - error radii of the positions for searching the counterparts
;
; searchfile  - the output file to be used in the electronic submission 
; (def=searchfile.txt'): email for SIMBAD: http://simbad.u-strasbg.fr/sim-flist.pl
; web for NED: nedbatch@ipac.caltech.edu, 
; tempfile - template of the file, including other various defaults.
;            (def=!tabdir+'tempfile.txt'; required only for NED)
; choice - (def=1 for a NED search), choice=0 for a simbad search
;
; written by wqd, July 7,2005, partly based on a simbad version
; written by Hui Dong 
;-
if n_params() eq 0 then begin
print,'CALL SEQUENCE - search_input,ra,dec,error,tempfile=tempfile,searchfile=searchfile'
return 
endif

if n_elements(searchfile) eq 0 then searchfile='searchfile.txt'
if n_elements(tempfile) eq 0 then tempfile=!tabdir+'tempfile.txt' 

trans_degree,ra,dec,ih,im,is,jd,jm,js,/deg
error=ra*0.+error/60. ;make sure the same dimension and in units of arcmin
ns=n_elements(ra)

openw,lun,searchfile,/get_lun
if n_elements(choice) eq 0 then choice=1
case choice of
0: begin
    error=error/60.
    for i=0,ns-1 do begin
        printf,lun,'filter {',format='(a8)'
        printf,lun,'CIRCLE ',ih(i),'',im(i),'',is(i),'',jd(i),'',jm(i),'' $
               ,js(i),'','RADIUS ',error(i) $
               ,format='(a7,2(i3,a1,i2,a1,f7.3,a1),a7,d10.7)'
        printf,lun,'}',format='(a1)'
        printf,lun,'',format='(a1)'
    endfor
   end
else: begin
    openr,untab,tempfile,/get_lun
    tabtext=''
    strid='FIND_OBJECTS_NEAR_POSITION'
    stridlen=strlen(strid)
    while not eof(untab) do begin
        readf,untab,tabtext
        printf,lun,tabtext 
        if strmid(tabtext,0,stridlen) eq strid then begin
            for i=0,ns-1 do begin
                printf,lun,ih(i),'',im(i),'',is(i),'; ',jd(i),'',jm(i), $
                       '',js(i),'; ',error(i),"'" $
                ,format='(i2,a1,i2,a1,f6.2,a2,i3,a1,i2,a1,f5.1,a2,f5.3,a1)'
            endfor
        endif 
    endwhile
    free_lun,untab
  end
endcase
free_lun,lun
return
end
  

