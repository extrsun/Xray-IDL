pro convert3d,instr,obseq,ftypes,dir,selectfiles=selectfiles,trenddata=trenddata
;
;   procedure to convert all binary table files from 3D binary table format
;   to STSDAS FUTS format so that normal STSDAS routines can read them.
;
if (n_elements(selectfiles) eq 0) then selectfiles=0   ;default is no choice
if (n_elements(trenddata) eq 0) then trenddata = 0  ;default is regular data
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' CONVERT3D, instr ("P", "H", or "B"), '
  print,'          ( obseq, ftypes ["A" for all] ), (dir),'
  print,'          (selectfiles=selectfiles, trenddata=trenddata)'
  return
endif
;
if (npar lt 4) then dir = ''
if (npar lt 3) then ftypes = 'A'             ;get a listing
if (npar lt 2) then obseq = 'A'              ;get a listing
;
instr = strupcase(strmid(instr,0,1))      ;just take the first letter
if ((instr ne 'P') and (instr ne 'H') and (instr ne "B")) then begin
  instr = ''
  read,' Please specify instrument - P = PSPC, H = HRI, B = both  ',instr
endif
instr = strupcase(instr)
;
getfcom,obseq,obscom,obsfiles,dir
names = obsfiles
getfcom,ftypes,typcom,quals,''
exts = quals
if (!debug ne 0) then stop,' Stopping after GETFCOM: names & exts defined'
;
; find all observations done using the desired instrument &/or all
; the files for a particular (single) observation
;
fspec = 'R'+instr 
if (trenddata eq 1) then fspec = 'P_'
if (obscom eq 1) then fspec = obseq
if (obscom le 1) then begin 
  f3dlist,dir,fspec,files,names,exts    ;get list of 3D binary FITS files
;
  s = size(files) & nn = s(0)
  if (nn lt 1) then begin
    print,' No files to convert. Returning.
    retall & endif
;
  ans = ''
  read,' Do you wish to continue? (Y or N)  ',ans  ;give user a chance to stop
  yesno,ans
  if (ans eq 0) then begin
    print,' Returning at user request.'
    retall & endif
endif
;
; allow user to choose if selectfiles keyword is set & > 1 file
;
nobs = n_elements(names)
indx = intarr(nobs)+1
if ((selectfiles ne 0) and (nobs gt 1))  then begin         
  for jj=0,nobs-1 do begin
    ans = ''
    print,' Convert files for ',names(jj),' ? (Y or N)'
    read,ans
    yesno,ans
    if (ans eq 0) then indx(jj) = 0
  endfor
endif
;
if (obscom eq 0) then begin         ;convert all of the (selected) files
  nobs = total(indx)
  obsfiles = dir+names(where(indx eq 1))
  exts = exts(where(indx eq 1))
endif
if (!debug ne 0) then stop,' Stopping after obsfiles & exts selected'
;
infiles = strarr(1)
outfiles = infiles
nfiles = 0
for jj=0,nobs-1 do begin
  filj = strtrim(obsfiles(jj),2)
  if (typcom le 0) then begin
    extj = exts(jj)
    if (!debug gt 1) then print,j,filj,extj
    getfquals,typcom,filj,extj,quals,selectquals=selectfiles
    if (!debug gt 1) then print,j,quals
  endif
  if (!debug ne 0) then print,quals
;
  nqual = n_elements(quals)         ;number of qualifiers for this observation
  for kk=0,nqual-1 do begin
    nfiles = nfiles + 1
    qk = strtrim(quals(kk),2)
;    if (!debug ne 0) then print,jj,kk,nfiles,' ',filj,' ',qk
    infiles = [infiles,filj+qk]
    qtry = qk
    qk = gettok(qtry,'.')
    outfiles = [outfiles,filj+qk+'_'+qtry]
  endfor
endfor
infiles = infiles(1:*)              ;strip off initial values
outfiles = outfiles(1:*)   
if (!debug ne 0) then print,infiles
if (!debug ne 0) then print,outfiles
;
for nn = 0,nfiles-1 do begin        ;now at last we can do the conversion!
  inf = infiles(nn)
  outf = outfiles(nn)
  if (!debug ne 0) then print,inf,' ',outf
  dfitsrd,inf,outf,noprompt=1
endfor
;
return
end         ;pro convert3d
