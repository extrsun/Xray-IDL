pro f3dlist,dir,fspec,files,names,exts
;
; procedure to search a directory for 3D binary table FITS files which match
; the file specification
; ignore files ending in .hhh or .hhd
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' F3DLIST, dir, fspec, ( FILES, NAMES, EXTS )'
  return
endif
;
; define valid extensions for Rosat PSPC & HRI 3D binary table files
;
trenddata = 0                        ;assume regular data
instr = strupcase(fspec)
if (strlen(fspec) gt 2) then $
   instr=strmid(instr,0,2)           ;need this to get valid extensions
if (instr eq 'R') then instr = 'RB'  ;get extensions for both HRI & PSPC
if (instr eq 'P_') then begin
  instr = 'RP'                       ;this is a trend analysis file
  trenddata = 1
endif
if (instr eq 'H_') then begin
  instr = 'RH'                       ;this is a trend analysis file
  trenddata = 1
endif
fexts = f3d_exts(instr)
nvalid = n_elements(fexts)
;
files = findfile(dir+fspec+'*.*',count=nlist)
if (nlist eq 0) then begin
   print,' No matching files found in directory '+spec_dir(dir)
   files = ''
   return
endif
;
indx = intarr(nlist) + 1
names = strarr(nlist)
quals = strarr(nlist)
for jj=0,nlist-1 do begin                    ;decompose the list of file names
  fdecomp,files(jj),disk,dir2,name,qual,ver
  qual = '.'+strtrim(qual,2)
  try = name
  name = gettok(try,'_')
  names(jj) = name
  if (trenddata eq 1) then begin       ;this is trend analysis data
    try2 = gettok(try,'_')
    names(jj) = names(jj)+'_'+try2
  endif 
  if (try ne '') then quals(jj) = '_'+try+qual else quals(jj) = qual
endfor
;
for jj=0,nlist-1 do begin
  qual = quals(jj)
  qual = strupcase(qual)
  name = names(jj)
  jmatch = where(qual eq strupcase(fexts)) ;is it a valid extension?
  jmatch = jmatch(0)
  if (jmatch lt 0) then indx(jj) = 0
;
  try = gettok(qual,'.HH')
  if ((qual eq 'HHH') or (qual eq 'HHD')) then begin  ;was it converted?
    indx(jj) = 0
    ext = gettok(try,'_') & ext = gettok(try,'_')
    ext = '.'+ext
    kk = where( (names eq name) and (strupcase(quals) eq strupcase(ext)) ) 
    if ( kk(0) ge 0) then indx(kk) = 0
  endif        ;removing corresponding names, if converted
endfor
;
nlist = total(indx)
if (nlist gt 0) then begin
  files = files(where(indx eq 1))
  print,' '
  print,'You have the following unconverted',$
        ' Rosat 3D binary table FITS files in directory   ',dir
  for jj=0,nlist-1 do print,jj+1,'  ',files(jj)
  print,' '
endif else begin
  print,' No valid unconverted files found in directory '+spec_dir(dir)
  print,'  '
  files = ''
  return
endelse
;
; for each unique observation sequence, save the valid 3D extensions
; in a single string, separated by semi-colons (;)
; names will only have one entry for each unique observation sequence
;
nobs = 0
names = strarr(1)
exts = names
qualsav = ''
for jj=0,nlist-1 do begin
  fdecomp,files(jj),disk,dir2,name,qual,ver
  name = strtrim(name,2)
  qual = '.'+strtrim(qual,2)
  try = name
  name = gettok(try,'_')
  if (trenddata eq 1) then begin       ;this is trend analysis data
    try2 = gettok(try,'_')
    name = name+'_'+try2
  endif 
  if (try ne '') then qual = '_'+try+qual
;
  if (!debug ne 0) then print,jj,nobs,' ',name,' ',names(nobs)
  if (!debug ne 0) then print,jj,qualsav
  if (name eq names(nobs)) then qualsav=qualsav+';'+qual else begin
    nobs = nobs + 1
    names = [names,name]
    if (nobs gt 1) then exts = [exts,qualsav]
    qualsav = ';'+qual
  endelse
  if (!debug ne 0) then print,jj,nobs,qual,qualsav
endfor
exts = [exts,qualsav]        ;save final list of extensions
;
names = names(1:*)           ;strip off initial entries
exts = exts(1:*)
;
return
end        ;pro f3dlist
