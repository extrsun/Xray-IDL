;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;+
;
;*NAME:
;			match_files
;
;*PURPOSE:
;   A procedure to look for files matching the description root+ext
;   in a directory dir
;
;*CALLING SEQUENCE:
;       match_files,dir,root,ext,names,nlist,numsel,order=order
;
;*PARAMETERS:
; INPUTS:
;       dir - directory to be searched
;	root - root name of file to be matched (e.g., 'rp150011')
;       ext - extra bit of root name (e.g., '_evr_')
;
; OPTIONAL INPUTS:
;       numsel - numbers of extensions to be selected (default = 0 for all)
;       order - keyword which specifies whether files are to be ordered by
;               extension number or not (1 = yes is default, 0 = no)
; OUTPUTS:
;       names - vector of file names starting with dir+root+ext
;       nlist - number of files found (= 0 if none were found)
;
;*EXAMPLES:
;
;*RESTRICTIONS:
;       Mainly useful for finding FITS table files of a given type, e.g.
;       finding all CAS extensions for a particular observation sequence
;       RP123456_CAS_1, RP123456_CAS_2, ...
;
;*NOTES:
;
;*PROCEDURE:
;
;*MODIFICATION HISTORY:
;    written 17 May 1991 by GAR
;    modified 28 Aug 1991 by GAR to order FITS tables by extension number
;    modified 30 Aug 1991 by GAR to allow a subset of extension numbers 
;                                to be selected and to use keyword ORDER
;    modified 06 Oct 1991 by GAR to allow directory to be corrected as well
;    modified 8 Nov 1991 for compatibility with Sun Unix (GAR)
;-
;-------------------------------------------------------------------------------
pro match_files,dir,root,ext,names,nlist,numsel,order=order
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' MATCH_FILES, dir, root, ext, NAMES, NLIST, [numsel, order=order]'
  retall
endif
if (npar lt 6) then numsel = 0
sel = numsel
if (n_elements(sel) eq 1) then sel = intarr(1) + sel
;
if (n_elements(order) eq 0) then order = 1       ; default is to order
if (sel(0) gt 0) then order = 1
;
find = 1                                         ; look for matching files
while (find eq 1) do begin
  fspec = root+ext
  files = findfile(dir+fspec,count=nlist)
;
  if (nlist gt 0) then begin                       ; some files were found
    names = strarr(nlist) 
    for jj=0,nlist-1 do begin
      fdecomp,files(jj),disk,dir2,name,qual,ver
      names(jj) = disk+dir2+name
    endfor
    find = 0                                       ; don't look any more
  endif else begin                                 ; no matching files found
    print,' I could not find any files that match ',dir+fspec
    rootname = ''
    print,' Please enter a valid root name or type Q to quit'
    read,'    (include the correct directory name) : ',rootname
    rootname = strtrim(rootname,2)
    if (strupcase(rootname) eq 'Q') then begin
      print,' Returning.'
      retall
    endif else begin
      fdecomp,rootname,disk,dir,name,qual,ver
      root = name
    endelse
  endelse
endwhile            ;finished looking for matching files
;
; If these are FITS table files, then sort in order of increasing extension
; number (if desired) and select extension numbers (if desired)
;
if (order gt 0) then begin
  extnums = bytarr(nlist)
  for kk=0,nlist-1 do begin
    namek=names(kk)
    while (namek ne '') do try = gettok(namek,'_')
    extnums(kk) = fix(try)
  end
  ind=sort(extnums)
  extnums=extnums(ind)
  names=names(ind)
;
  if (sel(0) gt 0) then begin
    match,extnums,sel,indext,indnum
    names = names(indext)
    nlist = n_elements(names)
  endif
endif
;
return              ;pro match_files
end
