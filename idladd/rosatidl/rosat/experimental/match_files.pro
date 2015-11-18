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
;    modified 04 Jun 1992 for compatibility with routines using newest
;      version of READFITS to access data in anciliary files
;    modified 09 Sep 1992 to find files on other disks (GAR)
;-
;-------------------------------------------------------------------------------
pro match_files,dir,root,ext,names,nlist,numsel,order=order
;
npar = n_params(0)
if (npar eq 0) then begin
  print,' MATCH_FILES, dir, root, ext, NAMES, NLIST
  retall
endif
;
find = 1                                         ; look for matching files
while (find eq 1) do begin
  fspec = root+ext
  names = findfile(dir+fspec,count=nlist)
  if (nlist gt 0) then find = 0 else begin
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
      dir = disk+dir
    endelse
  endelse
endwhile            ;finished looking for matching files
;
return              ;pro match_files
end
