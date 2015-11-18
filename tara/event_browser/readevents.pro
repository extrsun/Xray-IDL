;+
;==============================================================================
;;; FILE NAME:    $Id: readevents.pro 968 2000-01-25 12:03:35Z patb $
;;;
;;; DESCRIPTION:  A program to read event files into an Event Browser style
;;;		  data structure.
;;;
;;; AUTHOR:       Scott Koch (tsk@astro.psu.edu)
;;;               Pat Broos  (patb@astro.psu.edu)
;;;               Copyright (C) 1997, Pennsylvania State University
;;;
;;; NOTES:        The input filename can be a FITS event file or an index
;;;               file which lists a number of event files to be processed.
;;;               If the last line of the index file contains the word 'END',
;;;               then this line is ignored.
;;;
;;;		The Event Browser routine ReadDomainDataset is used to read
;;;		the event lists and the data are stored in a hidden "database"
;;;		(implemented as a common block) that can be accessed using
;;;		the function GetProperty() (see Event Browser code for 
;;;		examples).
;-

@eb_parameter
@eb_property
@eb_derived_property
@domain_dataset
;==============================================================================
PRO ReadEvents, infile

InitializeDomainDataset, GUI_ACTIVE=0, /UNDEFINE_PROPERTIES

;==============================================================================
; Read the input file(s)
;==============================================================================
; Is this a FITS file or an index file?
openr, unit, infile, /GET_LUN
stat = fstat(unit)
item = bytarr( 8 < stat.size )
readu, unit, item

if (string(item) EQ 'SIMPLE  ') then begin
  n_files = 1
  filenames = [infile]
endif else begin
  n_files = numlines(infile)
  filenames = strarr(n_files)
  point_lun,unit,0
  readf,unit,filenames
  str = filenames(n_files-1)
  if(strmid(str,0,3) EQ 'END') then begin
    n_files = n_files - 1
    filenames = filenames(0:n_files-1)
  endif
  fdecomp,infile,disk,dir,file,qual,version
  filenames = dir + '/' + filenames
endelse
close,unit
free_lun,unit

 
; Read in the file(s)
if( n_files GT 0 ) then begin
  ReadDomainDataset, filenames
endif else begin
  print, 'Index file is empty.  No data read.'
endelse

RETURN
END
