;+
;==============================================================================
;;; FILE NAME:     @(#)write_spectral_file.pro	1.2
;;;
;;; DESCRIPTION:   Write an OGIP-standard spectral file.  Pass in an array
;;;                containing event amplitudes and a filename.  The output
;;;                is a FITS files with a blank primary HDU and a binary
;;;                table which contains a histogram of event amplitudes in
;;;                one of the following formats:
;;;
;;;                - 5000 10eV bins (0 - 50 keV). 
;;;                - 4096 1-DN wide PHA channels
;;;                - 4096 1-DN wide PI channels
;;;
;;; AUTHOR:        Scott Koch (tsk@astro.psu.edu)
;;;                Pat Broos  (patb@astro.psu.edu)
;;;                Copyright (C) 1997, Pennsylvania State University
;;;
;;; NOTES:
;;;               Clients must set one of the DN, PI, or EV keywords to indicate
;;;               which way the output spectral file is binned.  If none 
;;;               is set, then print an error and halt execution.
;==============================================================================
;-
PRO WriteSpectralFile, amplitudes, filename, DN=dn_flag, EV=ev_flag, PI=pi_flag

; Either /EV or /DN must be set
if (     keyword_set(dn_flag) AND $
     NOT keyword_set(ev_flag) AND $
     NOT keyword_set(pi_flag)) then begin
  ev_flag = 0
  pi_flag = 0
endif else if (    keyword_set(ev_flag) AND $
               NOT keyword_set(dn_flag) AND $
               NOT keyword_set(pi_flag)) then begin
  dn_flag = 0
  pi_flag = 0
endif else if (    keyword_set(pi_flag) AND $
               NOT keyword_set(ev_flag) AND $
               NOT keyword_set(dn_flag)) then begin
  ev_flag = 0
  dn_flag = 0
endif else begin
  message, 'You must set one of the following flags:  /DN, /EV, or /PI.'
endelse

print, '  Writing spectrum ' + filename, + '...'

; Handle the creation of the histogram separately for EV and DN.  For EV,
; we want to create 5000 10.0 eV channels.  The CHANNEL array is a vector
; of floating point numbers which contain the **middle of each bin** in eV.
; For DN type data, we want 4096 (12-bit pixels) 1 DN channels.  The CHANNELS
; array is an integer array with the channel number with the first channel
; being 1 (this is what XSPEC wants).
if(dn_flag OR pi_flag) then begin
  min_amplitude = 0
  binsize = 1           ;DN
  num_channels = 4096
  bin_table = replicate( { CHANNEL: 0, COUNTS: 0 }, num_channels )
  bin_table.CHANNEL = (indgen( num_channels ) * binsize + 1) 
endif else begin
  min_amplitude = 0
  binsize = 10.0        ;eV
  num_channels = 5000.0
  bin_table = replicate( { CHANNEL: 0.0, COUNTS: 0 }, num_channels )
  bin_table.CHANNEL = (findgen( num_channels ) * binsize) + (binsize / 2.0)
endelse

 
; The amplitudes array may actually be empty.  The client, for example, may
; be AcisFilter().  If it's running in batch mode, then all of the events
; may legitimately have been filtered away, leaving a NULL array.  Handle
; this case.
if( (n_elements(amplitudes) EQ 1) AND (amplitudes[0] EQ -1) ) then begin
  counts = make_array( num_channels, /LONG, value=0)
endif else begin
  counts = histogram( amplitudes, MIN=min_amplitude, BINSIZE=binsize )
endelse


; Sometimes the histogram() function may generate a histogram which is either
; a bin shy of "num_channels" or with an extra bin.  Handle these cases.
nel = n_elements(counts)
if (nel GT num_channels) then begin
  bin_table.COUNTS = counts(0:num_channels-1)
endif else if (nel LT num_channels) then begin
  temp    = intarr( num_channels )
  temp(0) = counts
  bin_table.COUNTS = temp
endif


;; Write the FITS file.
sxaddpar, fits_header, 'EXTNAME',  'SPECTRUM'
sxaddpar, fits_header, 'TELESCOP',  'UNKNOWN'
sxaddpar, fits_header, 'INSTRUME',  'UNKNOWN'
sxaddpar, fits_header, 'FILTER',    'none'
sxaddpar, fits_header, 'EXPOSURE',  1.0
sxaddpar, fits_header, 'AREASCAL',  1.0
sxaddpar, fits_header, 'BACKFILE',  'none'
sxaddpar, fits_header, 'BACKSCAL',  1.0
sxaddpar, fits_header, 'CORRFILE',  'none'
sxaddpar, fits_header, 'CORRSCAL',  1.0
sxaddpar, fits_header, 'RESPFILE',  'none'
sxaddpar, fits_header, 'ANCRFILE',  'none'
sxaddpar, fits_header, 'XFLT0001',  'none'
sxaddpar, fits_header, 'HDUCLASS',  'OGIP'
sxaddpar, fits_header, 'HDUCLAS1',  'SPECTRUM'
sxaddpar, fits_header, 'HDUVERS1',  '1.1.0'
sxaddpar, fits_header, 'HDUCLAS2',  '        '
sxaddpar, fits_header, 'HDUCLAS3',  'COUNT'
if (pi_flag) then begin
  sxaddpar, fits_header, 'CHANTYPE',  'PI'
endif else begin
  sxaddpar, fits_header, 'CHANTYPE',  'PHA'
endelse
sxaddpar, fits_header, 'POISSERR', 'T'
sxaddpar, fits_header, 'SYS_ERR',  0
sxaddpar, fits_header, 'QUALITY',  0
sxaddpar, fits_header, 'GROUPING', 0
sxaddpar, fits_header, 'DETCHANS', num_channels

if(ev_flag) then begin
  sxaddpar, fits_header, 'TUNIT1', 'eV'

  str = string( min_amplitude, FORMAT='(F7.1)' )
  comment = "First channel starts at event amplitude " + str + " eV."
  sxaddpar, fits_header, 'HISTORY', comment

  str = string( min_amplitude + binsize*num_channels, FORMAT='(F7.1)')
  comment = "Last  channel starts at event amplitude " + str + " eV."
  sxaddpar, fits_header, 'HISTORY', comment

  str = string( binsize )
  comment = "Channels are " + str + " eV wide."
  sxaddpar, fits_header, 'HISTORY', comment
endif else begin
  sxaddpar, fits_header, 'TUNIT1', 'DN'

  str = string( min_amplitude + 1, FORMAT='(I)' )
  comment = "First channel starts at channel " + str + " DN."
  sxaddpar, fits_header, 'HISTORY', comment

  str = string( min_amplitude + binsize*num_channels, FORMAT='(I)')
  comment = "Last  channel starts at channel " + str + " DN."
  sxaddpar, fits_header, 'HISTORY', comment

  str = string( binsize )
  comment = "Channels are " + str + " DN wide."
  sxaddpar, fits_header, 'HISTORY', comment
endelse

; Write the FITS spectral file
mwrfits, bin_table, filename, fits_header, /CREATE
print, "  ", filename, " written"

RETURN
END
