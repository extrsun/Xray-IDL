;+
;========================================================================
;;;
;;; FILE NAME:    $Id: binhist.pro 825 1999-08-02 15:38:48Z tsk $
;;;
;;; DESCRIPTION:  A tool to bin ACIS histogram data into histograms for each
;;;               amp of each active CCD.
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1999, Pennsylvania State University
;;;
;;; NOTES:
;;;
;========================================================================
;-
PRO binhist, infile, outfile

; Read the input file
data = mrdfits(infile,1,header)

; Create a structure which will represent the structure of the output file.
nbins = n_elements(data[0].HISTGRM)
outstruct = {TIME:0D, CCD_ID:-1, AMP_ID:-1, EXPOSURE:1L, EXPCOUNT:1L, $
	     HISTGRM:dblarr(nbins)}
outdata = [outstruct]
outtemp = outstruct

; Loop over all amps of all ACIS CCDs.  When data is detected, bin up all data
; for that amp and add it to the output structure.
for ccd=0,9 do begin
  for amp=0,3 do begin
    index = where( (data[*].CCD_ID EQ ccd) AND (data[*].AMP_ID EQ amp) )
    if(index[0] EQ -1) then begin
    endif else begin
      total_exp = 0L
      total_hist = dblarr(nbins)
      for kk=0,n_elements(index)-1 do begin
	total_hist = total_hist + double(data[index[kk]].HISTGRM)
	total_exp  = total_exp  + data[index[kk]].EXPCOUNT
      endfor
      outtemp.TIME   = 0.0D
      outtemp.CCD_ID = ccd
      outtemp.AMP_ID = amp
      outtemp.EXPOSURE = 1
      outtemp.EXPCOUNT = total((data[index]).EXPCOUNT)
      outtemp.HISTGRM = total_hist
      if(outdata[0].CCD_ID EQ -1) then begin
	outdata[0] = outtemp
      endif else begin
	outdata = [outdata,outtemp]
      endelse
    endelse
  endfor
endfor

mwrfits,outdata,outfile,/CREATE

RETURN
END
