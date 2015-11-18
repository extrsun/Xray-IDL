;+
;========================================================================
;;;
;;; FILE NAME:    $Id: histtoolevent.pro 826 1999-08-02 17:39:25Z tsk $
;;;
;;; DESCRIPTION:  Histogram Tool Application
;;;
;;; AUTHOR:       Scott Koch (tsk@astro.psu.edu)
;;;               Pat Broos (patb@astro.psu.edu)
;;;               Copyright (C) 1999, Pennsylvania State University
;;;
;;; NOTES:        Event handler and supporting routines for the histogram tool.
;;;
;========================================================================
;-
;---------------------------------------------------------------------------
; The subtitle of HistTool plots contains the CCD_ID, AMP_ID, exposure number,
; and the number of CCD exposures which were summed to make the histogram.
; This routine handles the formatting of that subtitle.
;---------------------------------------------------------------------------
FUNCTION FormatInfoLabel, ccd, amp, exposure, count

case amp of
  0: a = 'A'
  1: a = 'B'
  2: a = 'C'
  3: a = 'D'
  default: begin
    a = 'x'
    print, 'Invalid amplifier ID detected', amp
  end
endcase

c         = strtrim(string(ccd,   FORMAT='(I2.2)'),2)
exp_count = strtrim(string(count, FORMAT='(I4.4)'),2)
exp_num   = strtrim(string(exposure, FORMAT='(I4.4)'),2)

str = 'CCD ' + c + '; AMP ' + a + '; Exposure Number ' + exp_num + $
      '; Exposure Count ' + exp_count

return, str
END

;---------------------------------------------------------------------------
; Extract the gain and offset for the given amp of the given CCD and use that
; information to compute the axis for a plot in energy (eV) coordinates.
;---------------------------------------------------------------------------
FUNCTION ComputeEvAxis, table, ccd_id, amp_id, nbins

; Find the gain and offset for the given (ccd,amp) pair
for ii=0,39 do begin
  if (((*table).FIELD1[0,ii] EQ ccd_id) AND $
      ((*table).FIELD1[1,ii] EQ amp_id)) then begin
     gain = (*table).FIELD1[2,ii]
     offset = (*table).FIELD1[3,ii]
  end
endfor

axis = (findgen(nbins) * gain) + offset
return, axis
END

;---------------------------------------------------------------------------
; A routine to handle the plotting of histogram data.
;---------------------------------------------------------------------------
PRO PlotEventHistogramData, st

if( NOT ptr_valid((*st).data) ) then begin
  print,"No input file selected."
  RETURN
endif

test_id = sxpar(*(*st).fileheader,'FNFITS')
if( size(test_id,/TNAME) NE 'STRING' ) then begin
  test_id = (*st).filename
endif
str = FormatInfoLabel((*(*st).data)[(*st).current_bin].CCD_ID, $
			    (*(*st).data)[(*st).current_bin].AMP_ID, $
			    (*(*st).data)[(*st).current_bin].EXPOSURE, $
			    (*(*st).data)[(*st).current_bin].EXPCOUNT)
if((*st).ev_button_selected) then begin
  x = ComputeEvAxis( (*st).dn2evtable, (*(*st).data)[(*st).current_bin].CCD_ID,$
		     (*(*st).data)[(*st).current_bin].AMP_ID, $
		     (*st).histogram_bins )
  function_1d,(*st).function_1d, x, (*(*st).data)[(*st).current_bin].HISTGRM, $
              DATASET_NAME='Histogram Data', TITLE=test_id, $
              SUBTITLE=str, PSYM=10, XTITLE='Energy (eV)', YTITLE='Counts'
endif else begin
  function_1d,(*st).function_1d, indgen((*st).histogram_bins), $
              (*(*st).data)[(*st).current_bin].HISTGRM, $
              DATASET_NAME='Histogram Data', TITLE=test_id, $
              SUBTITLE=str, PSYM=10, XTITLE='Energy (DN)', YTITLE='Counts'
endelse

RETURN
END

;---------------------------------------------------------------------------
; Event handler for the Histogram tool.
;---------------------------------------------------------------------------
PRO HistToolEvent, event

;; Get the widget state structure.
top_base = event.handler
widget_control, widget_info(top_base,/CHILD),  GET_UVALUE=st
widget_control, /HOURGLASS

destroyFlag = 0

case event.id of

  ;------------------------------------------------------------------------
  (*st).top_base: 
 
  ;------------------------------------------------------------------------
  (*st).next_button: begin
    if( (*st).current_bin LT (*st).num_bins-1) then begin
      (*st).current_bin = (*st).current_bin + 1
      PlotEventHistogramData, st
    endif
  end
 
  ;------------------------------------------------------------------------
  (*st).prev_button: begin
    if( (*st).current_bin GT 0) then begin
      (*st).current_bin = (*st).current_bin - 1
      PlotEventHistogramData, st
    endif
  end
 
  ;------------------------------------------------------------------------
  (*st).file_button: begin
     infile = Dialog_PickFile(TITLE="Select an Input File", /MUST_EXIST, $
			      FILTER='te*h*')
     if(infile NE '') then begin
       data = mrdfits(infile, 1)

       (*st).current_bin = 0
       (*st).num_bins = n_elements(data)
       (*st).filename = infile
       (*st).histogram_bins = n_elements(data[0].HISTGRM)

       ptr_free,(*st).fileheader
       (*st).fileheader = ptr_new(headfits(infile),/NO_COPY)

       ptr_free,(*st).data
       (*st).data = ptr_new(data,/NO_COPY)

       PlotEventHistogramData, st
     endif
  end

  ;------------------------------------------------------------------------
  (*st).dn2ev_button: begin
     infile = Dialog_PickFile(TITLE="Select a DN to eV Conversion File", $
			      /MUST_EXIST, FILTER='*.dn2ev')
     if(infile NE '') then begin
       table = read_ascii(infile)
       ptr_free, (*st).dn2evtable
       (*st).dn2evfile  = infile
       (*st).dn2evtable = ptr_new(table,/NO_COPY)
       PlotEventHistogramData, st
     endif
  end

  ;------------------------------------------------------------------------
  (*st).dnevselect: begin
    if(event.value EQ 'DN') then begin
      (*st).ev_button_selected = 0
      PlotEventHistogramData, st
    endif else begin
      (*st).ev_button_selected = 1
      if((*st).dn2evfile EQ '') then begin
        infile = Dialog_PickFile(TITLE="Select a DN to eV Conversion File", $
  			       /MUST_EXIST, FILTER='*.dn2ev')
        if(infile NE '') then begin
          table = read_ascii(infile)
          ptr_free, (*st).dn2evtable
          (*st).dn2evfile  = infile
          (*st).dn2evtable = ptr_new(table,/NO_COPY)
          PlotEventHistogramData, st
        endif else begin
          (*st).ev_button_selected = 0
	  widget_control,(*st).dnevselect, SET_VALUE=0
	endelse
      endif else begin
        (*st).ev_button_selected = 1
        PlotEventHistogramData, st
      endelse
    endelse
  end

  ;------------------------------------------------------------------------
  (*st).quit_button: $
     destroyFlag = 1

  ;------------------------------------------------------------------------
  else: message, 'event not handled'

endcase

;;; Destroy the widget if desired.
if (Destroyflag) then begin
  widget_control, top_base, /DESTROY
endif

END

