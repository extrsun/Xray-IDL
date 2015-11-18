;+
;========================================================================
;;;
;;; FILE NAME:    $Id: histtool.pro 824 1999-07-30 11:40:11Z tsk $
;;;
;;; DESCRIPTION:  Histogram Tool Application
;;;
;;; AUTHOR:       Scott Koch (tsk@astro.psu.edu)
;;;               Pat Broos (patb@astro.psu.edu)
;;;               Copyright (C) 1999, Pennsylvania State University
;;;
;;; NOTES:        This tool is used to analyze data which is stored as a
;;;               1-D histogram.  This tool allows the user to look at
;;;               histogram data which is stored in the first extension of
;;;               a FITS file.  For the case of ACIS, histogram data of a
;;;               given exposure of (ccd_id,amp_id) is stored as a separate
;;;               histogram in each row of a binary table.  This tool allows
;;;               the user to cycle through each row of that FITS table and
;;;               play with each histogram.
;;;
;;;               If you want to bin up all of the histograms in an ACIS
;;;               TE HISTOGRAM or TE EVENT HISTOGRAM file, then use the 'bh'
;;;               tool.  The output of that file will contain one summed
;;;               histogram for each amp of each ccd.  Use Histtool to
;;;               play with the output of 'bh'.
;;;
;;;               Non-histogram data is better analyzed with the 'dataset_1d'
;;;               tool.
;;;
;========================================================================
;-
PRO histtool

top_base = widget_base( TITLE='Histogram Tool', /COLUMN )
  base0 = widget_base(top_base,/ROW)
  function_1d, function_1d, PARENT_WIDGET=base0

  base2 = widget_base(top_base, /ROW, /FRAME)
    next_button = widget_button(base2,VALUE='Next Histogram')
    prev_button = widget_button(base2,VALUE='Previous Histogram')
    dnevselect = cw_bgroup(base2,['DN','eV'],/ROW,/EXCLUSIVE,SET_VALUE=0, $
			   /RETURN_NAME)
    file_button = widget_button(base2,VALUE='Input File')
    dn2ev_button = widget_button(base2,VALUE='dn2ev File')
    quit_button = widget_button(base2,VALUE='Quit')

state = { $
  top_base:top_base, $
  function_1d:function_1d, base0:base0, base2:base2, $
  next_button:next_button, prev_button:prev_button, $
  dnevselect:dnevselect, $
  file_button:file_button, dn2ev_button:dn2ev_button, $
  quit_button:quit_button, $

  ; State of exclusve buttons
  ev_button_selected:0, $

  ; Histogram Data
  data:ptr_new(), $
  current_bin:0L, $
  num_bins:0L,    $
  histogram_bins:0L, $
  filename:'',    $ 
  fileheader:ptr_new(), $
  dn2evfile:'',  $
  dn2evtable:ptr_new() $
}

widget_control, widget_info(top_base, /CHILD), $
		SET_UVALUE=ptr_new(state, /NO_COPY)

;; Realize the widget and register with manager.
widget_control, top_base, /REALIZE

event = {ID:file_button, TOP:top_base, HANDLER:top_base}
widget_control, file_button, SEND_EVENT=event

xmanager, 'Histogram Tool', top_base, EVENT_HANDLER='HistToolEvent', /NO_BLOCK

END
