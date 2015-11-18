;+
;========================================================================
;;;
;;; Routine to concatenate multiple segments of a vector: $Id: concat_array_segments.pro 1137 2000-07-10 09:52:28Z patb $
;;;
;;; Copyright (C) 1998, Pennsylvania State University
;;; Pat Broos (patb@astro.psu.edu)
;;;
;========================================================================
;-
PRO concat_array_segments, sorted_data, seg_start, seg_end, data, num_data

num_data     = total( (seg_end - seg_start + 1) > 0 )
num_segments = n_elements(seg_start)

if (num_data EQ 0) then begin
  data = 0 & dum = temporary(data)
  return
endif

if (num_segments EQ 1) then begin
  data = sorted_data[seg_start[0]:seg_end[0]]
  return
endif

data = make_array( num_data, TYPE=size(sorted_data,/TYPE), /NOZERO )
i_data = 0L
for ii=0L, num_segments-1 do begin
  seg_length = seg_end[ii] - seg_start[ii] + 1
  if (seg_length GT 0) then begin
    data[i_data] = sorted_data[seg_start[ii]:seg_end[ii]]
    i_data = i_data + seg_length
  endif
endfor
return
end


