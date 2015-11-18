;;; $Id: $
;;; Patrick Broos, 2008

;;; Tool to visualize differences between the DATA (not keywords) in two event lists.
;;; NOTE that the CIAO tool dmdiff is also very useful for comparing files!!!!
;;; This tool, however, can compare the STATUS column of ACIS files.

;;; FILTER_EXPRESSION (optional) can be any boolean expression involving the variables "old" and "new", e.g.
;;    FILTER_EXPRESSION='(new.GRADE EQ 0) OR (new.GRADE EQ 2) OR (new.GRADE EQ 3) OR (new.GRADE EQ 4) OR (new.GRADE EQ 6)'
;;  or
;;    FILTER_EXPRESSION='(new.GRADE EQ 1) OR (new.GRADE EQ 5) OR (new.GRADE EQ 7)'

PRO compare_event_lists, eventlist1_fn, eventlist2_fn, PLOT=plot, FILTER_EXPRESSION=filter_expression, TITLE=user_title
           
if n_elements(plot      ) EQ 0 then plot=1
if n_elements(user_title) EQ 0 then user_title=''

old = mrdfits(eventlist1_fn, 1)
new = mrdfits(eventlist2_fn, 1)
num_events = n_elements(new)
if (n_elements(old) NE num_events) then begin
  print, 'ERROR!  The number of events has changed!!!'
  retall
endif

if keyword_set(filter_expression) then begin      
  cmd = "ind = where("+filter_expression+", num_events)"
  if NOT execute(cmd) then message, 'Could not execute this command: '+cmd
  old = old[ind]
  new = new[ind]
endif                                                 

old_status = swap_endian(ulong(old.STATUS,0,num_events), /SWAP_IF_LITTLE_ENDIAN)
new_status = swap_endian(ulong(new.STATUS,0,num_events), /SWAP_IF_LITTLE_ENDIAN)
old_tag_names = tag_names(old)                    
new_tag_names = tag_names(new)                    

plot_names = ['TIME',     'X',     'Y','PHA','ENERGY',     'PI','FLTGRADE','GRADE']
plot_units = [   's','skypix','skypix', 'DN',    'eV','channel',    'none', 'none']

name=strarr(32)
; Taken from "ACIS Event Data STATUS Bits memo, revision 2.2".
; Bits 16-19 and 31 descriptions revised for afterglow workaround used in our L1->L2 recipe..
name[0]  = 'invalid CHIP coordinates'
name[1]  = 'invalid PHAS'
name[2]  = 'PHAS overflow'
name[3]  = 'PHA too high'
name[4]  = '"bad pixel"'
name[5]  = 'island touches "bad pixel"'
name[6]  = 'bias=4095'
name[7]  = 'bias missing'
name[8]  = 'bias parity error'
name[9]  = 'overclock unknown'
name[10] = 'overclock out of range'
name[11] = 'corner pixels too small'
name[14] = 'corner pixels "bad"'
name[15] = 'flag from destreak tool'
name[16] = 'afterglow flag from acis_detect_afterglow'
name[17] = 'afterglow info from acis_detect_afterglow'
name[18] = 'afterglow info from acis_detect_afterglow'
name[19] = 'afterglow info from acis_detect_afterglow'
name[20] = 'CTI algorithm did not converge'
name[21] = 'flag from acisreadcorr tool'
name[22] = 'flag from acisreadcorr tool'
name[23] = 'flag from Clean55 algorithm'
name[31] = 'afterglow flag from acis_run_hotpix'

if keyword_set(plot) then begin
  distance_moved = 0.
  
  row_number = lindgen(num_events) 
  
  for ii=0,n_elements(plot_names)-1 do begin
    new_tag_index = where(new_tag_names EQ plot_names[ii], count)
    if (count EQ 0) then continue
    old_tag_index = where(old_tag_names EQ plot_names[ii], count)
    
    ordinate_title = 'new '+plot_names[ii]+' - old '+plot_names[ii]+' ['+plot_units[ii]+']'
    
    if (plot_names[ii] EQ 'X') || (plot_names[ii] EQ 'Y') then begin
      id=0L
      offset = new.(new_tag_index[0])-old.(old_tag_index[0])
      distance_moved += offset^2
      dataset_1d,id, offset, XTIT=ordinate_title, DENSITY_TITLE=user_title + (keyword_set(filter_expression) ? filter_expression : ' ')
      
      abscissa       = row_number
      abscissa_title = 'row number'
    endif else begin
      abscissa       = new.(new_tag_index[0])
      abscissa_title = 'new '+plot_names[ii]+' ['+plot_units[ii]+']'
    endelse
    
    id=0L
    dataset_2d,id, abscissa, new.(new_tag_index[0])-old.(old_tag_index[0]),XTIT=abscissa_title, YTIT=ordinate_title, TITLE=user_title + (keyword_set(filter_expression) ? filter_expression : ' '), PSYM=1   
  endfor
  
  dataset_1d, id_dist, sqrt(distance_moved), XTIT='Distance Moved [skypix]', DENSITY_TITLE=user_title + (keyword_set(filter_expression) ? filter_expression : ' ')
endif

print
if keyword_set(filter_expression) then print, 'Plots and table are for events selected by: ', filter_expression 
print, '------number of events------   bit#  description'
print, '    1->0     0->1  (now set)'
for ii=0,31 do begin
  mask = ishft('1'XUL,ii)
  old_bit  = long(ishft(mask AND old_status, -ii))
  new_bit  = long(ishft(mask AND new_status, -ii))
  diff_bit = new_bit - old_bit  
  print, total(/INT, diff_bit EQ -1), total(/INT, diff_bit EQ 1), total(/INT, new_bit), ii, name[ii], F='(%"%8d %8d (%8d)  bit%d (%s)")'
endfor

return
end


