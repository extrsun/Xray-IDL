;+
;========================================================================
;;;
;;; Plot Window Topbase: $Id: plot_window_topbase.pro 1736 2003-02-07 16:57:06Z patb $
;;;
;;; Pat Broos (patb@astro.psu.edu)
;;;
;========================================================================
;;; DESCRIPTION:  
;;; This base widget should be used as the top-level base for widgets that
;;; include a plot_window widget.  This base catches resize events and
;;; interacts with the plot_window widget to resize it's draw widget so that
;;; the entire widget hierarchy achieves the desired size (if possible).
;;; The widgets sitting between this top-level base and the plot_window 
;;; don't need to do anything.
;-

PRO PlotWindowTopbaseEventFn, Event

widget_control, Event.handler, /HOURGLASS, UPDATE=0

event_type = tag_names( event, /STRUCTURE_NAME )
if (event_type EQ 'WIDGET_BASE') then begin
  ; Locate the plot_window widget in this hierarchy.
  pw_id = widget_info( Event.handler, FIND_BY_UNAME='plot_window' )
  if (pw_id EQ 0) then message, 'plot_window widget not found'

  ; Note the desired size of the top-level base.
  desired_x = event.x  &  desired_y = event.y
;print
;print, desired_x, desired_y
  
  ; Adjust the plot_window a little bit to force layout to be recomputed so
  ; we know the current natural size of the top-level base.
  plot_window, pw_id, CHANGE_DRAW_SIZE=[1,1]

  ; Compute an initial adjustment.
  ; If we need to grow we start with a big step since the growth code below
  ; will NOT increase the stepsize.
  ; If we need to shrink we start with a small step since the shrink code
  ; below DOES increase the stepsize.
  geom        = widget_info( Event.handler, /GEOMETRY)
  x_error     = fix(desired_x-geom.xsize)
  y_error     = fix(desired_y-geom.ysize)
  x_direction = ( (-1) > x_error < 1 )
  y_direction = ( (-1) > y_error < 1 )
  
  min_stepsize= 4
  x_stepsize  = abs(x_error) > min_stepsize
  y_stepsize  = abs(y_error) > min_stepsize
  
  
  ; Iteratively resize the plot_window until the top-level base has the 
  ; desired size or until we can make no progress.
  while (x_direction NE 0) OR (y_direction NE 0) do begin
    ; Try to adjust the plot_window's size.
    delta_x = x_direction*x_stepsize
    delta_y = y_direction*y_stepsize
    plot_window, pw_id, CHANGE_DRAW_SIZE=[delta_x,delta_y]
;help, delta_x, delta_y  
    
    ; Save the TLB's size before the adjustment, and measure its new size.
    prev_xsize = geom.xsize 
    prev_ysize = geom.ysize 
    geom       = widget_info( Event.handler, /GEOMETRY)
  
    ; Decide what the next adjustment should be.
    case x_direction of
      -1: begin
          if (geom.xsize EQ desired_x) then begin
            ; The TLB is the desired size.
            ; Switch to expanding the TLB with the minimum stepsize so we 
            ; ensure that the plot_window is maximized.
            x_direction = 1
            x_stepsize  = min_stepsize
            
          endif else if (geom.xsize GE desired_x) then begin
            ; The TLB is still bigger than desired.
            
            if (geom.xsize GT (prev_xsize+delta_x)) then begin
              ; The TLB is bigger than the size we requested, so we've
              ; run into a lower boundary in that last move.
              ; This minimized TLB won't shrink any more, but we can't stop
              ; now because the plot_window may very well be able to grow
              ; larger without expanding the TLB.
              ; So, we switch to growth mode with a smaller stepsize.
              x_direction = 1
              x_stepsize  = (x_stepsize/2) > min_stepsize
              
            endif else begin
              ; The TLB shrank by the amount requested.
              ; Continue shrinking, and try a larger step.
              x_direction = -1
              x_stepsize  = (x_stepsize*2)
            endelse
            
          endif else begin
            ; We have gone too far -- the TLB is too small now -- so we 
            ; need to start expanding it (with a smaller stepsize).
            x_direction = 1
            x_stepsize  = (x_stepsize/2) > min_stepsize
          endelse
          end
          
       1: begin
          if (geom.xsize EQ desired_x) then begin
            ; The TLB is the desired size.
            if (geom.xsize GT prev_xsize) then begin
              ; The TLB grew on that last adjustment, so the plot_window must
              ; be maximized.  WE'RE DONE!!
              x_direction = 0
            endif else begin
              ; The TLB remained the same size on that last adjustment.
              ; This indicates that the plot_window has some room to grow,
              ; so let's just keep going with the same stepsize.
              x_direction = 1
            endelse
            
          endif else if (geom.xsize LT desired_x) then begin
            ; The TLB is still smaller than desired.
            ; Continue expanding.  We can't try a larger step here because
            ; that could lead to an infinite loop.
            x_direction = 1
            
          endif else begin
            ; The TLB is bigger than desired.
            
            if (geom.xsize GT prev_xsize) then begin
              ; The TLB grew on that last adjustment, so the plot_window must
              ; be maximized within the TLB.
            
              if (x_stepsize EQ min_stepsize) then begin
                ; The stepsize we just used was the smallest one.
                ; Switching to contraction mode is not going to help -- if 
                ; contracting by a tiny step was ok we wouldn't have been
                ; sent here.
                ; So, the desired size of the TLB must be smaller than it's 
                ; minimum size (which we've currently achieved).
                ; Since we know the plot_window is maximized we're DONE!!
                x_direction = 0
                
              endif else begin
                ; We've overshot the goal so let's start contracting 
                ; with a smaller stepsize.
                x_direction = -1
                x_stepsize  = (x_stepsize/2) > min_stepsize
              endelse

            endif else begin
              ; The TLB remained the same size on that last adjustment.
              ; This indicates that the plot_window has some room to grow,
              ; so let's just keep going with the same stepsize
              x_direction = 1
            endelse
          endelse ;TLB too big
          end
          
       0: 
    endcase

    ; Do the same for Y.  We omit the comments here.
    case y_direction of
      -1: begin
          if (geom.ysize EQ desired_y) then begin
            y_direction = 1
            y_stepsize  = min_stepsize
            
          endif else if (geom.ysize GE desired_y) then begin
            
            if (geom.ysize GT (prev_ysize+delta_y)) then begin
              y_direction = 1
              y_stepsize  = (y_stepsize/2) > min_stepsize
              
            endif else begin
              y_direction = -1
              y_stepsize  = (y_stepsize*2)
            endelse
            
          endif else begin
            y_direction = 1
            y_stepsize  = (y_stepsize/2) > min_stepsize
          endelse
          end
          
       1: begin
          if (geom.ysize EQ desired_y) then begin
            if (geom.ysize GT prev_ysize) then begin
              y_direction = 0
            endif else begin
              y_direction = 1
            endelse
            
          endif else if (geom.ysize LT desired_y) then begin
            y_direction = 1
            
          endif else begin
            
            if (geom.ysize GT prev_ysize) then begin
            
              if (y_stepsize EQ min_stepsize) then begin
                y_direction = 0
                
              endif else begin
                y_direction = -1
                y_stepsize  = (y_stepsize/2) > min_stepsize
              endelse

            endif else begin
              y_direction = 1
            endelse
          endelse ;TLB too big
          end
          
       0: 
    endcase


  endwhile

; Ask the plot_window to generate an event it its parent.
plot_window, pw_id, /GENERATE_EVENT
endif

widget_control, Event.handler, UPDATE=1
return
end


PRO plot_window_realize, id

event = {WIDGET_BASE, ID:id, TOP:id, HANDLER:id, X:10, Y:10}
PlotWindowTopbaseEventFn, event

return
end


FUNCTION plot_window_topbase, _EXTRA=extra

return, widget_base( /TLB_SIZE_EVENTS, _EXTRA=extra )
end

