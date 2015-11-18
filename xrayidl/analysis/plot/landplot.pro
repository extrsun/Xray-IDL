
pro landplot
;** To plot over an entire page in the 'landscape' mode.
;** Modified by kachun 25 May, 1994; changed from (xsize,ysize)=(9.06,7.)".
set_plot,'ps'
device,/landscape,ysize=6.3,xsize=8.154,/inches,bits_per_pixel=8
end
