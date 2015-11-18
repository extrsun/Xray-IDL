;;;===========================================================================
;;; $Id: connected_region.pro 1295 2000-12-08 10:22:56Z patb $
;;;
;;; Routine to find the smallest connected region of pixels in an image that
;;; meets the following conditions:
;;; * The root of the group is pixel [root_x, root_y].
;;; * All the pixel locations satisfy a specified boolean condition passed 
;;;   via the 2-D array 'pixel_is_good'.  
;;; * The sum of the pixels in the region is at least 'min_counts'
;;;===========================================================================

PRO connected_region, root_x, root_y, pixel_is_good, counts, min_counts, $
		      region_index

size_str = size(pixel_is_good, /STRUCTURE)
xdim     = (size_str.dimensions)[0]
ydim     = (size_str.dimensions)[1]

max_x = xdim-1
max_y = ydim-1

;; Create a boolean array representing the pixels known to be members of
;; the region.
region_index = root_y*xdim + root_x
region_size  = 1

total_counts = counts[root_x, root_y]
done         = total_counts GE min_counts

while ( NOT done ) do begin
  ;; Construct the x & y coordinates of a set of pixels proposed to be in 
  ;; the region.  We start with pixels already in the region, then step
  ;; outward in the four directions, being careful to prevent wrapping.
  region_y = fix(region_index / xdim)
  region_x = region_index - (region_y * xdim)

  proposed_x = [region_x, region_x+1<max_x, region_x-1>0, region_x, region_x]
  proposed_y = [region_y, region_y, region_y, region_y+1<max_y, region_y-1>0]
    
  ;; Throw away duplicates -- most easily done in 1D indexing space.
  proposed_i = proposed_y*xdim + proposed_x
  proposed_i = proposed_i[ uniq(proposed_i, sort(proposed_i)) ]

  ;; We want to retain only those pixels in proposed_region which are 
  ;; marked as good by caller.
  ;; Compute the region size & total counts in the new region.
  prev_region_size = region_size
  
  good          = where( pixel_is_good[proposed_i], region_size )
  region_index  = proposed_i[good]
    
  total_counts  = total( counts[region_index] )
  
  
  ;; Stop if the region is not growing, or now has enough counts.
  if ((region_size LE prev_region_size) OR $
      (total_counts GE min_counts)) then done = 1
endwhile

return
end


PRO test
nx=734
ny=578
min_counts=50

counts = long(random(nx,ny) GE 0.5)
pixel_is_good = replicate(1B,nx,ny)
t0=systime(1)
for x=0,100,2 do begin
  for y = 0,100,2 do begin
;    connected_region, x, y, pixel_is_good, counts, min_counts, findex
    slow,             x, y, pixel_is_good, counts, min_counts, sindex
;    if n_elements(findex) NE  n_elements(sindex) then message
;    if (total(findex-sindex) NE 0) then message
  endfor
  print, x, systime(1)-t0
endfor
return
end

  
