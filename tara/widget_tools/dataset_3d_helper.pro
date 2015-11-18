;;; $Id: dataset_3d_helper.pro 4213 2012-05-01 16:48:49Z psb6 $
;;; Copyright (C) 2002, Pennsylvania State University
;;; Pat Broos (patb@astro.psu.edu)

;;; This routine is executed by a second "helper" IDL process
;;; spawned by dataset_3d to compute statistic maps.
;;; It carries on a carefully orchestrated dance with the main
;;; IDL process to pass information back and forth.

PRO dataset_3d_helper

;; Read the input filename used by the parent process.
input_filename = ''
read, input_filename


;; Build a kernel descriptor filename, remove any existing file, and
;; send the name to the parent process.
kernel_descriptor_filename = input_filename + '.kernels'

file_delete, kernel_descriptor_filename, /QUIET
dum = findfile(kernel_descriptor_filename, COUNT=count)
if (count NE 0) then message, 'Could not remove ' + kernel_descriptor_filename

print, kernel_descriptor_filename
printf, -2, 'kernel_descriptor_filename = '+ kernel_descriptor_filename


;; Read the input save file which should contain variables named:
;;   name, min_significance, 
;;   require_compatible_flux, counts_per_group 
restore, input_filename
printf, -2, 'restored '+ input_filename
file_delete, input_filename

;map_filename = input_filename + '.maps'

if require_compatible_flux then begin
  ;; PERFORM ADAPTIVE KERNEL SMOOTHING WITH PRUNING
  
  clipped_density_2d, counts_per_group, min_significance, $
			flux_map, error_map, radius_map, exclude_map, $
			KERNEL_FILE=kernel_descriptor_filename
;  save, FILENAME=map_filename, flux_map, error_map, radius_map, exclude_map
    
endif else begin
  ;; PERFORM SIMPLE ADAPTIVE KERNEL SMOOTHING
  
  adaptive_density_2d, counts_per_group, min_significance, $
			flux_map, error_map, radius_map, $
			KERNEL_FILE=kernel_descriptor_filename
;  save, FILENAME=map_filename, flux_map, error_map, radius_map
  
  
endelse

;printf, -2, 'Flux, error, radius, & exclude maps saved in ', map_filename

printf, -2, 'dataset_3d_helper finished'
exit	   
end


