;;; $Id: tara_install.pro 3801 2010-11-19 14:15:01Z psb6 $
;;; Perform TARA installation tasks.

PRO tara_install

;; The indexed_add shared library name is based on 32bit or 64bit architecture & OS.
if (!VERSION.MEMORY_BITS EQ 64) then begin
  link_base   = 'indexed_add_64' 
  extra_cflags=''
  extra_lflags=''
endif else begin
  link_base   = 'indexed_add'
  extra_cflags='-m32'
  extra_lflags='-m32'
endelse  

lib_base = !VERSION.OS + '_' +!VERSION.ARCH + '_' + link_base
print, lib_base

color_manager, UTILITY_DIR=utility_dir

if NOT file_test(utility_dir + lib_base + '.so') then begin
  ;; Compile utilities/indexed_add.c into a sharable library.
  print, 'Compiling indexed_add.c'
  make_dll, 'indexed_add', lib_base, ['indexed_add_long','indexed_add_float','indexed_add_double'], COMPILE_DIRECTORY=utility_dir, EXTRA_CFLAGS=extra_cflags, EXTRA_LFLAGS=extra_lflags, /VER, /SHOW
endif

;; Link to correct shared library for your OS.
link_path = utility_dir + link_base + '.so'
file_delete, link_path, /QUIET
file_link, lib_base + '.so', link_path, /VERBOSE


;; Test the indexed_add.so
linkimage, 'indexed_add_double', link_path
function_2d, id, indgen(16,16)

return
end
