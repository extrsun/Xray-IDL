;============================================================================
;;; Routine to save the WORKING datasets from TARA's event database to 
;;; a FITS file.
;;; $Id: writeevents.pro 2878 2007-11-01 16:06:05Z patb $
;;; PRO WriteEvents, names, pathname, ERROR=error
;;;
;;; "names" is a list of property names to be saved.
;;; "pathname" is a file pathname.
;;; If an error occurs, an error string is returned in ERROR.
;============================================================================

PRO WriteEvents, names, pathname, ERROR=error

error = ''

;; Write the primary HDU.
pheader = GetPrimaryKywds()
fxhmake, pheader, /EXTEND

fdecomp, pathname, disk, item_path, item_name, item_qual
if ('' NE item_qual)  then item_name = item_name+ '.' +item_qual
fxaddpar, pheader, "FNFITS", item_name

writefits, pathname, 0, pheader


num_events = CountXrayData(/WORKING_DATASET)
if (num_events EQ 0) then return

num_names = n_elements(names)

col_types = ptrarr(num_names)
col_names = strarr(num_names)

;; Construct the FITS table we want to write out
theader = GetTableKywds()
fxaddpar, theader, 'EXTNAME', 'EVENTS'

;; Add FITS keywords that describe the table columns.
col_num = 1
for ii=0, num_names-1 do begin
  cn  = string(col_num, f='(I0)')
        
  found = GetProperty(names[ii], prop_ptr, DDS_DATA=dds_data, $
  					   ONE_DATUM=one_datum)
  if (0 EQ found) then begin
    error=string(names[ii],f='("Property ",A," cannot be defined!")')
    return
  endif
        
  col_types[ii] = ptr_new(one_datum)
  col_names[ii] = (*prop_ptr).col_name
        
  fxaddpar, theader, 'TUNIT'+cn, $
        		   (*prop_ptr).unit_name, (*prop_ptr).unit_txt
        
  if ((*prop_ptr).dim_spec NE '') then begin
          fxaddpar, theader, 'TDIM'+cn, $
        		   (*prop_ptr).dim_spec, (*prop_ptr).dim_txt
  endif
        
  wcs_object, (*prop_ptr).wcs, IS_NULL=is_null, $
        	    CTYP=ctyp, CRVL=crvl, CRPX=crpx, CDLT=cdlt, /GET
  if (is_null EQ 0) then begin
          fmt = '(G20.14)'
          fxaddpar, theader, 'TCTYP'+cn, ctyp
          fxaddpar, theader, 'TCRVL'+cn, crvl, F=fmt
          fxaddpar, theader, 'TCRPX'+cn, crpx, F=fmt
          fxaddpar, theader, 'TCDLT'+cn, cdlt, F=fmt
  endif
        
  ; The tool ds9 needs TLMIN & TLMAX.
  if (col_names[ii] EQ 'X' OR col_names[ii] EQ 'Y') then begin
	  tlmin = min( *dds_data, MAX=tlmax )
	  fxaddpar, theader, 'TLMIN'+cn, floor(tlmin)
	  fxaddpar, theader, 'TLMAX'+cn, ceil(tlmax)
  endif

  col_num = col_num + 1
endfor
    

fxaddpar, theader, 'COMMENT', ' ', before='TUNIT1'
fxaddpar, theader, 'COMMENT', ' *** Column units ***', before='TUNIT1'
fxaddpar, theader, 'COMMENT', ' ', before='TUNIT1'


;; Verify there are no duplicate column names.
if (num_names NE n_elements(uniq(col_names,sort(col_names)))) then begin
  error='Duplicate FITS column names found.'
  return
endif


;; Create a structure template for the FITS table data , checking for
;; duplicate FITS column names (== structure tags)..
st  = create_struct( col_names[0], *(col_types[0]) )
        		
for ii=1, num_names-1 do begin
    st  = create_struct( st, col_names[ii], *(col_types[ii]) )
endfor
  
ptr_free, col_types    

;; We've now defined the format of the table -- let's create the table
;; and fill it with data.
bin_table = replicate( st, num_events )
    
for ii=0, num_names-1 do begin
      dum = GetProperty( names[ii], WDS_DATA=wds_data )
      bin_table.(ii) = temporary(wds_data)
endfor
      


;; Write the binary table.
mwrfits, bin_table, pathname, theader

print, num_events, pathname, F='("Wrote ",I0," events to ",A," ...")'

return
end
