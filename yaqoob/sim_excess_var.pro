pro sim_excess_var, times, srcrate, bgdrate, npoints, src_sigrms, bgd_sigrms, $
       verbose=verbose, src_mean_sigrms=src_mean_sigrms, $
       bgd_mean_sigrms=bgd_mean_sigrms

if n_params(0) eq 0 then begin
  print, 'sim_excess_var, times, srcrate, bgdrate, npoints, src_sigrms, $'
  print, '  bgd_sigrms, verbose=verbose'
  retall
endif

src_sigrms = fltarr(npoints) & bgd_sigrms = fltarr(npoints)
src_mean_sigrms = 0.
bgd_mean_sigrms = 0.
if n_elements(times) gt 1 then times = reform(times)
if n_elements(srcrate) gt 1 then srcrate = reform(srcrate)
if n_elements(bgdrate) gt 1 then bgdrate = reform(bgdrate)
ntimes = n_elements(times)
totmu = (srcrate + bgdrate)*times
bgdmu = bgdrate*times
bgd_var = 0
bgd_var_err = 0
bgd_cnts = 0
for i=0, npoints-1 do begin
  tot_cnts = poidev(totmu, seed=seed)
  if bgdrate gt 0. then bgd_cnts = poidev(bgdmu, seed=seed)
  src_lc = (tot_cnts - bgd_cnts)/times
  src_lc_err = sqrt(tot_cnts + bgd_cnts)/times
  if bgdrate gt 0. then begin
    bgd_lc = bgd_cnts/times
    bgd_lc_err = sqrt(bgd_cnts)/times
  endif 
  
  if bgdrate gt 0. then compute_excess_var, src_lc, src_lc_err, src_var, $
    src_var_err, bgd_lc, bgd_lc_err, bgd_var, bgd_var_err else $
    compute_excess_var, src_lc, src_lc_err, src_var, src_var_err

  if keyword_set(verbose) then print, i+1, src_var, src_var_err, bgd_var, $
    bgd_var_err
  src_sigrms(i) = src_var
  bgd_sigrms(i) = bgd_var
  src_mean_sigrms = src_mean_sigrms + src_var_err
  bgd_mean_sigrms = bgd_mean_sigrms + bgd_var_err

endfor

src_mean_sigrms = src_mean_sigrms/npoints
bgd_mean_sigrms = bgd_mean_sigrms/npoints

if keyword_set(verbose) then begin
  print, 'Mean error in src. sigrms = ', src_mean_sigrms
  print, 'Mean error in bgd. sigrms = ', bgd_mean_sigrms
endif

return
end
