pro compute_excess_var, srclc, srclcerr, srcvar, srcvarerr, bgdlc, bgdlcerr, $
  bgdvar, bgdvarerr, verbose=verbose

if n_params(0) eq 0 then begin
  print, 'compute_excess_var, srclc, srclcerr, bgdlc, bgdlcerr, srcvar, '
  print, '  srcvarerr, bgdvar, bgdvarerr
  retall
endif

np = n_elements(srclc)
;if n_elements(bgdlc) ne np then stop  
smean = avg(srclc)
if keyword_set(verbose) then print, 'Source mean = ' + strn(smean)
srcvar = total((srclc - smean)^2 - srclcerr^2)/smean^2/np
srcsd = total(((srclc - smean)^2 - srclcerr^2 - smean^2*srcvar)^2)/(np - 1)
srcvarerr = sqrt(srcsd)/smean^2/sqrt(np)

if n_elements(bgdlc) eq n_elements(srclc)  then begin 
  bmean = avg(bgdlc)
  if keyword_set(verbose) then print, 'Bgd mean = ' + strn(bmean)
  bgdvar = total((bgdlc - bmean)^2 - bgdlcerr^2)/bmean^2/np
  bgdsd = total(((bgdlc - bmean)^2 - bgdlcerr^2 - $
                bmean^2*bgdvar)^2)/(np - 1)
  bgdvarerr = sqrt(bgdsd)/bmean^2/sqrt(np)
endif

return
end
