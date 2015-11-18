function figure_acis_res, plist, e=e

if n_params(0) eq 0 then begin
    print, 'acis_res = figure_acis_res(plist, e=e)'
    print, 'Computes acis resolution based on Karen Leighly''s fit to Figure 1.2'
    print, 'of the AXAF Prop. Guide'
    print, 'If eneries have already been estimated, supply in E otherwise they'
    print, 'are estimated using acis_pha_to_kev.'
    retall
endif

if n_elements(e) eq 0 then e = acis_pha_to_kev(plist)

res = plist.pha*0.
w = where(plist.ccdid ne 5 and plist.ccdid ne 7, n)
if n gt 0 then res(w) = 1./10^(0.609*alog10(e(w))+1.176)
w = where(plist.ccdid eq 5 or plist.ccdid eq 7, n)
if n gt 0 then res(w) = 1./10^(0.821*alog10(e(w))+0.903)

return, res
end
