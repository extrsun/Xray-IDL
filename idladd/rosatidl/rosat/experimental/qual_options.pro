pro qual_options,type,docdir=docdir
;
; procedure to list options for doc_list
;
if (n_elements(docdir) eq 0) then docdir = ''
if (docdir eq '') then zdoc = !docdir else zdoc = docdir
if (!version.os ne 'vms') then zdoc = zdoc+'/'
;
print,'    '
print,' You have selected project '+type
print,' To get a list of procedures for a particular topic, set qual equal to '
print,'    one of the following strings: '
;
type = strtrim(type,2)
type,zdoc+type+'.lst'
print,'    '
print,' or set qual = "ALL" to get a list for all topics'
print,'      '
print,' For more complete information on a particular procedure, '$
     +'use doc_library'
print,'      '
;
return
end       ;pro qual_options
