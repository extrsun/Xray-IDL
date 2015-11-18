pro qual_list,filename,chatter
;
; procedure to print out file containing list of useful procedures
;
filename=strupcase(strmid(filename))
spawn,'print filename'+'.LST'
print,'   '
;
return
end        ;pro qual_list
