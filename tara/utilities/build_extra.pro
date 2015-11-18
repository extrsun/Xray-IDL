;; This routine is used in a very convoluted way to get IDl to 
;; extract keyword parameters from a user-supplied string variable.
;; For example:
;; mykwds = 'XTITLE="dog", YTITLE="cat"'
;; dum = execute( 'build_extra, extra, ' + mykwds )
;; plot, indgen(5), _EXTRA=extra

PRO build_extra, parameter_structure, _EXTRA=extra

parameter_structure = extra
return
end
