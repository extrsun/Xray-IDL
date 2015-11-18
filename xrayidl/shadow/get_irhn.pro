pro get_irhn,ir100,ir60,irhn,para=para
if n_params() eq 0 then begin
print,'CALLING SEQUENCE --- get_irhn,ir100,ir60,irhn,para=para'
return
endif
if n_elements(para) eq 0 then para=1.
irhn=ir100*(0.247+0.1849*para*(imdiv(ir100,ir60) > 5.))
return
end