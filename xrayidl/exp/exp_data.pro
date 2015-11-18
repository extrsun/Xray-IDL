pro	exp_data,attitude,livtime
; this program is to replace the original exp_data.pro writen by JM
	if !proc eq 'MPE' then seqno=strtrim(!seq_no,2)+'_attitude' else $
		seqno=strtrim(!seq_no,2)
	inputs='obseq='+seqno+',dir='+strtrim(!data_dir,2)+ $
		',proc='+strtrim(!proc,2)

;	create attitude file.
	print,'Creating attitude data file...'
	rsgetasp,inputs,0,sctime,roll,xoff,yoff
	b=n_elements(sctime)
	attitude=lonarr(5,b)
	attitude(0,*)=indgen(b)
	attitude(1,*)=sctime
  	attitude(2,*)=xoff
	attitude(3,*)=yoff
	attitude(4,*)=roll*(7200.) ;converted to the units of 0.5 arcsec

;	create livtime.
	if !proc eq 'MPE' then seqno=strtrim(!seq_no,2)+'_eventrates' else $
		seqno=strtrim(!seq_no,2)
	inputs='obseq='+seqno+',dir='+strtrim(!data_dir,2)+ $
		',proc='+strtrim(!proc,2)

	print,'Creating livtime and eventrates data files...'
	rsgetevr,inputs,0,sctime,rates

;	livtime=fltarr(5,b) ;cause errors in the conversion
	b=n_elements(sctime)
	livtime=lonarr(5,b)
	livtime(0,*)=indgen(b)
	livtime(1,*)=sctime
	livtime(2,*)=rates.terate
	livtime(3,*)=rates.aerate
	livtime(4,*)=rates.a1rate
	return
	end
