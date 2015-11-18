pro lx
;** Close and plot out idl.ps file, and return to XTERM plotting window.
device,/close
file='idl.'+strlowcase(!D.NAME)
cmd='lpr -s -Pps '+file
spawn,cmd
set_plot,'x'
end

