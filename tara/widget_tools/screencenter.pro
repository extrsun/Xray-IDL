;+
;========================================================================
;;;
;;; FILE NAME:    $Id: screencenter.pro 311 1997-07-15 10:13:57Z patb $
;;;
;;; DESCRIPTION:  Widget Programming utility to find the center of the
;;;               caller's screen
;;;
;;;               This program determines the center of the caller's screen,
;;;               and returns the (x,y) coordinates in pixels as a two-element
;;;               INTEGER vector.
;;;
;;;               Calling Sequence: center = ScreenCenter()
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1996, Pennsylvania State University
;;;
;;; NOTES:
;;;               If the current graphics device is not set for screen output,
;;;               this routine will fail by stopping execution within the DEVICE
;;;               command.
;;;
;;;               Fractional values are rounded down.
;;;
;-
;==========================================================================
FUNCTION ScreenCenter

center = intarr(2)
device,get_screen_size=center
center = center / 2

RETURN,center
END
