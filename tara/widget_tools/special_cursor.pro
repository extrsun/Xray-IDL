;+
;========================================================================
;;;
;;; FILE NAME:    $Id: special_cursor.pro 311 1997-07-15 10:13:57Z patb $
;;;
;;; DESCRIPTION:  Routine to change the cursor to one of several special styles.
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1994, Pennsylvania State University
;;;
;;; NOTES:
;;;               $Id: special_cursor.pro 311 1997-07-15 10:13:57Z patb $
;-
;==========================================================================
PRO special_cursor, Style

case Style of
 ;; Left boundary  -->|
 'LEFT_BOUNDARY': begin
     Image = [  [255B, 063B],			$
		[255B, 063B],			$
		[255B, 063B],			$
		[255B, 057B],			$
		[255B, 051B],			$
		[255B, 039B],			$
		[255B, 031B],			$
		[000B, 000B],			$
		[000B, 000B],			$
		[255B, 031B],			$
		[255B, 039B],			$
		[255B, 051B],			$
		[255B, 057B],			$
		[255B, 063B],			$
		[255B, 063B],			$
		[255B, 063B]			$
		]
     device, CURSOR_IMAGE=fix(Image,0,16), CURSOR_XY=[15,8], $
		  CURSOR_MASK =replicate(-1,16)
     end

 ;; Right boundary  |<--
  'RIGHT_BOUNDARY': begin
     Image = [  [252B, 255B],			$
		[252B, 255B],			$
		[252B, 255B],			$
		[156B, 255B],			$
		[204B, 255B],			$
		[228B, 255B],			$
		[248B, 255B],			$
		[000B, 000B],			$
		[000B, 000B],			$
		[248B, 255B],			$
		[228B, 255B],			$
		[204B, 255B],			$
		[156B, 255B],			$
		[252B, 255B],			$
		[252B, 255B],			$
		[252B, 255B]			$
		]
     device, CURSOR_IMAGE=fix(Image,0,16), CURSOR_XY=[0,8], $
		  CURSOR_MASK =replicate(-1,16)
     end

 ;; Top boundary (see above) 
  'TOP_BOUNDARY': begin
     Image = [  [127B, 254B],			$
		[127B, 254B],			$
		[127B, 254B],			$
		[127B, 254B],			$
		[127B, 254B],			$
		[127B, 254B],			$
		[127B, 254B],			$
		[127B, 254B],			$
		[127B, 254B],			$
		[119B, 238B],			$
		[103B, 230B],			$
		[079B, 242B],			$
		[095B, 250B],			$
		[063B, 252B],			$
		[000B, 000B],			$
		[000B, 000B]			$
		]
     device, CURSOR_IMAGE=fix(Image,0,16), CURSOR_XY=[8,0], $
		  CURSOR_MASK =replicate(-1,16)
     end

 ;; Bottom  boundary (see above)
  'BOTTOM_BOUNDARY': begin
     Image = [  [000B, 000B],			$
		[000B, 000B],			$
		[063B, 252B],			$
		[095B, 250B],			$
		[079B, 242B],			$
		[103B, 230B],			$
		[119B, 238B],			$
		[127B, 254B],			$
		[127B, 254B],			$
		[127B, 254B],			$
		[127B, 254B],			$
		[127B, 254B],			$
		[127B, 254B],			$
		[127B, 254B],			$
		[127B, 254B],			$
		[127B, 254B]			$
		]
     device, CURSOR_IMAGE=fix(Image,0,16), CURSOR_XY=[8,15], $
		  CURSOR_MASK =replicate(-1,16)
     end

 ;; Upper Left Corner marker
  'UL_CORNER': begin
     Image = [  [000B, 000B],			$
		[000B, 000B],			$
		[252B, 255B],			$
		[252B, 255B],			$
		[252B, 255B],			$
		[252B, 255B],			$
		[252B, 255B],			$
		[252B, 255B],			$
		[252B, 255B],			$
		[252B, 255B],			$
		[252B, 255B],			$
		[252B, 255B],			$
		[252B, 255B],			$
		[252B, 255B],			$
		[252B, 255B],			$
		[252B, 255B]			$
		]
     device, CURSOR_IMAGE=fix(Image,0,16), CURSOR_XY=[0,15], $
		  CURSOR_MASK =replicate(-1,16)
     end

 ;; Lower Right Corner marker
  'LR_CORNER': begin
     Image = [  [255B, 063B],			$
		[255B, 063B],			$
		[255B, 063B],			$
		[255B, 063B],			$
		[255B, 063B],			$
		[255B, 063B],			$
		[255B, 063B],			$
		[255B, 063B],			$
		[255B, 063B],			$
		[255B, 063B],			$
		[255B, 063B],			$
		[255B, 063B],			$
		[255B, 063B],			$
		[255B, 063B],			$
		[000B, 000B],			$
		[000B, 000B]			$
		]
     device, CURSOR_IMAGE=fix(Image,0,16), CURSOR_XY=[15,0], $
		  CURSOR_MASK =replicate(-1,16)
     end

 else: print, 'Unknown style in special_cursor'
endcase
return
end

