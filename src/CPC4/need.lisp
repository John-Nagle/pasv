"@(#)need.l	2.1"


(declare (macros t))

; This macro is called from every file that uses macros.
; If macros are being expanded at compile time, the function call
; goes away.  However, the name is bound to a function that loads
; macros at run time so that if macros are not expanded, a call
; to this name will cause them to be loaded at run time.

(defun needs-macros macro (none)
  '(comment Macros are being expanded at compile time.))
