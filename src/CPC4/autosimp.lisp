"@(#)autosimp.l	2.1"

; load and execute simplifier.
(defun autostart nil
  (sstatus load-search-path (|/usr/p/frl/cpc4.d|))
  (patom '|Loading simplifier...|) (terpr)
  (load 'setup)
  (setq startarg 3)
  (main))

(setq user-top-level 'autostart)
