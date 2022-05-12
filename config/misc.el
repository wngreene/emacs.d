;;; Miscellaneous config.

;; Misc stuff.
(setq inhibit-startup-screen t)
(setq use-file-dialog nil)

;; Auto refresh.
(global-auto-revert-mode t)
(setq auto-revert-interval 0.1)

;; desktop-save
;; Code below allows desktop-save to restore buffer/window layout when in
;; terminal mode.
(setq desktop-restore-forces-onscreen nil)
(add-hook 'desktop-after-read-hook
          (lambda ()
            (frameset-restore
             desktop-saved-frameset
             :reuse-frames (eq desktop-restore-reuses-frames t)
             :cleanup-frames (not (eq desktop-restore-reuses-frames 'keep))
             :force-display desktop-restore-in-current-display
                 :force-onscreen desktop-restore-forces-onscreen)))

;; Functions to undo fill-paragraph.
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
        (fill-region (region-beginning) (region-end) nil)))

(global-set-key (kbd "M-Q") 'unfill-paragraph)
