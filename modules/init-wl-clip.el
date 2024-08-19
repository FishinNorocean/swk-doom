;;; init-wl-clip.el --- wayland clipboard support
;;; Commentary:
;;;     stupid kde plasma6 fuck you

;;; Code:

;; (setq wl-copy-process nil)
;; (defun wl-copy (text)
;;     (setq wl-copy-process (make-process :name "wl-copy"
;;                               :buffer nil
;;                               :command '("wl-copy" "-f" "-n")
;;                               :connection-type 'pipe))
;;     (process-send-string wl-copy-process text)
;;     (process-send-eof wl-copy-process))
;; (defun wl-paste ()
;;     (if (and wl-copy-process (process-live-p wl-copy-process))
;;         nil ; should return nil if we're the current paste owner
;;         (shell-command-to-string "wl-paste -n | tr -d \r")))
;; (setq interprogram-cut-function 'wl-copy)
;; (setq interprogram-paste-function 'wl-paste)

;; (provide 'init-wl-clip)

(defun wl-copy (text)
  "Copy TEXT to the clipboard using wl-copy with -f -n options."
  (let ((process-connection-type nil)) ; Ensure we use a pipe, not a pty.
    (let ((proc (start-process "wl-copy" nil "wl-copy" "-f" "-n")))
      (process-send-string proc text)
      (process-send-eof proc))))

;; (defun wl-copy (text)
;;   "Copy TEXT to the clipboard using wl-copy with -f -n options."
;;   (let ((process-connection-type nil)) ; Ensure we use a pipe, not a pty.
;;     (call-process "wl-copy" nil nil nil "-f" "-n" text)))

;; (defun wl-paste ()
;;   "Paste the contents from the clipboard using wl-paste."
;;   (with-output-to-string
;;     (call-process "wl-paste" nil standard-output nil "-n")))

(setq interprogram-cut-function 'wl-copy)
;; (setq interprogram-paste-function 'wl-paste)

(provide 'init-wl-clip)

;;; init-wl-clip.el ends here
