;;; preload-local.el -*- lexical-binding: t; -*-

;; mapping
(map! "C-x C-k" 'kill-this-buffer
      "C-c '" 'comment-or-uncomment-region
      "RET" 'newline-and-indent
      "C-j" nil
      "C-t" nil
      "M-w" 'kill-region
      "C-w" 'kill-ring-save
      "C-c '" 'comment-or-uncomment-region
      "C-t C-t" 'vterm)

(cond ((string-equal system-type "darwin")
       (progn
         ;; modify option and command key
         (setq mac-command-modifier 'meta)
         (setq mac-option-modifier 'super))))
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
