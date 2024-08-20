;;; pack-config.el -*- lexical-binding: t; -*-

(use-package! dirvish
  :init
  (if *is-a-mac* (dirvish-override-dired-mode))
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
                                        ;("m" "/mnt/"                       "Drives")
     ("e" "~/.emacs.d/"                 "emacs.d")
     ("p" "~/projects/"                 "Projects")
     ("t" "~/.Trash/"                   "TrashCan")
     ("o" "~/org/"                      "Org notes")
     ))
  :config
  (setq centaur-tabs-icon-type 'nerd-icons)
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   ;;("C-x d" . dirvish)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("TAB" . dirvish-subtree-toggle)
   ("C-b" . dired-up-directory)
   ("C-f" . dired-find-file)
   ("M-S-t" . dirvish-layout-toggle)
   ))

(use-package! buffer-move
  :config
  (global-set-key (kbd "C-c <left>") 'buf-move-left)
  (global-set-key (kbd "C-c <right>") 'buf-move-right)
  (global-set-key (kbd "C-c <up>") 'buf-move-up)
  (global-set-key (kbd "C-c <down>") 'buf-move-down))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-<return>" . 'copilot-accept-completion)
              ("M-<return>" . 'copilot-accept-completion-by-line)
              ("S-TAB" . 'copilot-accept-completion-by-word)
              ("S-<tab>" . 'copilot-accept-completion-by-word)))
(add-hook 'emacs-lisp-mode-hook (lambda () (copilot-mode nil)))



