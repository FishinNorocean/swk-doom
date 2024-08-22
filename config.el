;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-city-lights)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; my own configuration starts here

(defconst *is-a-mac* (eq system-type 'darwin))

(add-hook 'window-setup-hook #'toggle-frame-maximized)

(global-unset-key (kbd "C-j"))

(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 16)
      doom-variable-pitch-font (font-spec :family "Open Sans")
      doom-unicode-font (font-spec :family "DejaVu Sans Mono")
      doom-big-font (font-spec :family "DejaVu Sans Mono" :size 19))

(setq use-short-answers t)
(setq fancy-splash-image (expand-file-name "doom-vapourwave.png" doom-private-dir))
;; (load! "modules/init-utils")

(when (getenv "WAYLAND_DISPLAY") (load! "modules/init-wl-clip"))

;; keybindings and some previous preloads

(map! "C-x C-k" 'kill-this-buffer
      "C-c '" 'comment-or-uncomment-region
      "RET" 'newline-and-indent
      "C-t" nil
      "M-w" 'kill-region
      "C-w" 'kill-ring-save
      "C-c '" 'comment-or-uncomment-region
      "C-t C-t" 'vterm
      "M-n" (lambda () (interactive) (forward-line 10))
      "M-p" (lambda () (interactive) (forward-line -10))
      "C-S-c" 'kill-ring-save
      "C-S-v" 'yank)

(after! consult
  (map! "C-s" #'consult-line
        "C-S-s" #'consult-line-multi))

(use-package! avy
  :bind
  (("C-j C-SPC" . avy-goto-char-timer)
   ("C-j C-k" . avy-move-line)
   ("C-j M-k" . avy-kill-ring-save-whole-line)
   ("C-j C-l" . avy-copy-line)
   ("C-j C-i" . avy-copy-region)
   ("C-j C-w" . avy-kill-ring-save-region)
   ("C-j M-w" . avy-kill-region)))


;; (after! avy
;;   (map!
;;    "C-j C-SPC" #'avy-goto-char-timer
;;    "C-j C-k"   #'avy-move-line
;;    "C-j M-k"   #'avy-kill-ring-save-whole-line
;;    "C-j C-l"   #'avy-copy-line
;;    "C-j C-i"   #'avy-copy-region
;;    "C-j C-w"   #'avy-kill-ring-save-region
;;    "C-j M-w"   #'avy-kill-region))


;; Normal files

(load! "modules/pack-config")
(load! "modules/init-customized-packages")
(load! "modules/init-org")
(after! doom-modeline
  (setq doom-modeline-total-line-number t)
  (setq doom-modeline-battery t)
  (setq display-time-format "%A %b%d %R")
  (setq display-time-default-load-average nil)
  ;; (setq doom-modeline-check-simple-format t)
  ;; (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  ;;(setq doom-modeline-buffer-encoding nil)
  ;; (setq doom-modeline-indent-info nil)
  (if *is-a-mac*
      (display-time-mode 1))
                                        ; (setq doom-modeline-buffer-encoding nil)
  ;; (setq doom-modeline-env-version t)
  )

(after! latex
  (add-hook 'latex-mode-hook
	    (lambda () (local-set-key (kbd "C-j") nil)))
  (setq-default TeX-engine 'xetex)
  (setq-default TeX-PDF-mode t)
  (define-key LaTeX-mode-map (kbd "C-<return>") 'copilot-accept-completion)
  (define-key LaTeX-mode-map (kbd "C-j") nil)
  (define-key LaTeX-mode-map (kbd "C-j C-SPC") 'avy-goto-char-timer)
  (define-key LaTeX-mode-map (kbd "C-j C-k") 'avy-move-line)
  (define-key LaTeX-mode-map (kbd "C-j M-k") 'avy-kill-ring-save-whole-line)
  (define-key LaTeX-mode-map (kbd "C-j C-l") 'avy-copy-line)
  (define-key LaTeX-mode-map (kbd "C-j C-i") 'avy-copy-region)
  (define-key LaTeX-mode-map (kbd "C-j C-w") 'avy-kill-ring-save-region)
  (define-key LaTeX-mode-map (kbd "C-j M-w") 'avy-kill-region))

;; (use-package! chatgpt-shell
;;   :custom
;;   ((chatgpt-shell-api-url-base "https://gptswkser.openai.azure.com")
;;    (chatgpt-shell-api-url-path "/openai/deployments/swk_35/chat/completions?api-version=2024-02-15-preview")
;;    (chatgpt-shell-openai-key
;;     (lambda ()
;;       (auth-source-pick-first-password :host "gptswkser.openai.azure.com")))
;;    (chatgpt-shell-auth-header (lambda () (format "api-key: %s" (chatgpt-shell-openai-key))))))

(use-package! langtool
  :config
  (setq langtool-default-language "en-US")
  (setq langtool-java-classpath nil)
  (setq langtool-language-tool-jar
        (expand-file-name "~/.local/opt/LanguageTool-6.4/languagetool-commandline.jar")))

(after! flyspell
  (map! "C-;" 'flyspell-correct-wrapper))

(after! dap-mode
  (setq dap-python-debugger 'debugpy))

(after! highlight-indent-guides
  (setq highlight-indent-guides-auto-enabled t)
  (setq highlight-indent-guides-method 'column)
  (setq highlight-indent-guides-auto-odd-face-perc 40)
  (setq highlight-indent-guides-auto-even-face-perc 50)
  (setq highlight-indent-guides-auto-character-face-perc 50)
  ;; (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  ;; (set-face-background 'highlight-indent-guides-even-face "dimgray")
  ;; (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  )

(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))
