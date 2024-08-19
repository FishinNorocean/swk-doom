;;; init-customized-packages.el --- Chinese support
;;; Commentary:
;;;     Chinese words seperator support added with jieba

;;; Code:

(add-to-list 'load-path
             (expand-file-name "~/.config/doom/customized_packages/emacs-chinese-word-segmentation"))

(use-package cns
  :custom
  (cns-prog (expand-file-name "~/.emacs.d/site-lisp/emacs-chinese-word-segmentation/cnws"))
  (cns-dict-directory (expand-file-name "~/.emacs.d/site-lisp/emacs-chinese-word-segmentation/cppjieba/dict"))
  :hook
  (text-mode . cns-mode)
  :config
  (setq cns-debug nil))


(provide 'init-customized-packages)
;;; init-customized-packages.el ends here
