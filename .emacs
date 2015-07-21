;;(add-to-list 'load-path "/home/lite/.emacs.d")    ; This may not be appeared if you have already added.
(add-to-list 'load-path "/home/lite/.emacs.d/plugins")


(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/plugins/ac-dict")
(ac-config-default)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

; for rope emacs support
(defun load-ropemacs ()
  "Load pymacs and ropemacs"
  (interactive)
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-")
  ;; Automatically save project python buffers before refactorings
  (setq ropemacs-confirm-saving 'nil)
  (setq ropemacs-autoimport-modules 't)
  (setq ropemacs-autoimport-modules '("os" "shutil" "xmmsclient" "sys"))
)
(global-set-key "\C-xpl" 'load-ropemacs)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "fixed" :foundry "misc" :slant normal :weight normal :height 113 :width normal)))))


(setq c-default-style "stroustrup"
      c-basic-offset 4)

;; don't use tabls for indentions
(setq-default indent-tabs-mode nil)
