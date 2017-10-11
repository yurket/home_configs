;;(add-to-list 'load-path "/home/lite/.emacs.d")    ; This may not be appeared if you have already added.
(add-to-list 'load-path "/home/lite/.emacs.d/plugins")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (misterioso)))
 '(flycheck-gcc-language-standard "c++11")
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "fixed" :foundry "misc" :slant normal :weight normal :height 113 :width normal)))))


;; MELPA support
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/plugins/ac-dict")
(ac-config-default)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

;; (autoload 'octave-mode "octave-mod" nil t)
;; (setq auto-mode-alist
;;      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(setq c-default-style "linux"
      c-basic-offset 4)

(setq-default indent-tabs-mode nil)
(setq ruby-indent-level 4)

(add-hook 'after-init-hook #'global-flycheck-mode)
;; for gcc checker
;; (setq 'flycheck-gcc-args
(setq flycheck-cppcheck-checks '"all")

(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)


(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )


(add-to-list 'auto-mode-alist '("\\.h$" . dummy-h-mode))
(autoload 'dummy-h-mode "dummy-h-mode" "Dummy H mode" t)
(add-hook 'dummy-h-mode-hook
    (lambda ()
      (setq dummy-h-mode-default-major-mode 'c++-mode)))


