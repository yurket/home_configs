;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; remove trailing whitespaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'init-miscellaneous)
