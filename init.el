;;; init.el --- A minimal init.el  -*- lexical-binding: t; -*-


;;; Commentary:
;;

;;; Code:


(setq
 inhibit-startup-message t
 gc-cons-threshold (* 100 1024 1024) ; 100mb
 gc-cons-percentage 0.1
 tramp-default-method "ssh"
 ring-bell-function #'ignore
 visible-bell nil
 delete-by-moving-to-trash t
 mac-command-modifier 'super
 mac-option-modifier 'meta
 insert-directory-program "/usr/local/bin/gls"
 trash-directory "~/.Trash"
 scroll-preserve-screen-position t
 backup-directory-alist
 (list (cons "." (expand-file-name "var/backup/" user-emacs-directory)))
 auto-save-list-file-prefix
 (expand-file-name "var/auto-save/" user-emacs-directory))


(setq-default fill-column 80)


(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


(global-set-key (kbd "C-M-r") #'raise-sexp)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-x k") #'kill-this-buffer)
(global-set-key (kbd "M-i") #'delete-other-windows)
(global-set-key (kbd "M-o") #'other-window)
(global-set-key (kbd "s-<backspace>") #'kill-whole-line)
(global-set-key (kbd "s-q") #'save-buffers-kill-emacs)
(global-set-key [remap downcase-word] #'downcase-dwim)
(global-set-key [remap capitalize-word] #'capitalize-dwim)
(global-set-key [remap upcase-word] #'upcase-dwim)


(add-hook 'prog-mode-hook #'show-paren-mode)
(add-hook 'emacs-startup-hook #'recentf-mode)
(add-hook 'emacs-startup-hook #'savehist-mode)
(add-hook 'emacs-startup-hook #'save-place-mode)
(add-hook 'emacs-startup-hook #'fido-vertical-mode)
(add-hook 'emacs-startup-hook #'column-number-mode)
(add-hook 'emacs-startup-hook #'electric-pair-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-repl-buffer-size-limit 100000)
 '(cider-repl-display-help-banner nil)
 '(cider-repl-display-in-current-window nil)
 '(cider-repl-pop-to-buffer-on-connect nil)
 '(cider-repl-use-pretty-printing t)
 '(cljr-magic-requires nil)
 '(completion-styles '(partial-completion flex))
 '(dired-auto-revert-buffer t)
 '(dired-dwim-target t)
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'always)
 '(find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
 '(history-length 20000)
 '(isearch-allow-scroll t)
 '(magit-diff-refine-hunk 'all)
 '(magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
 '(recentf-max-saved-items 200)
 '(savehist-save-minibuffer-history t))


;;; Package manager


(setq straight-build-dir (format "build-%s" emacs-version)
      straight-check-for-modifications '(check-on-save find-when-checking))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;;; Third party packages


(straight-use-package 'rg)
(straight-use-package 'magit)
(straight-use-package 'clojure-mode)
(straight-use-package 'cider)
(straight-use-package 'org)
(straight-use-package 'slime)


(setq inferior-lisp-program "sbcl")


(provide 'init)

;;; init.el ends here
