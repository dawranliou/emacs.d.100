;;; early-init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(add-to-list 'exec-path "/usr/local/bin")
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))


(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      package-enable-at-startup nil
      frame-inhibit-implied-resize t)

(setq-default default-frame-alist
              '(;;(menu-bar-lines         . 0)
                ;;(tool-bar-lines         . 0)
                ;;(vertical-scroll-bars   . nil)
                (horizontal-scroll-barr . nil)
                (width                  . 90)
                (height                 . 40)
                (font                   . "Iosevka Fixed-15")))

(provide 'early-init)

;;; early-init.el ends here
