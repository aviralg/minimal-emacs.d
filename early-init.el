;;; early-init.el --- Early Init -*- no-byte-compile: t; lexical-binding: t; -*-

;; Author: James Cherti
;; URL: https://github.com/jamescherti/minimal-emacs.d
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 1.0.2
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; This file contains early initialization settings for Emacs. It is designed
;; to optimize the startup process and configure essential settings before the
;; main initialization.

;;; Code:

;;; Load pre-early-init.el

(defvar minimal-emacs-user-directory user-emacs-directory
  "The default value of the `user-emacs-directory' variable.")

(setq custom-theme-directory (expand-file-name "themes/" user-emacs-directory))
(setq custom-file
      (expand-file-name "custom.el"
                        minimal-emacs-user-directory))

(defun minimal-emacs-load-user-init (filename)
  "Execute a file of Lisp code named FILENAME."
  (let ((user-init-file
         (expand-file-name filename
                           minimal-emacs-user-directory)))
    (when (file-exists-p user-init-file)
      (load user-init-file nil t))))

(minimal-emacs-load-user-init "pre-early-init.el")

;;; Variables
(defvar minimal-emacs-debug nil
  "Non-nil to enable debug.")

(defvar minimal-emacs-gc-cons-threshold (* 16 1024 1024)
  "The value of `gc-cons-threshold' after Emacs startup.")

(defvar minimal-emacs-frame-title-format "%b – Emacs"
  "Template for displaying the title bar of visible and iconified frame.")

(defvar minimal-emacs-default-gc-cons-threshold gc-cons-threshold
  "The default value of `gc-cons-threshold'.")

(defvar minimal-emacs--default-mode-line-format mode-line-format
  "Default value of `mode-line-format'.")

;;; Misc



;; Set-language-environment sets default-input-method, which is unwanted.
(setq default-input-method nil)

;;; Garbage collection
;; Garbage collection significantly affects startup times. This setting delays
;; garbage collection during startup but will be reset later.

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold minimal-emacs-gc-cons-threshold)))

;;; Performance

;; Prefer loading newer compiled files
(setq load-prefer-newer t)



;; Reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)


;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; By default, Emacs "updates" its ui more often than it needs to
(setq idle-update-delay 1.0)




;;; package.el
;; Since Emacs 27, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'.
(setq package-enable-at-startup t)

(setq package-quickstart nil)



(setq frame-title-format minimal-emacs-frame-title-format
      icon-title-format minimal-emacs-frame-title-format)


;;; Load post-early-init.el
(minimal-emacs-load-user-init "post-early-init.el")

(provide 'early-init)

;;; early-init.el ends here
