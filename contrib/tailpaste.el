;;; tailpaste.el --- tailpaste client for Emacs -*- lexical-binding: t; -*-

;; Copyright (C)
;;    2023 Tailscale, Inc.
;; Author: Xe Iaso <xe@tailscale.com>
;; Maintainer: Xe Iaso <xe@tailscale.com>
;; Created: 2023-01-13
;; Version: 0.1
;; Keywords: tailscale, pastebin, sharing
;; Homepage: https://github.com/tailscale/paste

;;; Commentary:
;;
;; This uses request-el to make requests to your tailnet's tailpaste server. You
;; can install request-el with M-x package-install.
;;
;; This package requires that you have a tailpaste server set up. This package
;; reaches out to a tailpaste server over either plain HTTP, or HTTPS should you
;; configure the variable `tailpaste-server'.
;;
;; Usage:
;;
;; To submit the contents of the current buffer to tailpaste:
;;  M-x tailpaste-submit-buffer
;; To submit the contents of the currently highlighted region to tailpaste:
;;  M-x tailpaste-submit-region
;;
;; Customization:
;;
;; To customize the tailpaste server this package reaches out to:
;;  M-x customize-group tailpaste
;;
;; You can customize the tailpaste server URL by changing the value of `tailpaste-server':
;;  (setq tailpaste-server "https://paste.shark-harmonic.ts.net")

;;; Code:

(require 'request)

(defgroup tailpaste nil
  "Tailpaste server configuration."
  :prefix "tailpaste-"
  :group 'tailpaste)

(defcustom tailpaste-server "http://paste"
  "The server that is running tailpaste or a service with a compatible API to tailpaste. This should NOT end with a trailing slash."
  :group 'tailpaste
  :type 'string)

(defun tailpaste--send-paste (fname content)
  "Internal function that actually fires off the paste with name FNAME and content CONTENT to the tailpaste server."
  (let ((body
         (url-build-query-string
                `(("filename" ,fname)
                  ("content" ,content))))
         (headers '(("Accept" . "text/plain"))))
    (request "http://paste/api/post"
      :type "POST"
      :data body
      :headers headers
      :timeout 60
      :success (cl-function
                (lambda (&key response &allow-other-keys)
                  (message "%s" (request-response-data response)))))))

(defun tailpaste-submit-buffer ()
  "Submits the entire current buffer to tailpaste."
  (interactive)
  (let* ((fname (format "%s.%s"
                        (file-name-base (buffer-file-name))
                        (file-name-extension (buffer-file-name))))
         (content (buffer-string)))
    (tailpaste--send-paste fname content)))

(defun tailpaste-submit-region ()
  "Submits the highlighted region to tailpaste."
  (interactive)
  (let* ((fname (format "%s.%s"
                        (file-name-base (buffer-file-name))
                        (file-name-extension (buffer-file-name))))
         (content (buffer-substring-no-properties (region-beginning) (region-end))))
    (tailpaste--send-paste fname content)))

(provide 'tailpaste)
;;; tailpaste.el ends here
