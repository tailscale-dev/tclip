;;; infinipaste.el --- infinipaste client for Emacs -*- lexical-binding: t; -*-

;; Copyright (C)
;;    2023 Tailscale, Inc.
;; Author: Xe Iaso <xe@tailscale.com>
;; Maintainer: Xe Iaso <xe@tailscale.com>
;; Created: 2023-01-13
;; Version: 0.1
;; Keywords: tailscale, pastebin, sharing
;; Homepage: https://github.com/tailscale-dev/infinipaste

;;; Commentary:
;;
;; This uses request-el to make requests to your tailnet's infinipaste server. You
;; can install request-el with M-x package-install.
;;
;; This package requires that you have a infinipaste server set up. This package
;; reaches out to a infinipaste server over either plain HTTP, or HTTPS should you
;; configure the variable `infinipaste-server'.
;;
;; Usage:
;;
;; To submit the contents of the current buffer to infinipaste:
;;  M-x infinipaste-submit-buffer
;; To submit the contents of the currently highlighted region to infinipaste:
;;  M-x infinipaste-submit-region
;;
;; Customization:
;;
;; To customize the infinipaste server this package reaches out to:
;;  M-x customize-group infinipaste
;;
;; You can customize the infinipaste server URL by changing the value of `infinipaste-server':
;;  (setq infinipaste-server "https://paste.shark-harmonic.ts.net")

;;; Code:

(require 'request)

(defgroup infinipaste nil
  "Infinipaste server configuration."
  :prefix "infinipaste-"
  :group 'infinipaste)

(defcustom infinipaste-server "http://paste"
  "The server that is running infinipaste or a service with a compatible API to infinipaste. This should NOT end with a trailing slash."
  :group 'infinipaste
  :type 'string)

(defun infinipaste--send-paste (fname content)
  "Internal function that actually fires off the paste with name FNAME and content CONTENT to the infinipaste server."
  (request (format "%s/api/post" infinipaste-server)
    :type "POST"
    :data `(("filename" . ,fname)
            ("content" . ,content))
    :headers '(("Accept" . "text/plain"))
    :timeout 60
    :success (cl-function
              (lambda (&key response &allow-other-keys)
                (message "%s" (request-response-data response))))))

(defun infinipaste-submit-buffer ()
  "Submits the entire current buffer to infinipaste."
  (interactive)
  (let ((fname (format "%s.%s"
                        (file-name-base (buffer-file-name))
                        (file-name-extension (buffer-file-name))))
         (content (buffer-string)))
    (infinipaste--send-paste fname content)))

(defun infinipaste-submit-region ()
  "Submits the highlighted region to infinipaste."
  (interactive)
  (let ((fname (format "%s.%s"
                        (file-name-base (buffer-file-name))
                        (file-name-extension (buffer-file-name))))
         (content (buffer-substring-no-properties (region-beginning) (region-end))))
    (infinipaste--send-paste fname content)))

(provide 'infinipaste)
;;; infinipaste.el ends here
