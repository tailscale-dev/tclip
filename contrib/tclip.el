;;; tclip.el --- tclip client for Emacs -*- lexical-binding: t; -*-

;; Copyright (C)
;;    2023 Tailscale, Inc.
;; Author: Xe Iaso <xe@tailscale.com>
;; Maintainer: Xe Iaso <xe@tailscale.com>
;; Created: 2023-01-13
;; Version: 0.1
;; Keywords: tailscale, pastebin, sharing
;; Homepage: https://github.com/tailscale-dev/tclip

;;; Commentary:
;;
;; This uses request-el to make requests to your tailnet's tclip server. You
;; can install request-el with M-x package-install.
;;
;; This package requires that you have a tclip server set up. This package
;; reaches out to a tclip server over either plain HTTP, or HTTPS should you
;; configure the variable `tclip-server'.
;;
;; Usage:
;;
;; To submit the contents of the current buffer to tclip:
;;  M-x tclip-submit-buffer
;; To submit the contents of the currently highlighted region to tclip:
;;  M-x tclip-submit-region
;;
;; Customization:
;;
;; To customize the tclip server this package reaches out to:
;;  M-x customize-group tclip
;;
;; You can customize the tclip server URL by changing the value of `tclip-server':
;;  (setq tclip-server "https://paste.shark-harmonic.ts.net")

;;; Code:

(require 'request)

(defgroup tclip nil
  "Tclip server configuration."
  :prefix "tclip-"
  :group 'tclip)

(defcustom tclip-server "http://paste"
  "The server that is running tclip or a service with a compatible API to tclip. This should NOT end with a trailing slash."
  :group 'tclip
  :type 'string)

(defun tclip--send-paste (fname content)
  "Internal function that actually fires off the paste with name FNAME and content CONTENT to the tclip server."
  (request (format "%s/api/post" tclip-server)
    :type "POST"
    :data `(("filename" . ,fname)
            ("content" . ,content))
    :headers '(("Accept" . "text/plain"))
    :timeout 60
    :success (cl-function
              (lambda (&key response &allow-other-keys)
                (message "%s" (request-response-data response))))))

(defun tclip-submit-buffer ()
  "Submits the entire current buffer to tclip."
  (interactive)
  (let ((fname (format "%s.%s"
                        (file-name-base (buffer-file-name))
                        (file-name-extension (buffer-file-name))))
         (content (buffer-string)))
    (tclip--send-paste fname content)))

(defun tclip-submit-region ()
  "Submits the highlighted region to tclip."
  (interactive)
  (let ((fname (format "%s.%s"
                        (file-name-base (buffer-file-name))
                        (file-name-extension (buffer-file-name))))
         (content (buffer-substring-no-properties (region-beginning) (region-end))))
    (tclip--send-paste fname content)))

(provide 'tclip)
;;; tclip.el ends here
