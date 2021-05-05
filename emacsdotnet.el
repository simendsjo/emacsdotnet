;;; emacsdotnet.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Simen Endsjø
;;
;; Author: Simen Endsjø <https://github.com/simendsjo>
;; Maintainer: Simen Endsjø <simendsjo@gmail.com>
;; Created: May 05, 2021
;; Modified: May 05, 2021
;; Version: 0.0.1
;; Keywords: dotnet, fsharp, csharp
;; Homepage: https://github.com/simendsjo/emacsdotnet
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar simendsjo/emacsdotnet/process-host "localhost"
  "Host where the emacsdotnet server is running")
(defvar simendsjo/emacsdotnet/process-port 5000
  "Port where the emacsdotnet server is running")
(defvar simendsjo/emacsdotnet/process-name "emacsdotnet"
  "Process name")
(defvar simendsjo/emacsdotnet/process-buffer "*emacsdotnet*"
  "Process buffer")
(defvar simendsjo/emacsdotnet/process-last-response nil
  "Last response from server. Useful for syncronous operations")

(defun simendsjo/emacsdotnet/process-sentinel (proc msg)
  (when (string= msg "connection broken by remote peer\n")
    (message (format "client %s has quit" proc))))

(defun simendsjo/emacsdotnet/process-filter (proc string)
  (setq simendsjo/emacsdotnet/process-last-response (read string))
  (message string))

(defun simendsjo/emacsdotnet/process-start ()
  (interactive)
  (make-network-process :name simendsjo/emacsdotnet/process-name
                        :buffer simendsjo/emacsdotnet/process-buffer
                        :family 'ipv4
                        :host simendsjo/emacsdotnet/process-host
                        :service simendsjo/emacsdotnet/process-port
                        :sentinel 'simendsjo/emacsdotnet/process-sentinel
                        :filter 'simendsjo/emacsdotnet/process-filter))

(defun simendsjo/emacsdotnet/process-get-or-start ()
  (or (get-process simendsjo/emacsdotnet/process-name) (simendsjo/emacsdotnet/process-start)))

(defun simendsjo/emacsdotnet/send-async (data)
  (let ((proc (simendsjo/emacsdotnet/process-get-or-start)))
    (process-send-string simendsjo/emacsdotnet/process-name (prin1-to-string data))
    proc))

(defun simendsjo/emacsdotnet/send (data)
  (let ((proc (simendsjo/emacsdotnet/send-async data)))
    (accept-process-output proc)
    simendsjo/emacsdotnet/process-last-response))

(defmacro dotnet (&rest body)
  "Calls dotnet function
Example (dotnet System/Math/Abs -1.2) calls System.Math.Abs(-1.2) and returns 1.2"
  `(simendsjo/emacsdotnet/send '(,@body)))

(defmacro dotnet-async (&rest body)
  "Calls dotnet function asyncronously
Example (dotnet-async System/Math/Abs -1.2) calls System.Math.Abs(-1.2) and returns the process immediately"
  `(simendsjo/emacsdotnet/send-async '(,@body)))

(provide 'emacsdotnet)
;;; emacsdotnet.el ends here
