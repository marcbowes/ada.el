;;; ada.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Marc Bowes
;;
;; Author: Marc Bowes <bowes@amazon.com>
;; Maintainer: Marc Bowes <bowes@amazon.com>
;; Created: March 04, 2025
;; Modified: March 04, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc
;; Homepage: https://github.com/marcbowes/ada
;; Package-Requires: ((emacs "26.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar ada--credentials-cache nil
  "Cache for storing AWS credentials. Format: (profile credentials expiration-time)")

(defun ada-get-credentials (profile)
  "Get AWS credentials for given profile, using cache if valid"
  (let* ((cached (assoc profile ada--credentials-cache))
         (now (current-time))
         (cached-expiration (and cached (ada--parse-iso8601-time-string (nth 3 (cadr cached)))))
         (cached-credentials (and cached (cadr cached))))
    (if (and cached-credentials
             cached-expiration
             (time-less-p now cached-expiration))
        ;; Return cached credentials without expiration time
        (butlast cached-credentials)
      ;; Fetch new credentials
      (let ((new-credentials (ada--fetch-credentials profile)))
        ;; Update cache
        (setq ada--credentials-cache
              (cons (list profile new-credentials)
                    (assoc-delete-all profile ada--credentials-cache)))
        ;; Return credentials without expiration time
        (butlast new-credentials)))))

(defun ada--fetch-credentials (profile)
  "Fetch AWS credentials from ada CLI for given profile and return as list of strings"
  (let* ((json-string (shell-command-to-string (format "ada credentials print --profile %s" profile)))
         (json-data (json-read-from-string json-string)))
    (list (cdr (assoc 'AccessKeyId json-data))
          (cdr (assoc 'SecretAccessKey json-data))
          (cdr (assoc 'SessionToken json-data))
          (cdr (assoc 'Expiration json-data)))))

(defun ada--parse-iso8601-time-string (time-string)
  "Convert ISO8601 time string to Emacs time value"
  (encode-time (parse-time-string time-string)))

(provide 'ada)
;;; ada.el ends here
