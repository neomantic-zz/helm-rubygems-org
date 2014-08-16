;; helm-rubygems-org.el ---

;; Copyright (C) 2014 Chad Albers

;; Author: Chad Albers <calbers@neomantic.com>
;; URL: https://github.com/neomantic/helm-rubygems
;; Version: 0.0.1
;; Keywords: ruby, rubygems, gemfile
;; Package-Requires: ()

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(require 'json)
(require 'url)
(require 'cl-lib)

(defgroup helm-rubygems-org nil
  "Customizations for search online for rubygems"
  :group 'helm)

(defcustom rubygems-api-key nil
  "The API Key issued by rubygems.org, required to its use API"
  :group 'helm-rubygems-org
  :type 'string)

(defun rubygems-search (search-term)
  "Given the string SEARCH-TERM, returns a parsed JSON list of results"
  (cl-flet ((get-page (page-number)
		      (with-current-buffer
			  (let ((url-mime-accept-string  "application/json")
				(url-request-extra-headers
				 (list (cons "Authorization" api-key))))
			    (url-retrieve-synchronously
			     (concat "https://rubygems.org/api/v1/"
				     (format "search?query=%s&page=%d" (url-hexify-string search-term) page-number))))
			(goto-char (+ 1 url-http-end-of-headers))
			(json-read))))
    (cl-loop for page-number from 1 to 3
	     for candidates = (get-page page-number)
	     until (eq (length candidates) 0)
	     append (mapcar (lambda (candidate)
			      candidate)
			    candidates))))

(defun rubygems-gem-descriptor (descriptor gem-candidate)
  "Returns the value descriptor by the DESCRIPTOR symbol for GEM-CANDIDATE parsed rubygems resource representation"
  (lexical-let ((descriptor-cell (assoc descriptor gem-candidate)))
    (if descriptor-cell
	(cdr descriptor-cell)
      nil)))

(defun rubygems-candidate-kill-new (gem-candidate)
  "Populates the kill-ring with a string suitable for including an a Gemfile"
  (kill-new (format "gem '%s', '~> %s'"
		    (rubygems-gem-descriptor 'name gem-candidate)
		    (rubygems-gem-descriptor 'verson gem-candidate))))

(defun rubygems-candidate-browse (gem-candidate)
  "Opens a browser to project_uri of the GEM-CANDIDATE"
  (helm-browse-url
   (rubygems-gem-descriptor 'project_uri gem-candidate)))

(defun rubygems-candidate-browse-source-code (gem-candidate)
  "Opens a browser to source_code_uri of then GEM-CANDIDATE"
  (lexical-let ((source-code-uri (rubygems-gem-descriptor 'source_code_uri gem-candidate)))
    (if source-code-uri
	(helm-browse-url source-code-uri)
      (rubygems-candidate-browse gem-candidate))))

(defun helm-rubygems-search ()
  "Returns a list of gem candidates suitable for helm"
  (mapcar (lambda (gem-candidate)
	    (cons (format "%s ~> %s"
			  (rubygems-gem-descriptor 'name gem-candidate)
			  (rubygems-gem-descriptor 'version gem-candidate))
		  gem-candidate))
	  (rubygems-search helm-pattern)))

(defvar helm-source-rubygems-search
  '((name . "Rubygems.org")
    (candidates . helm-rubygems-search)
    (volatile)
    (delayed)
    (requires-pattern . 2)
    (action . (("Copy gemfile require" . rubygems-candidate-kill-new)
	       ("Browse source code project" . rubygems-candidate-browse-source-code)
	       ("Browse on rubygems.org" . rubygems-candidate-browse)))))

(defun helm-rubygems-org ()
  "List Rubygems"
  (interactive)
  (helm :sources 'helm-source-rubygems-search :buffer "*helm-rubygems*"))

(provide 'helm-rubygems-org)
