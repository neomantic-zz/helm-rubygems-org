;;; hub.el --- An Emacs Lisp Library for the Github API v3

;; Copyright (C) 2014 Chad Albers

;; Author: Chad Albers <calbers@neomantic.com>
;; URL: https://github.com/neomantic/helm-rubygems
;; Version: 0.5.0
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

(defgroup helm-rubygems nil
  "Customizations for search online for rubygems"
  :group 'helm)

(defcustom rubygems-api-key nil
  "The API Key issued by rubygems.org, required to its use API"
  :group 'helm-rubygems
  :type 'string)

;; TODO - put this in rubygems-search
(defun rubygems-request (request-url api-key)
  "Given the string REQUEST-URL and the API-KEY, performs a request to rubygems.org's api"
  (let ((url-mime-accept-string  "application/json")
		(url-request-extra-headers
		 (list (cons "Authorization" api-key))))
	(url-retrieve-synchronously request-url)))

(defun rubygems-search (search-term)
  "Given the string SEARCH-TERM, returns a parsed JSON list of results"
  (with-current-buffer
      ;; TODO - PAGINATION!
      (rubygems-request
       (concat "https://rubygems.org/api/v1/"
	       (format "search?query=%s" (url-hexify-string search-term)))
       api-key)
    (goto-char (+ 1 url-http-end-of-headers))
    (json-read)))

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
  (lexical-let ((source-uri (rubygems-gem-descriptor 'source_uri gem-candidate)))
	(if source-uri
		(helm-browse-url source-uri)
	  (rubygems-candidate-browse gem-candidate))))

(defun rubygems-search-format (search-results)
  "Formats the parsed json SEARCH-RESULTS, return a list of cons cells, whose car is the gem's name and version, and whose cdr is the resource itself"
  (mapcar (lambda (gem-candidate)
	    (cons (format "%s ~> %s"
			  (rubygems-gem-descriptor 'name gem-candidate)
			  (rubygems-gem-descriptor 'version gem-candidate))
		  gem-candidate))
	  search-results))

(defun helm-rubygems-search ()
  "Returns a list of gem candidates suitable for helm"
  (rubygems-search-format (rubygems-search helm-pattern)))

(defvar helm-source-rubygems-search
  '((name . "Rubygems.org")
    (candidates . helm-rubygems-search)
    (volatile)
    (delayed)
    (requires-pattern . 2)
    (action . (("Copy gemfile require" . rubygems-candidate-kill-new)
	       ("Browse source code project" . rubygems-candidate-browse-source-code)
	       ("Browse on rubygems.org" . rubygems-candidate-browse)))))

(defun helm-rubygems ()
  "List Rubygems"
  (interactive)
  (helm :sources 'helm-source-rubygems-search :buffer "*helm-rubygems*"))

(provide 'helm-rubygems)
