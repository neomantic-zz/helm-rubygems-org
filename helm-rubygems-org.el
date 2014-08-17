;;; -*- lexical-binding: t -*-

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

(defun rubygems-gem-description (gem-candidate)
  "Given a deserialized JSON gem representation, show a description of the gem in a new buffer"
  (let* ((name (rubygems-gem-descriptor 'name gem-candidate))
		 (buffer-name
		  (format "*rubygems.org: %s*" name)))
    (if (get-buffer buffer-name)
	(switch-to-buffer buffer-name)
      (progn
       	(generate-new-buffer buffer-name)
	(with-current-buffer
	    buffer-name
	  (insert name)
	  (newline 2)
	  (insert (rubygems-gem-descriptor 'info gem-candidate))
	  (fill-paragraph)
	  (newline 2)
	  (insert "Click to copy to kill ring: ")
	  (insert-button (rubygems-format-for-gemfile gem-candidate)
			 'action (lambda (button)
				   (rubygems-candidate-kill-new gem-candidate))
			 'follow-link t
			 'point (point)
			 'buffer (current-buffer))
	  (newline 2)
	  (cl-loop for link-pair in
		   '(("Project Page" . project_uri)
		     ("Homepage" . homepage_uri)
		     ("Source Code" . source_code_uri))
		   do
		   (let ((uri (rubygems-gem-descriptor (cdr link-pair) gem-candidate)))
		     (if uri
			 (progn
			   (insert-button (car link-pair)
					  'action (lambda (button)
						    (helm-browse-url uri))
					  'follow-link t
					  'point (point)
					  'buffer (current-buffer))
			   (insert "  ")))))
	  (setq buffer-read-only t))
	(switch-to-buffer buffer-name)))))

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
  (let ((descriptor-cell (assoc descriptor gem-candidate)))
    (if descriptor-cell
	(cdr descriptor-cell)
      nil)))

(defun rubygems-format-for-gemfile (gem-candidate)
  "Returns a string suitable for inclusion in a Gemfile; gem '<gem name>', '~> <version>"
  (format "gem '%s', '~> %s'"
	  (rubygems-gem-descriptor 'name gem-candidate)
	  (rubygems-gem-descriptor 'version gem-candidate)))

(defun rubygems-candidate-kill-new (gem-candidate)
  "Populates the kill-ring with a string suitable for including an a Gemfile"
  (kill-new (rubygems-format-for-gemfile gem-candidate)))

(defun rubygems-candidate-browse (gem-candidate)
  "Opens a browser to project_uri of the GEM-CANDIDATE"
  (helm-browse-url
   (rubygems-gem-descriptor 'project_uri gem-candidate)))

(defun rubygems-candidate-browse-source-code (gem-candidate)
  "Opens a browser to source_code_uri of then GEM-CANDIDATE"
  (let ((source-code-uri (rubygems-gem-descriptor 'source_code_uri gem-candidate)))
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
	       ("Browse on rubygems.org" . rubygems-candidate-browse)
	       ("View Description" . rubygems-gem-description)))))

(defun helm-rubygems-org ()
  "List Rubygems"
  (interactive)
  (helm :sources 'helm-source-rubygems-search :buffer "*helm-rubygems*"))

(provide 'helm-rubygems-org)
