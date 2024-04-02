;;; recentz.el --- Minimalized and KISS replacements of built-in recentf  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 kuanyui

;; Author: kuanyui <azazabc123@gmail.com>
;; License: GPLv3
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: files
;; Url: https://github.com/kuanyui/recentz.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;; A KISS, DWIM replacement of `recentf'.
;;
;;  - Minimalized, no confusing behavior, no extra dependency. Fuzzy search is provided by Ido or Helm.
;;  - Never cache, even never store anything in memory. (Everything are stored as a plaintext file in =~/.emacs.d/.recentz-data=.)
;;  - Never lost any item after exiting Emacs. (Even unexpectedly exit)
;;  - Always synchronize recents list between multiple Emacs instances.
;;  - Supported list: files / directories / projects (directories controlled by VC, ex: =git=)
;;
;; Installation
;;
;;     (require 'recentz)
;;     (setq recentz-ignore-path-patterns '("/COMMIT_EDITMSG$" "~$" "/node_modules/"))
;;
;;     ;; If you prefer (Emacs built-in) Ido
;;     (global-set-key (kbd "C-x C-r") 'recentz-files)
;;     (global-set-key (kbd "C-x C-d") 'recentz-directories)
;;     (global-set-key (kbd "C-x C-p") 'recentz-projects)
;;
;;     ;; If you prefer Helm
;;     (global-set-key (kbd "C-x C-r") 'helm-recentz-files)
;;     (global-set-key (kbd "C-x C-d") 'helm-recentz-directories)
;;     (global-set-key (kbd "C-x C-p") 'helm-recentz-projects)

(require 'cl-lib)

(defvar recentz-vc-directory-names '(".git" ".hg" ".svn" ".bzr")
  "The folder name or file name to detect if a directory path is a
VC-controlled folder. If so, this folder will be seems as a
project.")
(defvar recentz-data-file-path (file-name-concat user-emacs-directory ".recentz-data")
  "The path to store recents list file."
  )
(defvar recentz-max-history
  '((files . 150)
    (directories . 50)
    (projects . 50))
  "The maximum items amount for each types of recent items.")

(defvar recentz-ignore-path-patterns
  '(
    "/COMMIT_EDITMSG$"
    "~$"
    ".+org-clock-save\\.el$"
    "\\.eln$"
    "\\.elc$"
    "\\.emacs\\.d/session\\.[a-z0-9]\\{10\\}"
    "\\.git/objects/"
    )
  "Exclude the item from recents list if its path match any of the
regexp patterns.")

(defun recentz-is-vc-root (dirpath)
  (cl-some (lambda (vc-dir-name)
	     (file-exists-p (file-name-concat dirpath vc-dir-name)))
	   recentz-vc-directory-names))

(defun recentz-find-vc-root (filepath)
  "If the FILEPATH (or dirpath) is inside in, or itself is, a
version control repo, return the path of repo root folder."
  (cl-some (lambda (vc-dir-name)
	     (locate-dominating-file filepath vc-dir-name))
	   recentz-vc-directory-names))

(defun recentz-formalize-path (path)
  "If PATH is a dir, append a slash."
  (setq path (expand-file-name path))
  (if (file-directory-p path)
      (file-name-as-directory path)
    path))

(defun recentz--write-data-to-file (data)
  (with-temp-file recentz-data-file-path
    (insert (prin1-to-string data))))

(defun recentz--read-data-from-file ()
  (if (not (file-exists-p recentz-data-file-path))
      (recentz--write-data-to-file (recentz-fix-data nil)))
  (recentz-fix-data (with-temp-buffer
		      (insert-file-contents recentz-data-file-path)
		      (ignore-errors (read (current-buffer))))))

(defun recentz-fix-data (data)
  (let ((final (if (listp data) data '())))
    (mapc (lambda (type)
	    (if (not (assoc type final))
		(push (cons type '()) final)
	      (progn
		(if (not (listp (alist-get type final)))
		    (setf (alist-get type final) '()))
		(cl-delete-if-not #'stringp (alist-get type final))  ; FIXME: potential error due to "destructive" cl-delete ...?
		)))
	  '(files directories projects))  ; types
    final))

(defun recentz-path-should-be-ignore (path)
  (cl-some (lambda (patt)
	     (string-match patt path))
	   recentz-ignore-path-patterns))

(defun recentz-push (type path)
  (if (recentz-path-should-be-ignore path)
      ()
    (let* ((all-data (recentz--read-data-from-file))
	   (paths (alist-get type all-data))
	   (expected-len (alist-get type recentz-max-history 30)))
      (setq path (recentz-formalize-path path))
      ;; setq again. Fuck the useless "destructive function". Indeed, destructive for user.
      (setq paths (cl-delete-duplicates paths :test #'equal))
      (setq paths (cl-delete path paths :test #'equal))
      (push path paths)
      (nbutlast paths (- (length paths) expected-len))  ; trim the list and keep expected length in head.
      (setf (alist-get type all-data) paths)
      (recentz--write-data-to-file all-data)
      )))

(defun recentz-get (type)
  (let* ((all-data (recentz--read-data-from-file))
	 (ori-paths (alist-get type all-data))
	 (new-paths (cl-delete-if (lambda (path) (or (not (file-exists-p path))
						     (recentz-path-should-be-ignore path)))
				  ori-paths))
	 (new-paths (mapcar #'recentz-formalize-path new-paths))   ; migration from old version
	 (new-paths (cl-delete-duplicates new-paths :test #'equal))   ; migration from old version
	 )
    ;; If two paths list are not equal (NOTE: Lisp's `equal' can compare list elements), write new data
    (when (not (equal ori-paths new-paths))
      (setf (alist-get type all-data) new-paths)
      (recentz--write-data-to-file all-data))
    new-paths))

(defun recentz-get-helm-candidates (type)
  (mapcar (lambda (path)
	    (cond ())
	    )
	  (recentz-get type)))

(defun recentz--hookfn-find-file ()
  (recentz-push 'files (buffer-file-name))
  ;; If current file is inside a VCS repo, also add the repo directory as project.
  (let* ((cur-dir (file-name-directory (buffer-file-name)))
	 (repo-dir (recentz-find-vc-root cur-dir)))
    (if repo-dir
	(recentz-push 'projects repo-dir))))

(defun recentz--hookfn-dired (&optional dirpath)
  (if dirpath
      (setq dirpath (expand-file-name dirpath))
    (setq dirpath default-directory))
  (recentz-push 'directories dirpath)
  (let ((vc-root (recentz-find-vc-root dirpath)))
    (if vc-root
	(recentz-push 'projects vc-root))))

(defun recentz--hookfn-vc (&optional dirpath)
  (if dirpath
      (setq dirpath (expand-file-name dirpath))
    (setq dirpath default-directory))
  (recentz-push 'directories dirpath)
  (let ((vc-root (recentz-find-vc-root dirpath)))
    (if vc-root
	(recentz-push 'projects vc-root))))

(defun recentz--hookfn-emacs-startup ()
  ;;   (message "====================
  ;; %S
  ;; file name: %s
  ;; major mode: %s
  ;; ========================"
  ;;   command-line-args
  ;;   (buffer-file-name)
  ;;   major-mode)
  (let ((is-folder (equal major-mode 'dired-mode))
	(is-file (buffer-file-name)))
    (cond (is-folder (recentz--hookfn-dired default-directory))
	  (is-file (recentz--hookfn-find-file (buffer-file-name))))))

(add-hook 'find-file-hook 'recentz--hookfn-find-file)
(add-hook 'find-directory-functions 'recentz--hookfn-dired)
(with-eval-after-load 'magit
  (add-hook 'magit-status-mode-hook 'recentz--hookfn-vc)
  )
(add-hook 'emacs-startup-hook 'recentz--hookfn-emacs-startup)

;;;###autoload
(defun recentz-files ()
  "List recently opened files."
  (interactive)
  (require 'ido)
  (find-file (ido-completing-read "Recentz Files: " (recentz-get 'files) nil t)))

;;;###autoload
(defun recentz-projects ()
  "List recently opened projects."
  (interactive)
  (require 'ido)
  (find-file (ido-completing-read "Recentz Projects: " (recentz-get 'projects) nil t)))

;;;###autoload
(defun recentz-directories ()
  "List recently opened directories."
  (interactive)
  (require 'ido)
  (find-file (ido-completing-read "Recentz Directories: " (recentz-get 'directories) nil t)))

(defmacro recentz-ensure-helm (&rest body)
  `(if (not (featurep 'helm-core))
       (message "This feature requires helm-core, but it's not installed on your Emacs yet. Please install helm, or use ido version (M-x recentz-*) instead.")
     (progn
       (require 'helm-core)
       ,@body)))

;;;###autoload
(defun helm-recentz-files ()
  "List recently opened files."
  (interactive)
  (recentz-ensure-helm
   (helm :sources (helm-build-sync-source "Recentz Files"
		    :candidates (lambda () (recentz-get 'files))
		    :volatile t
		    :action (lambda (str) (find-file str))
		    )
	 :buffer "*Recentz*"
	 :prompt "Recent files: ")))

;;;###autoload
(defun helm-recentz-projects ()
  "List recently opened projects."
  (interactive)
  (recentz-ensure-helm
   (helm :sources (helm-build-sync-source "Recentz Projects"
		    :candidates (lambda () (recentz-get 'projects))
		    :volatile t
		    :action (lambda (str) (find-file str))
		    )
	 :buffer "*Recentz*"
	 :prompt "Recent projects: ")))

;;;###autoload
(defun helm-recentz-directories ()
  "List recently opened directories."
  (interactive)
  (recentz-ensure-helm
   (helm :sources (helm-build-sync-source "Recentz Directories"
		    :candidates (lambda () (recentz-get 'directories))
		    :volatile t
		    :action (lambda (str) (find-file str))
		    )
	 :buffer "*Recentz*"
	 :prompt "Recent directories: ")))

(provide 'recentz)
