;;; fuff.el --- Find files with findutils.

;; Copyright (C) 2017  Joel Moberg

;; Author: Joel Moberg
;; Git: https://github.com/joelmo/fuff.git
;; Version: 0.1
;; Keywords: files, project, convenience

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library extends `find-file' by listing files recursively if
;; any directory specified by `fuff-start-directories' is
;; entered.  This library depends on find from findutils.  Call
;; `customize-group' fuff for more.

;;; Code:

(require 'seq)
(require 'ido)

(defgroup fuff nil
  "Switch between files using findutils find."
  :group 'convenience
  :group 'files
  :group 'project)

(defcustom fuff-patterns '("*.adb" "*.c" "*.cpp" "*.cs" "*.el" "*.go"
"*.html" "*.java" "*.js" "*.md" "*.nix" "*.org" "*.php" "*.pl" "*.py"
"*.r" "*.rb" "*.rs" "*.sh" "*.sql" "*.txt")
  "List of patterns to look for when using `fuff-find-file'.
This is used by `fuff-query'."
  :type '(repeat (string :tag "Pattern")))

(defcustom fuff-start-directories '("~/Documents")
  "Indicators telling where `fuff-find-file' can be used.
The command will be enabled if any entry here is a prefix for
`default-directory', or `ido-current-directory' if the ido hook is
enabled (`fuff-ido-switch').  Also see `fuff-start-directory'."
  :type '(repeat directory))

(defun fuff-start-directory (file)
  "Return a starting point directory or nil if not possible.
Argument FILE is a file or directory above the starting point."
  (let ((file (expand-file-name file)))
      (seq-some (lambda (start-dir)
	      (when (string-match start-dir file) start-dir))
	    (mapcar 'expand-file-name fuff-start-directories))))

(defun fuff-query ()
  "Turn `fuff-patterns' into a string that the find command can use."
  (mapconcat (lambda (pat) (format "-name \"%s\"" pat))
             fuff-patterns " -or "))

(defun fuff-files (dir)
  "Return a list of all filenames in DIR matching `fuff-patterns'."
  (split-string (shell-command-to-string
		 (format "find %s -type f \\( %s \\) -printf '%%P\n'"
			 dir (fuff-query)))))

(defvar fuff-enable-ido-switch nil
  "If non-nil, enable `fuff-ido-switch' in ido.")

(defun fuff-internal (dir)
  "Internal command for `fuff-find-file'.
Files will be listed recursively from DIR."
  (setq fuff-enable-ido-switch t)
  (let ((selected (ido-completing-read "Find file (fu): " (fuff-files dir))))
    (if (file-exists-p selected)
	(ido-file-internal ido-default-file-method 'fuff-find-file selected)
      (find-file (concat dir "/" selected)))))

(defun fuff-ido-switch ()
  "Switch to `fuff-find-file' if a start directory can be entered."
  (let ((dir (fuff-start-directory ido-current-directory)))
    (if dir
    	(fuff-internal dir))))

(defun fuff-ido-setup ()
  "Hook for ido, determines when to switch to fuff."
  (if fuff-enable-ido-switch
      (add-hook 'ido-make-file-list-hook 'fuff-ido-switch)
    (remove-hook 'ido-make-file-list-hook 'fuff-ido-switch))
  (setq fuff-enable-ido-switch nil))

(add-hook 'ido-setup-hook 'fuff-ido-setup)

;;;###autoload
(defun fuff-find-file ()
  "This can be used as a replacement for `find-file'."
  (interactive)
  (let ((start-dir (fuff-start-directory default-directory)))
    (if start-dir
	  (fuff-internal start-dir)
      (command-execute 'find-file))))

(provide 'fuff)
;;; fuff.el ends here
