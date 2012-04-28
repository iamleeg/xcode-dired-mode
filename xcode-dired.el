;;; xcode-dired.el --- Xcode minor mode for dired
 ;; Based largely on erlang-dired-mode.el by Joseph(纪秀峰)
 ;; Author: Graham Lee <graham@iamleeg.com>
 ;; URL: http://twitter.com/secboffin
 ;; Keywords: xcode dired
 ;; Version: 0.0.1
 ;; Installation:
 ;; Put this file somewhere in your load-path (e.g. /usr/share/emacs/site-lisp) 
 ;; Add this to your .emacs:
 ;; (require 'xcode-dired-mode "xcode-dired")
 ;; Copyright:
 ;; This file is free software; you can redistribute it and/or modify 
 ;; it under the terms of the GNU General Public License as published by 
 ;; the Free Software Foundation; either version 2, or (at your option) 
 ;; any later version. 
 ;; This file is distributed in the hope that it will be useful, 
 ;; but WITHOUT ANY WARRANTY; without even the implied warranty of 
 ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
 ;; GNU General Public License for more details. 
 ;; You should have received a copy of the GNU General Public License 
 ;; along with GNU Emacs; see the file COPYING.  If not, write to 
 ;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330, 

;; this function was borrowed from an old version of files.el
(defun xcode-locate-dominating-files (file regexp)
"Look up the directory hierarchy from FILE for a file matching REGEXP.
Stop at the first parent where a matching file is found and return the list of
files that match in this directory."
  (catch 'found
    ;; `user' is not initialized yet because `file' may not exist, so we may
    ;; have to walk up part of the hierarchy before we find the "initial UID".
    (let ((user nil)
	  ;; Abbreviate, so as to stop when we cross ~/.
          (dir (abbreviate-file-name (file-name-as-directory file)))
          files)
      (while (and dir
                  ;; As a heuristic, we stop looking up the hierarchy of
                  ;; directories as soon as we find a directory belonging to
                  ;; another user.  This should save us from looking in
                  ;; things like /net and /afs.  This assumes that all the
                  ;; files inside a project belong to the same user.
                  (let ((prev-user user))
                    (setq user (nth 2 (file-attributes dir)))
                    (or (null prev-user) (equal user prev-user))))
        (if (setq files (condition-case nil
                        (directory-files dir 'full regexp 'nosort)
                      (error nil)))
            (throw 'found files)
          (if (equal dir
                     (setq dir (file-name-directory
                                (directory-file-name dir))))
              (setq dir nil))))
      nil)))

(defun xcode-find-project()
  "Look for <Project>.xcodeproj directory to see if we're in an Xcode project.
Return the project directory if found, or nil."
  (let ((xcode-root (xcode-locate-dominating-files default-directory "\\.xcodeproj$")))
    (if xcode-root
	(expand-file-name (car xcode-root))
    nil)
  )
)

(defun xcode-build()
  "Build the default target and configuration."
  (interactive)
  (let ((xcode-root (xcode-find-project)))
    (if xcode-root
	(shell-command (format "cd \"%s/..\" && xcodebuild" xcode-root))
      (error "Unable to find an Xcode project to build")
    )
  )
)

(defvar xcode-dired-mode-map
  (let ((map (make-sparse-keymap)))
  (define-key map (kbd "C-c C-b") 'xcode-build)
  (define-key map (kbd "C-c C-k") 'xcode-clean)
  map)
)

(define-minor-mode xcode-dired-mode
  "Xcode project development minor mode."
  nil
  " Xcode"
  xcode-dired-mode-map)

(defun xcode-dired-mode-loader()
  (let ((xcode-project-folder (xcode-find-project)))
    (when xcode-project-folder (xcode-dired-mode t))
  )
)

(add-hook 'dired-mode-hook 'xcode-dired-mode-loader)

(provide 'xcode-dired-mode)
