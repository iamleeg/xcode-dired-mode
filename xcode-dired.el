;; Xcode minor mode for dired
;; Based largely on erlang-dired-mode.el by Joseph(纪秀峰)

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
Return the enclosing directory if found, or nil."
  (let ((xcode-root (xcode-locate-dominating-files default-directory "\\.xcodeproj$")))
    (if xcode-root
	(expand-file-name (car xcode-root))
    nil)
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
