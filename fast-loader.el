(defcustom fast-loader-cache-filename "inits.el"
  "Default cache file name."
  :type 'string
  :group 'fast-loader)

(defcustom fast-loader-cache-dir (concat user-emacs-directory "fast-loader-cache")
  "Default directory for saving cache."
  :type 'string
  :group 'fast-loader)

(defcustom fast-loader-byte-compile nil
  "Complie cached file to .elc (default nil)."
  :type 'boolean
  :group 'fast-loader)

(defun fast-loader-needs-update ()
  "Returns t if need to update the cache file"
  (let ((cache-modtime (nth 5 (file-attributes "~/.emacs.d/fast-loader-cache/inits.el")))
        (inits-modtime (nth 6(nth 0 (sort (directory-files-and-attributes "~/.emacs.d/inits")
                                          #'(lambda (x y) (time-less-p (nth 6 y) (nth 6 x))))))))
    (time-less-p cache-modtime inits-modtime))
  )

(defun fast-loader-load (inits-dir)
  "Load init files in INITS-DIR."
  (if (fast-loader-needs-update)
      (progn
        (let* ((elfile (expand-file-name (concat fast-loader-cache-dir "/" fast-loader-cache-filename)))
               (cmd (concat "cd " inits-dir " && cat * | sed -e 's/;;.*$//' | sed -e '/^ *$/d' > " elfile)))
          ;; execute: cd inits-dir && cat * | sed -e 's/;;.*$//' | sed -e '/^ *$/d' > path/to/fast-loader-cache-dir/fast-loader-cache-filename
          (shell-command cmd)
          (when fast-loader-byte-compile
            (add-hook 'after-init-hook
                      '(lambda ()
                         (message elfile)
                         (let* ((elcfile (concat elfile "c")))
                           (when (file-newer-than-file-p elfile elcfile)
                             (byte-compile-file el)))))
            ))
        ))
  (add-to-list 'load-path fast-loader-cache-dir)
  (load (file-name-sans-extension fast-loader-cache-filename)))

(provide 'fast-loader)
