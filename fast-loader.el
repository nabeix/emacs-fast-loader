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

(defun fast-loader-needs-update (inits cache)
  "Returns t if need to update the cache file."
  (if (file-exists-p cache)
      (progn
        (let ((cache-modtime (nth 5 (file-attributes cache)))
              (sorted (sort (directory-files-and-attributes inits)
                            #'(lambda (x y) (time-less-p (nth 6 y) (nth 6 x))))))
          (if (string-equal (car (car sorted)) "..")
              (time-less-p cache-modtime (nth 6 (nth 1 sorted)))
            (time-less-p cache-modtime (nth 6 (car sorted))))))
    t))

(defun fast-loader-load (inits-dir)
  "Load init files in INITS-DIR."
  (let ((elfile (expand-file-name (concat fast-loader-cache-dir "/" fast-loader-cache-filename))))
    (if (fast-loader-needs-update inits-dir elfile)
        (progn
          (if (not (file-exists-p fast-loader-cache-dir))
              (make-directory fast-loader-cache-dir))
          (let ((cmd (concat "cd " inits-dir " && cat *.el | sed -e 's/;;.*$//' | sed -e '/^ *$/d' > " elfile))
                (elcfile (concat elfile "c")))
            ;; execute: cd inits-dir && cat *.el | sed -e 's/;;.*$//' | sed -e '/^ *$/d' > path/to/fast-loader-cache-dir/fast-loader-cache-filename
            (shell-command cmd)
            (when fast-loader-byte-compile
              (if (file-exists-p elcfile)
                  (byte-recompile-file elfile)
                (byte-compile-file elfile)))))))
  (add-to-list 'load-path fast-loader-cache-dir)
  (load (file-name-sans-extension fast-loader-cache-filename)))

(provide 'fast-loader)
