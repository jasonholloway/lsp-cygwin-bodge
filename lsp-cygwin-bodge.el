
(defun decyg (path)
  "Coerce PATH to be Windows style."
  (let* ((output (shell-command-to-string (concat "cygpath --windows '" path "'")))
         (trimmed (substring output 0 -1)))
    trimmed
    ))

(defun recyg (path)
  "Coerce PATH to be unix style."
  (let* ((output (shell-command-to-string (concat "cygpath --unix '" path "'")))
         (trimmed (substring output 0 -1)))
    trimmed
    ))

(defun recyg-uri (uri)
  "Convert URI back into Cygwin path."
  (let ((path (substring uri 8)))
    (recyg path)))

(defun decyg-reslash (path)
  "Desyg and reslash PATH."
  (let* ((decyged (decyg path))
         (reslashed (s-replace "\\" "/" decyged))
         )
    reslashed
    ))

(defun decyg-uri (path)
  "Both decyg, reslash and urify PATH."
  (let* ((reslashed (decyg-reslash path))
         (lowerCased (downcase reslashed))
         ;;(rehexed (url-hexify-string reslashed lsp--url-path-allowed-chars))
         (prefixed (concat "file:///" lowerCased))
         )
    prefixed
    ))

(defun mangleIn (space uri)
  "Read URI into SPACE."
  (let* (
         (pathRoots (car space))
         (uriRoot (cdr space))
         (path (if (s-starts-with? uriRoot uri)
                   (let ((suffix (substring uri (length uriRoot))))
                     (concat (first pathRoots) suffix))
                 (recyg-uri uri)))
         )
    (message "%s <- %s" path uri)
    path
  ))

(defun mangleOut (space path)
  (if-let (parts (pathFromRoot space path))
      (let* (
             (uriRoot (cdr space))
             (uri (concat uriRoot "/" (s-join "/" parts)))
             )
        (message "%s -> %s" path uri)
        uri)
    (decyg-uri path)
    ))

(defun ensureList (arg)
  (if (listp arg) arg (list arg))
  )

(defun pathToParts (path)
  (seq-filter
   (lambda (part) (not (s-equals? part "")))
   (s-split "/" path)))


(defun prepSpace (arg)
  "Prepare space tuple from ARG."
  (let* (
         (roots (ensureList arg))
         (firstRoot (first roots))
         (uri (decyg-uri firstRoot))
         )
    (cons roots uri))
  )

(defun pathFromRoot (space path)
  "Find first matching root from SPACE for PATH; return the split relative path."
  (let* (
         (roots (car space))
         (matchedRoot (seq-find (lambda (r) (s-starts-with? r path)) roots))
         )
    (when matchedRoot
      (pathToParts (substring path (length matchedRoot))))))



(ert-deftest recyg-uri-test ()
  (should (equal
           (recyg-uri "file:///c:/blah/moo.rs")
           "/c/blah/moo.rs")))

(ert-deftest pathToParts-test ()
  (should (equal
           (pathToParts  "/home/jason/blah/moo.rs")
           '("home" "jason" "blah" "moo.rs"))))

(ert-deftest pathFromRoot-test ()
  (should (equal
           (pathFromRoot '(("/home/jason") "") "/home/jason/blah/moo.rs")
           '("blah" "moo.rs"))))

(ert-deftest prepSpace-test ()
  (should (equal
           (prepSpace "/home/jason")
           '(("/home/jason") . "file:///c:/msys/home/jason"))))

(ert-deftest mangleOut-test ()
  (let* ((space (prepSpace "/home/jason"))
         (mapped (mangleOut space "/home/jason/blah/moo.rs")))
    (should (equal mapped "file:///c:/msys/home/jason/blah/moo.rs")))
  )

(ert-deftest mangleOut2-test ()
  (let* ((space (prepSpace "/home/jason"))
         (mapped (mangleOut space "/c/src/kb/src/windows/mod.rs")))
    (should (equal mapped "file:///c:/src/kb/src/windows/mod.rs")))
  )

(ert-deftest mangleIn-test ()
  (let* ((space (prepSpace "/home/jason"))
         (path (mangleIn space "file:///c:/msys/home/jason/blah/moo.rs")))
    (should (equal path "/home/jason/blah/moo.rs")))
  )

(ert-deftest mangleIn2-test ()
  (let* ((space (prepSpace "/home/jason"))
         (path (mangleIn space "file:///c:/src/kb/src/main.rs")))
    (should (equal path "/c/src/kb/src/main.rs")))
  )



(setq lsp-log-io 1)

(lexical-let ((space (prepSpace '("/home/jason" "/c/Users/Jason.Holloway" "/c/msys/home/jason"))))

  (defun mangleOut-advice (path)
    (mangleOut space path))

  (advice-remove 'lsp--path-to-uri-1 'mangle)
  (advice-add 'lsp--path-to-uri-1 :override #'mangleOut-advice '((name . mangle)))


  (defun mangleIn-advice (uri)
    (mangleIn space uri))

  (advice-remove 'lsp--uri-to-path "mangle")
  (advice-add 'lsp--uri-to-path :override #'mangleIn-advice '((name . mangle)))

  )


;; (defun decyg-args-advice (args)
;;  "Blah ARGS."
;;  (let* (
;;        (path (car args))
;;        (fixed (decyg path))
;;        )
;;    (list fixed)))


(defun decyg-targets-advice (targets)
  "Blah TARGETS."
  (message "start")
  (print targets)
  (let* (
         (oldPath (alist-get 'src_path (car (car targets))))
         (newPath (recyg oldPath))
         )
    (setf (alist-get 'src_path (car (car targets))) newPath)
    (print targets)
    (message "stop")
    targets
    )
  )

(advice-remove 'rustic-flycheck-get-cargo-targets 'decyg)
(advice-add 'rustic-flycheck-get-cargo-targets :filter-return #'decyg-targets-advice '((name . decyg)))


;; (advice-remove 'rustic-flycheck-find-cargo-target 'decyg)
;; (advice-add 'rustic-flycheck-find-cargo-target :filter-args #'decyg-args-advice '((name . decyg)))

