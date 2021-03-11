
(defun decyg (path)
	"Coerce PATH to be Windows style."
	(let* ((output (shell-command-to-string (concat "cygpath --windows '" path "'")))
				 (trimmed (substring output 0 -1)))
		trimmed
		))

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

(defun mapIn (root uri)
	(let* (
				 (pathRoot (car root))
				 (uriRoot (cdr root))
				 (suffix (if (s-starts-with? uriRoot uri)
						 (substring uri (length uriRoot))
					 uri))
				 (path (concat pathRoot suffix)))
		path)
	)

(defun mapOut (root path)
	(let* (
				 (pathRoot (car root))
				 (uriRoot (cdr root))
				 (parts (pathFromRoot pathRoot path))
				 (uri (concat uriRoot "/" (s-join "/" parts))))
		uri)
	)

(defun prepRoot (root)
	"Prepare tuple from ROOT."
	(let ((uri (decyg-uri root)))
		(cons root uri))
	)

(defun pathFromRoot (root path)
	(let* (
				 (p (if (s-starts-with? root path)
								(length root)
							0))
				 (suffix (substring path p))
				 (split (pathToParts suffix)))
		split))

(defun pathToParts (path)
	(seq-filter
	 (lambda (part) (not (s-equals? part "")))
	 (s-split "/" path)))



(ert-deftest pathToParts-test ()
	(should (equal
					 (pathToParts  "/home/jason/blah/moo.rs")
					 '("home" "jason" "blah" "moo.rs"))))

(ert-deftest pathFromRoot-test ()
	(should (equal
					 (PathFromRoot "/home/jason" "/home/jason/blah/moo.rs")
					 '("blah" "moo.rs"))))

(ert-deftest prepRoot-test ()
	(should (equal
					 (prepRoot "/home/jason")
					 '("/home/jason" . "file:///C%3A/msys/home/jason"))))

(ert-deftest mapOut-test ()
	(let* ((root (prepRoot "/home/jason"))
				 (mapped (mapOut root "/home/jason/blah/moo.rs")))
		(should (equal mapped "file:///C%3A/msys/home/jason/blah/moo.rs")))
	)

(ert-deftest mapIn-test ()
	(let* ((root (prepRoot "/home/jason"))
				 (path (mapIn root "file:///c:/msys/home/jason/blah/moo.rs")))
		(should (equal path "/home/jason/blah/moo.rs")))
	)


(prepRoot "/home/jason")
;; (mapIn (prepRoot "/home/jason") "file:///C%3A/msys/home/jason/blah/moo.rs")
;; (mapOut (prepRoot "/home/jason") "/home/jason/one/two")
;; (pathFromRoot "/home/jason" "/home/jason/one/two")
;; (pathToParts "/hello")
;; (rustic-flycheck-find-cargo-target "/home/jason/src/kb/src/main.rs")


(setq lsp-log-io 1)

(lexical-let ((root (prepRoot "/")))

	(defun mapOut-advice (path)
		(mapOut root path))

	(advice-add 'lsp--path-to-uri-1 :override #'mapOut-advice)


	(defun mapIn-advice (uri)
		(mapIn root uri))

	(advice-add 'lsp--uri-to-path :override #'mapIn-advice)

	)


(defun decyg-args-advice (args)
	"Blah ARGS."
	(let* (
				(path (car args))
				(fixed (decyg path))
				)
		(list fixed)))

(advice-add 'rustic-flycheck-find-cargo-target :filter-args #'decyg-args-advice)
