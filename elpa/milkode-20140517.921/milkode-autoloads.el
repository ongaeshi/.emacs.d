;;; milkode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (milkode:update milkode:add milkode:display-history
;;;;;;  milkode:search-from-all-packages milkode:search-at-point
;;;;;;  milkode:search) "milkode" "milkode.el" (21385 59553 0 0))
;;; Generated autoloads from milkode.el

(autoload 'milkode:search "milkode" "\
Milkode search current package using `M-x grep`

\(fn)" t nil)

(autoload 'milkode:search-at-point "milkode" "\
Milkode search current package at point text. If the prefix was C-u, search all registered packages

\(fn N)" t nil)

(autoload 'milkode:search-from-all-packages "milkode" "\
Milkode search all registered packages using `M-x grep`

\(fn)" t nil)

(autoload 'milkode:display-history "milkode" "\
Dispaly search history

\(fn)" t nil)

(autoload 'milkode:add "milkode" "\
Execute `milk add`

\(fn DIRECTORY)" t nil)

(autoload 'milkode:update "milkode" "\
Execute `milk update`

\(fn DIRECTORY)" t nil)

;;;***

;;;### (autoloads nil nil ("milkode-pkg.el") (21385 59553 450041
;;;;;;  0))

;;;***

(provide 'milkode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; milkode-autoloads.el ends here
