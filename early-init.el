;; early-init.el -*- lexical-binding: t; -*-

;; disable package.el before anything else
(setq package-enable-at-startup nil)

;; bringing up package/use-package/no-littering
(defvar no-littering-etc-directory (expand-file-name ".cache/etc/" user-emacs-directory))
(defvar no-littering-var-directory (expand-file-name ".cache/var/" user-emacs-directory))
(setq package-user-dir (expand-file-name "elpa/" no-littering-var-directory))

;; redirect native-compilation files to no-littering folder
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "eln-cache/" no-littering-var-directory))))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
