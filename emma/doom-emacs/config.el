(setq display-line-numbers 'relative)

(with-eval-after-load "ispell"
  ;; Configure 'LANG', otherwise 'ispell.el' cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  ;; in next line.
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-program-name "hunspell")

  (setq ispell-dictionary "en_US,sv_SE")

  ;; 'ispell-set-spellchecker-params' has to be called
  ;; before 'ispell-hunspell-add-multi-dic' will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,sv_SE")

  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
  (setq ispell-personal-dictionary "~/.hunspell_personal_dictionary"))

;; The personal dictionary file has to exist, otherwise hunspell will
;; silently not use it.
(unless (file-exists-p ispell-personal-dictionary)
  (write-region "" nil ispell-personal-dictionary nil 0))
