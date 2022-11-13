


;;; ORG

(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(after! org

  ;; TODO keywords and their colors

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "PROJ(p)" "WATCH(w)" "HOLD(h)" "BACKLOG(b)" "|" "DONE(d)" "KILL(k)")
          (sequence "MEET(m)" "|" "MEET_(_)")
          (sequence "MAYBE/SOMEDAY(s)" "|" "ABANDONED(a)")))

  ; Use `M-x list-colors-display' for available colors
  (setq org-todo-keyword-faces
        '(("TODO" . "medium sea green")
          ("PROJ" . "dark cyan")
          ("WATCH" . "khaki")
          ("HOLD" . "coral")
          ("BACKLOG" . "yellow green")
          ("MEET" . "light sea green")
          ("MAYBE/SOMEDAY" . "pale goldenrod")))

  ;; move by visual lines

  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-backward-char>") 'left-char)
  (define-key evil-motion-state-map (kbd "<remap> <evil-forward-char>") 'right-char)
  (define-key evil-normal-state-map (kbd "<remap> <evil-backward-char>") 'left-char)
  (define-key evil-motion-state-map (kbd "<remap> <evil-forward-char>") 'right-char)

  ;; customize what text looks like

  (custom-theme-set-faces 'user
                          '(org-level-1 ((t (:foreground "gray" :height 1.3 :weight bold))))
                          '(org-level-2 ((t (:foreground "gray" :height 1.075 :weight bold))))
                          '(org-level-3 ((t (:foreground "gray" :height 1.075 :weight bold))))
                          '(org-level-4 ((t (:foreground "gray" :height 1.075 :weight bold))))
                          '(org-level-5 ((t (:foreground "gray" :height 1.075 :weight bold))))
                          '(org-level-6 ((t (:foreground "gray" :height 1.075 :weight bold))))
                          '(org-level-7 ((t (:foreground "gray" :height 1.075 :weight bold))))
                          '(org-level-8 ((t (:foreground "gray" :height 1.075 :weight bold))))
                          '(org-block ((t (:inherit (shadow fixed-pitch)))))
                          '(org-code ((t (:inherit (shadow fixed-pitch)))))
                          '(org-document-info ((t (:foreground "dark orange"))))
                          '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
                          '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
                          '(org-link ((t (:foreground "royal blue" :underline t))))
                          '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
                          '(org-property-value ((t (:inherit fixed-pitch))) t)
                          '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
                          '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
                          '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
                          '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
                          '(line-number ((t (:inherit fixed-pitch :foreground "#3f444a"))))
                          '(line-number-current-line ((t (:inherit fixed-pitch :foreground "#bbc2cf"))))
                          )

  (setq org-hide-emphasis-markers t)

  ;; What programs to use when opening `'file:///`-style hyperlinks

  (setq org-file-apps
        '((auto-mode . emacs)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . default)
          ("\\.pdf\\'" . "firefox %s"))))



;;; Misc


(setq display-line-numbers-type 'relative)



;;; Spell-checking


;;(with-eval-after-load "ispell"
  ;; Configure 'LANG', otherwise 'ispell.el' cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  ;; in next line.
  ;(setenv "LANG" "en_US.UTF-8")
  ;(setq ispell-program-name "hunspell")

  ;(setq ispell-dictionary "en_US,sv_SE")

  ;; 'ispell-set-spellchecker-params' has to be called
  ;; before 'ispell-hunspell-add-multi-dic' will work
  ;(ispell-set-spellchecker-params)
  ;(ispell-hunspell-add-multi-dic "en_US,sv_SE")

  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
  ;(setq ispell-personal-dictionary "~/.hunspell_personal_dictionary"))

  ;; The personal dictionary file has to exist, otherwise hunspell will
  ;; silently not use it.
  ;(unless (file-exists-p ispell-personal-dictionary)
  ;  (write-region "" nil ispell-personal-dictionary nil 0))
