(add-hook 'org-mode-hook 'visual-line-mode)

(after! org
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-backward-char>") 'left-char)
  (define-key evil-motion-state-map (kbd "<remap> <evil-forward-char>") 'right-char)
  (define-key evil-normal-state-map (kbd "<remap> <evil-backward-char>") 'left-char)
  (define-key evil-motion-state-map (kbd "<remap> <evil-forward-char>") 'right-char)
)

(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "PROJ(p)" "WATCH(w)" "HOLD(h)" "BACKLOG(b)" "|" "DONE(d)" "KILL(k)")
          (sequence "MEET(m)" "|" "MEET_(_)")
          (sequence "MAYBE/SOMEDAY(s)" "|" "ABANDONED(a)"))))

(after! org
  (setq org-todo-keyword-faces
        '(("TODO" . "medium sea green")
          ("PROJ" . "dark cyan")
          ("WATCH" . "khaki")
          ("HOLD" . "coral")
          ("BACKLOG" . "yellow green")
          ("MEET" . "light sea green")
          ("MAYBE/SOMEDAY" . "pale goldenrod"))))

(after! org (setq org-hide-emphasis-markers t))

(add-hook 'org-mode-hook 'variable-pitch-mode)

(after! org
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
                          ))

(after! org
  (setq org-file-apps
        '((auto-mode . emacs)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . default)
          ("\\.pdf\\'" . "firefox %s"))))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(defvar nterms 0)

(defun term_ (program)
  "Modified version of ~term~"
  (interactive (list "/run/current-system/sw/bin/bash"))
  (setq nterms (+ 1 nterms))
  (let ((termname (concat "terminal-" (number-to-string nterms))))
      (set-buffer (make-term termname program))
      (term-mode)
      (term-char-mode)
      (switch-to-buffer (concat "*" termname "*"))))

(define-key (current-global-map) (kbd "C-c t") #'term_)

(setq emojify-display-style 'unicode)

(setq emojify-user-emojis '(("—" . (("name" . "Em Dash")
                                    ("unicode" . "—")
                                    ("style" . "unicode")))
                            ("⇔" . (("name" . "\\iff")
                                    ("unicode" . "⇔")
                                    ("style" . "unicode")))
                            ("⇒" . (("name" . "\\implies")
                                    ("unicode" . "⇒")
                                    ("style" . "unicode")))
                            ("∀" . (("name" . "\\forall")
                                    ("unicode" . "∀")
                                    ("style" . "unicode")))
                            ("∃" . (("name" . "\\exists")
                                    ("unicode" . "∃")
                                    ("style" . "unicode")))
                            ("■" . (("name" . "\\qed")
                                    ("unicode" . "■")
                                    ("style" . "unicode")))
                            ("≅" . (("name" . "\\cong")
                                    ("unicode" . "≅")
                                    ("style" . "unicode")))
                            ("≤" . (("name" . "\\leq")
                                    ("unicode" . "≤")
                                    ("style" . "unicode")))
                            ))

;; If emojify is already loaded refresh emoji data
(when (featurep 'emojify)
  (emojify-set-emoji-data))

(setq org-drill-add-random-noise-to-intervals-p t)

(setq org-drill-save-buffers-after-drill-sessions-p nil)

(setq org-drill-hide-item-headings-p t)

(defun org-drill-present-babel-generated (session)
  "Present a simple card."
  (org-babel-next-src-block)
  (let ((ret (org-babel-execute-src-block)))
    (org-drill-with-hidden-comments
     (org-drill-with-hidden-cloze-hints
      (org-drill-with-hidden-cloze-text
       (org-drill-hide-all-subheadings-except nil)
       (org-drill--show-latex-fragments)  ; overlay all LaTeX fragments with images
       (ignore-errors
         (org-display-inline-images t))
       (org-hide-block-all)
       (prog1 (org-drill-presentation-prompt session)
         (org-drill-hide-subheadings-if 'org-drill-entry-p)))))))

(add-to-list
 'org-drill-card-type-alist
 '("babel_generated" org-drill-present-babel-generated))
