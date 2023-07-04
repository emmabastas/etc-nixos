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

(after! org-roam
  (setq my-org-roam-capture-tempalte-default
        '("d" "default" plain
        "%?"
        :target (file+head "%<%y%m%d%h%m%s>-${slug}.org" "#+title: ${title}\n")
        :unnarrowed t)))

(after! org-roam
  (setq my-org-roam-capture-template-book-note
        '("b" "book note" plain
        "\n*Child of:* [[id:33cdaa07-757a-491d-af0c-a25cbc9b7231][📚 Notes from reading books]]\n\n*Date read:*\n*ISBN:* \n*Source URI:* \n\n%?"
        :target (file+head "%<%y%m%d%h%m%s>-${slug}.org" "#+title: 📚 ${title}\n")
        :unnarrowed t)))

(after! org-roam
  (setq org-roam-capture-templates
        (list my-org-roam-capture-tempalte-default
              my-org-roam-capture-template-book-note)))

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

(setq org-drill-add-random-noise-to-intervals-p t)

(setq org-drill-save-buffers-after-drill-sessions-p nil)

(setq org-drill-hide-item-headings-p t)
