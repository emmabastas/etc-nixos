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

(setq org-drill-add-random-noise-to-intervals-p t)

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
