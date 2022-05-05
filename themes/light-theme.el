;;; light-theme.el --- My light theme for Emacs
;;; Commentary:
;;; This theme is based on a color scheme found at https://materialui.co/flatuicolors/
;;; Code:
(deftheme light
  "A UI theme for Emacs based on material design colors")
(display-color-cells (selected-frame))

(let* ((class '((class color) (min-colors 89)))
       (background "#ffffff")
       (faded-background "#ecf0f1")
       (current-line "#ecf0f1")
       (far-background "#bdc3c7")
       (header-color "#C8E6C9")
       (subtle "#95a5a6")
       (selection "#ecf0f1")
       (secondary-selection "#ecf0f1")
       (foreground "#2c3e50")
       (comment "#7f8c8d")
       (red "#e74c3c")
       (faded-red "#c0392b")
       (orange "#e67e22")
       (faded-orange "#d35400")
       (yellow "#f1c40f")
       (faded-yellow "#f39c12")
       (green "#2ecc71")
       (faded-green "#27ae60")
       (aqua "#1abc9c")
       (faded-aqua "#16a085")
       (blue "#3498db")
       (faded-blue "#2980b9")
       (purple "#9b59b6")
       (faded-purple "#8e44ad"))

  (custom-theme-set-faces
   'light
   `(variable-pitch ((t (:font ,"Roboto 14"))))
   `(fixed-pitch ((t (:font ,"Roboto Mono 14"))))
   `(default ((,class (:foreground ,foreground :background ,background :font ,"Roboto Mono 14"))))
   `(bold ((,class (:weight bold))))
   `(bold-italic ((,class (:slant italic :weight bold))))
   `(underline ((,class (:underline t))))
   `(italic ((,class (:slant italic))))
   `(font-lock-builtin-face ((,class (:foreground ,orange))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-constant-face ((,class (:foreground ,green))))
   `(font-lock-doc-face ((,class (:foreground ,faded-green))))
   `(font-lock-doc-string-face ((,class (:foreground ,yellow))))
   `(font-lock-function-name-face ((,class (:foreground ,aqua))))
   `(font-lock-keyword-face ((,class (:foreground ,aqua))))
   `(font-lock-negation-char-face ((,class (:foreground ,blue))))
   `(font-lock-preprocessor-face ((,class (:foreground ,yellow))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,yellow))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,purple))))
   `(font-lock-string-face ((,class (:foreground ,green))))
   `(font-lock-type-face ((,class (:foreground ,blue))))
   `(font-lock-variable-name-face ((,class (:foreground ,yellow))))
   `(font-lock-warning-face ((,class (:weight bold :foreground ,red))))
   `(highlight-numbers-number ((,class (:foreground ,green))))
   `(shadow ((,class (:foreground ,comment))))
   `(success ((,class (:foreground ,faded-aqua))))
   `(error ((,class (:foreground ,red))))
   `(warning ((,class (:foreground ,orange))))

   ;; highlight indentation
   `(highlight-indentation-face ((,class (:background, current-line))))
   `(highlight-indentation-current-column-face ((,class (:background, far-background))))

   ;; Flymake
   `(flymake-warnline ((,class (:underline (:style wave :color ,yellow) :background ,background))))
   `(flymake-errline ((,class (:underline (:style wave :color ,red) :background ,background))))

   ;; Rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,yellow))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,green))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,aqua))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,blue))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,purple))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,faded-yellow))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,faded-green))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,faded-aqua))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,faded-blue))))
   `(rainbow-delimiters-unmatched-face ((,class (:foreground ,foreground :background ,red))))

   ;; Search
   `(match ((,class (:foreground ,background :background ,green :inverse-video nil))))
   `(isearch ((,class (:foreground ,background :background ,green))))
   `(lazy-highlight ((,class (:foreground ,background :background ,green :inverse-video nil))))
   `(isearch-fail ((,class (:background ,background :inherit font-lock-warning-face :inverse-video t))))

   ;; iedit
   `(iedit-occurrence ((,class (:foreground ,background :background ,green))))

   ;; Corfu
   `(corfu-annotations ((,class (:foreground ,comment))))
   `(corfu-bar ((,class (:background ,subtle))))
   `(corfu-border ((,class (:background ,aqua))))
   `(corfu-current ((,class (:foreground ,foreground :background ,aqua))))
   `(corfu-default ((,class (:foreground ,foreground :background ,background))))
   `(corfu-deprecated ((,class (:foreground ,subtle :background ,faded-background))))
   `(corfu-echo ((,class (:foreground ,foreground :background ,background))))
   
   ;; IDO
   `(ido-subdir ((,class (:foreground ,purple))))
   `(ido-first-match ((,class (:foreground ,orange))))
   `(ido-only-match ((,class (:foreground ,green))))
   `(ido-indicator ((,class (:foreground ,red :background ,background))))
   `(ido-virtual ((,class (:foreground ,comment))))

   ;; which-function
   `(which-func ((,class (:foreground ,blue :background nil))))

   ;; Emacs interface
   `(cursor ((,class (:background ,yellow))))
   `(fringe ((,class (:background ,background))))
   `(linum ((,class (:background ,faded-background :foreground ,foreground))))
   `(linum-highlight-face ((,class (:background ,current-line :foreground ,foreground))))
   `(border ((,class (:background ,background))))
   `(internal-border ((,class (:background ,background))))
   `(vertical-border ((,class (:background ,background
                                           :foreground ,subtle))))
   `(border-glyph ((,class (nil))))
   `(highlight ((,class (:inverse-video nil :background ,current-line))))
   `(hl-line ((,class (:inverse-video nil :background ,current-line))))
   `(gui-element ((,class (:background ,current-line :foreground ,foreground))))
   `(mode-line ((,class (:foreground ,foreground :background ,far-background
                                     :box nil))))
   `(mode-line-buffer-id ((,class (:foreground ,foreground :background nil :weight bold))))
   `(mode-line-inactive ((,class (:inherit mode-line
                                           :foreground ,subtle
                                           :background ,faded-background
                                           :weight normal))))
   `(mode-line-emphasis ((,class (:foreground ,foreground :slant italic))))
   `(mode-line-highlight ((,class (:foreground ,purple :box nil))))
   `(minibuffer-prompt ((,class (:foreground ,blue))))
   `(region ((,class (:background ,selection))))
   `(secondary-selection ((,class (:background ,secondary-selection))))

   ;; Orderless
   `(orderless-match-face-0 ((,class (:foreground ,blue :weight bold))))
   `(orderless-match-face-1 ((,class (:foreground ,purple :weight bold))))
   `(orderless-match-face-2 ((,class (:foreground ,green :weight bold))))
   `(orderless-match-face-3 ((,class (:foreground ,orange :weight bold))))
   
   `(header-line ((,class (:inherit mode-line :foreground ,purple :background nil))))

   `(trailing-whitespace ((,class (:foreground ,red :inverse-video t :underline nil))))
   `(whitespace-trailing ((,class (:foreground ,red :inverse-video t :underline nil))))
   `(whitespace-space-after-tab ((,class (:foreground ,red :inverse-video t :underline nil))))
   `(whitespace-space-before-tab ((,class (:foreground ,red :inverse-video t :underline nil))))
   `(whitespace-empty ((,class (:foreground ,red :inverse-video t :underline nil))))
   `(whitespace-line ((,class (:background nil :foreground ,red))))
   `(whitespace-indentation ((,class (:background nil :foreground ,aqua))))
   `(whitespace-space ((,class (:background nil :foreground ,selection))))
   `(whitespace-newline ((,class (:background nil :foreground ,selection))))
   `(whitespace-tab ((,class (:background nil :foreground ,selection))))
   `(whitespace-hspace ((,class (:background nil :foreground ,selection))))

   ;; Parenthesis matching (built-in)
   `(show-paren-match-face ((,class (:background ,blue :foreground ,background))))
   `(show-paren-mismatch-face ((,class (:background "red1" :foreground "white"))))

   ;; Smartparens paren matching
   `(sp-show-pair-match-face ((,class (:foreground ,background :background ,blue :inherit show-paren-match))))
   `(sp-show-pair-mismatch-face ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))

   ;; Parenthesis matching (mic-paren)
   `(paren-face-match ((,class (:foreground nil :background nil :inherit show-paren-match))))
   `(paren-face-mismatch ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))
   `(paren-face-no-match ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))

   ;; Parenthesis dimming (parenface)
   `(paren-face ((,class (:foreground ,comment :background nil))))

   `(sh-heredoc ((,class (:foreground nil :inherit font-lock-string-face :weight normal))))
   `(sh-quoted-exec ((,class (:foreground nil :inherit font-lock-preprocessor-face))))
    `(csv-separator-face ((,class (:foreground ,orange))))

   `(diff-hl-insert ((,class (:background ,green :foreground ,green))))
   `(diff-hl-change ((,class (:background ,blue :foreground ,blue))))
   `(diff-hl-delete ((,class (:background ,orange :foreground ,orange))))

   `(diff-added ((,class (:foreground ,green))))
   `(diff-changed ((,class (:foreground ,blue))))
   `(diff-removed ((,class (:foreground ,orange))))
   `(diff-header ((,class (:foreground ,aqua :background nil))))
   `(diff-hunk-header ((,class (:foreground ,purple))))
   `(diff-refine-added ((,class (:inherit diff-added :inverse-video t))))
   `(diff-refine-removed ((,class (:inherit diff-removed :inverse-video t))))

   `(ediff-even-diff-A ((,class (:foreground nil :background nil :inverse-video t))))
   `(ediff-even-diff-B ((,class (:foreground nil :background nil :inverse-video t))))
   `(ediff-odd-diff-A  ((,class (:foreground ,comment :background nil :inverse-video t))))
   `(ediff-odd-diff-B  ((,class (:foreground ,comment :background nil :inverse-video t))))

   `(eldoc-highlight-function-argument ((,class (:foreground ,green :weight bold))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face ((,class (:foreground ,foreground))))
   `(undo-tree-visualizer-current-face ((,class (:foreground ,green :weight bold))))
   `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,red))))
   `(undo-tree-visualizer-register-face ((,class (:foreground ,yellow))))

   ;; dired+
   `(diredp-compressed-file-suffix ((,class (:foreground ,blue))))
   `(diredp-deletion ((,class (:inherit error :inverse-video t))))
   `(diredp-deletion-file-name ((,class (:inherit error))))
   `(diredp-dir-heading ((,class (:foreground ,green :weight bold))))
   `(diredp-dir-priv ((,class (:foreground ,aqua :background nil))))
   `(diredp-exec-priv ((,class (:foreground ,blue :background nil))))
   `(diredp-executable-tag ((,class (:foreground ,red :background nil))))
   `(diredp-file-name ((,class (:foreground ,yellow))))
   `(diredp-file-suffix ((,class (:foreground ,green))))
   `(diredp-flag-mark ((,class (:foreground ,green :inverse-video t))))
   `(diredp-flag-mark-line ((,class (:background nil :inherit highlight))))
   `(diredp-ignored-file-name ((,class (:foreground ,comment))))
   `(diredp-link-priv ((,class (:background nil :foreground ,purple))))
   `(diredp-mode-line-flagged ((,class (:foreground ,red))))
   `(diredp-mode-line-marked ((,class (:foreground ,green))))
   `(diredp-no-priv ((,class (:background nil))))
   `(diredp-number ((,class (:foreground ,yellow))))
   `(diredp-other-priv ((,class (:background nil :foreground ,purple))))
   `(diredp-rare-priv ((,class (:foreground ,red :background nil))))
   `(diredp-read-priv ((,class (:foreground ,green :background nil))))
   `(diredp-symlink ((,class (:foreground ,purple))))
   `(diredp-write-priv ((,class (:foreground ,yellow :background nil))))

   ;; diredfl
   `(diredfl-compressed-file-suffix ((,class (:foreground ,blue))))
   `(diredfl-compressed-file-name ((,class (:foreground ,blue))))
   `(diredfl-ignored-file-name ((,class (:foreground ,comment))))
   `(diredfl-date-time ((,class (:foreground ,green))))
   `(diredfl-file-name ((,class (:foreground ,foreground))))
   `(diredfl-read-priv ((,class (:foreground ,green :background nil))))
   `(diredfl-write-priv ((,class (:foreground ,yellow :background nil))))
   `(diredfl-exec-priv ((,class (:foreground ,red :background nil))))
   `(diredfl-rare-priv ((,class (:foreground ,orange :background nil))))
   `(diredfl-no-priv ((,class (:background nil))))
   `(diredfl-deletion ((,class (:inherit error :inverse-video t))))
   `(diredfl-deletion-file-name ((,class (:inherit error))))
   `(diredfl-dir-heading ((,class (:foreground ,green :weight bold))))
   `(diredfl-symlink ((,class (:foreground ,purple))))
   `(diredfl-dir-priv ((,class (:foreground ,aqua :background nil))))
   `(diredfl-dir-name ((,class (:foreground ,aqua :background nil))))
   `(diredfl-number ((,class (:foreground ,yellow :background nil))))
   `(diredfl-flag-mark ((,class (:foreground ,orange :background nil))))
   `(diredfl-flag-mark-line ((,class (:foreground ,nil :background ,selection))))
   `(diredfl-file-suffix ((,class (:foreground ,aqua :background nil))))

   ;; Magit
   `(magit-branch ((,class (:foreground ,green))))
   `(magit-diff-added ((,class (:inherit diff-added))))
   `(magit-diff-added-highlight ((,class (:inherit magit-diff-added
                                          :background ,faded-background))))
   `(magit-diff-removed ((,class (:inherit diff-removed))))
   `(magit-diff-removed-highlight ((,class (:inherit magit-diff-removed
                                            :background ,faded-background))))
   `(magit-header ((,class (:inherit nil :weight bold))))
   `(magit-item-highlight ((,class (:inherit highlight :background nil))))
   `(magit-log-author ((,class (:foreground ,aqua))))
   `(magit-log-graph ((,class (:foreground ,comment))))
   `(magit-log-date ((,class (:foreground ,yellow))))
   `(magit-section-title ((,class (:foreground ,blue :weight bold))))
   `(magit-section-highlight           ((t (:background ,current-line))))
   `(magit-section-heading             ((t (:foreground ,yellow :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,faded-background  :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,background
                                            :foreground ,orange :weight bold))))
   `(magit-diff-hunk-heading           ((t (:background ,header-color))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,header-color))))
   `(magit-diff-hunk-heading-selection ((t (:background ,background
                                            :foreground ,orange))))
   `(magit-diff-lines-heading          ((t (:background ,orange
                                            :foreground ,background))))
   `(magit-blame-heading          ((t (:background ,far-background
                                                   :foreground ,aqua))))
   `(magit-blame-date             ((t (:background ,far-background
                                                   :foreground ,blue))))
   `(magit-blame-summary          ((t (:background ,far-background
                                                   :foreground ,green))))
   `(magit-diff-context-highlight      ((t (:background ,faded-background
                                            :foreground "grey70"))))
   `(magit-diff-context                ((t (:foreground "grey70"))))
   `(magit-diffstat-added   ((t (:foreground ,green))))
   `(magit-diffstat-removed ((t (:foreground ,red))))
   `(magit-process-ok    ((t (:foreground ,green  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,red    :weight bold))))
   `(magit-branch-local  ((t (:foreground ,blue   :weight bold))))
   `(magit-branch-remote ((t (:foreground ,green  :weight bold))))
   `(magit-tag           ((t (:foreground ,orange :weight bold))))
   `(magit-hash          ((t (:foreground ,comment))))
   `(magit-sequence-stop ((t (:foreground ,green))))
   `(magit-sequence-part ((t (:foreground ,yellow))))
   `(magit-sequence-head ((t (:foreground ,blue))))
   `(magit-sequence-drop ((t (:foreground ,red))))

   ;; git-gutter
   `(git-gutter:modified ((,class (:foreground ,purple :weight bold))))
   `(git-gutter:added ((,class (:foreground ,green :weight bold))))
   `(git-gutter:deleted ((,class (:foreground ,red :weight bold))))
   `(git-gutter:unchanged ((,class (:background ,yellow))))

   ;; git-gutter-fringe
   `(git-gutter-fr:modified ((,class (:foreground ,purple :weight bold))))
   `(git-gutter-fr:added ((,class (:foreground ,green :weight bold))))
   `(git-gutter-fr:deleted ((,class (:foreground ,red :weight bold))))

   `(link ((,class (:foreground nil :underline t))))
   `(widget-button ((,class (:underline t :weight bold))))
   `(widget-field ((,class (:background ,current-line :box (:line-width 1 :color ,foreground)))))

   ;; Compilation (most faces politely inherit from 'success, 'error, 'warning etc.)
   `(compilation-column-number ((,class (:foreground ,yellow))))
   `(compilation-line-number ((,class (:foreground ,yellow))))
   `(compilation-message-face ((,class (:foreground ,blue))))
   `(compilation-mode-line-exit ((,class (:foreground ,green))))
   `(compilation-mode-line-fail ((,class (:foreground ,red))))
   `(compilation-mode-line-run ((,class (:foreground ,blue))))

   ;; line numbers
   `(line-number ((,class (:foreground ,subtle :background ,faded-background))))
   `(line-number-current-line ((,class (:foreground ,subtle :background ,far-background))))
   `(linum ((,class (:foreground ,subtle :background ,faded-background))))
   `(linum-highlight-face ((,class (:foreground ,subtle, :background ,far-background))))
   `(linum-relative-current-face ((,class (:foreground ,subtle :background ,far-background))))
   
   ;; Grep
   `(grep-context-face ((,class (:foreground ,comment))))
   `(grep-error-face ((,class (:foreground ,red :weight bold :underline t))))
   `(grep-hit-face ((,class (:foreground ,blue))))
   `(grep-match-face ((,class (:foreground nil :background nil :inherit match))))

   `(regex-tool-matched-face ((,class (:foreground nil :background nil :inherit match))))

   ;; which-key
   `(which-key-key-face ((,class (:foreground ,foreground  :weight bold))))
   `(which-key-special-key-face ((,class (:foreground ,orange  :weight bold :height 1.1))))
   `(which-key-command-description-face ((,class (:foreground ,foreground ))))
   `(which-key-group-description-face ((,class (:foreground ,blue ))))
   `(which-key-separator-face ((,class (:foreground ,comment ))))

   ;; mark-multiple
   `(mm/master-face ((,class (:inherit region :foreground nil :background nil))))
   `(mm/mirror-face ((,class (:inherit region :foreground nil :background nil))))

   `(org-agenda-structure ((,class (:foreground ,aqua :bold t))))
   `(org-agenda-date ((,class (:foreground ,blue :underline nil))))
   `(org-agenda-done ((,class (:foreground ,green))))
   `(org-agenda-dimmed-todo-face ((,class (:foreground ,comment))))
   `(org-block ((,class (:foreground ,foreground :background ,faded-background :extend t :inherit fixed-pitch))))
   `(org-block-background ((,t (:background ,faded-background :extend t))))
   `(org-block-begin-line ((,class (:foreground ,foreground :background ,faded-background :extend t))))
   `(org-block-end-line ((,class (:foreground ,foreground :background ,faded-background :extend t :inherit fixed-pitch))))
   `(org-code ((,class (:foreground ,foreground :background ,faded-background :inherit fixed-pitch))))
   `(org-column ((,class (:background ,faded-background))))
   `(org-column-title ((,class (:inherit org-column :weight bold :underline t))))
   `(org-date ((,class (:foreground ,aqua :underline t))))
   `(org-document-info ((,class (:foreground ,aqua :height 1.35))))
   `(org-document-info-keyword ((,class (:foreground ,green :height 1.35))))
   `(org-document-title ((,class (:weight bold :foreground ,foreground :height 1.35))))
   `(org-done ((,class (:bold t :foreground ,green))))
   `(org-drawer ((,class (:foreground ,yellow :inherit fixed-pitch))))
   `(org-ellipsis ((,class (:foreground ,comment))))
   `(org-footnote ((,class (:foreground ,aqua))))
   `(org-formula ((,class (:foreground ,orange))))
   `(org-hide ((,class (:foreground ,background :background ,background))))
   `(org-kbd ((,class (:background ,far-background :foreground ,foreground))))
   `(org-level-1 ((,class (:inherit outline-1 :height 1.5))))
   `(org-level-2 ((,class (:inherit outline-2 :height 1.4))))
   `(org-level-3 ((,class (:inherit outline-3 :height 1.3))))
   `(org-level-4 ((,class (:inherit outline-4 :height 1.2))))
   `(org-level-5 ((,class (:inherit outline-5 :height 1.1))))
   `(org-level-6 ((,class (:inherit outline-6 ))))
   `(org-level-7 ((,class (:inherit outline-7 ))))
   `(org-level-8 ((,class (:inherit outline-8 ))))
   `(org-level-9 ((,class (:inherit outline-9 ))))
   `(org-link ((,class (:foreground ,blue :underline t))))
   `(org-property-value ((,class (:inherit fixed-pitch))))
   `(org-scheduled ((,class (:foreground ,green))))
   `(org-scheduled-previously ((,class (:foreground ,orange))))
   `(org-scheduled-today ((,class (:foreground ,green))))
   `(org-special-keyword ((,class (:foreground ,comment :inherit fixed-pitch))))
   `(org-tag ((,class (:inherit fixed-pitch))))
   `(org-table ((,class (:foreground ,green :inherit fixed-pitch))))
   `(org-todo ((,class (:bold t :foreground ,red))))
   `(org-upcoming-deadline ((,class (:foreground ,orange))))
   `(org-verbatim ((,class (:inherit fixed-pitch))))
   `(org-warning ((,class (:weight bold :foreground ,red))))

   `(markdown-header-face-1 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.3 ))))
   `(markdown-header-face-2 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.2 ))))
   `(markdown-header-face-3 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-face-4 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-face-5 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-face-6 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-face-7 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-face-8 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-face-9 ((,class (:inherit font-lock-function-name-face :weight bold :height 1.1 ))))
   `(markdown-header-delimiter-face ((,class (:inherit font-lock-function-name-face :weight bold
                                              :height 1.2))))
   `(markdown-url-face ((,class (:inherit link))))
   `(markdown-link-face ((,class (:foreground ,blue :underline t))))

   ;`(hl-sexp-face ((,class (:background ,current-line))))
   `(highlight-symbol-face ((,class (:background ,selection))))
   `(highlight-80+ ((,class (:background ,current-line))))

   ;; Python-specific overrides
   `(py-builtins-face ((,class (:foreground ,orange :weight normal))))

   ;; ein (emacs-ipython-notebook) specific colors
   `(ein:cell-input-area ((,class (:background ,"#EFEFEF"))))
   `(ein:cell-input-prompt ((,class (:inherit org-block-begin-line))))
   `(ein:cell-output-prompt ((,class (:inherit org-block-end-line))))

   ;; js2-mode
   `(js2-warning ((,class (:underline ,orange))))
   `(js2-error ((,class (:foreground nil :underline ,red))))
   `(js2-external-variable ((,class (:foreground ,purple))))
   `(js2-function-param ((,class (:foreground ,blue))))
   `(js2-instance-member ((,class (:foreground ,blue))))
   `(js2-private-function-call ((,class (:foreground ,red))))

   ;; js3-mode
   `(js3-warning-face ((,class (:underline ,orange))))
   `(js3-error-face ((,class (:foreground nil :underline ,red))))
   `(js3-external-variable-face ((,class (:foreground ,purple))))
   `(js3-function-param-face ((,class (:foreground ,blue))))
   `(js3-jsdoc-tag-face ((,class (:foreground ,orange))))
   `(js3-jsdoc-type-face ((,class (:foreground ,aqua))))
   `(js3-jsdoc-value-face ((,class (:foreground ,yellow))))
   `(js3-jsdoc-html-tag-name-face ((,class (:foreground ,blue))))
   `(js3-jsdoc-html-tag-delimiter-face ((,class (:foreground ,green))))
   `(js3-instance-member-face ((,class (:foreground ,blue))))
   `(js3-private-function-call-face ((,class (:foreground ,red))))

   ;; nxml
   `(nxml-name-face ((,class (:foreground unspecified :inherit font-lock-constant-face))))
   `(nxml-attribute-local-name-face ((,class (:foreground unspecified :inherit font-lock-variable-name-face))))
   `(nxml-ref-face ((,class (:foreground unspecified :inherit font-lock-preprocessor-face))))
   `(nxml-delimiter-face ((,class (:foreground unspecified :inherit font-lock-keyword-face))))
   `(nxml-delimited-data-face ((,class (:foreground unspecified :inherit font-lock-string-face))))
   `(rng-error-face ((,class (:underline ,red))))

   ;; RHTML
   `(erb-delim-face ((,class (:background ,current-line))))
   `(erb-exec-face ((,class (:background ,current-line :weight bold))))
   `(erb-exec-delim-face ((,class (:background ,current-line))))
   `(erb-out-face ((,class (:background ,current-line :weight bold))))
   `(erb-out-delim-face ((,class (:background ,current-line))))
   `(erb-comment-face ((,class (:background ,current-line :weight bold :slant italic))))
   `(erb-comment-delim-face ((,class (:background ,current-line))))

   ;; Message-mode
   `(message-header-other ((,class (:foreground nil :background nil :weight normal))))
   `(message-header-subject ((,class (:inherit message-header-other :weight bold :foreground ,yellow))))
   `(message-header-to ((,class (:inherit message-header-other :weight bold :foreground ,orange))))
   `(message-header-cc ((,class (:inherit message-header-to :foreground nil))))
   `(message-header-name ((,class (:foreground ,blue :background nil))))
   `(message-header-newsgroups ((,class (:foreground ,aqua :background nil :slant normal))))
   `(message-separator ((,class (:foreground ,purple))))

   ;; cfw emacs calendar
   `(cfw:face-title ((,class (:background ,background :foreground ,"#424242" :height 1.3 :weight bold))))
   `(cfw:face-today ((,class (:foreground ,foreground))))
   `(cfw:face-day-title ((,class (:background ,current-line :foreground ,foreground))))
   `(cfw:face-today-title ((,class (:background ,secondary-selection :foreground ,foreground))))
   `(cfw:face-header ((,class (:background ,current-line :foreground ,foreground))))
   `(cfw:face-sunday ((,class (:background ,current-line :foreground ,aqua :weight bold))))
   `(cfw:face-saturday ((,class (:background ,current-line :foreground ,aqua :weight bold))))
   `(cfw:face-select ((,class (:background ,selection :foreground ,foreground))))
   `(cfw:face-toolbar ((,class (:background ,aqua :foreground ,background :weight bold))))
   `(cfw:face-toolbar-button-off ((,class (:background ,aqua :foreground ,background :weight bold))))
   `(cfw:face-toolbar-button-on ((,class (:background ,aqua :foreground ,secondary-selection :weight bold))))
   `(cfw:face-holiday ((,class (:background ,current-line :foreground ,green :weight bold))))

   ;; Outline
   `(outline-1 ((,class (:inherit nil :foreground ,blue))))
   `(outline-2 ((,class (:inherit nil :foreground ,yellow))))
   `(outline-3 ((,class (:inherit nil :foreground ,purple))))
   `(outline-4 ((,class (:inherit nil :foreground ,orange))))
   `(outline-5 ((,class (:inherit nil :foreground ,aqua))))
   `(outline-6 ((,class (:inherit nil :foreground ,green))))
   `(outline-7 ((,class (:inherit nil :foreground ,faded-blue))))
   `(outline-8 ((,class (:inherit nil :foreground ,faded-yellow))))
   `(outline-9 ((,class (:inherit nil :foreground ,faded-purple))))


   ;; Ledger-mode
   `(ledger-font-comment-face ((,class (:inherit font-lock-comment-face))))
   `(ledger-font-occur-narrowed-face ((,class (:inherit font-lock-comment-face :invisible t))))
   `(ledger-font-occur-xact-face ((,class (:inherit highlight))))
   `(ledger-font-payee-cleared-face ((,class (:foreground ,green))))
   `(ledger-font-payee-uncleared-face ((,class (:foreground ,aqua))))
   `(ledger-font-posting-account-cleared-face ((,class (:foreground ,blue))))
   `(ledger-font-posting-account-face ((,class (:foreground ,purple))))
   `(ledger-font-posting-account-pending-face ((,class (:foreground ,yellow))))
   `(ledger-font-xact-highlight-face ((,class (:inherit highlight))))
   `(ledger-occur-narrowed-face ((,class (:inherit font-lock-comment-face :invisible t))))
   `(ledger-occur-xact-face ((,class (:inherit highlight))))

   ;; auctex
   `(font-latex-bold-face                 ((t (:inherit bold :foreground ,foreground))))
   `(font-latex-doctex-documentation-face ((t (:background unspecified))))
   `(font-latex-doctex-preprocessor-face ((t (:inherit (font-latex-doctex-documentation-face
                                                        font-lock-builtin-face font-lock-preprocessor-face)))))
   `(font-latex-italic-face               ((t (:inherit italic :foreground ,foreground))))
   `(font-latex-math-face                 ((t (:foreground ,blue))))
   `(font-latex-sectioning-0-face         ((t (:inherit outline-1 :height 1.4))))
   `(font-latex-sectioning-1-face         ((t (:inherit outline-2 :height 1.35))))
   `(font-latex-sectioning-2-face         ((t (:inherit outline-3 :height 1.3))))
   `(font-latex-sectioning-3-face         ((t (:inherit outline-4 :height 1.25))))
   `(font-latex-sectioning-4-face         ((t (:inherit outline-5 :height 1.2))))
   `(font-latex-sectioning-5-face         ((t (:inherit outline-6 :height 1.1))))
   `(font-latex-sedate-face               ((t (:foreground ,green))))
   `(font-latex-slide-title-face          ((t (:inherit font-lock-type-face :weight bold :height 1.2))))
   `(font-latex-string-face               ((t (:inherit font-lock-string-face))))
   `(font-latex-subscript-face            ((t (:height 0.8))))
   `(font-latex-superscript-face          ((t (:height 0.8))))
   `(font-latex-warning-face              ((t (:inherit font-lock-warning-face))))

   ;; mu4e
   `(mu4e-header-face ((,class (:foreground ,subtle :inherit nil))))
   `(mu4e-header-highlight-face ((,class (:background ,current-line :underline nil :inherit region))))
   `(mu4e-header-marks-face ((,class (:underline nil :foreground ,yellow))))
   `(mu4e-flagged-face ((,class (:foreground ,orange :inherit nil))))
   `(mu4e-forwarded-face ((,class (:foreground ,aqua :inherit nil))))
   `(mu4e-replied-face ((,class (:foreground ,green :inherit nil))))
   `(mu4e-unread-face ((,class (:foreground ,foreground :inherit nil))))
   `(mu4e-cited-1-face ((,class (:inherit outline-1 :slant normal))))
   `(mu4e-cited-2-face ((,class (:inherit outline-2 :slant normal))))
   `(mu4e-cited-3-face ((,class (:inherit outline-3 :slant normal))))
   `(mu4e-cited-4-face ((,class (:inherit outline-4 :slant normal))))
   `(mu4e-cited-5-face ((,class (:inherit outline-5 :slant normal))))
   `(mu4e-cited-6-face ((,class (:inherit outline-6 :slant normal))))
   `(mu4e-cited-7-face ((,class (:inherit outline-7 :slant normal))))
   `(mu4e-ok-face ((,class (:foreground ,green))))
   `(mu4e-view-contact-face ((,class (:inherit nil :foreground ,yellow))))
   `(mu4e-view-link-face ((,class (:inherit link :foreground ,blue))))
   `(mu4e-view-url-number-face ((,class (:inherit nil :foreground ,aqua))))
   `(mu4e-view-attach-number-face ((,class (:inherit nil :foreground ,orange))))
   `(mu4e-highlight-face ((,class (:inherit highlight))))
   `(mu4e-title-face ((,class (:inherit nil :foreground ,green))))

   ;; emms
   `(emms-playlist-selected-face ((,class (:foreground ,orange))))
   `(emms-playlist-track-face ((,class (:foreground ,blue))))
   `(emms-browser-track-face ((,class (:foreground ,blue))))
   `(emms-browser-artist-face ((,class (:foreground ,red :height 1.3))))
   `(emms-browser-composer-face ((,class (:inherit emms-browser-artist-face))))
   `(emms-browser-performer-face ((,class (:inherit emms-browser-artist-face))))
   `(emms-browser-album-face ((,class (:foreground ,green :height 1.2))))

   ;; erc
   `(erc-direct-msg-face ((,class (:foreground ,orange))))
   `(erc-error-face ((,class (:foreground ,red))))
   `(erc-header-face ((,class (:foreground ,foreground :background ,selection))))
   `(erc-input-face ((,class (:foreground ,green))))
   `(erc-keyword-face ((,class (:foreground ,yellow))))
   `(erc-current-nick-face ((,class (:foreground ,green))))
   `(erc-my-nick-face ((,class (:foreground ,green))))
   `(erc-nick-default-face ((,class (:weight normal :foreground ,purple))))
   `(erc-nick-msg-face ((,class (:weight normal :foreground ,yellow))))
   `(erc-notice-face ((,class (:foreground ,comment))))
   `(erc-pal-face ((,class (:foreground ,orange))))
   `(erc-prompt-face ((,class (:foreground ,blue))))
   `(erc-timestamp-face ((,class (:foreground ,aqua))))
   `(erc-keyword-face ((,class (:foreground ,green))))

   `(custom-variable-tag ((,class (:foreground ,blue))))
   `(custom-group-tag ((,class (:foreground ,blue))))
   `(custom-state ((,class (:foreground ,green))))

   ;; ansi-term
   `(term ((,class (:foreground nil :background nil :inherit default))))
   `(term-color-black   ((,class (:foreground ,foreground :background ,foreground))))
   `(term-color-red     ((,class (:foreground ,red :background ,red))))
   `(term-color-green   ((,class (:foreground ,green :background ,green))))
   `(term-color-yellow  ((,class (:foreground ,yellow :background ,yellow))))
   `(term-color-blue    ((,class (:foreground ,blue :background ,blue))))
   `(term-color-magenta ((,class (:foreground ,purple :background ,purple))))
   `(term-color-cyan    ((,class (:foreground ,aqua :background ,aqua))))
   `(term-color-white   ((,class (:foreground ,background :background ,background))))

   ;; elfeed
   `(elfeed-log-date-face ((,class (:foreground ,aqua))))
   `(elfeed-log-error-level-face ((,class (:foreground ,red))))
   `(elfeed-log-info-level-face ((,class (:foreground ,blue))))
   `(elfeed-log-warn-level-face ((,class (:foreground ,orange))))
   `(elfeed-search-date-face ((,class (:foreground ,purple))))
   `(elfeed-search-feed-face ((,class (:foreground ,yellow))))
   `(elfeed-search-tag-face ((,class (:foreground ,green))))

   ;; rpm-spec-mode
   `(rpm-spec-dir-face ((,class (:foreground ,green))))
   `(rpm-spec-doc-face ((,class (:foreground ,green))))
   `(rpm-spec-ghost-face ((,class (:foreground ,red))))
   `(rpm-spec-macro-face ((,class (:foreground ,yellow))))
   `(rpm-spec-obsolete-tag-face ((,class (:foreground ,red))))
   `(rpm-spec-package-face ((,class (:foreground ,red))))
   `(rpm-spec-section-face ((,class (:foreground ,yellow))))
   `(rpm-spec-tag-face ((,class (:foreground ,blue))))
   `(rpm-spec-var-face ((,class (:foreground ,red)))))

  (custom-theme-set-variables
   'light
   `(fci-rule-color ,current-line)
   `(vc-annotate-color-map
     '((20  . ,red)
       (40  . ,orange)
       (60  . ,yellow)
       (80  . ,green)
       (100 . ,aqua)
       (120 . ,blue)
       (140 . ,purple)
       (160 . ,red)
       (180 . ,orange)
       (200 . ,yellow)
       (220 . ,green)
       (240 . ,aqua)
       (260 . ,blue)
       (280 . ,purple)
       (300 . ,red)
       (320 . ,orange)
       (340 . ,yellow)
       (360 . ,green)))
   `(vc-annotate-very-old-color nil)
   `(vc-annotate-background nil)

   ;; highlight-sexp-mode
   `(hl-sexp-background-color ,current-line)

   `(ansi-color-names-vector (vector ,foreground ,red ,green ,yellow ,blue ,purple ,aqua ,background))
   '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])))
(provide-theme 'light)
;;; light-theme.el ends here
