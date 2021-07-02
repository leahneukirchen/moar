;;; moar.el --- a very simple single-file hypertext system

;; Author: Leah Neukirchen <leah@vuxu.org>
;; To the extent possible under law, the creator of this work has waived
;; all copyright and related or neighboring rights to this work.
;; https://creativecommons.org/publicdomain/zero/1.0/

(require 'seq)
(require 'pcase)
(require 'cl-extra)
(eval-when-compile (require 'subr-x))

(defface moar-title-face
  '((t (:bold t :underline t)))
  "Face to use for Moar titles."
  :group 'font-lock-highlighting-faces)

(defface moar-link-face
  '((t))
  "Face to use for Moar links."
  :group 'font-lock-highlighting-faces)

(defface moar-page-break-face
  '((t (:foreground "grey50")))
  "Face to use for Moar page breaks."
  :group 'font-lock-highlighting-faces)

(defvar moar-mode-abbrev-table nil)
(define-abbrev-table 'moar-mode-abbrev-table
  '(("-today" "" moar-insert-current-iso-date)
    ("-tomorrow" "" moar-insert-tomorrow-iso-date)
    ("-tmrw" "" moar-insert-tomorrow-iso-date)
    ("-time" "" moar-insert-current-time)
    ("-dottime" "" moar-insert-current-dottime)))

(defun moar-insert-current-iso-date ()
  (insert (format-time-string "%Y-%m-%d")))

(defun moar-insert-tomorrow-iso-date ()
  (insert (format-time-string "%Y-%m-%d"
                              (time-add (current-time)
                                        (* 24 3600)))))

(defun moar-insert-current-time ()
  (insert (format-time-string "%H:%M")))

(defun moar-insert-current-dottime ()
  (insert (format-time-string "%Y-%m%-dT%H\u00B7%M" (current-time) t))
  (insert (string-remove-suffix "00" (format-time-string "%z"))))

(defvar-local moar-history nil)

(defun moar-add-page (title)
  (goto-char (point-max))
  (unless (= (preceding-char) ?\n)
    (insert "\n"))
  (insert "\C-l\n" text "\n\n"))

(defun moar-visit-link (text)
  (if (string-match "#" text)
      (progn
        (let ((file (substring text 0 (match-beginning 0)))
              (target (substring text (match-end 0))))
          (find-file file)
          (when (equal major-mode 'fundamental-mode)
            (moar-mode))
          (moar-visit-link target)))
    (let ((old-point (point))
          (narrowed (buffer-narrowed-p)))
      (widen)
      (goto-char (point-min))
      (if (re-search-forward (concat "\C-l\n[^\n]*" (regexp-quote text))
                             nil t)
          (move-beginning-of-line 1)
        (goto-char old-point)
        (when (y-or-n-p (concat "Link `" text "' not found, create?"))
          (moar-add-page text)))
      (when narrowed
        (narrow-to-page)))))

(defun moar-visit-today ()
  (interactive)
  (moar-visit-link (format-time-string "%Y-%m-%d" (current-time))))

(defun moar-visit-yesterday ()
  (interactive)
  (moar-visit-link (format-time-string "%Y-%m-%d"
                                       (time-subtract (current-time)
                                                      (* 24 3600)))))

(defun moar-go-back ()
  (interactive)
  (if (null moar-history)
      (progn
        (widen)
        (goto-char (point-min)))
    (let ((n (pop moar-history)))
      (widen)
      (goto-char n))))

(defun moar-link-at-point ()
  (let (link-start 
        link-end)
    (if (or (and (looking-back "\\[.*?")
                 (setq link-start (1+ (match-beginning 0)))
                 (looking-at ".*?\\]")
                 (setq link-end (1- (match-end 0)))
                 (< link-start link-end))   ; don't match empty links
            (and (looking-back "#[[:word:]._-]*?")
                 (setq link-start (1+ (match-beginning 0)))
                 (looking-at "[[:word:]._-]*")
                 (setq link-end (match-end 0))
                 (< link-start link-end)))   ; don't match empty links
        (buffer-substring-no-properties link-start link-end))))

(defun moar-follow-link-or-newline ()
  (interactive)
  (if-let (link (moar-link-at-point))
      (progn
        (push (point) moar-history)
        (if (string-prefix-p "<" link)
            (moar-visit-backlink-interactive (substring link 1))
          (moar-visit-link link)))
    (newline)))

(defun moar-follow-link-from-mouse (e)
  (interactive "e")
  (mouse-set-point e)
  (when-let (link (moar-link-at-point))
    (push (point) moar-history)
    (if (string-prefix-p "<" link)
        (moar-visit-backlink-interactive (substring link 1))
      (moar-visit-link link))))

(defun moar-all-links ()
  (let ((matches))
    (save-match-data
      (save-excursion
        (save-restriction
          (widen)
          (goto-char 1)
          (while (search-forward-regexp "^\C-l\n.*" nil t 1)
            (push (substring (match-string 0) 2) matches)))))
    matches))

(defun moar-all-links-and-titles ()
  (let (matches
        current-title)
    (save-match-data
      (save-excursion
        (save-restriction
          (widen)
          (goto-char 1)
          (while (search-forward-regexp "^\C-l\n\\(.*\\)\\|\\[\\(.+?\\)\\]\\|#\\([[:word:]._-]+\\)"
                                        nil t 1)
            (if (match-string 1)
                (setq current-title (match-string-no-properties 1))
              (if (match-string 2)
                  (push (cons current-title (match-string-no-properties 2)) matches)
                (push (cons current-title (match-string-no-properties 3)) matches))
              )))))
    matches))

(defun moar-current-title ()
  (save-match-data 
    (save-excursion
      (save-restriction
        (widen)
        (end-of-line 1)
        (if (search-backward-regexp "^\C-l\n\\(.*\\)" nil t)
            (match-string-no-properties 1)
          nil)))))

(defun moar-insert-link ()
  (interactive)
  (let* ((links (moar-all-links))
         (read (if (fboundp 'ivy-read)
                   #'ivy-read
                 #'completing-read))
         (target (funcall read "Link target? " links)))
    (when target
      (insert "[" target "]"))))

(defun moar-visit-link-interactive ()
  (interactive)
  (let* ((links (moar-all-links))
         (read (if (fboundp 'ivy-read)
                   #'ivy-read
                 #'completing-read))
         (target (funcall read "Go to: " links)))
    (when target
      (moar-visit-link target))))

(defun moar-visit-backlink-interactive (&optional title)
  (interactive)
  (let* ((links-titles (moar-all-links-and-titles))
         (current-title (or title (moar-current-title)))
         (read (if (fboundp 'ivy-read)
                   #'ivy-read
                 #'completing-read))
         (links (mapcar #'car
                        (seq-filter #'(lambda (x)
                                        (cl-equalp (cdr x) current-title))
                                    links-titles)))
         (target (funcall read "Go to backlink: " links)))
    (when target
      (moar-visit-link target)
      (when (re-search-forward (concat "\\[" (regexp-quote current-title) "\\]"
                                       "\\|#" (regexp-quote current-title))
                               nil t)
        (goto-char (1+ (match-beginning 0))))
      )))

(define-derived-mode moar-mode
  text-mode "Moar"
  "Major mode for Moar hypertext files."
  (setq local-abbrev-table moar-mode-abbrev-table)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((("^\\(\C-l\\)\n\\(.*\\)"
            (1 'moar-page-break-face)
            (2 'moar-title-face))
           ("\\[\\(.+?\\)\\]" 
            (1 'moar-link-face))
           )
          t nil))
  (add-to-list 'font-lock-extra-managed-props 'display)
  (font-lock-add-keywords nil
                          '(("^\\(\C-l\\)\n" 1 '(face nil display "∇"))))
  (set (make-local-variable 'font-lock-multiline) t)
  (font-lock-mode 1)
  (abbrev-mode 1)
  (when (fboundp 'orgalist-mode)
    (orgalist-mode 1)))

(modify-syntax-entry ?- "w" moar-mode-syntax-table)

(define-key moar-mode-map (kbd "RET") 'moar-follow-link-or-newline)
(define-key moar-mode-map [mouse-1] 'moar-follow-link-from-mouse)

(define-key moar-mode-map (kbd "C-c [") 'moar-insert-link)
(define-key moar-mode-map (kbd "C-c C-v") 'moar-visit-link-interactive)
(define-key moar-mode-map (kbd "C-c C-b") 'moar-visit-backlink-interactive)
(define-key moar-mode-map (kbd "C-c C-t") 'moar-visit-today)
(define-key moar-mode-map (kbd "C-c C-y") 'moar-visit-yesterday)
(define-key moar-mode-map (kbd "C-c C-n") 'narrow-to-page)
(define-key moar-mode-map (kbd "C-c C-w") 'widen)
(define-key moar-mode-map (kbd "C-c C-o") 'moar-go-back)

(provide 'moar)

;;; moar.el ends here
