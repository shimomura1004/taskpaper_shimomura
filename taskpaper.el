;;; taskpaper.el --- Taskpaper implementation for Emacs

;; Copyright (C) 2008 Kentaro Kuribayashi

;; Author: kentaro <kentarok@gmail.com>
;; Keywords: tools, task

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; アウトラインモードを使って todo を折りたたみ
(add-hook 'taskpaper-mode-hook 'my-taskpaper-hook)
(defun taskpaper-outline-level ()
  (save-excursion
    (skip-chars-forward "- ")
    (current-column)))
(defun my-taskpaper-hook ()
  (setq outline-regexp "^\\(- \\)*.+\\(:\\)$")
  (setq outline-level 'taskpaper-outline-level)
  (setq outline-minor-mode-prefix "\C-c\C-o")
  (outline-minor-mode t))

;; 指定された正規表現にマッチするタグを探し、
;; マッチした文字列が条件propを満たしたらアウトラインを開く
(defun taskpaper-open-line (regexp prop)
  (save-excursion
    (hide-body)
    (beginning-of-buffer)
    (while (re-search-forward regexp nil t)
      (when (apply prop nil) (show-branches) (show-entry)))))

;; "2011-07-30" みたいな文字列を 20110730 という数値に変換する
(defun parse-date (date-string)
  (apply
   (lambda (year mon day) (+ (* year 10000) (* mon 100) day))
   (mapcar (lambda (str) (string-to-number str)) (split-string date-string "-"))))

;; 簡単な日付の計算を行う
(defun calc-date (offset &optional date)
  (apply
   (lambda (seconds minutes hour day month year dow dst zone)
     (encode-time seconds minutes hour (+ day offset) month year dow dst zone))
   (if date (decode-time date) (decode-time (current-time)))))

;; today などの日常語で指定された日付を変換
(defun parse-natural-date (date-string)
  (cond ((string-equal "today" date-string)
         (format-time-string "%Y-%m-%d" (calc-date 0)))
        ((string-equal "tomorrow" date-string)
         (format-time-string "%Y-%m-%d" (calc-date 1)))
        ((string-match "+\\([0-9]*\\)" date-string)
         (format-time-string "%Y-%m-%d"
                             (calc-date (string-to-number (match-string 1 date-string)))))
        (t date-string)))

;;; Code:

;; Hook
(defvar taskpaper-mode-hook nil
  "*Hooks for Taskpaper major mode")

;; Keymap
(defvar taskpaper-mode-map (make-keymap)
  "*Keymap for Taskpaper major mode")

(define-key taskpaper-mode-map "\C-c\C-p" 'taskpaper-create-new-project)
(define-key taskpaper-mode-map "\C-c\C-t" 'taskpaper-create-new-task)
(define-key taskpaper-mode-map "\C-c\C-d" 'taskpaper-toggle-task)
;; (define-key taskpaper-mode-map "-"        'taskpaper-electric-mark)
(define-key taskpaper-mode-map "\C-cp" 'taskpaper-occur-project)
(define-key taskpaper-mode-map "\C-ca" 'taskpaper-occur-at-tag)
(define-key taskpaper-mode-map "\C-cdd" 'taskpaper-done-line-del-region)
(define-key taskpaper-mode-map "\C-c\C-f" 'taskpaper-open-at-tag)
(define-key taskpaper-mode-map "\C-c<" 'taskpaper-open-before-date)

;; Face
(defface taskpaper-project-face
  '((((class color) (background light))
     (:foreground "SteelBlue" :underline "SteelBlue" :weight bold))
    (((class color) (background dark))
     (:foreground "SteelBlue" :underline "SteelBlue" :weight bold)))
  "Face definition for project name")

(defface taskpaper-task-marked-as-done-face
  '((((class color) (background light))
     (:foreground "gray75" :weight light :strike-through t))
    (((class color) (background dark))
     (:foreground "gray25" :weight light :strike-through t)))
  "Face definition for task marked as done")

(defface taskpaper-task-mark-face
  '((((class color) (background light))
     (:foreground "SteelBlue"))
    (((class color) (background dark))
     (:foreground "SteelBlue")))
  "Face definition for task mark")

(defface taskpaper-at-tag-face
  '((((class color) (background light))
     (:foreground "gray75"))
    (((class color) (background dark))
     (:foreground "gray55")))
  "Face definition for tag")

(defface taskpaper-comment-face
  '((((class color) (background light))
     (:foreground "orange"))
    (((class color) (background dark))
     (:foreground "orange")))
  "Face definition for comment mark")

(defvar taskpaper-project-face 'taskpaper-project-face)
(defvar taskpaper-task-marked-as-done-face 'taskpaper-task-marked-as-done-face)
(defvar taskpaper-task-mark-face 'taskpaper-task-mark-face)
(defvar taskpaper-at-tag-face 'taskpaper-at-tag-face)
(defvar taskpaper-comment-face 'taskpaper-comment-face)
(defvar taskpaper-font-lock-keywords
  '(("^.+:[ \t]*$" 0 taskpaper-project-face)
    ("^ ?*-[ \t]*\\(.*\\)\\( @done([0-9]+-[0-9]+-[0-9]+)\\)$"
     (1 taskpaper-task-marked-as-done-face)
     (2 taskpaper-at-tag-face t))
    ("^ ?*-[ \t]*\\(.*\\)\\( @done\\)$"
     (1 taskpaper-task-marked-as-done-face)
     (2 taskpaper-at-tag-face t))
    ("^ ?*-" (0 taskpaper-task-mark-face t))
    ("\\( @\\w+\\)" (1 taskpaper-at-tag-face t))
    ("^ ?*[^- \n].*[^:\n]$\\|^ ?*[^- \n]$" (0 taskpaper-comment-face t))
    ))

;; Taskpaper major mode
(define-derived-mode taskpaper-mode fundamental-mode "Taskpaper"
  "Major mode to manage tasks easily"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'taskpaper-mode)
  (setq mode-name "Taskpaper")
  (use-local-map taskpaper-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(taskpaper-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'taskpaper-indent-line)
  (run-hooks 'taskpaper-mode-hook))

(add-to-list 'auto-mode-alist (cons "\\.taskpaper$" 'taskpaper-mode))

;; Commands
(defun taskpaper-create-new-project (name)
  "Creates new project"
  (interactive "sProject Name: ")
  (insert (concat name ":\n\n")))

(defun taskpaper-create-new-task ()
  "Creates new task"
  (interactive)
  (let ((task (read-from-minibuffer "New Task: "))
        (due (read-from-minibuffer "Due: ")))
    (insert (concat "- " task))
    (when (not (string-equal "" due))
      (insert (concat " @due(" (parse-natural-date due) ")")))
    (newline-and-indent)))

(defun taskpaper-toggle-task (beg end)
  "Marks task as done"
  (interactive (if mark-active
                   (list (region-beginning) (region-end))
                 (list (point) (point))))
  (let ((end-mark nil))
    (save-excursion
      (goto-char end)
      (end-of-line)
      (setq end-mark (point-marker))
      (goto-char beg)
      (beginning-of-line)
      (while (< (point) end-mark)
        (when (looking-at "-")
          (if (or (looking-at ".*\\( @done\\)$")
                  (looking-at ".*\\( @done([0-9]+-[0-9]+-[0-9]+)\\)$"))
              (delete-region (match-beginning 1) (match-end 1))
            (progn (end-of-line)
                   (insert
                    (concat " @done(" (format-time-string "%Y-%m-%d") ")")))))
        (forward-line)
        ))))

(defun taskpaper-indent-line ()
  "Detects if list mark is needed when indented"
  (interactive)
  (let ((mark-flag nil))
    (save-excursion
      (forward-line -1)
      (when (looking-at "-") (setq mark-flag t)))
    (when mark-flag (insert "- "))))

(defun taskpaper-electric-mark (arg)
  "Inserts a list mark"
  (interactive "*p")
  (if (zerop (current-column))
    (progn
      (self-insert-command arg)
      (insert " "))
    (self-insert-command arg)))

(defun taskpaper-occur-project ()
  (interactive)
  (save-excursion
    (occur "^.*:$")
      ))

(defun taskpaper-occur-at-tag ()
  (interactive)
  (save-excursion
    (if (thing-at-point 'word)
        (if (looking-back "@\\w+")
            (occur (concat "@" (thing-at-point 'word)))
          ))))

(defun taskpaper-done-line-del-region (beg end)
  (interactive (if mark-active
                   (list (region-beginning) (region-end))
                 (list (point) (point))))
  (let ((end-mark nil) (end-line-point nil) (beg-line-point nil))
    (save-excursion
      (goto-char end)
      (end-of-line)
      (setq end-mark (point-marker))
      (goto-char beg)
      (beginning-of-line)
      (while (< (point) end-mark)
      (end-of-line)
      (setq end-line-point (point))
      (when (looking-back "@done(?.*)?")
        (beginning-of-line)
        (setq beg-line-point (point))
        (delete-region beg-line-point (+ end-line-point 1))
        (forward-line -1))
      (forward-line)
      ))))

;; タグを検索して、マッチしたものだけを開く
(defun taskpaper-open-at-tag (tagname)
  (interactive "sTAG: ")
  (taskpaper-find-tag tagname))
(defun taskpaper-find-tag (keyword)
  (taskpaper-open-line (concat "@\\w*" keyword "\\w*") (lambda () 1)))

;; 指定された日付よりも締め切りが前のものだけを開く
(defun taskpaper-open-before-date (date-string)
  (interactive "sBEFORE: ")
  (taskpaper-find-tag-with-due (parse-date (parse-natural-date date-string))))
(defun taskpaper-find-tag-with-due (date)
  (taskpaper-open-line
   "@due(\\(.*?\\))"
   (lambda () (let ((date-of-due (parse-date (match-string 1))))
                (message (match-string 1))
                (< date-of-due date)))))

(provide 'taskpaper)
;;; taskpaper.el ends here
