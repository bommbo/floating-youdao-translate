;;; floating-youdao-interactive.el --- Interactive floating Youdao translation -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: bommbo
;; Version: 0.1
;; Package-Requires: (emacs "30.0")
;; Keywords: translation, floating, convenience, youdao

;;; Commentary:

;; Interactive floating translation with Youdao Dictionary.
;; Features:
;; - Floating frame with cursor support
;; - Can interact with translation content
;; - Auto-hide after timeout
;; - Integration with youdao-translate

;;; Code:

(require 'youdao-translate)
(require 'cl-lib)

(defgroup floating-youdao-interactive nil
  "Interactive floating Youdao translation."
  :group 'applications
  :prefix "floating-youdao-interactive-")

(defcustom floating-youdao-interactive-width 50
  "Width of translation window."
  :type 'integer
  :group 'floating-youdao-interactive)

(defcustom floating-youdao-interactive-height 10
  "Height (lines) of translation window."
  :type 'integer
  :group 'floating-youdao-interactive)

;;; Internal Variables

(defvar floating-youdao-interactive--frame nil
  "Current interactive translation frame.")

(defvar floating-youdao-interactive--buffer nil
  "Buffer for interactive translation display.")

(defvar floating-youdao-interactive--timer nil
  "Auto-hide timer.")

;;; Major Mode

(defvar floating-youdao-interactive-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'floating-youdao-interactive-hide)
    (define-key map (kbd "C-c C-k") #'floating-youdao-interactive-hide)
    (define-key map (kbd "C-c C-c") #'floating-youdao-interactive-hide)
    map)
  "Keymap for floating-youdao-interactive-mode.")

(define-derived-mode floating-youdao-interactive-mode special-mode
  "YoudaoFloat"
  "Mode for interactive floating translation."
  (setq truncate-lines nil)
  (setq word-wrap t)
  (setq mode-line-format nil)
  (setq header-line-format nil)
  (setq-local display-line-numbers nil)
  (display-line-numbers-mode -1))

;;; Frame Functions

(defun floating-youdao-interactive--make-frame ()
  "Create child frame for interactive translation, aligned with floating-youdao."
  (let* ((buf floating-youdao-interactive--buffer)
         (parent (selected-frame))
         (width (if (boundp 'floating-youdao-width) floating-youdao-width 50))
         (height (if (boundp 'floating-youdao-visible-lines) floating-youdao-visible-lines 8))
         (border (if (boundp 'floating-youdao-border-width) floating-youdao-border-width 2))
         (params
          `((name . "youdao-interactive")
            (parent-frame . ,parent)
            (minibuffer . ,(minibuffer-window parent))
            (no-accept-focus . nil)
            (no-focus-on-map . nil)
            (cursor-type . bar)
            (undecorated . t)
            (border-width . 0)
            (internal-border-width . ,border)
            (child-frame-border-width . ,border)
            (width . ,width)
            (height . ,height)
            (vertical-scroll-bars . nil)
            (horizontal-scroll-bars . nil)
            (no-other-frame . t)
            (desktop-dont-save . t))))
    (setq floating-youdao-interactive--frame
          (make-frame params))
    (set-window-buffer (frame-root-window floating-youdao-interactive--frame) buf)
    floating-youdao-interactive--frame))

(defun floating-youdao-interactive--position-frame ()
  "Position frame exactly like floating-youdao."
  (let* ((window (selected-window))
         (pos (window-absolute-pixel-position))
         (cw (frame-char-width))
         (ch (frame-char-height))
         (width (if (boundp 'floating-youdao-width) floating-youdao-width 50))
         (height (if (boundp 'floating-youdao-visible-lines) floating-youdao-visible-lines 8))
         (x (+ (car pos) (* 2 cw)))
         (y (cdr pos))
         (fw (* width cw))
         (fh (* height ch))
         (screen-w (if (display-graphic-p) (x-display-pixel-width) (* (frame-width) cw)))
         (screen-h (if (display-graphic-p) (x-display-pixel-height) (* (frame-height) ch))))
    (when (> (+ x fw) screen-w) (setq x (- screen-w fw)))
    (when (> (+ y fh) screen-h) (setq y (- screen-h fh)))
    (if (display-graphic-p)
        (set-frame-position floating-youdao-interactive--frame x y)
      (set-frame-position floating-youdao-interactive--frame (/ x cw) (/ y ch)))))

;;; Core Functions

(defun floating-youdao-interactive--show (text lines)
  "Show TEXT translation LINES in interactive floating frame."
  (floating-youdao-interactive-hide)
  
  (setq floating-youdao-interactive--buffer
        (get-buffer-create "*youdao-float*"))
  
  (with-current-buffer floating-youdao-interactive--buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (line lines)
        (insert line)
        (insert "\n")))
    (goto-char (point-min))
    (floating-youdao-interactive-mode))
  
  (floating-youdao-interactive--make-frame)
  (floating-youdao-interactive--position-frame)
  (make-frame-visible floating-youdao-interactive--frame)
  (select-frame-set-input-focus floating-youdao-interactive--frame))

(defun floating-youdao-interactive--translate (text)
  "Translate TEXT and show in interactive frame."
  (message "Translating '%s'..." text)
  (youdao-translate--request-with-lines
   text
   (lambda (lines)
     (floating-youdao-interactive--show text lines))))

;;; Interactive Commands

;;;###autoload
(defun floating-youdao-interactive-hide ()
  "Hide interactive translation frame."
  (interactive)
  (when floating-youdao-interactive--timer
    (cancel-timer floating-youdao-interactive--timer))
  (setq floating-youdao-interactive--timer nil)
  
  (when (frame-live-p floating-youdao-interactive--frame)
    (delete-frame floating-youdao-interactive--frame))
  (setq floating-youdao-interactive--frame nil))

;;;###autoload
(defun floating-youdao-interactive-at-point ()
  "Show interactive Youdao translation for text at point."
  (interactive)
  (let ((text (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'word t))))
    (if text
        (floating-youdao-interactive--translate text)
      (message "No text at point"))))

;;;###autoload
(defun floating-youdao-interactive-input (text)
  "Translate TEXT from user input in interactive frame."
  (interactive "sTranslate: ")
  (when (string-empty-p (string-trim text))
    (user-error "Empty input"))
  (floating-youdao-interactive--translate text))

;;; Minor Mode

(defvar floating-youdao-interactive-global-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c y i") #'floating-youdao-interactive-at-point)
    (define-key map (kbd "C-c y I") #'floating-youdao-interactive-input)
    map)
  "Keymap for Floating Youdao Interactive global mode.")

;;;###autoload
(define-minor-mode floating-youdao-interactive-mode-global
  "Minor mode for interactive floating Youdao translation."
  :global t
  :lighter " 📖i"
  :keymap floating-youdao-interactive-global-mode-map
  (if floating-youdao-interactive-mode-global
      (message "Floating Youdao Interactive enabled")
    (floating-youdao-interactive-hide)
    (message "Floating Youdao Interactive disabled")))

(provide 'floating-youdao-interactive)
;;; floating-youdao-interactive.el ends here
