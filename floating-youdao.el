;;; floating-youdao.el --- Floating Youdao translation -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: bommbo
;; Version: 0.1
;; Package-Requires: (emacs "30.0")
;; Keywords: translation, floating, convenience, youdao

;;; Commentary:

;; Floating translation with Youdao Dictionary.
;; Features:
;; - Floating frame display with scrolling support
;; - Frame reuse (like corfu) instead of recreation
;; - Integration with youdao-translate
;; - Auto-hide and keyboard navigation
;; - Press 'i' to switch to interactive mode (requires floating-youdao-interactive)

;;; Code:

(require 'buframe)
(require 'youdao-translate)
(require 'cl-lib)
(require 'floating-youdao-interactive nil t)

(defgroup floating-youdao nil
  "Floating Youdao translation with scrolling."
  :group 'applications
  :prefix "floating-youdao-")

(defcustom floating-youdao-frame-name "youdao-trans"
  "Name for the floating translation frame."
  :type 'string
  :group 'floating-youdao)

(defcustom floating-youdao-width 50
  "Fixed width of the translation frame in characters."
  :type 'integer
  :group 'floating-youdao)

(defcustom floating-youdao-visible-lines 8
  "Number of visible lines in the translation frame."
  :type 'integer
  :group 'floating-youdao)

(defcustom floating-youdao-border-width 2
  "Width of the frame border."
  :type 'integer
  :group 'floating-youdao)

(defcustom floating-youdao-bar-width 0.3
  "Width of the scrollbar in units of character width."
  :type 'float
  :group 'floating-youdao)

(defcustom floating-youdao-auto-hide-delay 60
  "Seconds before auto-hiding. Set to nil to disable."
  :type '(choice (const :tag "Disable" nil)
                 (number :tag "Seconds"))
  :group 'floating-youdao)

;;; Faces

(defface floating-youdao-default
  '((((class color) (min-colors 88) (background dark)) 
     :background "#1e1e1e" :foreground "#d4d4d4")
    (((class color) (min-colors 88) (background light)) 
     :background "#f5f5f5" :foreground "#333333")
    (t :background "gray20" :foreground "white"))
  "Default face for translation frame."
  :group 'floating-youdao)

(defface floating-youdao-border
  '((((class color) (min-colors 88) (background dark)) 
     :background "#4a9eff")
    (((class color) (min-colors 88) (background light)) 
     :background "#0078d4")
    (t :background "blue"))
  "Face for frame border."
  :group 'floating-youdao)

(defface floating-youdao-bar
  '((((class color) (min-colors 88) (background dark)) 
     :background "#606060")
    (((class color) (min-colors 88) (background light)) 
     :background "#a0a0a0")
    (t :background "gray"))
  "Face for scrollbar."
  :group 'floating-youdao)

;;; Internal Variables

(defvar floating-youdao--current-frame nil
  "Current translation frame (reused across calls).")

(defvar floating-youdao--buffer nil
  "Buffer for translation display (reused).")

(defvar floating-youdao--hide-timer nil
  "Auto-hide timer.")

(defvar floating-youdao--parent-buffer nil
  "Buffer where translation was invoked.")

(defvar floating-youdao--scroll-pos 0
  "Current scroll position in lines.")

(defvar floating-youdao--total-lines 0
  "Total number of lines in translation.")

(defvar floating-youdao--lines nil
  "List of translation lines.")

(defvar floating-youdao--active nil
  "Non-nil when scrollable popup is active.")

(defvar floating-youdao--original-text nil
  "Original text that was translated in the current popup.")

;;; Keymap

(defvar-keymap floating-youdao-scroll-map
  :doc "Keymap active when translation popup is shown."
  "<down>" #'floating-youdao-scroll-down
  "<up>" #'floating-youdao-scroll-up
  "C-n" #'floating-youdao-scroll-down
  "C-p" #'floating-youdao-scroll-up
  "C-v" #'floating-youdao-page-down
  "M-v" #'floating-youdao-page-up
  "<next>" #'floating-youdao-page-down
  "<prior>" #'floating-youdao-page-up
  "<home>" #'floating-youdao-first
  "<end>" #'floating-youdao-last
  "<wheel-up>" #'floating-youdao-scroll-up
  "<wheel-down>" #'floating-youdao-scroll-down
  "i" #'floating-youdao-switch-to-interactive   ; ← NEW: enter interactive mode
  "q" #'floating-youdao-hide
  "C-g" #'floating-youdao-hide)

;;; Scrolling Functions

(defun floating-youdao-scroll-down (&optional n)
  "Scroll down N lines (default 1)."
  (interactive "p")
  (when floating-youdao--active
    (let ((new-pos (+ floating-youdao--scroll-pos (or n 1)))
          (max-scroll (max 0 (- floating-youdao--total-lines 
                                floating-youdao-visible-lines))))
      (setq floating-youdao--scroll-pos 
            (min new-pos max-scroll))
      (floating-youdao--update-display))))

(defun floating-youdao-scroll-up (&optional n)
  "Scroll up N lines (default 1)."
  (interactive "p")
  (when floating-youdao--active
    (setq floating-youdao--scroll-pos 
          (max 0 (- floating-youdao--scroll-pos (or n 1))))
    (floating-youdao--update-display)))

(defun floating-youdao-page-down ()
  "Scroll down one page."
  (interactive)
  (floating-youdao-scroll-down floating-youdao-visible-lines))

(defun floating-youdao-page-up ()
  "Scroll up one page."
  (interactive)
  (floating-youdao-scroll-up floating-youdao-visible-lines))

(defun floating-youdao-first ()
  "Go to first line."
  (interactive)
  (when floating-youdao--active
    (setq floating-youdao--scroll-pos 0)
    (floating-youdao--update-display)))

(defun floating-youdao-last ()
  "Go to last line."
  (interactive)
  (when floating-youdao--active
    (setq floating-youdao--scroll-pos 
          (max 0 (- floating-youdao--total-lines 
                    floating-youdao-visible-lines)))
    (floating-youdao--update-display)))

;;; Display Functions

(defun floating-youdao--wrap-text (text max-width)
  "Wrap TEXT to MAX-WIDTH display width per line."
  (if (<= (string-width text) max-width)
      (list text)
    (let ((lines nil)
          (current-line "")
          (pos 0)
          (len (length text)))
      (while (< pos len)
        (let* ((char (aref text pos))
               (char-str (char-to-string char))
               (current-width (string-width current-line))
               (char-width (string-width char-str))
               (new-width (+ current-width char-width)))
          
          (cond
           ((eq char ?\n)
            (push current-line lines)
            (setq current-line ""))
           
           ((> new-width max-width)
            (when (> (length current-line) 0)
              (push current-line lines))
            (setq current-line char-str))
           
           (t
            (setq current-line (concat current-line char-str))))
          
          (setq pos (1+ pos))))
      
      (when (> (length current-line) 0)
        (push current-line lines))
      
      (nreverse lines))))

(defun floating-youdao--update-display-content (buf)
  "Update content in BUF based on scroll position."
  (when floating-youdao--lines
    (let* ((start floating-youdao--scroll-pos)
           (end (min (+ start floating-youdao-visible-lines)
                     floating-youdao--total-lines))
           (visible-lines (seq-subseq floating-youdao--lines start end)))
      
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          
          (dolist (line visible-lines)
            (insert line)
            (insert "\n"))
          
          (when (> floating-youdao--total-lines 
                   floating-youdao-visible-lines)
            (floating-youdao--draw-scrollbar))
          
          (goto-char (point-min)))))))

(defun floating-youdao--update-display ()
  "Update the translation display based on scroll position."
  (when (and floating-youdao--lines
             floating-youdao--buffer
             (buffer-live-p floating-youdao--buffer))
    (floating-youdao--update-display-content floating-youdao--buffer)))

(defun floating-youdao--draw-scrollbar ()
  "Draw scrollbar indicator in the right margin."
  (let* ((bar-height (max 1 (/ (* floating-youdao-visible-lines 
                                  floating-youdao-visible-lines)
                               floating-youdao--total-lines)))
         (bar-start (/ (* floating-youdao--scroll-pos 
                          floating-youdao-visible-lines)
                       floating-youdao--total-lines))
         (char-width (frame-char-width))
         (bar-width (ceiling (* char-width floating-youdao-bar-width)))
         (bar-string (make-string 1 ?█)))
    
    (goto-char (point-min))
    (dotimes (i floating-youdao-visible-lines)
      (when (< (point) (point-max))
        (let ((is-bar (and (>= i bar-start) (< i (+ bar-start bar-height)))))
          (when is-bar
            (end-of-line)
            (let ((spaces (max 0 (- floating-youdao-width (current-column)))))
              (insert (make-string spaces ?\s))
              (insert (propertize bar-string 
                                  'face 'floating-youdao-bar
                                  'display `(space :width (,bar-width)))))))
        (forward-line 1)))))

(defun floating-youdao--make-buffer ()
  "Create or reuse buffer for translation display."
  (let ((buf-name (format "*youdao-%s*" floating-youdao-frame-name)))
    (or (and floating-youdao--buffer
             (buffer-live-p floating-youdao--buffer)
             floating-youdao--buffer)
        (setq floating-youdao--buffer
              (buframe-make-buffer 
               buf-name
               `((truncate-lines . t)
                 (word-wrap . nil)
                 (left-fringe-width . 0)
                 (right-fringe-width . 0)
                 (left-margin-width . 0)
                 (right-margin-width . 2)
                 (window-min-width . ,floating-youdao-width)
                 (window-min-height . ,floating-youdao-visible-lines)))))))

(defun floating-youdao--get-or-create-frame (buf parent-frame)
  "Get existing frame or create new one for BUF with PARENT-FRAME."
  (let ((frame (and floating-youdao--current-frame
                    (frame-live-p floating-youdao--current-frame)
                    floating-youdao--current-frame)))
    
    (if (and frame (eq (frame-parameter frame 'parent-frame) parent-frame))
        (progn
          (let ((win (frame-root-window frame)))
            (set-window-buffer win buf))
          frame)
      
      (when (and frame (frame-live-p frame))
        (delete-frame frame))
      
      (let* ((frame-params
              `((name . ,floating-youdao-frame-name)
                (parent-frame . ,parent-frame)
                (minibuffer . ,(minibuffer-window parent-frame))
                (width . 0)
                (height . 0)
                (visibility . nil)
                (no-accept-focus . t)
                (no-focus-on-map . t)
                (min-width . t)
                (min-height . t)
                (border-width . 0)
                (internal-border-width . ,floating-youdao-border-width)
                (child-frame-border-width . ,floating-youdao-border-width)
                (left-fringe . 0)
                (right-fringe . 0)
                (vertical-scroll-bars . nil)
                (horizontal-scroll-bars . nil)
                (menu-bar-lines . 0)
                (tool-bar-lines . 0)
                (tab-bar-lines . 0)
                (no-other-frame . t)
                (unsplittable . t)
                (undecorated . t)
                (cursor-type . nil)
                (no-special-glyphs . t)
                (desktop-dont-save . t)))
             (new-frame (make-frame frame-params)))
        
        (let ((win (frame-root-window new-frame)))
          (set-window-buffer win buf)
          (set-window-parameter win 'no-delete-other-windows t)
          (set-window-parameter win 'no-other-window t)
          (set-window-dedicated-p win t))
        
        (set-frame-size new-frame
                        floating-youdao-width
                        floating-youdao-visible-lines
                        nil)
        
        (let ((border-color (face-attribute 'floating-youdao-border 
                                            :background nil 'default)))
          (set-face-background 'internal-border border-color new-frame))
        
        (setq floating-youdao--current-frame new-frame)
        new-frame))))

(defun floating-youdao--position-frame (frame)
  "Position FRAME relative to current point."
  (let* ((window (selected-window))
         (pos (window-absolute-pixel-position))
         (char-width (frame-char-width))
         (char-height (frame-char-height))
         (frame-width-px (* floating-youdao-width char-width))
         (frame-height-px (* floating-youdao-visible-lines char-height)))
    
    (when pos
      (let* ((x (car pos))
             (y (cdr pos))
             (new-x (+ x (* 2 char-width)))
             (new-y y)
             (screen-width (if (display-graphic-p)
                               (x-display-pixel-width)
                             (* (frame-width) char-width)))
             (screen-height (if (display-graphic-p)
                                (x-display-pixel-height)
                              (* (frame-height) char-height))))
        
        (when (> (+ new-x frame-width-px) screen-width)
          (setq new-x (max 0 (- screen-width frame-width-px))))
        
        (when (> (+ new-y frame-height-px) screen-height)
          (setq new-y (max 0 (- screen-height frame-height-px))))
        
        (if (display-graphic-p)
            (cons new-x new-y)
          (cons (/ new-x char-width) (/ new-y char-height)))))))

(defun floating-youdao--show-frame (frame)
  "Show FRAME at appropriate position."
  (when-let ((pos (floating-youdao--position-frame frame)))
    (set-frame-position frame (car pos) (cdr pos))
    (unless (display-graphic-p)
      (redisplay t)))
  (make-frame-visible frame))

;;; Core Functions

(defun floating-youdao--get-text-at-point ()
  "Get text at point or region for translation."
  (cond
   ((use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end)))
   ((thing-at-point 'word)
    (thing-at-point 'word t))
   (t nil)))

(defun floating-youdao--show-translation (text lines)
  "Show TEXT translation LINES in scrollable floating frame."
  (when (minibufferp)
    (floating-youdao-hide)
    (message "Cannot show translation in minibuffer")
    (cl-return-from floating-youdao--show-translation))
  
  (floating-youdao-hide)
  
  (let* ((buf (floating-youdao--make-buffer))
         (parent-buf (current-buffer))
         (main-frame (selected-frame))
         (max-width (- floating-youdao-width 3))
         (wrapped-lines nil))
    
    ;; Wrap long lines
    (dolist (line lines)
      (if (> (string-width line) max-width)
          (setq wrapped-lines (nconc wrapped-lines (floating-youdao--wrap-text line max-width)))
        (setq wrapped-lines (nconc wrapped-lines (list line)))))
    
    ;; Save original text for 'i' switch
    (setq floating-youdao--original-text text)
    
    (setq floating-youdao--parent-buffer parent-buf
          floating-youdao--lines wrapped-lines
          floating-youdao--total-lines (length wrapped-lines)
          floating-youdao--scroll-pos 0
          floating-youdao--active t)
    
    (with-current-buffer buf
      (setq-local face-remapping-alist
                  (copy-tree (default-value 'face-remapping-alist)))
      (cl-pushnew 'floating-youdao-default 
                  (alist-get 'default face-remapping-alist)))
    
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map floating-youdao-scroll-map)
      (setq overriding-terminal-local-map map))
    
    (set-transient-map floating-youdao-scroll-map
                       (lambda () floating-youdao--active)
                       #'floating-youdao--cleanup)
    
    (let ((frame (floating-youdao--get-or-create-frame buf main-frame)))
      (floating-youdao--update-display-content buf)
      (floating-youdao--show-frame frame))
    
    (add-hook 'pre-command-hook #'floating-youdao--pre-command nil t)
    (add-hook 'post-command-hook #'floating-youdao--post-command nil t)
    
    (when (and floating-youdao-auto-hide-delay
               (> floating-youdao-auto-hide-delay 0))
      (setq floating-youdao--hide-timer
            (run-at-time floating-youdao-auto-hide-delay nil
                         #'floating-youdao-hide)))
    
    (message "Translation: %d lines (↑↓ to scroll, i to interact, q to quit)" 
             floating-youdao--total-lines)))

(defun floating-youdao--pre-command ()
  "Pre-command hook to handle popup state."
  (when floating-youdao--active
    (unless (memq this-command 
                  '(floating-youdao-scroll-down
                    floating-youdao-scroll-up
                    floating-youdao-page-down
                    floating-youdao-page-up
                    floating-youdao-first
                    floating-youdao-last
                    floating-youdao-switch-to-interactive
                    floating-youdao-hide
                    floating-youdao-at-point
                    floating-youdao-toggle))
      (setq floating-youdao--active nil))))

(defun floating-youdao--post-command ()
  "Post-command hook to hide popup if needed."
  (unless floating-youdao--active
    (floating-youdao-hide)))

(defun floating-youdao--cleanup ()
  "Cleanup function when transient map is deactivated."
  (setq overriding-terminal-local-map nil)
  (remove-hook 'pre-command-hook #'floating-youdao--pre-command t)
  (remove-hook 'post-command-hook #'floating-youdao--post-command t)
  (floating-youdao-hide))

(defun floating-youdao--translate-and-show (text)
  "Translate TEXT using youdao-translate and show results."
  (when (and text (not (string-empty-p (string-trim text))))
    (message "Translating '%s'..." text)
    (youdao-translate--request-with-lines 
     text
     (lambda (lines)
       (floating-youdao--show-translation text lines)))))

;;; NEW: Switch to interactive mode
(defun floating-youdao-switch-to-interactive ()
  "Switch from scrollable popup to interactive translation mode."
  (interactive)
  (unless (featurep 'floating-youdao-interactive)
    (user-error "`floating-youdao-interactive' is not loaded"))
  (unless (fboundp 'floating-youdao-interactive--translate)
    (user-error "Interactive translation function not available"))
  (when floating-youdao--active
    (let ((text floating-youdao--original-text))
      (floating-youdao-hide)
      (if text
          (floating-youdao-interactive--translate text)
        (message "No original text cached for interactive mode")))))

;;; Interactive Commands

;;;###autoload
(defun floating-youdao-at-point ()
  "Show scrollable Youdao translation for text at point."
  (interactive)
  (let ((text (floating-youdao--get-text-at-point)))
    (if text
        (floating-youdao--translate-and-show text)
      (message "No text found at point"))))

;;;###autoload
(defun floating-youdao-hide ()
  "Hide translation frame and cleanup."
  (interactive)
  (when floating-youdao--hide-timer
    (cancel-timer floating-youdao--hide-timer)
    (setq floating-youdao--hide-timer nil))
  
  (when (and floating-youdao--current-frame
             (frame-live-p floating-youdao--current-frame))
    (make-frame-invisible floating-youdao--current-frame))
  
  (setq floating-youdao--parent-buffer nil
        floating-youdao--active nil
        floating-youdao--lines nil
        floating-youdao--total-lines 0
        floating-youdao--scroll-pos 0
        floating-youdao--original-text nil)) ; ← clear original text

;;;###autoload
(defun floating-youdao-toggle ()
  "Toggle translation frame visibility."
  (interactive)
  (if (and floating-youdao--current-frame
           (frame-live-p floating-youdao--current-frame)
           (frame-visible-p floating-youdao--current-frame))
      (floating-youdao-hide)
    (floating-youdao-at-point)))

;;; Minor Mode

(defvar floating-youdao-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c y f") #'floating-youdao-at-point)
    (define-key map (kbd "C-c y h") #'floating-youdao-hide)
    (define-key map (kbd "C-c y t") #'floating-youdao-toggle)
    map)
  "Keymap for Floating Youdao mode.")

;;;###autoload
(define-minor-mode floating-youdao-mode
  "Minor mode for floating Youdao translation with scrolling."
  :lighter " 📖"
  :keymap floating-youdao-mode-map
  (if floating-youdao-mode
      (message "Floating Youdao Translation enabled")
    (floating-youdao-hide)
    (message "Floating Youdao Translation disabled")))

;;;###autoload
(defun floating-youdao-setup ()
  "Setup floating-youdao with recommended settings."
  (interactive)
  (floating-youdao-mode 1)
  (message "Floating Youdao Translation ready! Press C-c y f to translate"))

(provide 'floating-youdao)
;;; floating-youdao.el ends here
