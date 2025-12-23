;;; youdao-translate.el --- Translate with Youdao Dictionary -*- lexical-binding: t -*-

;; Copyright (C) 2024
;; Author: bommbo
;; Version: 0.1
;; Package-Requires: (emacs "30.0")
;; Keywords: convenience, translation, chinese

;;; Commentary:

;; A simple and elegant Youdao Dictionary translation package for Emacs.

;;; Code:

(require 'url)
(require 'dom)
(require 'thingatpt)

;;; Customization

(defgroup youdao-translate nil
  "Translate with Youdao Dictionary."
  :group 'convenience
  :prefix "youdao-translate-")

(defcustom youdao-translate-user-agent
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36"
  "User agent for HTTP requests."
  :type 'string
  :group 'youdao-translate)

(defcustom youdao-translate-buffer-name "*Youdao Translation*"
  "Buffer name for displaying translation results."
  :type 'string
  :group 'youdao-translate)

(defcustom youdao-translate-display-function 'youdao-translate-display-in-buffer
  "Function to display translation results."
  :type 'function
  :group 'youdao-translate)

(defcustom youdao-translate-enable-history t
  "Whether to save translation history."
  :type 'boolean
  :group 'youdao-translate)

;;; Faces

(defface youdao-translate-title
  '((t :weight bold :height 1.2 :foreground "#569cd6"))
  "Face for section titles."
  :group 'youdao-translate)

(defface youdao-translate-phonetic
  '((t :foreground "#ce9178"))
  "Face for phonetic symbols."
  :group 'youdao-translate)

(defface youdao-translate-translation
  '((t :foreground "#4ec9b0"))
  "Face for translation text."
  :group 'youdao-translate)

(defface youdao-translate-example
  '((t :foreground "#9cdcfe"))
  "Face for example sentences."
  :group 'youdao-translate)

;;; Internal Variables

(defvar youdao-translate-history nil
  "Translation history.")

(defvar youdao-translate-request-headers
  `(("User-Agent" . ,youdao-translate-user-agent))
  "HTTP request headers.")

;;; Utility Functions

(defun youdao-translate--dom-by-key (dom key)
  "Find DOM element by KEY selector."
  (let (func num)
    (when (string-match "\\[\\([0-9]+\\)\\]" key)
      (setq num (match-string 1 key))
      (setq key (substring key 0 (- (length key) (length num) 2))))
    (cond ((string-prefix-p "." key)
           (setq key (substring key 1) func 'dom-by-class))
          ((string-prefix-p "#" key)
           (setq key (substring key 1) func 'dom-by-id))
          ((string-prefix-p "*" key)
           (setq func (lambda (p _) (dom-children p))))
          (t (setq key (intern key) func 'dom-by-tag)))
    (if (null num)
        (funcall func dom key)
      (nth (string-to-number num) (funcall func dom key)))))

(defun youdao-translate--dom-find (dom xpath)
  "Find DOM element by XPATH."
  (cl-loop for key in (string-split (string-trim xpath) "/")
           if (consp dom)
           do (setq dom (youdao-translate--dom-by-key dom key))
           else return dom)
  dom)

;;; Core Translation Functions

(defun youdao-translate--parse-dom (dom)
  "Parse translation results from DOM."
  (when dom
    (let (results)
      ;; 句子翻译
      (let ((trans-content (dom-text (dom-by-class dom "trans-content"))))
        (when (and trans-content (not (string-empty-p (string-trim trans-content))))
          (push (cons 'translation (string-trim trans-content)) results)))
      
      (unless results
        (dolist (class '("trans-container" "fanyi-result" "output-bd" "outputTarget"))
          (when-let ((text (dom-text (dom-by-class dom class))))
            (when (> (length (string-trim text)) 0)
              (push (cons 'translation (string-trim text)) results)
              (cl-return)))))
      
      ;; 单词基本释义
      (unless results
        (let* ((word-exps (youdao-translate--dom-find dom ".simple dict-module/.trans-container/.word-exp"))
               (result (cl-loop for child in word-exps
                                when (consp child)
                                for text = (string-trim (dom-texts child))
                                unless (or (string-empty-p text)
                                          (string-prefix-p "【" text)
                                          (string-prefix-p "[" text))
                                collect text)))
          (unless (null result)
            (push (cons 'basic (string-join result "\n")) results))))
      
      ;; 词形变化
      (let ((result (cl-loop for child in (youdao-translate--dom-find dom ".simple dict-module/.trans-container/.word-wfs-cell-less")
                             when (consp child)
                             collect (format "%s: %s"
                                             (dom-text (youdao-translate--dom-find child ".wfs-name"))
                                             (dom-text (youdao-translate--dom-find child ".transformation"))))))
        (unless (null result)
          (push (cons 'morphology (string-join result "\n")) results)))
      
      ;; 音标
      (let ((result (cl-loop for child in (youdao-translate--dom-find dom ".ec dict-module/.trans-container/.phone_con/.per-phone")
                             when (consp child)
                             collect (dom-texts child))))
        (unless (null result)
          (push (cons 'phonetic (string-join result "\n")) results)))
      
      ;; 网络释义
      (let ((result (cl-loop for child in (youdao-translate--dom-find dom ".web_trans dict-module/.trans-container/.webPhrase/.mcols-layout")
                             when (consp child)
                             collect (format "- %s\n  %s"
                                             (dom-text (youdao-translate--dom-find child ".point"))
                                             (dom-text (youdao-translate--dom-find child ".sen-phrase"))))))
        (unless (null result)
          (push (cons 'phrase (string-join result "\n")) results)))
      
      ;; 双语例句
      (let ((result (cl-loop for child in (youdao-translate--dom-find dom ".blng_sents_part dict-module/.trans-container/.mcols-layout")
                             when (consp child)
                             collect (format "- %s\n  %s"
                                             (dom-texts (youdao-translate--dom-find child ".col2/.word-exp[0]"))
                                             (dom-texts (youdao-translate--dom-find child ".col2/.word-exp[1]"))))))
        (unless (null result)
          (push (cons 'examples (string-join result "\n")) results)))
      
      results)))

(defun youdao-translate--format-results-lines (text results)
  "Format RESULTS for TEXT into a list of lines with faces."
  (if (null results)
      (list (format "未找到「%s」的翻译结果" text))
    (let ((lines nil)
          (order (if (assq 'basic results)
                     '(basic phonetic morphology phrase examples)
                   '(translation phonetic morphology phrase examples))))
      
      (dolist (section-type order)
        (when-let* ((item (assq section-type results)))
          (pcase section-type
            ('basic
             (setq lines (nconc lines (list (propertize "【基本释义】" 'face 'youdao-translate-title))))
             (setq lines (nconc lines (split-string (cdr item) "\n" t)))
             (setq lines (nconc lines (list ""))))
            
            ('phonetic
             (setq lines (nconc lines (list (propertize "【音标】" 'face 'youdao-translate-title))))
             (setq lines (nconc lines (list (propertize (cdr item) 'face 'youdao-translate-phonetic))))
             (setq lines (nconc lines (list ""))))
            
            ('translation
             (setq lines (nconc lines (list (propertize "【翻译】" 'face 'youdao-translate-title))))
             (setq lines (nconc lines (list (propertize (cdr item) 'face 'youdao-translate-translation))))
             (setq lines (nconc lines (list ""))))
            
            ('morphology
             (setq lines (nconc lines (list (propertize "【词形变化】" 'face 'youdao-translate-title))))
             (setq lines (nconc lines (split-string (cdr item) "\n" t)))
             (setq lines (nconc lines (list ""))))
            
            ('phrase
             (setq lines (nconc lines (list (propertize "【网络释义】" 'face 'youdao-translate-title))))
             (dolist (line (split-string (cdr item) "\n" t))
               (setq lines (nconc lines (list (propertize line 'face 'youdao-translate-example)))))
             (setq lines (nconc lines (list ""))))
            
            ('examples
             (setq lines (nconc lines (list (propertize "【例句】" 'face 'youdao-translate-title))))
             (dolist (line (split-string (cdr item) "\n" t))
               (setq lines (nconc lines (list (propertize line 'face 'youdao-translate-example)))))
             (setq lines (nconc lines (list "")))))))
      
      ;; Remove trailing empty line
      (when (and lines (string-empty-p (car (last lines))))
        (setq lines (butlast lines)))
      
      lines)))

(defun youdao-translate--format-results (text results)
  "Format RESULTS for TEXT into readable string."
  (string-join (youdao-translate--format-results-lines text results) "\n"))

(defun youdao-translate--request (text callback)
  "Request translation for TEXT and call CALLBACK with formatted results."
  (let ((url (format "https://dict.youdao.com/result?word=%s&lang=en" 
                     (url-hexify-string text)))
        (url-request-extra-headers youdao-translate-request-headers))
    (url-retrieve url
                  (lambda (_status)
                    (goto-char url-http-end-of-headers)
                    (let* ((dom (libxml-parse-html-region (point)))
                           (results (youdao-translate--parse-dom dom))
                           (formatted (youdao-translate--format-results text results)))
                      (kill-buffer (current-buffer))
                      (funcall callback formatted)))
                  nil t)))

(defun youdao-translate--request-with-lines (text callback)
  "Request translation for TEXT and call CALLBACK with list of lines."
  (let ((url (format "https://dict.youdao.com/result?word=%s&lang=en" 
                     (url-hexify-string text)))
        (url-request-extra-headers youdao-translate-request-headers))
    (url-retrieve url
                  (lambda (_status)
                    (goto-char url-http-end-of-headers)
                    (let* ((dom (libxml-parse-html-region (point)))
                           (results (youdao-translate--parse-dom dom))
                           (lines (youdao-translate--format-results-lines text results)))
                      (kill-buffer (current-buffer))
                      (funcall callback lines)))
                  nil t)))

(defun youdao-translate--request-sync (text)
  "Request translation for TEXT synchronously and return formatted results."
  (let ((url (format "https://dict.youdao.com/result?word=%s&lang=en" 
                     (url-hexify-string text)))
        (url-request-extra-headers youdao-translate-request-headers))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char url-http-end-of-headers)
      (let* ((dom (libxml-parse-html-region (point)))
             (results (youdao-translate--parse-dom dom))
             (formatted (youdao-translate--format-results text results)))
        (kill-buffer (current-buffer))
        formatted))))

;;; Display Functions

(defun youdao-translate-display-in-buffer (text)
  "Display translation TEXT in a buffer."
  (let ((buffer (get-buffer-create youdao-translate-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (goto-char (point-min))
        (view-mode 1)))
    (display-buffer buffer)))

(defun youdao-translate-display-in-minibuffer (text)
  "Display translation TEXT in minibuffer."
  (message "%s" text))

;;; Interactive Commands

;;;###autoload
(defun youdao-translate-at-point ()
  "Translate word at point."
  (interactive)
  (if-let ((word (thing-at-point 'word t)))
      (progn
        (when youdao-translate-enable-history
          (push word youdao-translate-history))
        (message "Translating '%s'..." word)
        (youdao-translate--request word youdao-translate-display-function))
    (user-error "No word at point")))

;;;###autoload
(defun youdao-translate-input (text)
  "Translate TEXT from user input."
  (interactive "sTranslate: ")
  (when (string-empty-p (string-trim text))
    (user-error "Empty input"))
  (when youdao-translate-enable-history
    (push text youdao-translate-history))
  (message "Translating '%s'..." text)
  (youdao-translate--request text youdao-translate-display-function))

;;;###autoload
(defun youdao-translate-region (start end)
  "Translate text in region from START to END."
  (interactive "r")
  (if (use-region-p)
      (let ((text (buffer-substring-no-properties start end)))
        (when youdao-translate-enable-history
          (push text youdao-translate-history))
        (message "Translating region...")
        (youdao-translate--request text youdao-translate-display-function))
    (user-error "No region selected")))

;;;###autoload
(defun youdao-translate-sync (text)
  "Translate TEXT synchronously and return result."
  (interactive "sTranslate: ")
  (when (string-empty-p (string-trim text))
    (user-error "Empty input"))
  (message "Translating '%s'..." text)
  (let ((result (youdao-translate--request-sync text)))
    (funcall youdao-translate-display-function result)
    result))

(provide 'youdao-translate)
;;; youdao-translate.el ends here
