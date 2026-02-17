;;; naimacs.el --- A Gemini-powered coding assistant for Emacs  -*- lexical-binding: t; -*-

;; Author: Nemo Semret
;; Version: 0.1.1 ; Incrementing version for new feature
;; Keywords: ai, gemini, languages, help, conversation
;; URL: https://github.com/nemozen/nemo/naimacs

;;; Commentary:
;; This assistant allows you to talk about the current buffer context with Google Gemini
;; to help with coding, writing etc. It now supports conversational context.

;;; Example Usage:
;; 1. Set your API key: (setenv "GOOGLE_API_KEY" "YOUR_API_KEY")
;; 2. Load this file: (load "naimacs.el")
;; 3. Start chatting: (naimacs-chat-with-context)
;; 4. To clear history: (naimacs-clear-conversation-history)
;; 5. To view history: (naimacs-show-conversation-history)

;;; Code:
(require 'json)
(require 'markdown-mode)

(defvar naimacs-conversation-history nil
  "List storing history: '((\"user\" . \"text\") (\"model\" . \"text\")).")

(defun naimacs-clear-conversation-history ()
  "Clears the current Gemini conversation history."
  (interactive)
  (setq naimacs-conversation-history nil)
  (message "Gemini conversation history cleared."))

(defun naimacs-format-conversation-history (history)
  "Formats history for Gemini API."
  (mapcar (lambda (turn)
	    `((role . ,(car turn))
	      (parts . [((text . ,(cdr turn)))])))
	  history))

(defun naimacs-chat-with-context ()
  "naimacs: Context-aware chat with Gemini."
  (interactive)
  (let* ((api-key (getenv "GOOGLE_API_KEY"))
	 (model "gemini-2.5-flash-lite")
	 (context (buffer-substring-no-properties (point-min) (point-max)))
	 (prompt (read-string "Ask Gemini (C-c C-c to clear): "))
	 (buf-name "*Gemini-Response*"))

    (unless api-key (error "Set GOOGLE_API_KEY first"))

    (if (string-equal prompt "\C-c\C-c")
	(progn (naimacs-clear-conversation-history) (setq prompt ""))

      (unless (zerop (length prompt))
	(let* ((formatted-history (naimacs-format-conversation-history (reverse naimacs-conversation-history)))
	       (initial-msg `((role . "user")
			      (parts . [((text . ,(concat "Context:\n" context "\n\nQuestion: " prompt)))])))
	       (all-content-items (append formatted-history (list initial-msg)))
	       (json-data (json-encode `((contents . ,all-content-items))))
	       (url (concat "https://generativelanguage.googleapis.com/v1beta/models/" model ":generateContent?key=" api-key))
	       (curl-command (concat "curl -s -X POST " (shell-quote-argument url)
				     " -H 'Content-Type: application/json'"
				     " -d " (shell-quote-argument json-data)))
	       (raw-response (shell-command-to-string curl-command)))

	  (message "Gemini is thinking...")

	  (let* ((json-object (json-read-from-string raw-response))
	     ;; 1. Use assoc-default to find the candidates vector
	     (candidates (assoc-default 'candidates json-object))
	     ;; 2. Access vector element 0 safely
	     (first-candidate (when (and (vectorp candidates) (> (length candidates) 0))
				(elt candidates 0)))
	     (content (assoc-default 'content first-candidate))
	     (parts (assoc-default 'parts content))
	     ;; 3. Access parts vector element 0 for the text
	     (response-text (when (and (vectorp parts) (> (length parts) 0))
			      (assoc-default 'text (elt parts 0)))))

	(if (and response-text (not (zerop (length prompt))))
	    (progn
	      ;; Update history with dotted pairs
	      (push `("user" . ,prompt) naimacs-conversation-history)
	      (push `("model" . ,response-text) naimacs-conversation-history)

	      ;; Output to the correct buffer
	      (with-current-buffer (get-buffer-create buf-name)
		(let ((inhibit-read-only t))
		  (erase-buffer)
		  (insert "# Gemini Response\n\n")
		  (insert response-text)
		  (markdown-mode)
		  (goto-char (point-min)))
		(display-buffer (current-buffer))))

	  ;; Logic for handling API errors or safety blocks
	  (with-current-buffer (get-buffer-create "*Gemini-Debug*")
	    (let ((inhibit-read-only t))
	      (erase-buffer)
	      (insert "--- DEBUG: Extraction Failed ---\n")
	      (insert "Check if 'finishReason' is 'SAFETY' or 'OTHER'.\n\n")
	      (insert raw-response)
	      (json-pretty-print-buffer))
	    (display-buffer (current-buffer)))
	  (error "naimacs: No text found in response (see *Gemini-Debug*)"))))))))


(defun naimacs-show-conversation-history ()
  "Displays the current Gemini conversation history."
  (interactive)
  (let ((hist-buf (get-buffer-create "*Gemini-History*")))
    (with-current-buffer hist-buf
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert "# Gemini Conversation History\n\n")
	(if naimacs-conversation-history
	    (progn
	      ;; Reverse to display chronologically (newest first)
	      (dolist (turn (reverse naimacs-conversation-history))
		(insert (upcase (car turn)) ":\n")
		(insert (cdr turn))
		(insert "\n\n"))
	      (goto-char (point-min)))
	  (insert "No history yet.\n")))
      (markdown-mode)
      (display-buffer hist-buf)))
  (message "Displaying Gemini conversation history in *Gemini-History*."))
