;;; naimacs.el --- A Gemini-powered coding assistant for Emacs  -*- lexical-binding: t; -*-

;; Author: Nemo Semret
;; Version: 0.2.1
;; Keywords: ai, gemini, languages, help, conversation
;; URL: https://github.com/nemozen/nemo/naimacs

;;; Commentary:
;; This assistant allows you to talk about the current buffer context
;; with Google Gemini to help with coding, writing etc. Supports
;; conversational context across buffers. Supports changing models in
;; conversation.

;;; Example Usage:
;; 1. Set your API key: (setenv "GOOGLE_API_KEY" "YOUR_API_KEY")
;; 2. Load this file: (load "naimacs.el")
;; 3. Start chatting: (naimacs-chat-with-context)
;; 4. To clear history: (naimacs-clear-conversation-history)
;; 5. To view history: (naimacs-show-conversation-history)
;; 6. To change models: (naimacs-set-model)

;;; Code:
(require 'json)
(require 'markdown-mode)

(defvar naimacs-model-name "gemini-2.5-flash-lite"
  "Name of Gemini model used by naimacs.")

(defvar naimacs-conversation-history nil
  "List storing conversation history. Each item is a list: '(\"user\" \"text\") or '(\"model\" \"text\" \"model-name\").")

(defun naimacs-clear-conversation-history ()
  "Clears the current Gemini conversation history."
  (interactive)
  (setq naimacs-conversation-history nil)
  (message "Gemini conversation history cleared."))

(defun naimacs-format-conversation-history (history)
  "Formats history for Gemini API."
  (mapcar (lambda (turn)
	    (pcase turn
	      ;; Match both user and model turns, extracting only role and text for the API.
	      (`(,role ,text . ,_)
	       `((role . ,(if (string-equal role "model") "model" "user")) ; Ensure role is valid
		 (parts . [((text . ,text))])))))
	  history))

(defun naimacs-chat-with-context ()
  "naimacs: Context-aware chat with Gemini."
  (interactive)
  (let* ((api-key (getenv "GOOGLE_API_KEY"))
	 (model naimacs-model-name)
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
	       (message "Sending query to Gemini...")
	       (raw-response (shell-command-to-string curl-command)))


	  (let* ((json-object (json-read-from-string raw-response))
		 (api-error (assoc 'error json-object)))

	    (if api-error
		;; Handle API error JSON
		(let* ((error-details (cdr api-error))
		       (error-message (cdr (assoc 'message error-details))))
		  (error "naimacs API Error: %s" error-message))

	      ;; Handle API success JSON
	      (let* ((candidates (cdr (assoc 'candidates json-object)))
		     (first-candidate (when (and (vectorp candidates) (> (length candidates) 0))
					(elt candidates 0)))
		     (content (cdr (assoc 'content first-candidate)))
		     (parts (cdr (assoc 'parts content)))
		     (response-text (when (and (vectorp parts) (> (length parts) 0))
				      (cdr (assoc 'text (elt parts 0))))))

		(if (and response-text (not (zerop (length prompt))))
		    (progn
		      (push `("user" ,prompt) naimacs-conversation-history)
		      (push `("model" ,response-text ,naimacs-model-name) naimacs-conversation-history)

		      ;; Output to the correct buffer
		      (with-current-buffer (get-buffer-create buf-name)
			(let ((inhibit-read-only t))
			  (erase-buffer)
			  (insert (format "# Gemini Response (%s)\n\n" naimacs-model-name))
			  (insert response-text)
			  (markdown-mode)
			  (goto-char (point-min)))
			(display-buffer (current-buffer))))

		  ;; Logic for handling other extraction failures (e.g., safety blocks)
		  (with-current-buffer (get-buffer-create "*Gemini-Debug*")
		    (let ((inhibit-read-only t))
		      (erase-buffer)
		      (insert "--- DEBUG: Extraction Failed ---\n")
		      (insert "Check if 'finishReason' is 'SAFETY' or 'OTHER'.\n\n")
		      (insert "Raw Response:\n")
		      (insert raw-response)
		      (goto-char (point-min))
		      (json-pretty-print-buffer))
		    (display-buffer (current-buffer)))
		  (error "naimacs: No text found in response (see *Gemini-Debug*)"))))))))))

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
	      ;; Reverse to display chronologically (history is stored newest first)
	      (dolist (turn (reverse naimacs-conversation-history))
		(pcase turn
		  (`("user" ,text)
		   (insert (format "**USER:**\n%s" text)))
		  (`("model" ,text ,model)
		   (insert (format "**MODEL (%s):**\n%s" model text)))
		  ;; Fallback for any unexpected format
		  (_ (insert (format "UNKNOWN: %s" turn))))
		(insert "\n\n---\n\n"))
	      (goto-char (point-min)))
	  (insert "No history yet.\n")))
      (markdown-mode)
      (display-buffer hist-buf)))
  (message "Displaying Gemini conversation history in *Gemini-History*."))

(defun naimacs-set-model (model-name)
  "Set the Gemini model used by naimacs.
Prompts for a new model name, with the current model as default.
Example: `M-x naimacs-set-model` then type `gemini-1.5-pro-latest`."
  (interactive (list (read-string (format "Enter Gemini model name (current: %s): " naimacs-model-name)
				  naimacs-model-name)))
  (setq naimacs-model-name model-name)
  (message "naimacs model set to: %s" naimacs-model-name))

(provide 'naimacs)
