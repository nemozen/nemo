(require 'json)

(defun gemini-chat-with-context ()
  "Send current buffer to Gemini and show only the text response."
  (interactive)
  (let* ((api-key (getenv "GOOGLE_API_KEY"))
         (model "gemini-2.5-flash-lite") ; TODO: allow specifiying this
         (context (buffer-substring-no-properties (point-min) (point-max)))
         (prompt (read-string "Ask Gemini about this buffer: "))
         (buf-name "*Gemini-Response*")
         ;; Note: Using generateContent (non-streaming) for easier parsing
         (url (concat "https://generativelanguage.googleapis.com/v1beta/models/" 
                      model ":generateContent?key=" api-key))
         (json-data (json-encode
                     `((contents . [((parts . [((text . ,(concat "Context:\n" context "\n\nQuestion: " prompt)))]))])))))

    (unless api-key (error "Set GOOGLE_API_KEY first"))

    (message "Gemini is thinking...")

    ;; Create/Clear the response buffer
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (markdown-mode)))

    ;; Use curl to get the full response, then parse it
    (let* ((curl-command (concat "curl -s -X POST " url 
                                 " -H 'Content-Type: application/json'"
                                 " -d " (shell-quote-argument json-data)))
           (raw-response (shell-command-to-string curl-command))
           (json-object (json-read-from-string raw-response))
           ;; Extract the text: candidates -> content -> parts -> text
           (candidates (append (assoc-default 'candidates json-object) nil))
           (first-candidate (elt candidates 0))
           (content (assoc-default 'content first-candidate))
           (parts (assoc-default 'parts content))
           (response-text (assoc-default 'text (elt parts 0))))

      (with-current-buffer buf-name
        (let ((inhibit-read-only t))
          (insert (or response-text "Error: Could not parse response. Check *Messages*."))
          (display-buffer buf-name)))
      (message "Done."))))
