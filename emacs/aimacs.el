(defun gemini-chat-with-context ()
  "Send the current buffer as context to Gemini Pro and show response in a side buffer."
  (interactive)
  (let* ((api-key (getenv "GOOGLE_API_KEY"))
         (context (buffer-substring-no-properties (point-min) (point-max)))
         (prompt (read-string "Ask Gemini about this buffer: "))
         (buf-name "*Gemini-Response*")
         ;; Prepare the JSON payload
         (json-data (json-encode
                     `((contents . [((parts . [((text . ,(concat "Context:\n" context "\n\nQuestion: " prompt)))]))]))))
         ;; The curl command
         (cmd (concat "curl https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-pro:streamGenerateContent?key=" api-key
                      " -H 'Content-Type: application/json'"
                      " -X POST"
                      " -d " (shell-quote-argument json-data)
                      " --no-buffer")))

    (unless api-key
      (error "Please set the GOOGLE_API_KEY environment variable"))

    ;; Setup the response buffer
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; (markdown-mode) ; Assumes you have markdown-mode installed
        (display-buffer buf-name)))

    ;; Run the process asynchronously
    (message "Gemini is thinking...")
    (set-process-filter
     (start-process-shell-command "gemini-process" buf-name cmd)
     (lambda (proc string)
       (when (buffer-live-p (process-buffer proc))
         (with-current-buffer (process-buffer proc)
           (let ((inhibit-read-only t))
             (insert string)
             ;; Clean up the streaming JSON noise if you want it pretty, 
             ;; but for a simple script, we'll just append.
             (goto-char (point-max)))))))))
