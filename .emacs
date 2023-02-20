(menu-bar-mode -1)
(tool-bar-mode -1)
(set-background-color "black")
(set-foreground-color "GreenYellow")
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq js-indent-level 2)
(set-face-attribute 'default (selected-frame) :height 100)
(custom-set-variables
 '(send-mail-function (quote mail)))
(custom-set-faces)
(desktop-read)
(desktop-save-mode 1)
