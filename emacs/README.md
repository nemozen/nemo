Get API key from [https://aistudio.google.com/](Google AI Studio)

Set it as an environment variable GOOGLE_API_KEY 

To change which model you want to use as your agent, edit the `model` variable in [aimacs.el].

Get list of available models:
```curl https://generativelanguage.googleapis.com/v1beta/models?key=$GOOGLE_API_KEY ```

Requires markdown mode in emacs
```sudo apt install elpa-markdown-mode```


