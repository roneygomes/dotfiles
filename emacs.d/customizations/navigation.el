;; Some navigation configs, mostly helm related.
(require 'helm-config)

(evil-leader/set-key
  "w" 'save-file
  "e" 'find-file
  "b" 'helm-buffers-list
  "f" 'helm-find-files
  "x" 'helm-M-x
  "k" 'kill-buffer)
