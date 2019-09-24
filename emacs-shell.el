(setq tramp-histfile-override t)
(setq tramp-default-method "ssh")
(setq explicit-shell-file-name "sh")
(setq explicit-sh-args '("-l"))
(setq tramp-remote-shell-executable "sh")

; Get environment variables from shell
(require 'exec-path-from-shell) ; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns x))
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "KUBECONFIG"))
  (exec-path-from-shell-initialize))

(require 'shell)
(require 'tramp)
(require 'bash-completion)      ; https://github.com/szermatt/emacs-bash-completion
(require 'docker-tramp)         ; https://github.com/emacs-pe/docker-tramp.el
(require 'kubernetes-tramp)     ; https://github.com/gruggiero/kubernetes-tramp

(defvar user-remote-shell-history-directory
  (expand-file-name (concat user-emacs-directory "/" "shell-history" "/"))
  "Directory to save shell history files")
(make-directory user-remote-shell-history-directory t)

; proxy remote sudo-shells
(add-to-list 'tramp-default-proxies-alist
             '("." "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '("localhost" "\\`root\\'" nil))

(defvar tramp-shell-hook nil "Hook called before starting a tramp shell")

(defun emacs-shell (buffer-name directory history-file)
  (let* ((default-directory directory)
         (shell-buffer (shell (generate-new-buffer-name buffer-name))))
    (setq comint-input-ring (make-ring comint-input-ring-size))
    (setq comint-input-ring-file-name (concat user-remote-shell-history-directory "/" history-file))
    (comint-read-input-ring 'silent)
    (set-process-sentinel (get-buffer-process shell-buffer)
                          'shell-process-kill-buffer-sentinel)))

(defun tramp-shell (method host &optional history-name directory)
  (interactive "sMethod: \nsHost: ")
  (run-hook-with-args 'tramp-shell-hook method host history-name directory)
  (emacs-shell (concat method "-" host)
               (format "/%s:%s:%s" method host (or directory ""))
               (concat (or history-name host) "." method)))

(defun ssh-shell (host &optional directory)
  "Start ssh shell"
  (interactive
   (list
    (read-string "Host: ")
    (and current-prefix-arg (read-string "Directory: " "/"))))
  (tramp-shell "ssh" host nil directory))

(defun sudo-shell-source-user-bashrc (&rest rest)
  "Source bashrc from user starting sudo-shell"
  (process-send-string
   (get-buffer-process (current-buffer))
   "test -n \"$SUDO_USER\" -a -n \"$BASH\" && SETUP_HOME=$(eval echo ~$SUDO_USER) . $(eval echo ~$SUDO_USER)/.bashrc &>/dev/null\n"))

(defvar sudo-shell-post-start-hook nil "Hook run after starting sudo-shell")
(add-hook 'sudo-shell-post-start-hook 'sudo-shell-source-user-bashrc)

(defun sudo-shell (host &optional directory)
  "Start sudo shell"
  (interactive
   (list
    (read-string "Host: ")
    (and current-prefix-arg (read-string "Directory: " "/"))))
  (tramp-shell "sudo" host nil directory)
  (run-hook-with-args 'sudo-shell-post-start-hook host directory))

(defun docker-image-name (id)
  (let ((image (car (apply #'process-lines docker-tramp-docker-executable (list "inspect" "-f" "{{ .Config.Image }}" id)))))
    (replace-regexp-in-string "/" "_" (car (split-string image "[@:]")))))

(defun docker-shell (container &optional directory)
  "Start shell in docker container"
  (interactive
   (list
    (completing-read "Container: " (docker-tramp--running-containers))
    (and current-prefix-arg (read-string "Directory: " "/"))))
  (tramp-shell "docker" container (docker-image-name container) directory))

(defun pod-owner-name (pod)
  (let ((owner (car (apply #'process-lines kubernetes-tramp-kubectl-executable (list "get" "pod" pod "-o" "jsonpath={.metadata.ownerReferences[].kind}")))))
    (cond ((string= owner "ReplicaSet") (replace-regexp-in-string "-[0-9a-f]\\{8,10\\}-[0-9a-z]\\{5\\}$" "" pod))
          ((string= owner "DaemonSet") (replace-regexp-in-string "-[0-9a-z]\\{5\\}$" "" pod))
          ((string= owner "StatefulSet") (replace-regexp-in-string "-[0-9]+$" "" pod))
          (t pod))))

(defun pod-shell (pod &optional directory)
  "Start shell in Kubernetes pod"
  (interactive
   (list
    (completing-read "Pod: " (kubernetes-tramp--running-containers))
    (and current-prefix-arg (read-string "Directory: " "/"))))
  (tramp-shell "kubectl" pod (pod-owner-name pod) directory))

(defun localhost-shell ()
  "Start shell on localhost"
  (interactive)
  (emacs-shell "*shell*" "~" "localhost"))

(defun shell-process-kill-buffer-sentinel (process state)
  (message "shell(%s): %s" (buffer-name) state)
  (kill-buffer (current-buffer)))

(defun shell-mode-write-comint-input-ring ()
  (when (derived-mode-p 'shell-mode)
    (comint-write-input-ring)))

(add-hook 'kill-buffer-hook 'shell-mode-write-comint-input-ring)

(defun shell-mode-buffers-write-comint-input-ring ()
  (mapc
   (lambda (buffer)
     (with-current-buffer buffer
       (shell-mode-write-comint-input-ring)))
   (buffer-list)))

(add-hook 'kill-emacs-hook 'shell-mode-buffers-write-comint-input-ring)

(defun emacs-shell-interrupt-password-command ()
  "Abort minibuffer password entry and interrupt command"
  (interactive)
  (with-selected-window (minibuffer-selected-window)
    (comint-interrupt-subjob))
  (abort-recursive-edit))

(global-set-key (kbd "S-C-n") 'localhost-shell)
(define-key read-passwd-map (kbd "C-c C-c") 'emacs-shell-interrupt-password-command)
(define-key shell-mode-map (kbd "C-p") 'comint-previous-input)
(define-key shell-mode-map (kbd "C-n") 'comint-next-input)

(bash-completion-setup)

(setq shell-font-lock-keywords nil)
(setq comint-input-ring-size 50000)
(setq comint-buffer-maximum-size 100000)
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
(add-hook 'comint-mode-hook
          '(lambda () (setq comint-input-ignoredups t)))

(setq comint-password-prompt-regexp
      (concat comint-password-prompt-regexp
              "\\| (will be hidden): *\\'"
              "\\|^Password for [^:]+: *\\'"
              "\\|^Enter .*password[^:]*: *\\'"))

(provide 'emacs-shell)
