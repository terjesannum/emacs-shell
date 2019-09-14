(setq tramp-histfile-override t)
(setq tramp-default-method "ssh")
(setq explicit-shell-file-name "sh")
(setq explicit-sh-args '("-l"))
(setq tramp-remote-shell-executable "sh")

(require 'shell)
(require 'tramp)
(require 'exec-path-from-shell) ; https://github.com/purcell/exec-path-from-shell
(require 'bash-completion)      ; https://github.com/szermatt/emacs-bash-completion
(require 'docker-tramp)         ; https://github.com/emacs-pe/docker-tramp.el
(require 'kubernetes-tramp)     ; https://github.com/gruggiero/kubernetes-tramp

(defvar user-remote-shell-history-directory
  (expand-file-name (concat user-emacs-directory "/" "shell-history" "/"))
  "Directory to save shell history files")
(make-directory user-remote-shell-history-directory t)

(when (memq window-system '(mac ns x))
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "KUBECONFIG"))
  (exec-path-from-shell-initialize))

; proxy sudo-shells
(add-to-list 'tramp-default-proxies-alist
             '("." "\\`root\\'" "/ssh:%h:"))

(defvar tramp-shell-hook nil "Hook called before starting a tramp shell")

(defun tramp-shell (method host &optional history-name directory)
  (interactive "sMethod: \nsHost: ")
  (run-hook-with-args 'tramp-shell-hook method host history-name directory)
  (let ((default-directory (format "/%s:%s:%s" method host (or directory ""))))
    (shell (generate-new-buffer-name (concat method "-" host)))
    (setq comint-input-ring (make-ring comint-input-ring-size))
    (setq comint-input-ring-file-name (concat user-remote-shell-history-directory "/" (or history-name host) "." method))
    (comint-read-input-ring 'silent)))

(defun ssh-shell (host &optional directory)
  "Start ssh shell"
  (interactive
   (list
    (read-string "Host: ")
    (and current-prefix-arg (read-string "Directory: " "/"))))
  (tramp-shell "ssh" host nil directory))

(defvar sudo-shell-post-start-hook
  (lambda (&rest rest)
    (process-send-string
     (get-buffer-process (current-buffer))
     "test -n \"$SUDO_USER\" -a -n \"$BASH\" && SETUP_HOME=$(eval echo ~$SUDO_USER) . $(eval echo ~$SUDO_USER)/.bashrc &>/dev/null\n"))
  "Hook run after starting sudo-shell")

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

(add-hook 'comint-exec-hook
          (lambda ()
            (set-process-sentinel (get-buffer-process (current-buffer))
                                  'shell-process-kill-buffer-sentinel)))

(defun shell-process-kill-buffer-sentinel (process state)
  (message "shell(%s): %s" (buffer-name) state)
  (kill-buffer (current-buffer)))

(add-hook 'kill-buffer-hook
          (lambda ()
            (when (derived-mode-p 'comint-mode)
              (comint-write-input-ring))))

(add-hook 'kill-emacs-hook
          (lambda ()
            (loop for buffer in (buffer-list)
                  do (progn
                       (set-buffer buffer)
                       (when (derived-mode-p 'comint-mode)
                         (comint-write-input-ring))))))

(global-set-key (kbd "S-C-n") (lambda ()
                                (interactive)
                                (let ((default-directory "~"))
                                  (shell (generate-new-buffer-name "*shell*"))
                                  (setq comint-input-ring-file-name (concat user-remote-shell-history-directory "/localhost"))
                                  (comint-read-input-ring 'silent))))

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