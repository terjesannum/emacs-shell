;;; emacs-shell.el --- Emacs tramp and shell setup

;; Copyright (C) 2019 Terje Sannum

;; Author: Terje Sannum <terje@offpiste.org>
;; Created: 14 Sep 2019
;; Package-Requires: ((emacs "25.1") (exec-path-from-shell) (bash-completion) (docker-tramp) (kubernetes-tramp))
;; Keywords: shell
;; Homepage: https://github.com/terjesannum/emacs-shell

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See https://github.com/terjesannum/emacs-shell/blob/master/README.md

;;; Code:

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
(defvar tramp-shell-started-hook nil "Hook called after starting a tramp shell")

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
               (concat (or history-name host) "." method))
  (run-hook-with-args 'tramp-shell-started-hook method host history-name directory))

(defun ssh-shell (host &optional directory)
  "Start ssh shell"
  (interactive
   (list
    (read-string "Host: ")
    (and current-prefix-arg (read-string "Directory: " "/"))))
  (tramp-shell "ssh" host nil directory))

(defun sudo-shell (host &optional directory)
  "Start sudo shell"
  (interactive
   (list
    (read-string "Host: ")
    (and current-prefix-arg (read-string "Directory: " "/"))))
  (tramp-shell "sudo" host nil directory))

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

(defun sudo-shell-source-user-bashrc (&rest rest)
  "Source bashrc from user starting sudo-shell"
  (process-send-string
   (get-buffer-process (current-buffer))
   "test -n \"$SUDO_USER\" -a -r $(eval echo ~$SUDO_USER)/.bashrc && type bash >/dev/null 2>&1 && exec bash --rcfile $(eval echo ~$SUDO_USER)/.bashrc\n"))

(defun emacs-shell-run-command-silently (command)
  "Run command without showing in the shell buffer or shell history"
  (let ((process (get-buffer-process (current-buffer))))
    (with-temp-buffer
      (comint-redirect-send-command-to-process command (current-buffer) process nil t)
      (with-current-buffer (process-buffer process)
        (while (and (null comint-redirect-completed)
                    (accept-process-output process 1))))
      (buffer-string))))

(defun emacs-shell-exec-bash ()
  "Exec bash if available, return t when running bash"
  (let ((command-output (emacs-shell-run-command-silently
                         "if test `basename $0` = 'bash'; then echo running bash; else bash=`which bash` && echo run bash && exec $bash; fi")))
    (and (string-match "run.* bash" command-output) t)))

(defun emacs-shell-source-local-bashrc ()
  "Source bashrc from Emacs host in shell"
  (interactive)
  (let ((bashrc (with-temp-buffer
                  (insert-file-contents "~/.bashrc")
                  (buffer-string))))
    (setq bashrc (concat "PS2='>'\n'" bashrc)) ; ensure PS2 is set to get output on mulitline commands
    (dolist (command (split-string bashrc "\n"))
      (emacs-shell-run-command-silently command))))

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

;;; emacs-shell.el ends here
