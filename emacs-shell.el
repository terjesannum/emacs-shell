;;; emacs-shell.el --- Emacs tramp and shell setup

;; Copyright (C) 2019 Terje Sannum

;; Author: Terje Sannum <terje@offpiste.org>
;; Created: 14 Sep 2019
;; Package-Requires: ((emacs "26.1"))
;; Keywords: unix comm
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

(require 'shell)
(require 'tramp)
(require 'docker-tramp)         ; https://github.com/emacs-pe/docker-tramp.el
(require 'kubernetes-tramp)     ; https://github.com/gruggiero/kubernetes-tramp

(defvar emacs-shell-history-directory
  (expand-file-name (concat user-emacs-directory "/" "shell-history" "/"))
  "Directory to save shell history files.")
(make-directory emacs-shell-history-directory t)

;; proxy remote sudo-shells
(add-to-list 'tramp-default-proxies-alist
             '("." "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '("localhost" "\\`root\\'" nil))
;; disable timeout on sudo shells
(setcar (cdr (assoc 'tramp-session-timeout (assoc "sudo" tramp-methods))) nil)

;; hack kubernetes-tramp to use username as container name
(setcar (cdr (assoc 'tramp-login-args (assoc "kubectl" tramp-methods))) (list kubernetes-tramp-kubectl-options '("exec" "-it") '("-c" "%u") '("%h") '("sh")))

(defvar tramp-shell-hook nil "Hook called before starting a tramp shell")
(defvar tramp-shell-started-hook nil "Hook called after starting a tramp shell")

(defvar emacs-shell-input-ring-size 100000 "Size of input history ring in shell buffers.")

(defun emacs-shell-fill-input-ring (ring file-name)
  "Fill RING with contents of FILE-NAME."
  (when (file-exists-p file-name)
    (with-temp-buffer
      (insert-file-contents file-name)
      (goto-char (point-min))
      (while (not (eobp))
        (ring-insert ring (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
        (forward-line 1)))))

(defun emacs-shell (buffer-name directory history-file)
  (let* ((default-directory directory)
         (shell-buffer (shell (generate-new-buffer-name buffer-name))))
    (setq-local comint-input-ring-size emacs-shell-input-ring-size)
    (setq-local comint-input-ring (make-ring comint-input-ring-size))
    (setq-local comint-input-ring-file-name (concat emacs-shell-history-directory "/" history-file))
    (emacs-shell-fill-input-ring comint-input-ring comint-input-ring-file-name)
    (set-process-sentinel (get-buffer-process shell-buffer)
                          'shell-process-kill-buffer-sentinel)))

(defun tramp-shell (method host &optional history-name directory user)
  "Start an interactive shell on HOST using METHOD."
  (interactive "sMethod: \nsHost: ")
  (run-hooks 'tramp-shell-hook)
  (emacs-shell (concat method "-" host)
               (format "/%s:%s:%s" method (if user (format "%s@%s" user host) host) (or directory ""))
               (concat (or history-name host) "." method))
  (hack-connection-local-variables-apply `(:application 'emacs-shell :protocol ,method :host ,host))
  (run-hooks 'tramp-shell-started-hook))

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
    (completing-read "Container: " (docker-tramp--running-containers) nil t)
    (and current-prefix-arg (read-string "Directory: " "/"))))
  (tramp-shell "docker" container (docker-image-name container) directory))

(defun pod-owner-name (pod)
  (let ((owner (car (apply #'process-lines kubernetes-tramp-kubectl-executable (list "get" "pod" pod "-o" "jsonpath={.metadata.ownerReferences[].kind}")))))
    (cond ((string= owner "ReplicaSet") (replace-regexp-in-string "-[0-9a-f]\\{8,10\\}-[0-9a-z]\\{5\\}$" "" pod))
          ((string= owner "DaemonSet") (replace-regexp-in-string "-[0-9a-z]\\{5\\}$" "" pod))
          ((string= owner "StatefulSet") (replace-regexp-in-string "-[0-9]+$" "" pod))
          (t pod))))

(defun pod-shell (pod &optional container directory)
  "Start shell in Kubernetes pod"
  (interactive
   (let ((pod (completing-read "Pod: " (kubernetes-tramp--running-containers) nil t))
         (params nil))
     (setq params (append params (list pod)))
     (when current-prefix-arg
       (setq params (append params (list (completing-read "Container: " (split-string (car (apply #'process-lines kubernetes-tramp-kubectl-executable (list "get" "pod" pod "-o" "jsonpath={.spec.containers[*].name}"))) " " t) nil t))))
       (setq params (append params (list (read-string "Directory: " "/")))))
     params))
  (tramp-shell "kubectl" pod (pod-owner-name pod) directory (if (string= "" container) nil container)))

(defun localhost-shell ()
  "Start shell on localhost"
  (interactive)
  (emacs-shell "*shell*" "~/" "localhost"))

(defun shell-process-kill-buffer-sentinel (process state)
  (message "shell(%s): %s" process state)
  (let ((buffer (process-buffer process)))
    (when buffer
      (kill-buffer buffer))))

(defun shell-mode-cleanup ()
  (when (derived-mode-p 'shell-mode)
    (comint-write-input-ring)))

(add-hook 'kill-buffer-hook 'shell-mode-cleanup)

(defun shell-mode-buffers-write-comint-input-ring ()
  (mapc
   (lambda (buffer)
     (with-current-buffer buffer
       (when (derived-mode-p 'shell-mode)
         (comint-write-input-ring))))
   (buffer-list)))

(add-hook 'kill-emacs-hook 'shell-mode-buffers-write-comint-input-ring)

(defun emacs-shell-interrupt-password-command ()
  "Abort minibuffer password entry and interrupt command"
  (interactive)
  (with-selected-window (minibuffer-selected-window)
    (comint-interrupt-subjob))
  (abort-recursive-edit))

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
                         "if test `basename $0` = 'bash'; then echo running bash; else bash=`type bash` && bash=`echo $bash | grep -oE '[^ ]+$'` && echo run bash && exec $bash; fi")))
    (and (string-match "run.* bash" command-output) t)))

(defun emacs-shell-script-to-list (buffer)
  "Create list of commands from shell script in BUFFER."
  (let ((start 1)
        (script nil))
    (with-temp-buffer
      (insert-buffer-substring (or buffer (current-buffer)))
      (dolist (regexp '("\\(^\\| \\)#.*" "\\(^ +\\| +$\\)"))
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (replace-match "")))
      (flush-lines "^$" (point-min) (point-max))
      (goto-char (point-min))
      (while (not (eobp))
        (end-of-line)
        (cond ((looking-back "\\({\\|do\\|then\\|elif\\|else\\)" 4)
               (insert " "))
              (t (unless (looking-back "[\\\\;]" 1)
                   (insert ";"))))
        (unless (eobp)
          (delete-char 1))
        (setq script (append script (list (buffer-substring-no-properties start (point)))))
        (setq start (point)))
      script)))

(defun emacs-shell-compact-string-list (list max-length)
  "Compact LIST of strings to list with elements of MAX-LENGTH."
  (let ((batches nil)
        (current ""))
    (dolist (line list)
      (if (or (not max-length) (< (+ (length current) (length line)) max-length))
          (setq current (concat (replace-regexp-in-string "\\\\$" "" current) line))
        (setq batches (append batches (list current)))
        (setq current line)))
    (setq batches (append batches (list current)))
    batches))

(defvar emacs-shell-bashrc-batch-size 4000
  "Max size of batches of bashrc sourced on remote hosts.")

(defun emacs-shell-source-local-bashrc ()
  "Source bashrc from Emacs host in shell."
  (interactive)
  (when (file-readable-p "~/.bashrc")
    (let ((bashrc (with-temp-buffer
                    (insert-file-contents "~/.bashrc")
                    (emacs-shell-script-to-list (current-buffer)))))
      (dolist (batch (emacs-shell-compact-string-list bashrc emacs-shell-bashrc-batch-size))
        (emacs-shell-run-command-silently batch)))))

(defun emacs-shell-wait-for-prompt (times sleep)
  "Check for prompt n-times and sleep x ms between checks"
  (let ((i 0))
    (while (and (not (looking-back comint-prompt-regexp)) (< i times))
      (sleep-for 0 sleep)
      (setq i (1+ i)))))

(defun emacs-shell-start-bash-with-local-bashrc ()
  "Wait for prompt and source local bashrc."
  (emacs-shell-wait-for-prompt 10 100)
  (when (emacs-shell-exec-bash)
    (emacs-shell-source-local-bashrc)
    (comint-send-input)))

(add-hook 'tramp-shell-started-hook 'emacs-shell-start-bash-with-local-bashrc)

(global-set-key (kbd "S-C-n") 'localhost-shell)
(define-key read-passwd-map (kbd "C-c C-c") 'emacs-shell-interrupt-password-command)
(define-key shell-mode-map (kbd "C-p") 'comint-previous-input)
(define-key shell-mode-map (kbd "C-n") 'comint-next-input)

(setq shell-font-lock-keywords nil)
(setq comint-buffer-maximum-size 100000)
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
(add-hook 'comint-mode-hook
          #'(lambda () (setq comint-input-ignoredups t)))

(setq comint-password-prompt-regexp
      (concat comint-password-prompt-regexp
              "\\| (will be hidden): *\\'"
              "\\|^Password for [^:]+: *\\'"
              "\\|^Instance Password[^:]*: *\\'"
              "\\|^Enter .*password[^:]*: *\\'"))

(when (eq system-type 'darwin)
  (connection-local-set-profile-variables
   'mac-sudo-localhost
   '((emacs-shell-bashrc-batch-size . 1000)))
  (connection-local-set-profiles
   '(:application 'emacs-shell :protocol "sudo" :machine "localhost")
   'mac-sudo-localhost))

(provide 'emacs-shell)

;;; emacs-shell.el ends here
