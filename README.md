# emacs-shell

Setup tramp and shell-mode for interactive shells and remote file editing. Supports local shells, ssh, sudo, docker and kubernetes.

### Requirements

* [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell)
* [emacs-bash-completion](https://github.com/szermatt/emacs-bash-completion)
* [docker-tramp.el](https://github.com/emacs-pe/docker-tramp.el)
* [kubernetes-tramp](https://github.com/gruggiero/kubernetes-tramp)

### Installation

Install requirements, clone this repository and add to `load-path`:

```lisp
(add-to-list 'load-path "~/.emacs.d/emacs-shell")
(require 'emacs-shell)
```

### Interactive shells

To start interactive shells, use these commands:

| Command               | Description                      |
| --------------------- | -------------------------------- |
| `M-x localhost-shell` | Shell on localhost               |
| `M-x ssh-shell`       | Shell on remote host             |
| `M-x sudo-shell`      | Root shell (localhost or remote) |
| `M-x docker-shell`    | Shell in local docker container  |
| `M-x pod-shell`       | Shell in kubernetes pod          |

### Keybindings

In addition to the bindings from [shell-mode](https://www.gnu.org/software/emacs/manual/html_node/emacs/Shell-Mode.html), these keybindings are added:

| Keys    | Description               |
| ------- | ------------------------- |
| `S-C-n` | Start new localhost shell |
| `C-p`   | Previous history          |
| `C-n`   | Next history              |
