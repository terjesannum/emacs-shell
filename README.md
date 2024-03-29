# emacs-shell

Setup tramp and shell-mode for interactive shells and remote file editing. Supports local shells, ssh, sudo, docker and kubernetes.

### Installation

Clone this repository and add to `load-path`:

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

Bash will be started if available, and your `.bashrc` on the emacs host will be sourced in the remote shell. Shell history will also be saved in
individual files on the emacs host (`.emacs.d/shell-history` directory), so history is kept between sessions even if pods have been restarted etc.

### Keybindings

In addition to the bindings from [shell-mode](https://www.gnu.org/software/emacs/manual/html_node/emacs/Shell-Mode.html), these keybindings are added:

| Keys      | Where          | Description               |
| --------- | -------------- | ------------------------- |
| `S-C-n`   | global         | Start new localhost shell |
| `C-p`     | shell-mode     | Previous history          |
| `C-n`     | shell-mode     | Next history              |
| `C-c C-c` | password input | Abort command             |
