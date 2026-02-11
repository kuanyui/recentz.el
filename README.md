# `recentz` - A KISS, minimalized replacement of built-in `recentf`
## Why

Because the behaviors of Emacs built-in `recentf` has frustrated me for over 10 years. I'm never able to understand what on earth it is doing.

## What

A simple and stupid recents list, without any confusing behaviors -- just do what you mean, like any other IDE or editor. (I've falled into self-doubt for years whenever thinking that I have to implement such feature for a text editor in 2024.)

### Characteristics

- Minimalized, no confusing behavior, no extra dependency.
- Never caches, even never stores anything in memory. (Everything are stored as a plaintext file in `~/.emacs.d/.recentz-data`.)
- Never loses any item after exiting Emacs. (Even an unexpected exit or crash)
- Always synchronized recents list between multiple Emacs instances.
- Always automatically removes items which is no longer existed in file system.
- Recorded lists: files / directories / projects (directories controlled by VC, ex: `git`)
<!--
- A minimalized alternative of `project-find-file` / `projectile-find-file` (the recently opened files will be on top of the list)
(global-set-key (kbd "C-x C-S-p") 'recentz-find-file-in-project)  ;; Optional. This feature requires external program `ag` (the_silver_searcher)
-->

> [!NOTE]
> 1. `Recentz` always check the availabiliy (via `file-exists-p`) of each items in the list (and remove the non-existent-anymore items), so it should be better to run on SSD if you set a large items amount limit.
> 2. On the other hand, the availability of the items opened via TRAMP will never be checked nor removed.

## Installation

```emacs-lisp
(require 'recentz)
(setq recentz-ignore-path-patterns '("/COMMIT_EDITMSG$" "~$" "/node_modules/"))

;; Force choose prefered completion UI. Available options: 'vanilla (default), 'helm, 'ido
;; If helm is available, recentz will auto choose it by default.
(setq recentz-ui 'helm)

(global-set-key (kbd "C-x C-r") 'recentz-files)   ;; Add universal argument prefix "C-u" (that is "C-u C-x C-r") can open the recent TRAMP-opened files instead.
(global-set-key (kbd "C-x C-d") 'recentz-directories)
(global-set-key (kbd "C-x C-p") 'recentz-projects)
(global-set-key (kbd "C-x C-S-r") 'recentz-tramp-files)         ;; (Optional, because it's equivalient to C-u + `recentz-files')
(global-set-key (kbd "C-x C-S-d") 'recentz-tramp-directories)   ;; (Optional, the reason is same above)
(global-set-key (kbd "C-x C-S-p") 'recentz-tramp-projects)      ;; (Optional, the reason is same above)
```

> [!TIP]
> You don't need to enable `ido-mode` nor `helm-mode` globally. `recentz` only use their minibuffer completion feature, for easily filtering candidates list.
>
> The followings are some recommended settings for various users:
>
> #### For Most Users
> - If you have already been using `vertico` + `orderless`, just use it, they will enhance the Emacs native `completing-read`. You don't need to do anything.
>   > - `vertico`: show candidates vertically.
>   > - `orderless`: fuzzy matching (still).
> - If you are using >= Emacs 28, a Emacs native fuzzy-matching + vertical completion global minor-mode is available. (⚠️ Warning: If you enable `fido-vertical-mode`,)
> ```emacs-lisp
> (fido-vertical-mode
> ```
> #### For Ido Users
> Fuzzy matching is provided by Ido itself (`ido-enable-flex-matching`). You don't need to do anything for fuzzy matching.
>
> ```emacs-lisp
> ;; Show Ido items vertically
> (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
>
> ;; Use up/down keys to navigate among Ido candidates
> (defun my-ido-bind-key-for-vertical ()
>   "Keybindings for vertically-displayed ido-mode candidates list.
> (Use up/down to navigate among candidates)"
>   (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
>   (define-key ido-completion-map (kbd "<up>")   'ido-prev-match))
> (add-hook 'ido-setup-hook #'my-ido-bind-key-for-vertical)
> ```
> #### For Helm Users
> Fuzzy matching is enabled by default. You don't need to do anything for fuzzy matching.
> ```emacs-lisp
> ;; Behavior when helm splits frame
> (setq helm-always-two-windows nil)
> (setq helm-split-window-inside-p t)
> ```

You may also want to add `.recentz-data` into `.gitignore`, if you manage your `.emacs.d` via `git`:

```shell
cd ~/.emacs.d && echo ".recentz-data" >> .gitignore
```

## License
GPLv3
