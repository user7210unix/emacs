<div align="center">
  <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/0/08/EmacsIcon.svg/1280px-EmacsIcon.svg.png" width="96" alt="Emacs" />
  <h1>emacs.d</h1>
  <p>Emacs 30 configuration. Modular, fast, no evil-mode.</p>

  <img src="https://img.shields.io/badge/Emacs-30.2-blueviolet?style=flat-square&logo=gnuemacs&logoColor=white" alt="Emacs 30.2" />
  <img src="https://img.shields.io/badge/straight.el-package_manager-5c5c5c?style=flat-square" alt="straight.el" />
  <img src="https://img.shields.io/badge/theme-moe--dark-1c1c1c?style=flat-square&labelColor=222222&color=00d7ff" alt="moe-dark" />
  <img src="https://img.shields.io/badge/font-CaskaydiaCove_NF-orange?style=flat-square" alt="CaskaydiaCove NF" />
  <img src="https://img.shields.io/badge/platform-Linux_%7C_macOS_%7C_Windows-555?style=flat-square" alt="cross-platform" />
</div>

---

<div align="center">
  <br/>
  <img src="https://raw.github.com/kuanyui/moe-theme.el/master/pics/dark04.png" alt="moe-dark theme screenshot" width="860" />
</div>

---

## Overview

This is a personal Emacs 30 configuration structured as a collection of focused module files rather than one monolithic `init.el`. Each file owns a single concern. Packages are managed by `straight.el` by default with `package.el` + MELPA kept as a fallback for anything straight cannot resolve from its recipe repositories.

The design goal was to get something that behaves like a modern editor — instant completion popups, inline LSP diagnostics, a file tree, a minimap, a tab bar — without reaching for Doom Emacs or Spacemacs, and without evil-mode. Native Emacs keybindings throughout, with a small set of VSCode-parallel bindings layered on top where they make sense muscle-memory-wise.

---

## Architecture

```
~/.emacs.d/
├── early-init.el          # package.el suppression, GC, frame pre-init
├── init.el                # bootstrap straight.el, load modules in order
└── lisp/
    ├── config-os.el       # OS detection, font selection, platform paths
    ├── config-options.el  # core settings, ligatures, line numbers
    ├── config-theme.el    # moe-dark, rainbow-delimiters, nerd-icons
    ├── config-ui.el       # doom-modeline, centaur-tabs, dashboard, demap
    ├── config-completion.el  # corfu, vertico, consult, cape, orderless
    ├── config-lsp.el      # eglot, flymake, apheleia, sideline
    ├── config-treesitter.el  # tree-sitter grammars, mode remaps
    ├── config-dired.el    # dirvish, nerd-icons-dired, projectile
    ├── config-search.el   # consult-line, ripgrep, affe, deadgrep
    ├── config-keymaps.el  # all bindings in one place
    └── config-extra.el    # magit, eat, markdown, rust, web, org
```

`config-os.el` is loaded first and exports `my/os`, `my/font-family`, and `my/font-size`. Everything downstream reads those variables rather than hardcoding platform assumptions.

---

## Package Managers

Two managers coexist without conflict:

**straight.el** handles the majority — it clones packages from git, gives you reproducible builds, and supports arbitrary GitHub/GitLab sources via `:straight (:host github :repo "...")`.

**package.el + MELPA** is initialized manually in `init.el` as a fallback. Any package with `:straight nil :ensure t` in its `use-package` block goes through package.el instead. This covers packages like `moe-theme`, `doom-modeline`, `corfu`, and others that are on MELPA as tarballs but not in straight's recipe list.

`transient` is pinned and force-loaded early — before magit, rg, or anything else that uses it — to avoid the `transient--set-layout` void-function error that appears when Emacs 30's bundled transient version conflicts with what packages were compiled against.

---

## Completion

Completion uses the [minad stack](https://github.com/minad): `vertico` for minibuffer, `corfu` for in-buffer popups, `orderless` for fuzzy matching, `consult` for enhanced commands, `cape` for extra backends.

The popup is configured to appear immediately on the first character with zero delay. `TAB` inserts the top candidate. `RET` also accepts. The `corfu-separator` is set to `?\s` so orderless space-separated components work correctly inside the popup without dismissing it.

`completion-preview-mode` (Emacs 30 built-in) shows ghost-text inline across all buffers. `C-e` accepts the full suggestion, `M-f` accepts one word.

Eshell gets a separate lightweight setup: pcomplete + cape via `corfu-mode`, `M-r` for history search via `consult-history`, and a custom prompt. No external autosuggest packages.

---

## LSP

`eglot` (built-in since Emacs 29) handles language servers. It is configured with `jsonrpc` logging disabled for performance. Completion backends are merged using `cape-capf-super` so eglot, dabbrev, and file completion all feed into the same corfu popup.

`apheleia` handles async formatting — it runs formatters out-of-process and patches the buffer diff rather than replacing the whole buffer, so point does not jump.

Inline diagnostics come from `flymake-popon` (hover popups) and `sideline-flymake` (end-of-line annotations).

---

## File Tree and Minimap

`dirvish` overrides dired with a sidebar mode (`F8` or `C-c e`), preview pane, git status column, inline commit messages, and a tall header showing the current path with nerd icon breadcrumbs.

`demap` adds a detachable minimap buffer that tracks the active window (`F9` or `C-c m`). It is styled to match the moe-dark palette: dark grey background, cyan current-line highlight.

---

## OS Handling

`config-os.el` normalises the environment across all three platforms:

| | Linux | macOS | Windows |
|---|---|---|---|
| Shell | zsh / bash | zsh | pwsh / cmd |
| Font size | 12pt | 13pt | 11pt |
| `ls` flags | `--group-directories-first` | gls via coreutils | ls-lisp built-in |
| Modifier keys | default | Cmd = Meta, Opt = Super | default |
| Project paths | `~/Projects`, `~/.config` | `~/Developer` | `~/source` |

Font selection at startup checks whether CaskaydiaCove Nerd Font Mono is actually installed before applying it, falling back through JetBrainsMono NF to plain `monospace` so the config does not break on a fresh machine.

---

## Key Bindings

The full binding list lives in `config-keymaps.el`. The structure is a `C-c` prefix map with mnemonic sub-keys.

**Navigation**

| Binding | Action |
|---|---|
| `M-.` | go to definition (xref) |
| `M-,` | go back |
| `M-?` | find references |
| `M-j` | jump to visible char (avy) |
| `C-c j` | jump to line (avy) |
| `Shift + arrows` | move between windows |

**Buffers and files**

| Binding | Action |
|---|---|
| `C-s` | fuzzy line search in buffer (consult-line) |
| `C-S-s` | ripgrep project-wide |
| `C-p` | find file in project |
| `C-S-p` | command palette (M-x) |
| `C-w` | close current buffer |
| `C-c b b` | switch buffer |
| `C-c f r` | recent files |

**Layout**

| Binding | Action |
|---|---|
| `C-c e` / `F8` | file tree sidebar |
| `C-c m` / `F9` | minimap |
| `C-\` | split right |
| `C-c w s` | split below |
| `C-c w u / r` | undo/redo window layout |
| `` C-` `` | terminal (eat) |
| `C-PageUp/Down` | switch tab |

**LSP (active in eglot-managed buffers)**

| Binding | Action |
|---|---|
| `C-c l a` | code actions |
| `C-c l r` | rename symbol |
| `C-c l f` | format buffer |
| `C-c l i` | find implementation |
| `C-c l s` | workspace symbols |
| `C-c d n / p` | next / previous diagnostic |

---

## Warnings

All warning popups are suppressed via `early-init.el`:

```elisp
(setq warning-minimum-level                    :emergency
      warning-minimum-log-level                :warning
      native-comp-async-report-warnings-errors nil)
```

Warnings are still written to `*Warnings*` and readable with `M-x view-warnings`. They just never interrupt work.

---

## Notable Decisions

**No evil-mode.** The config uses native Emacs keybindings exclusively. A handful of VSCode-parallel bindings (`C-p`, `C-w`, `` C-` ``, `C-S-s`) are added on top where the muscle memory overlap is direct and unambiguous.

**straight.el over use-package's built-in elpaca/package.el.** Reproducibility and the ability to pin arbitrary git commits matters more than install speed. The MELPA fallback covers the edge cases.

**eglot over lsp-mode.** Built-in since Emacs 29, far less configuration surface, performance is comparable for most workflows. `jsonrpc` logging is disabled and `transient` is pinned early to avoid version drift issues.

**corfu over company.** Lighter, faster, composes cleanly with cape backends. The `global-corfu-mode` is activated in `:init` rather than `:config` to ensure it is in place before any other package touches `completion-in-region-function`.

**dirvish over neotree/treemacs.** It is a proper dired extension rather than a separate file browser, which means dired keybindings and dired functions all still work inside it.

---

## License

MIT
