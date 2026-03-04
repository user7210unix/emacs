# My Emacs Configuration

<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/5/5f/Emacs-logo.svg/330px-Emacs-logo.svg.png" 
     alt="Emacs" 
     width="140" 
     style="display: block; margin: 0 auto 1.5em;">

Minimal, clean, fast Emacs setup focused on practical editing and Java development.

Uses modern built-in tools where possible, `use-package` for everything else.

## What this config does

- Optimised startup (GC tuning, lazy loading)
- Modern completion: vertico + orderless + consult + marginalia + embark
- Eglot + jdtls for Java (Eclipse LSP)
- Company, flycheck, yasnippet
- Highlight indent guides, rainbow delimiters
- Sensible defaults: no backup files, no lockfiles, auto-revert, recentf, savehist, saveplace
- Doom-nord-light theme
- Basic org and
- Eat shell

## Preview of the theme Doom-nord-light

<img src="https://external-preview.redd.it/ZsrRaB5Tc_NjNTcPo6Mc6jMML7u8XgGWZkUN8MBqs4o.jpg?auto=webp&s=21d7dbd32ddad44f97987ac6c353f6f5a918e5bb" 
     alt="Tango-dark" 
     width="830" 
     style="display: block; margin: 0 auto 1.5em;">
## Installation

    git clone https://github.com/user7210unix/emacs ~/.emacs.d

Start Emacs. Packages install automatically.

For Java LSP support:

1. Download latest Eclipse JDT Language Server  
   https://download.eclipse.org/jdtls/milestones/
2. Extract to `~/.local/share/jdtls`
3. Open a .java file → Eglot starts automatically

Recommended font: JetBrainsMono Nerd Font  
https://www.nerdfonts.com/font-downloads

## Key bindings worth remembering

    C-s             consult-line
    C-x b           consult-buffer
    C-x C-r         consult-recent-file
    M-g g           consult-goto-line
    C-.             embark-act
    M-.             embark-dwim

    C-c e           eshell
    C-c c           compile
    C-c r           recompile
    C-c k           kill-this-buffer

    C-c l a         eglot-code-actions
    C-c l r         eglot-rename
    C-c l f         eglot-format
    C-c l o         organize imports

## Notes

- Most packages lazy-load by default (`:defer t`)
- Java LSP workspace is per-project in `~/.cache/jdtls-workspace`
- No huge framework (doom, spacemacs, etc.) – just readable elisp
- Custom file goes to `custom.el` and is never versioned
