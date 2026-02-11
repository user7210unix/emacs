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
- Doom modeline with nerd-icons
- JetBrains Mono + ligatures (if installed)
- Eglot + jdtls for Java (Eclipse LSP)
- Company, flycheck, yasnippet
- Highlight indent guides, rainbow delimiters
- Sensible defaults: no backup files, no lockfiles, auto-revert, recentf, savehist, saveplace
- Tango-dark theme
- Basic org and eshell tweaks

## Preview of the theme Tango-dark

<img src="https://lh3.googleusercontent.com/proxy/SCEvTpD5pFWF809u-RG9Xd2d88zfsJ2XR63PGmsW56j7-1FPuSmeEEsZjQIBEQZq9ZXnT9QGurrtthqN_laZfbxDv-bysO8MpZWpaiEhDsw0eWeEjlgQHoZ2xXsIw-C5" 
     alt="Tango-dark" 
     width="430" 
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
