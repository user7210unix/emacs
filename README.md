# 🚀 Minimal Emacs Configuration

A modern, minimal Emacs configuration focused on productivity and clean aesthetics.

## ✨ Features

- **🎨 Beautiful UI**: Doom themes with Challenger Deep color scheme
- **⚡ Modern Completion**: Corfu + Vertico + Orderless for lightning-fast completion
- **👁️ Enhanced Navigation**: Evil mode for Vim keybindings
- **🔧 Clean Interface**: Minimal GUI with doom-modeline
- **📱 Cross-platform**: macOS keybinding optimizations included

## 🛠️ What's Included

### Core Improvements
- 🚫 Removed GUI clutter (toolbar, scrollbar, splash screen)
- 📝 UTF-8 everywhere
- 🔤 Spaces over tabs (2-space indents)
- ✅ y/n prompts instead of yes/no

### Package Management
- 📦 **straight.el** - Modern package manager
- 🔧 **use-package** - Clean configuration syntax

### Completion & Navigation
- 🎯 **Corfu** - In-buffer completion popup
- 🔍 **Vertico** - Vertical minibuffer completion
- 🧠 **Orderless** - Flexible fuzzy matching
- 📚 **Marginalia** - Rich completion annotations  
- 🔭 **Consult** - Enhanced search and navigation
- 🗝️ **Which-key** - Discoverable keybindings

### Visual Enhancements
- 🎨 **Doom Themes** - Challenger Deep theme
- 📊 **Doom Modeline** - Modern status bar
- 🔢 **Line Numbers** - Absolute line numbering
- 🎯 **Nerd Icons** - Beautiful file icons
- 🔤 **Iosevka Nerd Font** - Programming-optimized font

### Editing
- ⚔️ **Evil Mode** - Vim keybindings for efficient editing
- 🍎 **macOS Integration** - Proper modifier key mapping

## 🚀 Quick Start

1. **Backup your existing config**:
   ```bash
   mv ~/.emacs.d ~/.emacs.d.backup
   ```

2. **Create new config directory**:
   ```bash
   mkdir ~/.emacs.d
   ```

3. **Add the configuration**:
   ```bash
   # Copy the elisp code to ~/.emacs.d/init.el
   ```

4. **Install Iosevka Nerd Font**:
   - Download from [Nerd Fonts](https://github.com/ryanoasis/nerd-fonts)
   - Install system-wide

5. **Start Emacs**:
   - First launch will automatically install all packages
   - Restart after initial setup for best results

## 📋 Requirements

- **Emacs 27.1+** (recommended: 29.1+)
- **Iosevka Nerd Font** (or modify font settings)
- **Internet connection** (for initial package installation)

## ⚙️ Customization

### 🎨 Change Theme
```elisp
;; Replace 'doom-challenger-deep with any doom theme:
(load-theme 'doom-one t)           ; Dark theme
(load-theme 'doom-solarized-light t) ; Light theme
```

### 🔤 Change Font
```elisp
;; Modify font family and size:
(set-face-attribute 'default nil 
  :font "JetBrains Mono" 
  :height 140) ; Height in 1/10pt (140 = 14pt)
```

### ⌨️ Disable Evil Mode
```elisp
;; Comment out or remove the evil configuration:
;; (use-package evil
;;   :demand
;;   :config
;;   (evil-mode 1))
```

## 🔧 Key Bindings

| Binding | Action | Mode |
|---------|--------|------|
| `j/k` | Move up/down | Evil |
| `C-x C-f` | Find file | Global |
| `C-c C-c` | Execute | Context |
| `SPC` | Which-key helper | Evil (after setup) |

## 📁 File Structure

```
~/.emacs.d/
├── init.el                 # Main configuration
├── straight/              # Package management
│   └── repos/            # Package repositories
└── auto-save-list/       # Auto-save files
```

## 🤝 Contributing

Feel free to fork this configuration and make it your own! Some ideas:

- 🌐 Add language-specific packages (LSP, syntax highlighting)
- 🔧 Include project management tools (Projectile, Magit)
- 📝 Extend with org-mode configuration
- 🎨 Try different themes and fonts

## 📝 License

This configuration is free to use and modify. Individual packages maintain their own licenses.

## 🙏 Acknowledgments

- 📦 [straight.el](https://github.com/radian-software/straight.el) - Package management
- 🎨 [Doom Themes](https://github.com/doomemacs/themes) - Beautiful themes
- ⚡ [Corfu](https://github.com/minad/corfu) - Completion framework
- 🔍 [Vertico](https://github.com/minad/vertico) - Minibuffer completion

---

**Happy Emacs-ing!** 🎉

> *"Emacs is not just an editor, it's a way of life."*
