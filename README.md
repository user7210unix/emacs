<div align="center">


<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/0/08/EmacsIcon.svg/1024px-EmacsIcon.svg.png" align="center" alt=" Preview" width="240" style="display: block; margin: 32px auto; border: 2px solid #555; border-radius: 12px; box-shadow: 0 4px 10px rgba(0, 0, 0, 0.3);">


<div align="center">


# 🚀 Emacs Living Environment

<div align="center">

![Emacs](https://img.shields.io/badge/Emacs-30.2+-7F5AB6?style=for-the-badge&logo=gnu-emacs&logoColor=white)
![Gentoo](https://img.shields.io/badge/Gentoo-54487A?style=for-the-badge&logo=gentoo&logoColor=white)
![License](https://img.shields.io/badge/License-GPL--3.0-blue?style=for-the-badge)
![Vim Keybindings](https://img.shields.io/badge/Vim-Keybindings-019733?style=for-the-badge&logo=vim&logoColor=white)

*Transform Emacs into a complete living environment - IDE, browser, media player, and more!*

[Features](#-features) • [Installation](#-installation) • [Keybindings](#-keybindings) • [Screenshots](#-screenshots) • [Contributing](#-contributing)

</div>

<div align="left">


## 📖 Overview

This configuration transforms GNU Emacs into a comprehensive living environment where you can code, browse the web, play media, manage files, and perform virtually any computing task without leaving your editor. Built specifically for **Gentoo Linux** with modern package management using **straight.el**.

### 🎯 Philosophy

- **Live Inside Emacs**: Everything you need in one unified interface
- **Vim-First**: Evil mode with comprehensive Vim keybindings
- **Modern & Beautiful**: Contemporary themes and UI components  
- **Performance Focused**: Optimized startup and runtime performance
- **Gentoo Native**: Full integration with Gentoo's package management

## ✨ Features

### 🛠️ Complete IDE Experience
- **Language Server Protocol (LSP)** support for C, C++, Java, Python, JavaScript, TypeScript
- **Advanced auto-completion** with Company mode and Company-box
- **Real-time syntax checking** with Flycheck
- **Project management** with Projectile and Treemacs
- **Git integration** with Magit and git-gutter
- **Tree-sitter** syntax highlighting

### 🎨 Modern Interface
- **Doom themes** with beautiful color schemes
- **Doom modeline** with informative status bar
- **All-the-icons** for beautiful file and mode icons
- **JetBrains Mono** font with ligatures support
- **Rainbow delimiters** and syntax highlighting
- **Relative line numbers** and visual enhancements

### 🌐 Web Browsing
- **EWW browser** for text-based web browsing
- **XWidget WebKit** for full-featured web browsing (when available)
- **Integrated search** and bookmarking

### 🎵 Multimedia Center
- **EMMS** (Emacs MultiMedia System) for music playback
- **VLC integration** for video files
- **MPV support** for media playback
- **Full codec support** (MP3, MP4, FLAC, etc.)

### 📁 File Management
- **Enhanced Dired** with icons and modern features
- **Dired-single** for streamlined navigation
- **PDF Tools** for PDF viewing and annotation
- **Archive support** for various formats

### 💻 Terminal Integration
- **VTerm** for full-featured terminal emulation
- **Multi-term** support for multiple terminals
- **Shell integration** with modern shell features

### ⚡ Productivity Tools
- **Org mode** with bullets and modern styling  
- **Which-key** for keybinding discovery
- **Ivy/Counsel/Swiper** for enhanced completion
- **Dashboard** with beautiful startup screen
- **Winner mode** for window management

### 🎮 Vim Experience
- **Evil mode** with comprehensive Vim emulation
- **Evil collection** for consistent Vim bindings
- **Evil commentary** for easy commenting
- **Evil surround** for text object manipulation
- **Leader key** system (Space-based)

## 📥 Installation

### Prerequisites

- **Gentoo Linux** (primary target)
- **Emacs 30.2+** with GUI support
- **Git** for package management

### 🔧 Gentoo Setup

1. **Configure USE flags** for Emacs:
```bash
# Add to /etc/portage/package.use/emacs
doas tee -a /etc/portage/package.use/emacs << 'EOF'
app-editors/emacs X gtk gui xwidgets webkit sound alsa imagemagick svg png jpeg gif tiff xml json ssl gnutls zlib threads harfbuzz cairo dbus gzip-el acl tree-sitter dynamic-loading modules toolkit-scroll-bars wide-int mailutils xft libxml2 gfile gsettings webp sqlite
EOF
```

2. **Install modern audio system**:
```bash
# Remove old pulseaudio
doas emerge --unmerge media-sound/pulseaudio

# Install PipeWire
doas emerge -av media-video/pipewire media-sound/alsa-utils
```

3. **Install Emacs and dependencies**:
```bash
# Core installation
doas emerge -av app-editors/emacs

# Development tools
doas emerge -av dev-util/clangd dev-java/openjdk:17 sys-devel/clang

# Media support
doas emerge -av media-video/vlc media-video/mpv

# Utilities and fonts
doas emerge -av sys-apps/ripgrep sys-apps/fd media-fonts/jetbrains-mono
```

### 📋 Configuration Installation

1. **Backup existing configuration**:
```bash
mv ~/.emacs.d ~/.emacs.d.backup
```

2. **Clone this repository**:
```bash
git clone https://github.com/user7210unix/emacs/tree/emacs-living-env ~/.emacs.d
```

3. **Install fonts** (essential for icons):
```bash
# Fonts are auto-installed on first run, or manually:
doas emerge -av media-fonts/jetbrains-mono media-fonts/fira-code media-fonts/noto-emoji
```

4. **Setup audio permissions**:
```bash
doas usermod -a -G audio $USER
# Log out and back in for changes to take effect
```

5. **First launch**:
```bash
emacs --debug-init
```

The configuration will automatically:
- Install all packages via straight.el
- Download and install icon fonts
- Configure LSP servers
- Set up modern themes

### 🔍 Language Server Installation

After Emacs is running, install LSP servers:

```bash
# Python
pip install --user python-lsp-server[all]

# JavaScript/TypeScript  
npm install -g typescript-language-server typescript

# Web development
npm install -g vscode-langservers-extracted

# Additional servers (optional)
npm install -g bash-language-server yaml-language-server
```

## 📖 Documentation

- **[Keybindings Reference](keybinds.md)** - Complete keybinding documentation
- **[Troubleshooting Guide](docs/troubleshooting.md)** - Common issues and solutions
- **[Customization Guide](docs/customization.md)** - How to customize the configuration

## 🎮 Quick Start

### Basic Navigation (Vim-style)
- `h/j/k/l` - Move left/down/up/right
- `Space` - Leader key for all major functions
- `Space f f` - Find file
- `Space b b` - Switch buffer  
- `Space p p` - Switch project

### Essential Leader Commands
- `Space f` - File operations
- `Space b` - Buffer management  
- `Space w` - Window management
- `Space p` - Project operations
- `Space g` - Git operations
- `Space h` - Help system

### Media Control
- `C-c m p` - Play/pause music
- `C-c m n` - Next track
- `C-c m l` - Show playlist

See the full [Keybindings Reference](keybinds.md) for complete documentation.

## 📷 Screenshots

<details>
<summary>🖼️ Click to view screenshots</summary>

### Main Interface
![Main Interface](screenshots/main-interface.png)

### Code Editing with LSP
![Code Editing](screenshots/code-editing.png)

### Web Browsing
![Web Browser](screenshots/web-browser.png)

### Media Player
![Media Player](screenshots/media-player.png)

### Project Management
![Project Tree](screenshots/project-tree.png)

</details>

## 📁 Project Structure

```
~/.emacs.d/
├── init.el              # Main configuration file
├── early-init.el        # Early initialization (prevents package.el conflicts)
├── custom.el           # Auto-generated customizations
├── keybinds.md         # Comprehensive keybinding documentation  
├── docs/               # Additional documentation
│   ├── troubleshooting.md
│   ├── customization.md
│   └── gentoo-setup.md
├── straight/           # Package management (auto-generated)
└── auto-saves/         # Auto-save files
```

## 🛠️ Key Technologies

- **[straight.el](https://github.com/radian-software/straight.el)** - Modern package management
- **[Evil](https://github.com/emacs-evil/evil)** - Vim emulation
- **[LSP Mode](https://emacs-lsp.github.io/lsp-mode/)** - Language Server Protocol
- **[Doom Themes](https://github.com/doomemacs/themes)** - Beautiful color schemes
- **[EMMS](https://www.gnu.org/software/emms/)** - Multimedia system
- **[Treemacs](https://github.com/Alexander-Miller/treemacs)** - Project tree viewer
- **[Magit](https://magit.vc/)** - Git porcelain

## 🎛️ Customization

### Changing Themes
```elisp
;; In init.el, change the theme
(load-theme 'doom-dracula t)  ; or doom-molokai, doom-tokyo-night, etc.
```

### Adding Languages
```elisp
;; Add to LSP configuration
(use-package lsp-mode
  :hook ((your-language-mode . lsp-deferred)))
```

### Custom Keybindings
```elisp
;; Add to leader key configuration
(my/leader-keys
  "x" '(:ignore t :which-key "custom")
  "xc" '(your-custom-function :which-key "custom function"))
```

## 🐛 Troubleshooting

### Common Issues

1. **Package conflicts**: Clear `~/.emacs.d/elpa` if it exists
2. **Missing icons**: Run `M-x all-the-icons-install-fonts`
3. **LSP not working**: Install language servers manually
4. **Audio issues**: Check PipeWire configuration

See [Troubleshooting Guide](docs/troubleshooting.md) for detailed solutions.

### Getting Help

- Check `*Messages*` buffer: `C-h e`
- Debug mode: `emacs --debug-init`
- Package installation logs in `*straight-process*`

## 🤝 Contributing

Contributions are welcome! Please read our [Contributing Guidelines](CONTRIBUTING.md) first.

### Areas for Contribution
- Additional language support
- Theme improvements  
- Performance optimizations
- Documentation enhancements
- Bug fixes and testing

### Development Setup
```bash
# Fork and clone
git clone https://github.com/yourusername/emacs-living-environment.git
cd emacs-living-environment

# Create feature branch
git checkout -b feature/your-feature

# Make changes and test
emacs --debug-init

# Submit pull request
```

## 📜 License

This project is licensed under the **GPL-3.0 License** - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- **[System Crafters](https://github.com/systemcrafters/crafted-emacs)** - Inspiration for modern Emacs configuration
- **[Elegant Emacs](https://github.com/rougier/elegant-emacs)** - Beautiful UI design principles  
- **[Awesome Emacs](https://github.com/emacs-tw/awesome-emacs)** - Comprehensive package resources
- **[Doom Emacs](https://github.com/doomemacs/doomemacs)** - Performance and UI inspiration
- **[Spacemacs](https://www.spacemacs.org/)** - Leader key system design

## 📊 Stats

![GitHub stars](https://img.shields.io/github/stars/yourusername/emacs-living-environment?style=social)
![GitHub forks](https://img.shields.io/github/forks/yourusername/emacs-living-environment?style=social)
![GitHub issues](https://img.shields.io/github/issues/yourusername/emacs-living-environment)
![GitHub last commit](https://img.shields.io/github/last-commit/yourusername/emacs-living-environment)

---

<div align="center">

**[⬆ Back to Top](#-emacs-living-environment)**

Made with ❤️ for the Emacs and Gentoo communities

*"Living the dream, one keystroke at a time"*

</div>
