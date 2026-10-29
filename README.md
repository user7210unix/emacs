## Emacs Configuration

> About EAF (emacs-application-framework)
> A free/libre and open-source extensible framework that revolutionizes the graphical capabilities of Emacs.
The key to ultimately Live in Emacs

> [!NOTE]
> Before starting emacs with this configuration the [emacs-application-framework](https://github.com/emacs-eaf/emacs-application-framework)

## EAF

```
Install

EAF supports Linux X11, Windows, macOS and FreeBSD. The installation method is very simple.

If you use Nix、 macOS or Gentoo， you need check Wiki first
1. Download EAF

git clone --depth=1 -b master https://github.com/emacs-eaf/emacs-application-framework.git ~/.emacs.d/site-lisp/emacs-application-framework/

2. Install/Update EAF applications and dependencies

You can use M-x eaf-install-and-update or manually run the install-eaf.py script in the EAF directory:

cd emacs-application-framework
chmod +x ./install-eaf.py
./install-eaf.py

There are many useful flags available for install-eaf.py, check it yourself using --help.
```

## FileTree

```file-tree
├── lisp
│   ├── my-completion.el
│   ├── my-core.el
│   ├── my-eaf.el
│   ├── my-icons.el
│   ├── my-java.el
│   ├── my-modeline.el
│   ├── my-org-capture.el
│   ├── my-org-export.el
│   ├── my-org.el
│   ├── my-prog.el
│   ├── my-scratch.el
│   ├── my-settings.el
│   ├── my-sidebar.el
│   └── my-ui.el
│
├── hannover-theme
│   ├── COPYING
│   ├── hannover-adaptive-theme.el
│   ├── hannover-day-theme.el
│   ├── hannover-midnight-theme.el
│   ├── hannover-night-theme.el
│   └── hannover-theme.el
│
├── early-init.el
└── init.el
```
