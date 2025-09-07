# 🎮 Emacs Living Environment - Complete Keybinding Reference

<div align="center">

*Your comprehensive guide to mastering the Emacs Living Environment*

**[Basic Navigation](#-basic-navigation)** • **[Leader Keys](#-leader-key-system-space)** • **[Programming](#-programming--ide)** • **[Media](#-media-control)** • **[Web](#-web-browsing)** • **[Terminal](#-terminal)** • **[Quick Reference](#-quick-reference-card)**

</div>

---

## 📖 Table of Contents

1. [**Basic Navigation**](#-basic-navigation) - Essential movement and editing
2. [**Leader Key System**](#-leader-key-system-space) - Space-based command system
3. [**Programming & IDE**](#-programming--ide) - Development tools and LSP
4. [**Media Control**](#-media-control) - Music and video playback
5. [**Web Browsing**](#-web-browsing) - Internet navigation
6. [**File Management**](#-file-management-dired) - File operations
7. [**Terminal**](#-terminal) - Command line integration  
8. [**Window Management**](#-window-management) - Layout control
9. [**Git Operations**](#-git-operations-magit) - Version control
10. [**Search & Navigation**](#-search--navigation) - Finding and moving
11. [**Org Mode**](#-org-mode) - Note-taking and planning
12. [**General Emacs**](#-general-emacs-commands) - Core editor functions
13. [**Quick Reference**](#-quick-reference-card) - Cheat sheet

---

## 🧭 Basic Navigation

*Essential Vim-style movement and editing commands*

### Movement Commands
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `h` | ← Move left | Normal | Move cursor one character left |
| `j` | ↓ Move down | Normal | Move cursor one line down |
| `k` | ↑ Move up | Normal | Move cursor one line up |
| `l` | → Move right | Normal | Move cursor one character right |
| `w` | Next word | Normal | Jump to beginning of next word |
| `W` | Next WORD | Normal | Jump to next whitespace-separated word |
| `b` | Previous word | Normal | Jump to beginning of previous word |
| `B` | Previous WORD | Normal | Jump to previous whitespace-separated word |
| `e` | End of word | Normal | Jump to end of current word |
| `E` | End of WORD | Normal | Jump to end of current WORD |

### Line Navigation
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `0` | Line start | Normal | Move to beginning of line |
| `^` | First non-blank | Normal | Move to first non-whitespace character |
| `$` | Line end | Normal | Move to end of line |
| `g g` | File start | Normal | Jump to first line of buffer |
| `G` | File end | Normal | Jump to last line of buffer |
| `{number}G` | Go to line | Normal | Jump to specific line number |
| `%` | Matching bracket | Normal | Jump to matching parenthesis/bracket |

### Screen Navigation  
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `H` | Top of screen | Normal | Move to highest line on screen |
| `M` | Middle of screen | Normal | Move to middle line on screen |
| `L` | Bottom of screen | Normal | Move to lowest line on screen |
| `C-f` | Page down | Normal | Scroll full page down |
| `C-b` | Page up | Normal | Scroll full page up |
| `C-d` | Half page down | Normal | Scroll half page down |
| `C-u` | Half page up | Normal | Scroll half page up |
| `z z` | Center screen | Normal | Center current line on screen |

### Text Objects and Marks
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `m{letter}` | Set mark | Normal | Set mark at current position |
| `'{letter}` | Go to mark | Normal | Jump to marked position |
| `''` | Previous position | Normal | Jump to previous position |
| `f{char}` | Find character | Normal | Jump to next occurrence of character |
| `F{char}` | Find backward | Normal | Jump to previous occurrence of character |
| `t{char}` | Till character | Normal | Jump to before next occurrence |
| `T{char}` | Till backward | Normal | Jump to after previous occurrence |
| `;` | Repeat find | Normal | Repeat last f/F/t/T command |
| `,` | Reverse find | Normal | Reverse last f/F/t/T command |

---

## 🎯 Basic Editing

*Fundamental text editing operations*

### Entering Insert Mode
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `i` | Insert before cursor | Normal | Enter insert mode at cursor |
| `I` | Insert at line start | Normal | Enter insert mode at beginning of line |
| `a` | Insert after cursor | Normal | Enter insert mode after cursor |
| `A` | Insert at line end | Normal | Enter insert mode at end of line |
| `o` | Open line below | Normal | Create new line below and enter insert mode |
| `O` | Open line above | Normal | Create new line above and enter insert mode |
| `s` | Substitute character | Normal | Delete character and enter insert mode |
| `S` | Substitute line | Normal | Delete line and enter insert mode |

### Deletion Commands
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `x` | Delete character | Normal | Delete character under cursor |
| `X` | Delete before cursor | Normal | Delete character before cursor |
| `d w` | Delete word | Normal | Delete from cursor to end of word |
| `d b` | Delete word backward | Normal | Delete from cursor to beginning of word |
| `d d` | Delete line | Normal | Delete entire line |
| `D` | Delete to end of line | Normal | Delete from cursor to end of line |
| `d $` | Delete to line end | Normal | Same as D |
| `d 0` | Delete to line start | Normal | Delete from cursor to beginning of line |
| `d t{char}` | Delete till character | Normal | Delete up to (but not including) character |

### Change Commands
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `c w` | Change word | Normal | Delete word and enter insert mode |
| `c c` | Change line | Normal | Delete line and enter insert mode |
| `C` | Change to end of line | Normal | Delete to end of line and enter insert mode |
| `c t{char}` | Change till character | Normal | Change up to character |
| `c i w` | Change inner word | Normal | Change word under cursor |
| `c i "` | Change inner quotes | Normal | Change text inside quotes |
| `c i (` | Change inner parentheses | Normal | Change text inside parentheses |
| `c a w` | Change around word | Normal | Change word including surrounding whitespace |

### Copy and Paste
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `y w` | Copy word | Normal | Copy word from cursor |
| `y y` | Copy line | Normal | Copy entire line |
| `Y` | Copy line | Normal | Same as y y |
| `y $` | Copy to end of line | Normal | Copy from cursor to end of line |
| `p` | Paste after cursor | Normal | Paste after cursor/current line |
| `P` | Paste before cursor | Normal | Paste before cursor/current line |
| `" + y` | Copy to system clipboard | Normal | Copy selection to system clipboard |
| `" + p` | Paste from system clipboard | Normal | Paste from system clipboard |

### Undo and Redo
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `u` | Undo | Normal | Undo last change |
| `C-r` | Redo | Normal | Redo undone change |
| `U` | Undo line | Normal | Undo all changes on current line |

---

## 👁️ Visual Mode

*Text selection and visual editing*

### Visual Mode Types
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `v` | Visual character mode | Normal | Select characters |
| `V` | Visual line mode | Normal | Select entire lines |
| `C-v` | Visual block mode | Normal | Select rectangular blocks |
| `g v` | Reselect visual | Normal | Reselect previous visual selection |

### Visual Mode Operations
| Keybinding | Action | Context | Visual | Description |
|------------|--------|---------|--------|-------------|
| `d` | Delete selection | Visual | Delete selected text |
| `y` | Copy selection | Visual | Copy selected text |
| `c` | Change selection | Visual | Delete selection and enter insert mode |
| `>` | Indent selection | Visual | Indent selected lines |
| `<` | Unindent selection | Visual | Unindent selected lines |
| `=` | Auto-indent | Visual | Auto-indent selected lines |
| `u` | Lowercase | Visual | Convert selection to lowercase |
| `U` | Uppercase | Visual | Convert selection to uppercase |

---

## 🔍 Search and Replace

*Finding and replacing text*

### Search Commands
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `/` | Search forward | Normal | Search for pattern forward |
| `?` | Search backward | Normal | Search for pattern backward |
| `n` | Next match | Normal | Jump to next search result |
| `N` | Previous match | Normal | Jump to previous search result |
| `*` | Search word forward | Normal | Search for word under cursor forward |
| `#` | Search word backward | Normal | Search for word under cursor backward |
| `g *` | Search word (partial) | Normal | Search for partial word under cursor |
| `g #` | Search word backward (partial) | Normal | Search backward for partial word |

### Replace Commands
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `:s/old/new/` | Replace on line | Command | Replace first occurrence on current line |
| `:s/old/new/g` | Replace all on line | Command | Replace all occurrences on current line |
| `:%s/old/new/g` | Replace all in file | Command | Replace all occurrences in file |
| `:%s/old/new/gc` | Replace with confirmation | Command | Replace all with confirmation prompts |

---

## 🎮 Leader Key System (Space)

*The heart of the configuration - Space-based command system*

### 📁 File Operations (`SPC f`)
| Keybinding | Action | Description |
|------------|--------|-------------|
| `SPC f f` | Find file | Open file browser |
| `SPC f r` | Recent files | Show recently opened files |
| `SPC f s` | Save file | Save current buffer |
| `SPC f S` | Save all | Save all modified buffers |
| `SPC f R` | Rename file | Rename current file |
| `SPC f D` | Delete file | Delete current file |
| `SPC f y` | Copy file path | Copy current file path to clipboard |
| `SPC f E` | Edit as root | Edit current file with elevated privileges |

### 🗂️ Buffer Management (`SPC b`)
| Keybinding | Action | Description |
|------------|--------|-------------|
| `SPC b b` | Switch buffer | Switch to another buffer |
| `SPC b k` | Kill buffer | Close current buffer |
| `SPC b K` | Kill other buffers | Close all buffers except current |
| `SPC b l` | List buffers | Show buffer list |
| `SPC b r` | Rename buffer | Rename current buffer |
| `SPC b R` | Revert buffer | Reload buffer from disk |
| `SPC b s` | Switch to scratch | Switch to *scratch* buffer |
| `SPC b m` | Switch to messages | Switch to *Messages* buffer |

### 🪟 Window Management (`SPC w`)
| Keybinding | Action | Description |
|------------|--------|-------------|
| `SPC w h` | Move to left window | Focus window to the left |
| `SPC w j` | Move to down window | Focus window below |
| `SPC w k` | Move to up window | Focus window above |
| `SPC w l` | Move to right window | Focus window to the right |
| `SPC w s` | Split horizontal | Split window horizontally |
| `SPC w v` | Split vertical | Split window vertically |
| `SPC w d` | Delete window | Close current window |
| `SPC w o` | Delete other windows | Close all windows except current |
| `SPC w m` | Maximize window | Maximize current window |
| `SPC w =` | Balance windows | Make all windows same size |
| `SPC w u` | Undo window change | Undo last window configuration change |
| `SPC w r` | Redo window change | Redo window configuration change |

### 📋 Project Management (`SPC p`)
| Keybinding | Action | Description |
|------------|--------|-------------|
| `SPC p p` | Switch project | Change to different project |
| `SPC p f` | Find file in project | Search for file in current project |
| `SPC p g` | Grep in project | Search for text in project files |
| `SPC p r` | Replace in project | Find and replace across project |
| `SPC p d` | Find directory | Find directory in project |
| `SPC p b` | Switch to project buffer | Switch to buffer within project |
| `SPC p k` | Kill project buffers | Close all buffers in project |
| `SPC p D` | Dired project root | Open project root in file manager |
| `SPC p !` | Run shell command | Execute shell command in project root |
| `SPC p c` | Compile project | Run project compilation command |

### 🌿 Git Operations (`SPC g`)
| Keybinding | Action | Description |
|------------|--------|-------------|
| `SPC g s` | Git status | Open Magit status buffer |
| `SPC g c` | Git commit | Create git commit |
| `SPC g p` | Git push | Push changes to remote |
| `SPC g P` | Git pull | Pull changes from remote |
| `SPC g f` | Git fetch | Fetch from remote |
| `SPC g l` | Git log | Show git log |
| `SPC g b` | Git blame | Show git blame for current file |
| `SPC g t` | Git time machine | Browse file's git history |
| `SPC g d` | Git diff | Show git diff |
| `SPC g S` | Git stash | Stash current changes |

### 🎚️ Toggle Commands (`SPC t`)
| Keybinding | Action | Description |
|------------|--------|-------------|
| `SPC t t` | Choose theme | Select and apply color theme |
| `SPC t n` | Toggle line numbers | Show/hide line numbers |
| `SPC t r` | Toggle relative numbers | Switch between absolute/relative line numbers |
| `SPC t w` | Toggle word wrap | Enable/disable word wrapping |
| `SPC t h` | Toggle highlight line | Highlight current line |
| `SPC t i` | Toggle indent guides | Show/hide indentation guides |
| `SPC t f` | Toggle fill column | Show/hide fill column indicator |
| `SPC t F` | Toggle fullscreen | Toggle fullscreen mode |
| `SPC t m` | Toggle menu bar | Show/hide menu bar |
| `SPC t T` | Toggle tool bar | Show/hide tool bar |

### ❓ Help System (`SPC h`)
| Keybinding | Action | Description |
|------------|--------|-------------|
| `SPC h f` | Describe function | Get help for Emacs function |
| `SPC h v` | Describe variable | Get help for Emacs variable |
| `SPC h k` | Describe key | Show what a key binding does |
| `SPC h m` | Describe mode | Get help for current major mode |
| `SPC h b` | Describe bindings | List all current key bindings |
| `SPC h a` | Apropos | Search help for pattern |
| `SPC h i` | Info manual | Browse Emacs info manuals |
| `SPC h w` | Where is command | Find key binding for command |

### ⚙️ Configuration (`SPC f e`)
| Keybinding | Action | Description |
|------------|--------|-------------|
| `SPC f e d` | Edit config | Open init.el configuration file |
| `SPC f e r` | Reload config | Reload Emacs configuration |
| `SPC f e R` | Restart Emacs | Restart Emacs with current configuration |

---

## 💻 Programming & IDE

*Development tools and language server features*

### 🔧 LSP (Language Server Protocol)
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `C-c l r r` | LSP rename | LSP | Rename symbol across project |
| `C-c l g g` | Go to definition | LSP | Jump to symbol definition |
| `C-c l g r` | Find references | LSP | Show all references to symbol |
| `C-c l g i` | Go to implementation | LSP | Jump to symbol implementation |
| `C-c l g t` | Go to type definition | LSP | Jump to type definition |
| `C-c l a a` | Code actions | LSP | Show available code actions |
| `C-c l a l` | Lens actions | LSP | Execute code lens action |
| `C-c l f f` | Format buffer | LSP | Format entire buffer |
| `C-c l f r` | Format region | LSP | Format selected region |
| `C-c l h` | Hover documentation | LSP | Show symbol documentation |

### 📝 Company (Auto-completion)
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `TAB` | Complete | Company | Trigger or cycle completion |
| `S-TAB` | Complete previous | Company | Cycle to previous completion |
| `C-n` | Next completion | Company | Move to next completion candidate |
| `C-p` | Previous completion | Company | Move to previous completion candidate |
| `RET` | Accept completion | Company | Accept current completion |
| `C-g` | Cancel completion | Company | Cancel completion popup |
| `C-h` | Show documentation | Company | Show documentation for completion |
| `C-w` | Show source | Company | Show source code for completion |
| `M-{digit}` | Quick select | Company | Quickly select completion by number |

### 🐛 Flycheck (Syntax Checking)
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `C-c ! l` | List errors | Flycheck | Show all errors in buffer |
| `C-c ! n` | Next error | Flycheck | Jump to next error |
| `C-c ! p` | Previous error | Flycheck | Jump to previous error |
| `C-c ! c` | Check buffer | Flycheck | Check current buffer for errors |
| `C-c ! C` | Clear errors | Flycheck | Clear error display |
| `C-c ! h` | Error help | Flycheck | Show help for current error |
| `C-c ! v` | Verify checker | Flycheck | Verify flycheck setup |
| `C-c ! s` | Select checker | Flycheck | Select syntax checker |

### 🌳 Treemacs (Project Tree)
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `F8` | Toggle Treemacs | Global | Show/hide project tree |
| `TAB` | Toggle node | Treemacs | Expand/collapse directory |
| `RET` | Open file | Treemacs | Open file or enter directory |
| `o` | Open other window | Treemacs | Open file in other window |
| `r` | Rename | Treemacs | Rename file or directory |
| `d` | Delete | Treemacs | Delete file or directory |
| `c f` | Create file | Treemacs | Create new file |
| `c d` | Create directory | Treemacs | Create new directory |
| `R` | Refresh | Treemacs | Refresh project tree |
| `s` | Sort | Treemacs | Sort tree contents |

### 🔧 Programming Utilities
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `C-c C-c` | Compile | Programming | Compile current project |
| `C-c C-k` | Kill compilation | Programming | Stop compilation process |
| `M-;` | Comment/uncomment | Programming | Toggle comment on line/region |
| `C-c C-f` | Format code | Programming | Format code according to style |
| `C-c C-d` | Documentation | Programming | Show documentation at point |
| `C-c C-j` | Jump to definition | Programming | Jump to symbol definition |
| `C-c C-r` | Refactor | Programming | Trigger refactoring menu |

---

## 🎵 Media Control

*Music and video playback with EMMS*

### 🎶 Basic Playback Control
| Keybinding | Action | Description |
|------------|--------|-------------|
| `C-c m p` | Play/Pause | Toggle music playback |
| `C-c m s` | Stop | Stop music playback |
| `C-c m n` | Next track | Play next song |
| `C-c m b` | Previous track | Play previous song |
| `C-c m r` | Repeat mode | Toggle repeat mode |
| `C-c m z` | Shuffle mode | Toggle shuffle mode |
| `C-c m +` | Volume up | Increase playback volume |
| `C-c m -` | Volume down | Decrease playback volume |

### 📁 Playlist Management
| Keybinding | Action | Description |
|------------|--------|-------------|
| `C-c m l` | Show playlist | Open playlist buffer |
| `C-c m d` | Add directory | Add directory to playlist |
| `C-c m f` | Add file | Add single file to playlist |
| `C-c m c` | Clear playlist | Clear current playlist |
| `C-c m S` | Save playlist | Save current playlist |
| `C-c m L` | Load playlist | Load saved playlist |

### 🎧 EMMS Playlist Buffer
*When in the EMMS playlist buffer:*
| Keybinding | Action | Description |
|------------|--------|-------------|
| `RET` | Play track | Play track at cursor |
| `n` | Next track | Move to next track |
| `p` | Previous track | Move to previous track |
| `d` | Delete track | Remove track from playlist |
| `a` | Add files | Add files to playlist |
| `c` | Clear playlist | Clear entire playlist |
| `s` | Save playlist | Save current playlist |
| `q` | Quit | Close playlist buffer |

### 🎬 Video Playback (VLC Integration)
| Keybinding | Action | Description |
|------------|--------|-------------|
| `C-c v o` | Open video | Open video file with VLC |
| `C-c v p` | Play/Pause video | Toggle video playback |
| `C-c v s` | Stop video | Stop video playback |
| `C-c v f` | Fullscreen | Toggle video fullscreen |
| `C-c v q` | Quit VLC | Close VLC player |

---

## 🌐 Web Browsing

*Internet navigation within Emacs*

### 🌍 EWW Browser
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `M-x eww` | Open browser | Global | Launch EWW web browser |
| `g` | Go to URL | EWW | Navigate to URL |
| `G` | Go to URL (new buffer) | EWW | Open URL in new buffer |
| `r` | Reload page | EWW | Refresh current page |
| `H` | Back in history | EWW | Go back to previous page |
| `L` | Forward in history | EWW | Go forward in history |
| `l` | List history | EWW | Show browsing history |
| `b` | Add bookmark | EWW | Bookmark current page |
| `B` | List bookmarks | EWW | Show bookmark list |
| `d` | Download | EWW | Download link at point |
| `q` | Quit browser | EWW | Close browser buffer |

### 🔍 EWW Navigation
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `TAB` | Next link | EWW | Jump to next link |
| `S-TAB` | Previous link | EWW | Jump to previous link |
| `RET` | Follow link | EWW | Open link at cursor |
| `F` | Follow link (new buffer) | EWW | Open link in new buffer |
| `S` | Search page | EWW | Search within current page |
| `C` | Copy URL | EWW | Copy current page URL |
| `w` | Copy link URL | EWW | Copy link URL at point |
| `v` | View source | EWW | View page source |
| `R` | Reader mode | EWW | Toggle reader mode |

### 🌐 XWidget WebKit (if available)
| Keybinding | Action | Description |
|------------|--------|-------------|
| `C-c w` | Open webkit | Open full webkit browser |
| `C-c w n` | New webkit session | Create new webkit session |
| `C-c w k` | Kill webkit | Close webkit browser |

---

## 📁 File Management (Dired)

*Powerful file manager built into Emacs*

### 🗂️ Basic Dired Navigation
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `RET` | Open file/directory | Dired | Open file or enter directory |
| `^` | Go to parent directory | Dired | Move up one directory level |
| `g` | Refresh | Dired | Refresh directory listing |
| `n` | Next line | Dired | Move to next file |
| `p` | Previous line | Dired | Move to previous file |
| `j` | Jump to file | Dired | Jump to file by name |
| `<` | Beginning of buffer | Dired | Go to first file |
| `>` | End of buffer | Dired | Go to last file |
| `q` | Quit dired | Dired | Close dired buffer |

### ✏️ File Operations
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `C` | Copy files | Dired | Copy marked files |
| `R` | Rename/move files | Dired | Rename or move marked files |
| `D` | Delete files | Dired | Delete marked files immediately |
| `d` | Mark for deletion | Dired | Mark file for deletion |
| `x` | Execute deletions | Dired | Delete all marked files |
| `u` | Unmark file | Dired | Remove mark from file |
| `U` | Unmark all | Dired | Remove all marks |
| `t` | Toggle marks | Dired | Toggle marks on all files |
| `m` | Mark file | Dired | Mark current file |

### 📋 Advanced Dired Operations
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `+` | Create directory | Dired | Create new directory |
| `M` | Change file mode | Dired | Change file permissions |
| `G` | Change group | Dired | Change file group |
| `O` | Change owner | Dired | Change file owner |
| `T` | Touch file | Dired | Update file timestamp |
| `L` | Load elisp file | Dired | Load Emacs Lisp file |
| `B` | Byte compile | Dired | Compile Emacs Lisp file |
| `Z` | Compress/uncompress | Dired | Compress or uncompress file |
| `!` | Shell command | Dired | Run shell command on file |
| `&` | Async shell command | Dired | Run async shell command |

### 🔍 Dired Marking and Selection
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `* /` | Mark directories | Dired | Mark all directories |
| `* *` | Mark executables | Dired | Mark all executable files |
| `* .` | Mark by extension | Dired | Mark files by extension |
| `* %` | Mark by regexp | Dired | Mark files matching regexp |
| `* t` | Toggle marks | Dired | Toggle all file marks |
| `* c` | Change marks | Dired | Change mark character |
| `% d` | Mark for deletion by regexp | Dired | Mark files for deletion by pattern |
| `% m` | Mark by regexp | Dired | Mark files by regexp pattern |
| `% g` | Mark containing regexp | Dired | Mark files containing pattern |

---

## 💻 Terminal

*Command line integration within Emacs*

### 🖥️ VTerm (Primary Terminal)
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `M-x vterm` | Open terminal | Global | Launch VTerm terminal |
| `C-c C-t` | Toggle copy mode | VTerm | Switch between terminal and copy mode |
| `C-c C-n` | Next terminal | VTerm | Switch to next terminal buffer |
| `C-c C-p` | Previous terminal | VTerm | Switch to previous terminal buffer |
| `C-c C-k` | Clear screen | VTerm | Clear terminal screen |
| `C-c C-l` | Clear scrollback | VTerm | Clear terminal scrollback |

### 📟 Multi-term
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `M-x multi-term` | New terminal | Global | Create new terminal |
| `C-c C-j` | Next term | Multi-term | Switch to next terminal |
| `C-c C-k` | Previous term | Multi-term | Switch to previous terminal |
| `C-c C-a` | Switch to term | Multi-term | Switch to specific terminal |

### 🐚 Shell Integration
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `M-x shell` | Start shell | Global | Start shell buffer |
| `M-x eshell` | Start eshell | Global | Start Emacs shell |
| `C-c C-c` | Interrupt process | Shell | Send interrupt signal |
| `C-c C-z` | Suspend process | Shell | Suspend current process |
| `C-c C-d` | Send EOF | Shell | Send end-of-file |
| `C-c C-u` | Kill line | Shell | Kill line in shell |

---

## 🪟 Window Management

*Advanced window and frame control*

### 🔄 Window Navigation
| Keybinding | Action | Description |
|------------|--------|-------------|
| `C-x o` | Other window | Switch to next window |
| `C-x 0` | Delete window | Close current window |
| `C-x 1` | Delete other windows | Close all windows except current |
| `C-x 2` | Split horizontal | Split window horizontally |
| `C-x 3` | Split vertical | Split window vertically |
| `C-x ^` | Enlarge window | Make window taller |
| `C-x }` | Enlarge horizontally | Make window wider |
| `C-x {` | Shrink horizontally | Make window narrower |
| `C-x +` | Balance windows | Make all windows same size |

### 🔄 Winner Mode (Window History)
| Keybinding | Action | Description |
|------------|--------|-------------|
| `C-c <left>` | Undo window change | Restore previous window configuration |
| `C-c <right>` | Redo window change | Restore next window configuration |

### 🖼️ Frame Management
| Keybinding | Action | Description |
|------------|--------|-------------|
| `C-x 5 0` | Delete frame | Close current frame |
| `C-x 5 1` | Delete other frames | Close all frames except current |
| `C-x 5 2` | New frame | Create new frame |
| `C-x 5 f` | Find file other frame | Open file in new frame |
| `C-x 5 o` | Other frame | Switch to next frame |

---

## 🌿 Git Operations (Magit)

*Comprehensive Git integration*

### 📊 Magit Status Buffer
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `g` | Refresh | Magit | Refresh git status |
| `TAB` | Toggle section | Magit | Expand/collapse section |
| `s` | Stage file/hunk | Magit | Stage changes |
| `u` | Unstage file/hunk | Magit | Unstage changes |
| `S` | Stage all | Magit | Stage all changes |
| `U` | Unstage all | Magit | Unstage all changes |
| `c` | Commit | Magit | Start commit process |
| `P` | Push | Magit | Push changes |
| `F` | Pull | Magit | Pull changes |
| `f` | Fetch | Magit | Fetch from remote |

### 💾 Commit Operations
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `c c` | Commit | Magit | Create commit |
| `c a` | Amend commit | Magit | Amend last commit |
| `c r` | Reword commit | Magit | Reword commit message |
| `c f` | Fixup commit | Magit | Create fixup commit |
| `c s` | Squash commit | Magit | Create squash commit |

### 🌐 Remote Operations
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `P p` | Push to upstream | Magit | Push to upstream branch |
| `P u` | Push to upstream | Magit | Push and set upstream |
| `P e` | Push elsewhere | Magit | Push to specific remote |
| `F p` | Pull from upstream | Magit | Pull from upstream |
| `F u` | Pull from upstream | Magit | Pull and set upstream |
| `f a` | Fetch all | Magit | Fetch from all remotes |
| `f o` | Fetch origin | Magit | Fetch from origin |

### 🌿 Branch Operations
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `b b` | Switch branch | Magit | Switch to branch |
| `b c` | Create branch | Magit | Create new branch |
| `b n` | Create new branch | Magit | Create and switch to branch |
| `b d` | Delete branch | Magit | Delete branch |
| `b r` | Rename branch | Magit | Rename current branch |
| `b m` | Merge branch | Magit | Merge branch |

---

## 🔍 Search & Navigation

*Finding and moving through code and text*

### 🔎 Ivy/Counsel/Swiper
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `C-s` | Swiper search | Global | Search in current buffer with preview |
| `M-x` | Execute command | Global | Run Emacs command with completion |
| `C-x C-f` | Find file | Global | Open file with completion |
| `C-x b` | Switch buffer | Global | Switch buffer with completion |
| `M-y` | Yank from kill ring | Global | Select from kill ring history |
| `C-c g` | Counsel git grep | Global | Search in git repository |

### 🎯 Ivy Selection
*During Ivy/Counsel selection:*
| Keybinding | Action | Description |
|------------|--------|-------------|
| `C-n` | Next candidate | Move to next option |
| `C-p` | Previous candidate | Move to previous option |
| `C-j` | Select current | Select without exiting minibuffer |
| `RET` | Select and exit | Select option and exit |
| `C-o` | Actions menu | Show available actions |
| `C-c C-o` | Occur | Create occur buffer with matches |
| `M-o` | Hydra actions | Show action hydra menu |

### 🏷️ Search and Replace
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `C-c s s` | Search project | Global | Search across entire project |
| `C-c s d` | Search directory | Global | Search in specific directory |
| `C-c r r` | Replace project | Global | Replace across project |
| `M-%` | Query replace | Global | Interactive find and replace |
| `C-M-%` | Query replace regexp | Global | Regexp find and replace |

### 🎯 Jump Navigation
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `M-g g` | Go to line | Global | Jump to specific line number |
| `M-g c` | Go to character | Global | Jump to specific character position |
| `C-x r m` | Set bookmark | Global | Set bookmark at current position |
| `C-x r b` | Jump to bookmark | Global | Jump to saved bookmark |
| `C-x r l` | List bookmarks | Global | Show all bookmarks |

---

## 📝 Org Mode

*Note-taking, planning, and document authoring*

### 🗂️ Basic Org Navigation
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `TAB` | Cycle visibility | Org | Expand/collapse current heading |
| `S-TAB` | Global cycle | Org | Cycle visibility for entire buffer |
| `C-c C-n` | Next heading | Org | Move to next heading |
| `C-c C-p` | Previous heading | Org | Move to previous heading |
| `C-c C-f` | Forward same level | Org | Move to next heading at same level |
| `C-c C-b` | Backward same level | Org | Move to previous heading at same level |
| `C-c C-u` | Up heading | Org | Move to parent heading |

### ✅ TODO and Task Management
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `C-c C-t` | TODO state | Org | Cycle through TODO states |
| `S-left` | Previous TODO | Org | Previous TODO state |
| `S-right` | Next TODO | Org | Next TODO state |
| `C-c C-s` | Schedule | Org | Schedule TODO item |
| `C-c C-d` | Deadline | Org | Set deadline for TODO |
| `C-c a` | Agenda | Org | Open org agenda |
| `C-c c` | Capture | Org | Quick capture note/TODO |

### 📊 Org Structure
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `M-RET` | New heading | Org | Create new heading at same level |
| `M-S-RET` | New TODO heading | Org | Create new TODO heading |
| `M-left` | Promote heading | Org | Move heading up one level |
| `M-right` | Demote heading | Org | Move heading down one level |
| `M-S-left` | Promote subtree | Org | Move subtree up one level |
| `M-S-right` | Demote subtree | Org | Move subtree down one level |
| `M-up` | Move subtree up | Org | Move subtree up |
| `M-down` | Move subtree down | Org | Move subtree down |

### 🔗 Links and References
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `C-c C-l` | Insert link | Org | Insert or edit link |
| `C-c C-o` | Open link | Org | Open link at point |
| `C-c C-x C-v` | Toggle images | Org | Show/hide inline images |
| `C-c C-x C-l` | Preview LaTeX | Org | Preview LaTeX fragments |

### 📊 Tables
| Keybinding | Action | Context | Description |
|------------|--------|---------|-------------|
| `TAB` | Next field | Org Table | Move to next table field |
| `S-TAB` | Previous field | Org Table | Move to previous table field |
| `RET` | Next row | Org Table | Move to next table row |
| `M-left` | Move column left | Org Table | Move current column left |
| `M-right` | Move column right | Org Table | Move current column right |
| `M-up` | Move row up | Org Table | Move current row up |
| `M-down` | Move row down | Org Table | Move current row down |
| `M-S-left` | Delete column | Org Table | Delete current column |
| `M-S-right` | Insert column | Org Table | Insert column to right |

---

## ⚡ General Emacs Commands

*Core editor functionality*

### 💾 File Operations
| Keybinding | Action | Description |
|------------|--------|-------------|
| `C-x C-f` | Find file | Open file |
| `C-x C-s` | Save file | Save current buffer |
| `C-x C-w` | Save as | Save buffer with new name |
| `C-x s` | Save some buffers | Save multiple modified buffers |
| `C-x C-c` | Exit Emacs | Quit Emacs |
| `C-x C-z` | Suspend Emacs | Suspend Emacs (return with `fg`) |

### 🗃️ Buffer Operations
| Keybinding | Action | Description |
|------------|--------|-------------|
| `C-x b` | Switch buffer | Switch to another buffer |
| `C-x C-b` | List buffers | Show buffer list |
| `C-x k` | Kill buffer | Close current buffer |
| `C-x C-q` | Toggle read-only | Make buffer read-only or writable |

### ✂️ Text Editing
| Keybinding | Action | Description |
|------------|--------|-------------|
| `C-space` | Set mark | Start text selection |
| `C-w` | Cut region | Cut selected text |
| `M-w` | Copy region | Copy selected text |
| `C-y` | Paste (yank) | Paste from kill ring |
| `M-y` | Yank pop | Cycle through kill ring |
| `C-/` | Undo | Undo last change |
| `C-x u` | Undo | Alternative undo |
| `M-/` | Dynamic completion | Complete word at point |

### 🔍 Search and Replace (Classic)
| Keybinding | Action | Description |
|------------|--------|-------------|
| `C-s` | Search forward | Incremental search forward |
| `C-r` | Search backward | Incremental search backward |
| `M-%` | Query replace | Interactive find and replace |
| `M-x replace-string` | Replace string | Replace all occurrences |

### 🎯 Movement (Classic)
| Keybinding | Action | Description |
|------------|--------|-------------|
| `C-f` | Forward character | Move one character right |
| `C-b` | Backward character | Move one character left |
| `C-n` | Next line | Move to next line |
| `C-p` | Previous line | Move to previous line |
| `C-a` | Beginning of line | Move to start of line |
| `C-e` | End of line | Move to end of line |
| `M-f` | Forward word | Move one word right |
| `M-b` | Backward word | Move one word left |
| `M-<` | Beginning of buffer | Move to start of buffer |
| `M->` | End of buffer | Move to end of buffer |

### 🆘 Help and Information
| Keybinding | Action | Description |
|------------|--------|-------------|
| `C-h ?` | Help help | Show help options |
| `C-h f` | Describe function | Get help for function |
| `C-h v` | Describe variable | Get help for variable |
| `C-h k` | Describe key | Show what key does |
| `C-h m` | Describe mode | Get help for current mode |
| `C-h b` | Describe bindings | List current key bindings |
| `C-h a` | Apropos | Search help for pattern |
| `C-h i` | Info | Browse info manuals |
| `C-g` | Keyboard quit | Cancel current operation |

---

## 📋 Quick Reference Card

### 🚀 Most Essential Keybindings
*Master these first for immediate productivity*

#### **Movement (Vim-style)**
- `h/j/k/l` - Left/Down/Up/Right
- `w/b` - Next/Previous word  
- `0/# 🎮 Emacs Living Environment - Complete Keybinding Reference
