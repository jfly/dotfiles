### Text editors
AddPackage gvim # Vi Improved, a highly configurable, improved version of the vi text editor (with advanced features, such as a GUI)
CreateLink /usr/bin/vi /usr/bin/vim
IgnorePath '/usr/share/vim/vimfiles/doc/tags' # Autogenerated from installed vim plugins.
AddPackage neovim # Fork of Vim aiming to improve user experience, plugins, and GUIs
AddPackage code # The Open Source build of Visual Studio Code (vscode) editor
AddPackage editorconfig-core-c # EditorConfig core code written in C (for use by plugins supporting EditorConfig parsing)
AddPackage kakoune # Multiple-selection, UNIX-flavored modal editor

### Git
AddPackage diff-so-fancy # Good-looking diffs with diff-highlight and more
AddPackage git-crypt # Transparent file encryption in Git
AddPackage git-delta # Syntax-highlighting pager for git and diff output
AddPackage github-cli # The GitHub CLI
AddPackage hub # cli interface for Github
AddPackage git # the fast distributed version control system

### Docker
AddPackage docker # Pack, ship and run any application as a lightweight container
AddPackage docker-compose # Fast, isolated development environments using Docker
AddPackage --foreign docker-credential-secretservice # program to use secretservice to keep Docker credentials safe
IgnorePath '/var/lib/docker/*'
IgnorePath '/etc/docker/key.json' # https://stackoverflow.com/questions/47573820/what-is-etc-docker-key-json
# Part of running docker with a users remap. See
# https://github.com/jfly/dotfiles/blob/master/notes/setting-up-docker-user-remap
# for details.
echo "jeremy:1000:65536" > "$(CreateFile /etc/subgid)"
echo "jeremy:1000:65536" > "$(CreateFile /etc/subuid)"
# Enable the service.
cat > "$(CreateFile /etc/systemd/system/docker.service.d/override.conf)" <<EOF
[Service]
ExecStart=
# ExecStart=/usr/bin/dockerd -H fd:// -s overlay2 --userns-remap=jeremy
ExecStart=/usr/bin/dockerd -H fd:// -s overlay2
EOF
IgnorePath '/opt/containerd/*' # containerd likes to create this folder on startup. See https://github.com/containerd/containerd/blob/main/docs/managed-opt.md for more details.
IgnorePath '/var/lib/containerd/*'

### Virtualization
AddPackage vagrant # Build and distribute virtualized development environments
AddPackage virtualbox # Powerful x86 virtualization for enterprise as well as home use

### Cloud hosting
AddPackage --foreign aws-cli-v2-bin # Universal Command Line Interface for Amazon Web Services version 2

### nix
IgnorePath '/nix/*'

### Network
AddPackage nginx # Lightweight HTTP server and IMAP/POP3 proxy server
AddPackage curl # An URL retrieval utility and library
AddPackage wget # Network utility to retrieve files from the Web
AddPackage openbsd-netcat # TCP/IP swiss army knife. OpenBSD variant.
AddPackage traceroute # Tracks the route taken by packets over an IP network
AddPackage whois # Intelligent WHOIS client
AddPackage wireshark-qt # Network traffic and protocol analyzer/sniffer - Qt GUI

### Shell
AddPackage shellcheck # Shell script analysis tool

### Python
AddPackage python # Next generation of the python high-level scripting language
AddPackage python-pip # The PyPA recommended tool for installing Python packages
AddPackage python-pipenv # Sacred Marriage of Pipfile, Pip, & Virtualenv.
IgnorePath '*.pyc' # There's a surprising number of these files scattered around. I guess a lot of tools like to use Python somewhere internally.

### Node/Javascript
AddPackage yarn # Fast, reliable, and secure dependency management
AddPackage nodejs # Evented I/O for V8 javascript
AddPackage npm # A package manager for javascript

### Java
AddPackage jre-openjdk-headless
CreateLink /usr/lib/jvm/default java-17-openjdk
CreateLink /usr/lib/jvm/default-runtime java-17-openjdk
AddPackage maven # Java project management and project comprehension tool

### Perl
AddPackage perl # A highly capable, feature-rich programming language
AddPackage perl-authen-sasl # Perl/CPAN Module Authen
AddPackage perl-io-socket-ssl # Nearly transparent SSL encapsulation for IO
AddPackage perl-net-dbus # Binding for DBus messaging protocol

### Haskell
IgnorePath '/usr/lib/ghc-*/package.conf.d/*.conf'
IgnorePath '/usr/lib/ghc-*/package.conf.d/package.cache'

### Misc programming languages
AddPackage go # Core compiler tools for the Go programming language
AddPackage gtest # Google Test - C++ testing utility
AddPackage bash-bats # Bash Automated Testing System
AddPackage check # A unit testing framework for C
AddPackage cbindgen # A tool for generating C bindings to Rust code
AddPackage gdb # The GNU Debugger
AddPackage nasm # 80x86 assembler designed for portability and modularity
AddPackage m4 # The GNU macro processor
AddPackage mercurial # A scalable distributed SCM tool
AddPackage meson # High productivity build system
AddPackage php # A general-purpose scripting language that is especially suited to web development
AddPackage imake # X.Org imake program and related utilities
AddPackage --foreign cmake-format # Source code formatter for CMake listfiles
AddPackage autoconf2.13 # A GNU tool for automatically configuring source code (Legacy 2.1x version)
AddPackage binutils # A set of programs to assemble and manipulate binary and object files
AddPackage bison # The GNU general-purpose parser generator
AddPackage boost # Free peer-reviewed portable C++ source libraries (development headers)
AddPackage llvm # Collection of modular and reusable compiler and toolchain technologies
AddPackage extra-cmake-modules # Extra modules and scripts for CMake
AddPackage ctags # Generates an index file of language objects found in source files

### Dependencies to install stuff from the AUR
AddPackage gcc
AddPackage fakeroot
AddPackage patch # A utility to apply patch files to original sources
AddPackage autoconf
AddPackage automake
AddPackage make # GNU make utility to maintain groups of programs

### cubing/icons
AddPackage potrace # Utility for tracing a bitmap (input

### Debug utils
AddPackage strace # A diagnostic, debugging and instructional userspace tracer
AddPackage ghidra # Software reverse engineering framework

### Misc
AddPackage coreutils # The basic file, shell and text manipulation utilities of the GNU operating system
AddPackage doxygen # Documentation system for C++, C, Java, IDL and PHP
AddPackage expect # A tool for automating interactive applications
AddPackage diffstat # Display a histogram of diff changes
AddPackage diffutils # Utility programs used for creating patch files
AddPackage patchutils # A small collection of programs that operate on patch files
AddPackage wkhtmltopdf # Command line tools to render HTML into PDF and various image formats

### Graphviz
AddPackage graphviz # Graph visualization software
IgnorePath '/usr/lib/graphviz/config6' # Autogenerated by `dot -c`: https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=755065

### wrk
IgnorePath '/usr/local/bin/h4*.sh'
