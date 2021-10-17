# TODO: remove
AddPackage --foreign azure-cli # Command-line tools for Azure.
AddPackage --foreign driconf # Graphical configuration tool for the Direct Rendering Infrastructure
AddPackage --foreign eclipse-java # Highly extensible IDE for Java
AddPackage --foreign flashplayer-standalone # Adobe Flash Player Standalone (A.K.A. Adobe Flash Player Projector)
AddPackage --foreign fritzing # PCB layout prototyping application
AddPackage --foreign libopenaptx # Open Source aptX codec library
AddPackage --foreign ltunify-git # A command-line tool to pair Logitech Unifying devices
AddPackage --foreign ncurses5-compat-libs # System V Release 4.0 curses emulation library, ABI 5
AddPackage --foreign smag # Easily create graphs from cli commands and view them in the terminal.
AddPackage scrot # Simple command-line screenshot utility for X
AddPackage read-edid # Program that can get information from a PNP monitor
AddPackage file-roller # Create and modify archives
AddPackage gperf # Perfect hash function generator
AddPackage falkon # Cross-platform QtWebEngine browser
AddPackage gpick # Advanced color picker written in C++ using GTK+ toolkit
AddPackage exo # Application library for Xfce
AddPackage inotify-tools # inotify-tools is a C library and a set of command-line programs for Linux providing a simple interface to inotify.
AddPackage powertop # A tool to diagnose issues with power consumption and power management
AddPackage d-feet # D-Bus debugger for GNOME
AddPackage deepin-picker # Color picker tool for deepin
AddPackage dfu-util # Tool intended to download and upload firmware using DFU protocol to devices connected over USB
AddPackage ethtool # Utility for controlling network drivers and hardware
AddPackage iperf # A tool to measure maximum TCP bandwidth
AddPackage iw # nl80211 based CLI configuration utility for wireless devices
AddPackage garcon # Implementation of the freedesktop.org menu specification
AddPackage ghostscript # An interpreter for the PostScript language
AddPackage libcec # Pulse-Eight's libcec for the Pulse-Eight USB-CEC adapter
AddPackage qt5-tools # A cross-platform application and UI framework (Development Tools, QtHelp)
AddPackage rapidjson # Fast JSON parser/generator for C++ with both SAX/DOM style API
AddPackage tint2 # Basic, good-looking task manager for WMs
AddPackage exiv2 # Exif, Iptc and XMP metadata manipulation library and tools
AddPackage flex # A tool for generating text-scanning programs
AddPackage gobject-introspection # Introspection system for GObject-based libraries
AddPackage gst-libav # Multimedia graph framework - libav plugin
AddPackage interception-tools # A minimal composable infrastructure on top of libudev and libevdev
AddPackage jpegoptim # Jpeg optimisation utility
AddPackage lftp # Sophisticated command line based FTP client
AddPackage libaacs # Advanced Access Content System
AddPackage libldac # LDAC Bluetooth encoder library
AddPackage libtool # A generic library support script
AddPackage perf # Linux kernel performance auditing tool
AddPackage pkgconf # Package compiler and linker metadata toolkit
AddPackage psmisc # Miscellaneous procfs tools
AddPackage shairplay # Apple airplay and raop protocol server
AddPackage slop # Utility to query the user for a selection and print the region to stdout
AddPackage socat # Multipurpose relay
AddPackage sox # The Swiss Army knife of sound processing tools
AddPackage spdlog # Very fast, header-only/compiled, C++ logging library
AddPackage streamlink # CLI program that launches streams from various streaming services in a custom video player (livestreamer fork)
AddPackage terraform # HashiCorp tool for building and updating infrastructure as code idempotently
AddPackage thunar # Modern file manager for Xfce
AddPackage thunar-volman # Automatic management of removeable devices in Thunar
AddPackage tinyxml # Simple, small XML parser
AddPackage tk # A windowing toolkit for use with tcl
AddPackage seahorse # GNOME application for managing PGP keys.
AddPackage signify # OpenBSD tool to signs and verify signatures on files
AddPackage vimb # The vim like browser
AddPackage sloccount # Tools for counting physical source lines of code
AddPackage swig # Generate scripting interfaces to C/C++ code
AddPackage tracker # Desktop-neutral user information store, search tool and indexer
AddPackage tumbler # D-Bus service for applications to request thumbnails
AddPackage usbip # An USB device sharing system over IP network
AddPackage wavemon # Ncurses-based monitoring application for wireless network devices
AddPackage xfconf # Flexible, easy-to-use configuration management system
AddPackage xmltoman # Convert xml to man pages in groff format or html
AddPackage zbar # Application and library for reading bar codes from various sources
AddPackage --foreign xscope # A program to monitor X11/Client conversations
AddPackage usbutils # A collection of USB tools to query connected USB devices
AddPackage words # A collection of International 'words' files for /usr/share/dict.
AddPackage v4l2loopback-dkms # v4l2-loopback device – module sources
AddPackage youtube-dl # A command-line program to download videos from YouTube.com and a few more sites
AddPackage --foreign woeusb # A Linux program to create Windows USB stick installer from a Windows DVD or an image

# TODO: unignore?
IgnorePath '*/__pycache__/*'
IgnorePath '/usr/lib/python2.7/*'
IgnorePath '/usr/lib/python3.6/*'
IgnorePath '/usr/lib/python3.7/*'
IgnorePath '/usr/lib/python3.8/*'
IgnorePath '/usr/lib/ruby/gems/2.5.0/*'
IgnorePath '/usr/lib/ruby/gems/2.6.0/*'
IgnorePath '/usr/lib/ruby/gems/2.7.0/*'
IgnorePath '/usr/lib/ruby/gems/*/cache'

# TODO: remove this once we're no longer using dotbot to manage these files
cat > "$(CreateFile /etc/systemd/system/systemd-logind.service.d/override.conf)" <<EOF
[Service]
# Allow symlinks into the home directory.
ProtectHome=no
EOF

# TODO: stop managing these with dotbot
# This list generated by running `grep dotfiles 99-unsorted.sh`
CreateLink /etc/X11/xorg.conf.d/20-intel.conf /home/jeremy/gitting/dotfiles/etc/X11/xorg.conf.d/20-intel.conf
CreateLink /etc/X11/xorg.conf.d/20-trackpoint.conf /home/jeremy/gitting/dotfiles/etc/X11/xorg.conf.d/20-trackpoint.conf
CreateLink /etc/acpi/events/cd_play /home/jeremy/gitting/dotfiles/etc/acpi/events/cd_play
CreateLink /etc/acpi/events/lid /home/jeremy/gitting/dotfiles/etc/acpi/events/lid
CreateLink /etc/locale.conf /home/jeremy/gitting/dotfiles/etc/locale.conf
CreateLink /etc/modprobe.d/nobeep.conf /home/jeremy/gitting/dotfiles/etc/modprobe.d/nobeep.conf
CreateLink /etc/pacman.d/hooks/99-secureboot.hook /home/jeremy/gitting/dotfiles/etc/pacman.d/hooks/99-secureboot.hook
CreateLink /etc/pacman.d/hooks/mirrorupgrade.hook /home/jeremy/gitting/dotfiles/etc/pacman.d/hooks/mirrorupgrade.hook
CreateLink /etc/systemd/logind.conf.d/no-suspend-on-lidswitch.conf /home/jeremy/gitting/dotfiles/etc/systemd/logind.conf.d/no-suspend-on-lidswitch.conf
CreateLink /etc/systemd/logind.conf.d/suspend-on-powerbutton.conf /home/jeremy/gitting/dotfiles/etc/systemd/logind.conf.d/suspend-on-powerbutton.conf
CreateLink /etc/systemd/system/disable-bt-wakeup.service /home/jeremy/gitting/dotfiles/etc/systemd/system/disable-bt-wakeup.service
CreateLink /etc/systemd/system/fixinputs.path /home/jeremy/gitting/dotfiles/units/fixinputs.path
CreateLink /etc/systemd/system/fixinputs.service /home/jeremy/gitting/dotfiles/units/fixinputs.service
CreateLink /etc/systemd/system/fixinputs@.path /home/jeremy/gitting/dotfiles/etc/systemd/system/fixinputs@.path
CreateLink /etc/systemd/system/fixinputs@.service /home/jeremy/gitting/dotfiles/etc/systemd/system/fixinputs@.service
CreateLink /etc/systemd/system/fixinputs@jeremy.path /home/jeremy/gitting/dotfiles/etc/systemd/system/fixinputs@.path
CreateLink /etc/systemd/system/monitors@.service /home/jeremy/gitting/dotfiles/etc/systemd/system/monitors@.service
CreateLink /etc/systemd/system/multi-user.target.wants/disable-bt-wakeup.service /home/jeremy/gitting/dotfiles/etc/systemd/system//disable-bt-wakeup.service
CreateLink /etc/systemd/system/multi-user.target.wants/fixinputs@jeremy.path /home/jeremy/gitting/dotfiles/etc/systemd/system/fixinputs@.path
CreateLink /etc/systemd/system/sleep.target.wants/slock@jeremy.service /home/jeremy/gitting/dotfiles/etc/systemd/system/slock@.service
CreateLink /etc/systemd/system/slock@.service /home/jeremy/gitting/dotfiles/etc/systemd/system/slock@.service
CreateLink /etc/systemd/system/slock@jeremy.service /home/jeremy/gitting/dotfiles/etc/systemd/system/slock@.service
CreateLink /etc/udev/rules.d/10-local.rules /home/jeremy/gitting/dotfiles/etc/udev/rules.d/10-local.rules
CreateLink /etc/udevmon.yaml /home/jeremy/gitting/dotfiles/etc/udevmon.yaml
CreateLink /etc/vconsole.conf /home/jeremy/gitting/dotfiles/etc/vconsole.conf
CreateLink /usr/lib/systemd/system/monitors.service /home/jeremy/gitting/dotfiles/units/monitors.service
CreateLink /usr/share/themes/jfattymacs /home/jeremy/gitting/dotfiles/usr/share/themes/jfattymacs
