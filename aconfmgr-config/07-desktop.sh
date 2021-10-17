### Desktop/gui
AddPackage xmobar # Minimalistic Text Based Status Bar
AddPackage xmonad # Lightweight X11 tiled window manager written in Haskell
AddPackage xmonad-contrib # Add-ons for xmonad
AddPackage dunst # Customizable and lightweight notification-daemon
AddPackage xorg-server # Xorg X server
AddPackage xorg-server-xvfb # Virtual framebuffer X server
AddPackage xorg-xclock # X clock
AddPackage xorg-xev # Print contents of X events
AddPackage xorg-xinit # X.Org initialisation program
AddPackage xorg-xinput # Small commandline tool to configure devices
AddPackage xorg-xkill # Kill a client by its X resource
AddPackage xorg-xlsatoms # List interned atoms defined on server
AddPackage xorg-xmessage # Display a message or query in a window
AddPackage xorg-xmodmap # Utility for modifying keymaps and button mappings
AddPackage xorg-xprop # Property displayer for X
AddPackage xorg-xrandr # Primitive command line interface to RandR extension
AddPackage xorg-xrdb # X server resource database utility
AddPackage xorg-xsetroot # Classic X utility to set your root window background to a given pattern or color
AddPackage xprintidle # Print X idle time
AddPackage xsel # XSel is a command-line program for getting and setting the contents of the X selection
AddPackage xsettingsd # Provides settings to X11 applications via the XSETTINGS specification
AddPackage evtest # Input device event monitor and query tool
AddPackage arandr # Provide a simple visual front end for XRandR 1.2.
AddPackage --foreign zoom # Video Conferencing and Web Conferencing Service
AddPackage --foreign dmenu2 # Fork of dmenu with many useful patches applied and additional options like screen select, dim or opacity change
AddPackage --foreign parsec-bin # Remotely connect to a gaming pc for a low latency remote computing experience
AddPackage freerdp # Free implementation of the Remote Desktop Protocol (RDP)
AddPackage eog # Eye of Gnome
AddPackage feh # Fast and light imlib2-based image viewer
AddPackage guvcview # Simple GTK+ interface for capturing and viewing video from v4l2 devices
AddPackage --foreign trayer-srg-git # trayer fork with multi monitor support, cleaned up codebase and other fancy stuff (git-version)
AddPackage --foreign xcwd-git # xcwd is a simple tool that prints the current working directory of the currently focused window.
AddPackage slock # A simple screen locker for X
AddPackage numlockx # Turns on the numlock key in X11.
AddPackage xdotool # Command-line X11 automation tool
AddPackage xclip # Command line interface to the X11 clipboard
AddPackage gnome-keyring # Stores passwords and encryption keys
AddPackage --foreign trayer-srg-git # trayer fork with multi monitor support, cleaned up codebase and other fancy stuff (git-version)

### Misc ignore
IgnorePath '/usr/share/applications/mimeinfo.cache' # https://specifications.freedesktop.org/desktop-entry-spec/0.9.5/ar01s07.html
IgnorePath '/usr/local/share/applications/mimeinfo.cache' # https://specifications.freedesktop.org/desktop-entry-spec/0.9.5/ar01s07.html
IgnorePath '/usr/share/mime/application/*'
IgnorePath '/usr/share/mime/*'
IgnorePath '/usr/share/icons/*/icon-theme.cache'

### D-Bus
IgnorePath '/etc/dbus-1/system.d'

### Keyboard
AddPackage xcape # Configure modifier keys to act as other keys when pressed and released on their own
AddPackage --foreign interception-space2meta # space2meta

### Video card drivers
AddPackage xf86-video-intel # X.org Intel i810/i830/i915/945G/G965+ video drivers
AddPackage libva-intel-driver # VA-API implementation for Intel G45 and HD Graphics family
AddPackage libva-utils # Intel VA-API Media Applications and Scripts for libva
AddPackage libvdpau-va-gl # VDPAU driver with OpenGL/VAAPI backend

### Movie player
AddPackage mplayer # Media player for Linux
AddPackage mpv # a free, open source, and cross-platform media player
AddPackage vlc # Multi-platform MPEG, VCD/DVD, and DivX player
IgnorePath '/usr/lib/vlc/plugins/plugins.dat' # https://forums.debian.net/viewtopic.php?f=6&t=117859

### Terminal emulator
AddPackage alacritty # A cross-platform, GPU-accelerated terminal emulator
AddPackage --foreign xcwd-git # xcwd is a simple tool that prints the current working directory of the currently focused window.

### PDF
AddPackage evince # Document viewer (PDF, PostScript, XPS, djvu, dvi, tiff, cbr, cbz, cb7, cbt)

### GTK
IgnorePath '/usr/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache'
IgnorePath '/usr/lib/gtk-2.0/2.10.0/immodules.cache'
IgnorePath '/usr/lib/gtk-3.0/3.0.0/immodules.cache'
IgnorePath '/usr/lib/gio/modules/giomodule.cache'
IgnorePath '/usr/share/glib-2.0/schemas/gschemas.compiled' # https://developer.gnome.org/gio/stable/glib-compile-schemas.html

### Web browser
AddPackage chromium # A web browser built for speed, simplicity, and security
AddPackage firefox # Standalone web browser from mozilla.org
AddPackage elinks # An advanced and well-established feature-rich text mode web browser.

### Dropbox
AddPackage --foreign dropbox # A free service that lets you bring your photos, docs, and videos anywhere and share them easily.
# When you have enough files, Dropbox asks for this.
echo "fs.inotify.max_user_watches=524288" > "$(CreateFile /etc/sysctl.d/99-sysctl.conf)"

### Fonts
AddPackage ttf-opensans # Sans-serif typeface commissioned by Google
AddPackage gnu-free-fonts # A free family of scalable outline fonts
AddPackage noto-fonts-emoji # Google Noto emoji fonts
AddPackage ttf-bitstream-vera # Bitstream Vera fonts.
AddPackage ttf-liberation # Font family which aims at metric compatibility with Arial, Times New Roman, and Courier New
AddPackage xorg-xfd # Displays all the characters in a font using either the X11 core protocol or libXft2
AddPackage --foreign font-manager # A simple font management application for GTK+ Desktop Environments
AddPackage --foreign nerd-fonts-ubuntu-mono # Patched font UbuntuMono from the nerd-fonts library
AddPackage --foreign ttf-google-fonts-git # TrueType fonts from the Google Fonts project (git version)
AddPackage --foreign ttf-merriweather # A typeface that is pleasant to read on screens by Sorkin Type Co
AddPackage --foreign ttf-merriweather-sans # A sans-serif typeface that is pleasant to read on screens by Sorkin Type Co
AddPackage --foreign ttf-ms-win10 # Microsoft Windows 10 TrueType fonts
AddPackage --foreign ttf-oswald # Sans-serif typeface from Google by Vernon Adams
AddPackage --foreign ttf-quintessential # Calligraphic typeface from Google by Brian J. Bonislawsky
AddPackage --foreign ttf-signika # Sans-serif typeface from Google by Anna Giedryś
AddPackage --foreign ttf-twemoji # Twitter Emoji for everyone.
AddPackage --foreign ttf-twemoji-color # A color and B&W emoji SVG-in-OpenType font by Twitter with support for ZWJ, skin tone modifiers and country flags.
IgnorePath '/etc/fonts/conf.d/*'
IgnorePath '/usr/share/fonts/*'

### Windows compatibility
AddPackage steam # Valve's digital software delivery system
AddPackage dos2unix # Text file format converter
AddPackage dosbox # Emulator with builtin DOS for running DOS Games
AddPackage wine # A compatibility layer for running Windows programs
AddPackage wine-gecko # Wine's built-in replacement for Microsoft's Internet Explorer
AddPackage wine-mono # Wine's built-in replacement for Microsoft's .NET Framework
AddPackage mtools # A collection of utilities to access MS-DOS disks

### Ebooks
AddPackage calibre # Ebook management application

### Redshift
AddPackage redshift # Adjusts the color temperature of your screen according to your surroundings.
IgnorePath '/var/lib/geoclue' # redshift depends on geoclue, which seems to be intent upon creating this folder.

### Screenshots
AddPackage screenkey # A screencast tool to display your keys inspired by Screenflick
AddPackage byzanz # Record what's happening on your desktop
# TODO: get all this flameshot stuff into a package
IgnorePath '/usr/local/bin/flameshot'
IgnorePath '/usr/local/share/applications/flameshot.desktop'
IgnorePath '/usr/local/share/applications/org.flameshot.Flameshot.desktop'
IgnorePath '/usr/local/share/bash-completion/completions/flameshot'
IgnorePath '/usr/local/share/dbus-1/interfaces/org.dharkael.Flameshot.xml'
IgnorePath '/usr/local/share/dbus-1/interfaces/org.flameshot.Flameshot.xml'
IgnorePath '/usr/local/share/dbus-1/services/org.dharkael.Flameshot.service'
IgnorePath '/usr/local/share/dbus-1/services/org.flameshot.Flameshot.service'
IgnorePath '/usr/local/share/flameshot/*'
IgnorePath '/usr/local/share/icons/hicolor/128x128/apps/flameshot.png'
IgnorePath '/usr/local/share/icons/hicolor/128x128/apps/org.flameshot.Flameshot.png'
IgnorePath '/usr/local/share/icons/hicolor/48x48/apps/flameshot.png'
IgnorePath '/usr/local/share/icons/hicolor/48x48/apps/org.flameshot.Flameshot.png'
IgnorePath '/usr/local/share/icons/hicolor/scalable/apps/flameshot.svg'
IgnorePath '/usr/local/share/icons/hicolor/scalable/apps/org.flameshot.Flameshot.svg'
IgnorePath '/usr/local/share/metainfo/flameshot.appdata.xml'
IgnorePath '/usr/local/share/metainfo/org.flameshot.Flameshot.metainfo.xml'
IgnorePath '/usr/local/share/zsh/site-functions/_flameshot'

### volnoti
# TODO: get this into a package
IgnorePath '/usr/bin/volnoti'
IgnorePath '/usr/bin/volnoti-show'
IgnorePath '/usr/share/pixmaps/volnoti/display-brightness-symbolic.svg'
IgnorePath '/usr/share/pixmaps/volnoti/empty.png'
IgnorePath '/usr/share/pixmaps/volnoti/media-eject-symbolic.svg'
IgnorePath '/usr/share/pixmaps/volnoti/progressbar_empty.png'
IgnorePath '/usr/share/pixmaps/volnoti/progressbar_full.png'
IgnorePath '/usr/share/pixmaps/volnoti/volume_high.svg'
IgnorePath '/usr/share/pixmaps/volnoti/volume_low.svg'
IgnorePath '/usr/share/pixmaps/volnoti/volume_medium.svg'
IgnorePath '/usr/share/pixmaps/volnoti/volume_muted.svg'
IgnorePath '/usr/share/pixmaps/volnoti/volume_off.svg'
