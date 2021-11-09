### Desktop/gui
AddPackage xmonad # Lightweight X11 tiled window manager written in Haskell
AddPackage xmonad-contrib # Add-ons for xmonad
AddPackage --foreign polybar-git # Status bar (need the -git version to get support for wm-restart=generic: https://github.com/polybar/polybar/pull/2404#issuecomment-864411636)
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
AddPackage numlockx # Turns on the numlock key in X11.
AddPackage xdotool # Command-line X11 automation tool
AddPackage xclip # Command line interface to the X11 clipboard
AddPackage gnome-keyring # Stores passwords and encryption keys

### Run autoperipherals when hardware changes
cat > "$(CreateFile /etc/udev/rules.d/10-autoperipherals.rules)" <<EOF
SUBSYSTEM=="drm", ACTION=="change", RUN+="/usr/bin/systemctl start autoperipherals@jeremy.service"
EOF
cat > "$(CreateFile /etc/systemd/system/autoperipherals@.service)" <<EOF
[Unit]
Description=Refresh autoperipherals for user %i

[Service]
Type=simple
# Don't kill child processes started by autoperipherals
KillMode=none

ExecStart=/home/%i/bin/x11-run-as %i autoperipherals

[Install]
WantedBy=multi-user.target
EOF

### slock
AddPackage slock # A simple screen locker for X
# Lock the screen on suspend. Trick copied from
# https://wiki.archlinux.org/title/Slock#Lock_on_suspend.
cat > "$(CreateFile /etc/systemd/system/slock@.service)" <<EOF
[Unit]
Description=Lock X session using slock for user %i
Before=sleep.target

[Service]
User=%i
Environment=DISPLAY=:0
ExecStartPre=/usr/bin/xset dpms force suspend
ExecStart=/usr/bin/slock

[Install]
WantedBy=sleep.target
EOF
CreateLink /etc/systemd/system/sleep.target.wants/slock@jeremy.service /etc/systemd/system/slock@.service
CreateLink /etc/systemd/system/slock@jeremy.service /etc/systemd/system/slock@.service

### Trackpoint
cat > "$(CreateFile /etc/X11/xorg.conf.d/20-trackpoint.conf)" <<EOF
Section "InputClass"
  Identifier "Trackpoint Acceleration"
  MatchDriver "libinput"
  MatchProduct "TPPS/2 IBM TrackPoint|TPPS/2 Elan TrackPoint|ThinkPad Compact Bluetooth Keyboard with TrackPoint"

  # See https://wiki.archlinux.org/index.php/Mouse_acceleration#with_libinput.
  MatchIsPointer "yes"
  Option "AccelProfile" "flat"
  Option "AccelSpeed" "1"
EndSection

Section "InputClass"
  Identifier "TouchPad Acceleration"
  MatchDriver "libinput"
  MatchProduct "Synaptics TM3289-002"
  Option "AccelSpeed" "0.0"
  Option "Tapping" "yes"
  Option "TappingDragLock" "yes"
EndSection
EOF

### Misc ignore
IgnorePath '/usr/share/applications/mimeinfo.cache' # https://specifications.freedesktop.org/desktop-entry-spec/0.9.5/ar01s07.html
IgnorePath '/usr/local/share/applications/mimeinfo.cache' # https://specifications.freedesktop.org/desktop-entry-spec/0.9.5/ar01s07.html
IgnorePath '/usr/share/mime/application/*'
IgnorePath '/usr/share/mime/*'
IgnorePath '/usr/share/icons/*/icon-theme.cache'

### D-Bus
IgnorePath '/etc/dbus-1/system.d'

### Keyboard
AddPackage interception-tools # A minimal composable infrastructure on top of libudev and libevdev
AddPackage interception-caps2esc # Interception plugin that transforms the most useless key ever in the most useful one
AddPackage --foreign interception-space2meta # space2meta: turn space into meta when chorded to another key (on release)
# Hacky workaround for https://gitlab.com/interception/linux/tools/-/issues/50
# On my system, systemd-udev-settle causes all sorts of noise trying to start
# up various services that aren't eligible to run, then they all hit some
# systemd start limit, and then systemd-udev-settle then has to wait 3 minutes
# before letting the boot process continue.
f="$(GetPackageOriginalFile interception-tools /usr/lib/systemd/system/udevmon.service)"
sed -i 's/Wants=systemd-udev-settle.service/#Wants=systemd-udev-settle.service/' "$f"
sed -i 's/After=systemd-udev-settle.service/#After=systemd-udev-settle.service/' "$f"
# udevmon config from https://gitlab.com/interception/linux/plugins/space2meta
cat > "$(CreateFile /etc/interception/udevmon.yaml)" <<EOF
- JOB: intercept -g \$DEVNODE | caps2esc -m 1 | space2meta | uinput -d \$DEVNODE
  DEVICE:
    EVENTS:
      EV_KEY: [KEY_CAPSLOCK, KEY_ESC, KEY_SPACE]
EOF
# Enable udevmon
CreateLink /etc/systemd/system/multi-user.target.wants/udevmon.service /usr/lib/systemd/system/udevmon.service

### Run fixinputs whenever devices are added
cat > "$(CreateFile /etc/systemd/system/fixinputs@.path)" <<EOF
# Inspired by http://jasonwryan.com/blog/2014/01/20/udev/
# and http://www.ocsmag.com/2015/09/02/monitoring-file-access-for-dummies/
[Unit]
Description=Triggers the service that sets up external keyboard

[Path]
PathChanged=/dev/input/

[Install]
WantedBy=multi-user.target
EOF
cat > "$(CreateFile /etc/systemd/system/fixinputs@.service)" <<EOF
# Inspired by http://jasonwryan.com/blog/2014/01/20/udev/
# and http://www.ocsmag.com/2015/09/02/monitoring-file-access-for-dummies/
[Unit]
Description=Configure external keyboard for user %i

[Service]
Type=oneshot

ExecStart=/home/%i/bin/x11-run-as %i fixinputs
EOF
CreateLink /etc/systemd/system/fixinputs@jeremy.path /etc/systemd/system/fixinputs@.path
CreateLink /etc/systemd/system/multi-user.target.wants/fixinputs@jeremy.path /etc/systemd/system/fixinputs@jeremy.path

### Video card drivers
# Install the appropriate video card driver: https://wiki.archlinux.org/index.php/xorg#Driver_installation
# driconf is supposed to help with video tearing (see http://www.apolitech.com/2017/04/20how-to-solve-video-tearing-on-intel.html)
AddPackage xf86-video-intel # X.org Intel i810/i830/i915/945G/G965+ video drivers
# Needed for hardware acceleration on Parsec and Chrome. See
# https://github.com/jfly/dotfiles/commit/eef1e079114aaee1fe0740151a5340e1574b4659
# for details.
AddPackage intel-gpu-tools # Tools for development and testing of the Intel DRM driver
AddPackage libva-intel-driver # VA-API implementation for Intel G45 and HD Graphics family
AddPackage libva-utils # Intel VA-API Media Applications and Scripts for libva
AddPackage libvdpau-va-gl # VDPAU driver with OpenGL/VAAPI backend
cat > "$(CreateFile /etc/X11/xorg.conf.d/20-intel.conf)" <<EOF
Section "Device"
    Identifier "Intel Graphics"
    Driver "intel"
    Option "TearFree" "true"
EndSection
EOF

### Movie players
AddPackage kodi # A software media player and entertainment hub for digital media
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
AddPackage xorg-xfd # Displays all the characters in a font using either the X11 core protocol or libXft2
AddPackage ttf-opensans # Sans-serif typeface commissioned by Google
AddPackage gnu-free-fonts # A free family of scalable outline fonts
AddPackage noto-fonts-emoji # Google Noto emoji fonts
AddPackage ttf-bitstream-vera # Bitstream Vera fonts.
AddPackage ttf-liberation # Font family which aims at metric compatibility with Arial, Times New Roman, and Courier New
AddPackage --foreign nerd-fonts-ubuntu-mono # Patched font UbuntuMono from the nerd-fonts library
IgnorePath '/etc/fonts/conf.d/*'
IgnorePath '/usr/share/fonts/*'

### Windows compatibility
AddPackage dos2unix # Text file format converter
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
AddPackage maim # Utility to take a screenshot using imlib2
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
