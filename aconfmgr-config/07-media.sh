### Beets
AddPackage beets # Flexible music library manager and tagger
AddPackage python-pyacoustid # Bindings for Chromaprint acoustic fingerprinting and the Acoustid API
AddPackage python-eyed3 # A Python module and program for processing information about mp3 files
AddPackage --foreign mp3val # A tool for validating and repairing MPEG audio streams
AddPackage abcde # Frontend command-line utility that grabs tracks off a CD, encodes them to ogg or mp3 format, and tags them, all in one go

### MPD
AddPackage mpd # Flexible, powerful, server-side application for playing music
# TODO: figure out why mpd is so messy
IgnorePath '/var/lib/mpd/*'
AddPackage --foreign ashuffle # Automatic library-wide shuffle for mpd.
AddPackage mpc # Minimalist command line interface to MPD
AddPackage --foreign mpdscribble # MPD client which submits track info to {Libre,Last}.fm
AddPackage ncmpcpp # Almost exact clone of ncmpc with some new features
AddPackage --foreign mcg # A covergrid for the Music Player Daemon.
AddPackage --foreign vimpc-git # Vi/vim inspired client for Music Player Daemon (MPD)
cat > "$(CreateFile /etc/acpi/events/cd_play)" <<EOF
event=cd/play
action=xdotool key --clearmodifiers XF86AudioPlay
EOF
cat > "$(CreateFile /etc/acpi/events/lid)" <<EOF
event=button/lid
action=/home/jeremy/bin/x11-run-as jeremy lid.sh %e
EOF

### Media editing (images, audio, video)
AddPackage gimp # GNU Image Manipulation Program
AddPackage inkscape # Professional vector graphics editor
AddPackage audacity # A program that lets you manipulate digital audio waveforms
AddPackage avidemux-qt # Graphical tool to edit video (filter/re-encode/split) - Qt GUI
AddPackage audacity # A program that lets you manipulate digital audio waveforms
AddPackage avidemux-qt # Graphical tool to edit video (filter/re-encode/split) - Qt GUI
