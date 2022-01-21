### Audio
AddPackage alsa-utils # Advanced Linux Sound Architecture - Utilities
AddPackage pipewire-alsa # Low-latency audio/video router and processor - ALSA configuration
AddPackage pipewire-jack # Low-latency audio/video router and processor - JACK support
AddPackage pipewire-media-session # Low-latency audio/video router and processor - Session manager
AddPackage pipewire-pulse # Low-latency audio/video router and processor - PulseAudio replacement
AddPackage pamixer # Pulseaudio command-line mixer like amixer
AddPackage paprefs # Configuration dialog for PulseAudio
AddPackage pasystray # PulseAudio system tray (a replacement for padevchooser)
AddPackage pavucontrol # PulseAudio Volume Control

# Start various services
CreateLink /etc/systemd/user/pipewire-session-manager.service /usr/lib/systemd/user/pipewire-media-session.service
CreateLink /etc/systemd/user/pipewire.service.wants/pipewire-media-session.service /usr/lib/systemd/user/pipewire-media-session.service
CreateLink /etc/systemd/user/sockets.target.wants/pipewire-pulse.socket /usr/lib/systemd/user/pipewire-pulse.socket
CreateLink /etc/systemd/user/sockets.target.wants/pipewire.socket /usr/lib/systemd/user/pipewire.socket

# Ignore some stuff
IgnorePath '/var/lib/alsa/asound.state'
