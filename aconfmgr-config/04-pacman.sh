### pacman configuration
AddPackage pacman-contrib # Contributed scripts and tools for pacman systems

IgnorePath '/etc/pacman.d/mirrorlist'
IgnorePath '/var/lib/pacman/local/*' # package metadata
IgnorePath '/var/lib/pacman/sync/*.db' # repos
IgnorePath '/var/lib/pacman/sync/*.db.sig' # repo sigs
IgnorePath '/var/lib/pacman/sync/*.files' # for pacman -F
IgnorePath '/etc/pacman.d/gnupg/*' # keyring

pacman_conf="$(GetPackageOriginalFile pacman /etc/pacman.conf)"
function IgnorePkg() {
    local pkg="$1"
    sed -i "s/^#\?\(IgnorePkg *=.*\)/\1 ${pkg}/" "$pacman_conf"
}

echo "[multilib]
Include = /etc/pacman.d/mirrorlist" >> "$pacman_conf"

### bootstrapping/aconfmgr
AddPackage pacutils
AddPackage --foreign alma # Create Arch Linux based live USB

### pacaur
AddPackage --foreign pacaur # An AUR helper that minimizes user interaction

### GPG
AddPackage gnupg
CreateLink /etc/systemd/user/sockets.target.wants/dirmngr.socket /usr/lib/systemd/user/dirmngr.socket
CreateLink /etc/systemd/user/sockets.target.wants/gpg-agent-browser.socket /usr/lib/systemd/user/gpg-agent-browser.socket
CreateLink /etc/systemd/user/sockets.target.wants/gpg-agent-extra.socket /usr/lib/systemd/user/gpg-agent-extra.socket
CreateLink /etc/systemd/user/sockets.target.wants/gpg-agent-ssh.socket /usr/lib/systemd/user/gpg-agent-ssh.socket
CreateLink /etc/systemd/user/sockets.target.wants/gpg-agent.socket /usr/lib/systemd/user/gpg-agent.socket

### Auto update pacman mirrorlist
AddPackage reflector # A Python 3 module and script to retrieve and filter the latest Pacman mirror list.
cat > "$(CreateFile /etc/pacman.d/hooks/mirrorupgrade.hook)" <<EOF
[Trigger]
Operation = Upgrade
Type = Package
Target = pacman-mirrorlist

[Action]
Description = Updating pacman-mirrorlist with reflector and removing pacnew.
When = PostTransaction
Depends = reflector
Exec = /bin/sh -c "reflector --country 'United States' --latest 200 --age 24 --sort rate --save /etc/pacman.d/mirrorlist; rm -f /etc/pacman.d/mirrorlist.pacnew"
EOF
