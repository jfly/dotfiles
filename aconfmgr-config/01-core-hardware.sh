### Disable the obnoxious PC speaker beep.
# Trick from https://wiki.archlinux.org/title/PC_speaker#Disable_PC_Speaker
cat > "$(CreateFile /etc/modprobe.d/nobeep.conf)" << EOF
blacklist pcspkr
EOF

### Don't suspend on closing the lid.
cat > "$(CreateFile /etc/systemd/logind.conf.d/no-suspend-on-lidswitch.conf)" <<EOF
[Login]
HandleLidSwitch=ignore
EOF
# Configure other things to happen when the lid is closed.
cat > "$(CreateFile /etc/acpi/events/lid)" <<EOF
event=button/lid
action=/home/jeremy/bin/x11-run-as jeremy lid.sh %e
EOF

### Suspend when the user presses the power button.
cat > "$(CreateFile /etc/systemd/logind.conf.d/suspend-on-powerbutton.conf)" <<EOF
[Login]
HandlePowerKey=suspend
EOF
