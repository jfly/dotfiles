# This is where pacman installs things.
export PATH="/usr/bin"

# nix-env installs binaries here.
export PATH="$HOME/.nix-profile/bin:$PATH"

# Useful binaries that I don't want to version control, probably because
# they've got secrets in them. Hopefully I can get rid of this someday.
export PATH="/usr/local/bin:$PATH"

# Useful scripts managed in my dotfiles repo.
export PATH="$HOME/bin:$PATH"
