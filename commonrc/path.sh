export PATH="/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/lib/jvm/default/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl"
export PATH=$PATH:$HOME/bin
export PATH=$PATH:$HOME/.gem/ruby/2.6.0/bin

# `pip install --user` installs binaries here.
export PATH=$PATH:$HOME/.local/bin

export PATH=$PATH:$HOME/thirdrepos/google-cloud-sdk/bin

# add Pulumi to the PATH
export PATH=$PATH:$HOME/.pulumi/bin

# The next line updates PATH for the Google Cloud SDK.
if [ -f "$HOME/google-cloud-sdk/path.zsh.inc" ]; then . "$HOME/google-cloud-sdk/path.zsh.inc"; fi

# The next line enables shell command completion for gcloud.
if [ -f "$HOME/google-cloud-sdk/completion.zsh.inc" ]; then . "$HOME/google-cloud-sdk/completion.zsh.inc"; fi
