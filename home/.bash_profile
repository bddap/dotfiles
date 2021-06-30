source ~/.bashrc

if [ -e /home/a/.nix-profile/etc/profile.d/nix.sh ]; then . /home/a/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
. "$HOME/.cargo/env"
