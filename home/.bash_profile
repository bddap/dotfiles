source ~/.bashrc

export PATH="$HOME/.cargo/bin:$PATH"
if [ -e /home/a/.nix-profile/etc/profile.d/nix.sh ]; then . /home/a/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
