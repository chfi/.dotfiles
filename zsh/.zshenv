# setting user env variables


# export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/local/lib:"$NIX_LINK/lib/pkgconfig"

export PATH="$HOME/.local/bin/:$PATH"
export PATH="$HOME/.cargo/bin/:$PATH"

# Use emacs for editing everything, with emacs daemon
export EDITOR="emacsclient -t" # open in terminal
export VISUAL="emacsclient -c -a emacs" # open a new emacs instance if there isn't one

# OS specific zshenvs
case "$(uname)" in
  ("Darwin")
    export PATH="/usr/local/bin:$PATH"
    export PATH="/usr/local/sbin:$PATH"
    source ~/.zshenv.osx
    ;;
  # ("Linux")
  #   source ~/.zshenv.linux
  #   ;;
esac

case "$(hostname)" in
  ("jupiter-nix")
    source ~/.zshenv.nixos
    ;;
esac
