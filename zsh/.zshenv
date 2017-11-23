# setting user env variables


# export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/local/lib:"$NIX_LINK/lib/pkgconfig"

# OS specific zshenvs
case "$(uname)" in
  ("Darwin")
    export PATH="$HOME/.local/bin/:$PATH"
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
