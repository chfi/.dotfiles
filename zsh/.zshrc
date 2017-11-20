# executed when zsh is started as an interactive shell

# use antigen
source ~/.antigen/antigen.zsh

antigen use oh-my-zsh

antigen bundle git
antigen bundle history
antigen bundle git@github.com:spwhitt/nix-zsh-completions.git
antigen bundle vi-mode

antigen theme gallois

antigen apply
