export ZPLUG_HOME=/usr/local/opt/zplug
source $ZPLUG_HOME/init.zsh

export PATH="$HOME/.pyenv/bin:$PATH"
eval "$(pyenv init -)"
export SOLR_ENDPOINT='https://solr-admin:bibelot9585%2Fmellifluous@10-0-252-242-solr-admin-live5s.mercariapp.com/solr'

export GOROOT=`go env GOROOT`
#export GOROOT=`/usr/local/Cellar/go/1.12.4/libexec`
export GOPATH=$HOME/go
export PATH=$GOPATH/bin:$PATH

export PROFILE=~/.zshrc make setup 

# {{{ Alias
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi
export TERM=xterm-256color

#alias emacs='TERM=screen-16color emacs -nw'
alias emacs='emacs -nw'
#}}}

# {{{ Zplug Setting
if [ ! -f /usr/local/opt/zplug/init.zsh ]; then
  curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh| zsh
fi

source /usr/local/opt/zplug/init.zsh

zplug 'zplug/zplug', hook-build:'zplug --self-manage'
zplug mafredri/zsh-async, from:github
zplug sindresorhus/pure, use:pure.zsh, from:github, as:theme
zplug "mollifier/cd-gitroot"
zplug "zsh-users/zsh-syntax-highlighting", defer:3
zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-autosuggestions"
zplug "chrissicool/zsh-256color"
zplug "mollifier/cd-gitroot"
zplug "b4b4r07/emoji-cli"
zplug "b4b4r07/enhancd", use:enhancd.sh
zplug "peco/peco", as:command, from:gh-r
zplug 'b4b4r07/gomi', as:command, from:gh-r
zplug "rupa/z", use:z.sh
zplug "mrowa44/emojify", as:command
zplug "junegunn/fzf-bin", as:command, from:gh-r, rename-to:fzf

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi
zplug load --verbose > /dev/null

# }}}

# optionally define some options
autoload -U promptinit; promptinit
#prompt pure

export FLASK_APP="/Users/shohei.kikuchi/git/professor-x/professor-dashboard/professor_dashboard/routes.py"

autoload -Uz is-at-least

# Treat hook functions as array
typeset -ga chpwd_functions
typeset -ga precmd_functions
typeset -ga preexec_functions

# Simulate hook functions for older versions
if ! is-at-least 4.2.7; then
  function chpwd() { local f; for f in $chpwd_functions; do $f; done }
  function precmd() { local f; for f in $precmd_functions; do $f; done }
  function preexec() { local f; for f in $preexec_functions; do $f; done }
fi

function load-if-exists() { test -e "$1" && source "$1" }

# z - jump around {{{2
# https://github.com/rupa/z
_Z_CMD=j
_Z_DATA=$ZDOTDIR/.z
if is-at-least 4.3.9; then
  load-if-exists $ZPLUG_HOME/repos/rupa/z/z.sh
else
  _Z_NO_PROMPT_COMMAND=1
  load-if-exists $ZPLUG_HOME/repos/rupa/z/z.sh && {
    function precmd_z() {
      _z --add "$(pwd -P)"
    }
    precmd_functions+=precmd_z
  }
fi
test $? || unset _Z_CMD _Z_DATA _Z_NO_PROMPT_COMMAND
#}}}

fzf-z-search() {
    local res=$(z | sort -rn | cut -c 12- | fzf)
    if [ -n "$res" ]; then
        BUFFER+="cd $res"
        zle accept-line
    else
        return 1
    fi
}

zle -N fzf-z-search
#bindkey '' fzf-z-search

# Setting Brewfile
if [ -f $(brew --prefix)/etc/brew-wrap ];then
    source $(brew --prefix)/etc/brew-wrap
fi


# using tmux
function is_exists() { type "$1" >/dev/null 2>&1; return $?; }
function is_osx() { [[ $OSTYPE == darwin* ]]; }
function is_screen_running() { [ ! -z "$STY" ]; }
function is_tmux_runnning() { [ ! -z "$TMUX" ]; }
function is_screen_or_tmux_running() { is_screen_running || is_tmux_runnning; }
function shell_has_started_interactively() { [ ! -z "$PS1" ]; }
function is_ssh_running() { [ ! -z "$SSH_CONECTION" ]; }
function tmux_automatically_attach_session()
{
    if is_screen_or_tmux_running; then
        ! is_exists 'tmux' && return 1

        if is_tmux_runnning; then
            echo "${fg_bold[red]} _____ __  __ _   ___  __ ${reset_color}"
            echo "${fg_bold[red]}|_   _|  \/  | | | \ \/ / ${reset_color}"
            echo "${fg_bold[red]}  | | | |\/| | | | |\  /  ${reset_color}"
            echo "${fg_bold[red]}  | | | |  | | |_| |/  \  ${reset_color}"
            echo "${fg_bold[red]}  |_| |_|  |_|\___//_/\_\ ${reset_color}"
        elif is_screen_running; then
            echo "This is on screen."
        fi
    else
        if shell_has_started_interactively && ! is_ssh_running; then
            if ! is_exists 'tmux'; then
                echo 'Error: tmux command not found' 2>&1
                return 1
            fi
            if tmux has-session >/dev/null 2>&1 && tmux list-sessions | grep -qE '.*]$'; then
                # detached session exists
                tmux list-sessions
                echo -n "Tmux: attach? (y/N/num) "
                read
                if [[ "$REPLY" =~ ^[Yy]$ ]] || [[ "$REPLY" == '' ]]; then
                    tmux attach-session
                    if [ $? -eq 0 ]; then
                        echo "$(tmux -V) attached session"
                        return 0
                    fi
                elif [[ "$REPLY" =~ ^[0-9]+$ ]]; then
                    tmux attach -t "$REPLY"
                    if [ $? -eq 0 ]; then
                        echo "$(tmux -V) attached session"
                        return 0
                    fi
                fi
            fi

            if is_osx && is_exists 'reattach-to-user-namespace'; then
                # on OS X force tmux's default command
                # to spawn a shell in the user's namespace
                tmux_config=$(cat $HOME/.tmux.conf <(echo 'set-option -g default-command "reattach-to-user-namespace -l $SHELL"'))
                tmux -f <(echo "$tmux_config") new-session && echo "$(tmux -V) created new session supported OS X"
            else
                tmux new-session && echo "tmux created new session"
            fi
        fi
    fi
}
tmux_automatically_attach_session

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/shohei.kikuchi/Downloads/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/shohei.kikuchi/Downloads/google-cloud-sdk/path.zsh.inc'; fi

# direnv
export EDITOR=emacs
eval "$(direnv hook zsh)"
export ANYENV_ROOT="/Users/shohei.kikuchi/.anyenv"
export PATH=$PATH:"/Users/shohei.kikuchi/.anyenv/bin"
eval "$(anyenv init -)"
export PATH="$(brew --prefix php@7.1)/bin:$PATH"

# using brew-file
if [ -f $(brew --prefix)/etc/brew-wrap ];then
    source $(brew --prefix)/etc/brew-wrap
fi

# kuebctlc alias
alias k='kubectl'
source <(kubectl completion zsh)

# using kubernetes prompt info fo zsh
source $HOME/.ghq/github.com/shopetan/dotfiles/kube-ps1/share/kube-ps1.sh
KUBE_PS1_NS_ENABLE=false
KUBE_PS1_SYMBOL_ENABLE=false
KUBE_PS1_PREFIX=""
KUBE_PS1_SUFFIX=""
KUBE_PS1_CTX_COLOR="green"
PROMPT='$(kube_ps1)'$PROMPT

kube_context='$(color_kube_context $(kube_ps1))'
function color_kube_context() {
    if [ "$(echo $1 | grep prod)" ]; then
        echo -e "%{$bg[red]%}%{$fg[white]%}$1%{$reset_color%}"
    else
        echo -e "$1"
    fi
}

# ghq using peco
alias g='cd $(ghq root)/$(ghq list | peco)'


# jEnv
export PATH="$JENV_ROOT/bin:$PATH"
eval "$(jenv init -)"