function __user_host
  set -l content
  if [ (id -u) = "0" ];
    echo -n (set_color --bold red)
  else
    echo -n (set_color --bold green)
  end
  echo -n $USER@(hostname | cut -d . -f 1) (set color normal)
end

function __current_path
  if set -l _pwd (prompt_pwd | string trim --chars '/' --left)
      if [ $_pwd ]
        set _pwd "std/$_pwd"
      else
        echo -n (set_color yellow)std
        return
      end
  end
  echo -n "$(set_color brred) [ "
  echo -n (set_color yellow)
  echo -n (string split '/' $_pwd | string join "$(set_color brred)::$(set_color yellow)")
  echo -n "$(set_color brred) ]"
  echo -n (set_color normal)
end

function _git_branch_name
  echo (command git symbolic-ref HEAD 2> /dev/null | sed -e 's|^refs/heads/||')
end

function _git_is_dirty
  echo (command git status -s --ignore-submodules=dirty 2> /dev/null)
end

function __git_status
  if [ (_git_branch_name) ]
    set -l git_branch (_git_branch_name)

    if [ (_git_is_dirty) ]
      set git_color 'bryellow'
    else
      set git_color 'green'
    end

    echo -n (set_color $git_color) '<'$git_branch'>' (set_color normal)
  end
end

function __status
  if [ $_status -ne 0 ]
    echo -n (set_color "red")"[$_status]"(set_color normal)
  end
end

function fish_mode_prompt; end

function __fish_mode_prompt
  switch $fish_bind_mode
    case default
        set_color --bold brred
        echo '[N]'
    case insert
        set_color --bold green
        echo '[I]'
    case replace_one replace
        set_color --bold red
        echo '[R]'
    case visual
        set_color --bold blue
        echo '[V]'
    case '*'
        set_color --bold brred
        echo '[?]'
    end
  set_color normal
end

function fish_prompt
  set -g _status $status
  if [ $_status -ne 0 ]
    set -f prompt_symbol (set_color red)"✘"(set_color normal)
  else
    set -f prompt_symbol "λ"
  end
  echo -n (set_color white)"╭─"(set_color normal)
  __user_host
  __current_path
  __git_status
  echo ''
  echo (set_color white)"╰─"$prompt_symbol" "(set_color normal)
end

function fish_right_prompt
  echo -n $CONDA_PROMPT_MODIFIER
  __status
  echo -n ' '
  __fish_mode_prompt
end
