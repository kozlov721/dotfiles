set -l confedit_commands "(cat $HOME/.config/confedit/confedit.list | cut -d ' ' -f 1)"
complete -f -c confedit -n "not __fish_seen_subcommand_from $confedit_commands" -a $confedit_commands

