#!/usr/bin/env bash

# remove conflicting Super1, SuperN shortcuts; and set switch-to-workspace as in rde
for i in {1..9}
do
    gsettings set org.gnome.shell.keybindings switch-to-application-$i "[]"
done

for i in {1..9}
do
    gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-$i "[]"
    gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-$i "['<Super>${i}']"
done

gsettings set org.gnome.shell.extensions.dash-to-dock app-hotkey-10 "[]"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-10 "['<Super>0']"


# rebind overlay key to RDE's - no luck, <Super> alon isn't triggered, and <Super><Shift>d can't be set as overlay-key
# rebind overlay-key to right Super, to be triggered by xcape Alt+Shift+d
gsettings set org.gnome.mutter overlay-key 'Super_L'
# gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-5 "['<Super_L>']" ## <Super> alone isn't triggered

gsettings set org.gnome.shell.keybindings shift-overview-down "[]" # been binded to Super Alt Down
gsettings set org.gnome.shell.keybindings shift-overview-up "[]" # been binded to Super Alt Up

gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-left  "['<Super><Control>b', '<Super><Alt>Left']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-right "['<Super><Control>f', '<Super><Alt>Right']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-up    "['<Super><Control>p', '<Super><Alt>Up']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-down  "['<Super><Control>n', '<Super><Alt>Down']"

gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-left  "['<Shift><Super><Alt>Left']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-right "['<Shift><Super><Alt>Right']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-up    "['<Shift><Super><Alt>Up']"
gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-down  "['<Shift><Super><Alt>Down']"
