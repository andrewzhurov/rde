#!/usr/bin/env bash
# Workspaces as on haus: a static 3x3 grid, laid out and navigated in 2D by
# the Workspace Matrix extension (extensions.gnome.org/extension/1485).
# Workspace Grid (extension 484) is unmaintained and stops at GNOME 3.30.
#
# Run inside the GNOME session.  A freshly installed extension is loaded at
# the next login (Wayland can't restart the shell in place).  Workspace
# keybindings are set by configure-gnome-wm-shortcuts.sh.

set -euo pipefail

WSMATRIX=wsmatrix@martin.zurowietz.de
# Not ROWS/COLUMNS: bash overwrites $COLUMNS with the terminal width.
GRID_ROWS=3
GRID_COLS=3

gsettings set org.gnome.mutter dynamic-workspaces false
gsettings set org.gnome.desktop.wm.preferences num-workspaces $((GRID_ROWS * GRID_COLS))

# Via dconf: gsettings doesn't know the extension's schema before it's installed.
dconf write /org/gnome/shell/extensions/wsmatrix/num-rows $GRID_ROWS
dconf write /org/gnome/shell/extensions/wsmatrix/num-columns $GRID_COLS

# Alt+Tab and the dock show only the current workspace's windows.
gsettings set org.gnome.shell.app-switcher current-workspace-only true
gsettings set org.gnome.shell.extensions.dash-to-dock isolate-workspaces true

if gnome-extensions info "$WSMATRIX" >/dev/null 2>&1; then
    # Already loaded by the running shell.
    gnome-extensions enable "$WSMATRIX"
    gnome-extensions info "$WSMATRIX" | grep -E '^ *(Version|Enabled|State):'
    exit 0
fi

if [ ! -d "$HOME/.local/share/gnome-shell/extensions/$WSMATRIX" ]; then
    # Not the browser-style InstallRemoteExtension D-Bus call: its modal
    # confirmation dialog can hang the lock screen if it's still up when the
    # screen locks.
    shell_version=$(gnome-shell --version | awk '{print $3}')
    url="https://extensions.gnome.org/download-extension/$WSMATRIX.shell-extension.zip?shell_version=$shell_version"
    zip=$(mktemp --suffix=.zip)
    trap 'rm -f "$zip"' EXIT
    if command -v curl >/dev/null; then
        curl -fsSL -o "$zip" "$url"
    else
        wget -qO "$zip" "$url"
    fi
    gnome-extensions install --force "$zip"
fi

# `gnome-extensions enable' only works on extensions the shell has loaded,
# so list it in enabled-extensions directly; the shell reads that at login.
enabled=$(gsettings get org.gnome.shell enabled-extensions)
case $enabled in
    *"'$WSMATRIX'"*) ;;
    "@as []" | "[]") gsettings set org.gnome.shell enabled-extensions "['$WSMATRIX']" ;;
    *) gsettings set org.gnome.shell enabled-extensions "${enabled%]}, '$WSMATRIX']" ;;
esac

echo "Workspace Matrix installed and enabled; log out and back in to load it."
