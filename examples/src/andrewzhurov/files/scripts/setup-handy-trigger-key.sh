#!/usr/bin/env bash
# Pick the key for Handy's hold-to-talk on GNOME Wayland (Ubuntu 26.04 / GNOME 50),
# for the gnome-portal build of Handy (~/gits/Handy, branch gnome-portal).
#
#   setup-handy-trigger-key.sh copilot   # ThinkPad Copilot key: no remap needed
#   setup-handy-trigger-key.sh rctrl     # Right Ctrl -> F14  (needs sudo)
#   setup-handy-trigger-key.sh ralt      # Right Alt  -> F14  (needs sudo)
#   setup-handy-trigger-key.sh --undo    # remove the Right Ctrl/Alt remap
#
# Then open Handy -> Shortcut, click it and press the key; GNOME asks once to
# allow it. That Handy build binds through GNOME's GlobalShortcuts portal, where:
#  - The Copilot key works as is. Its firmware sends Super+Shift+F23 and
#    releases F23 first, so GNOME reports both press and release. (Handy
#    translates it; GNOME stores it as "<Super><Shift>TouchpadOff".) A scancode
#    remap can't turn it into a lone key: the chord comes from the firmware.
#  - A lone modifier (Right Ctrl/Alt) can't be a GNOME shortcut. And in combos
#    Mutter drops the release if the modifier comes up first, which breaks
#    hold-to-talk. So they're remapped in the kernel (udev hwdb, the supported
#    way; see /usr/lib/udev/hwdb.d/60-keyboard.hwdb) to F14: no keyboard here has
#    it and GNOME binds nothing to it (XKB calls it XF86Launch5).
# The remapped key loses its original function. Remapping in the kernel, not in
# XKB, means every layer (console, GNOME, Handy) sees the same single key.
set -euo pipefail

HWDB=/etc/udev/hwdb.d/62-handy-trigger.hwdb
TARGET=f14   # KEY_F14

reload() {
  sudo systemd-hwdb update
  sudo udevadm trigger --subsystem-match=input --action=change
}

case ${1:-} in
  copilot)
    echo "Nothing to remap. In Handy, set the shortcut by pressing the Copilot key."
    exit 0 ;;
  # Scancodes as the atkbd driver numbers them on a translated AT keyboard:
  # the e0 prefix becomes the high bit (e0 1d -> 9d), per drivers/input/keyboard/atkbd.c.
  rctrl) SCANCODE=9d ;;   # Right Ctrl
  ralt)  SCANCODE=b8 ;;   # Right Alt
  --undo)
    sudo rm -f "$HWDB"; reload
    echo "removed $HWDB"; exit 0 ;;
  *) sed -n '2,8p' "$0"; exit 2 ;;
esac

# Built-in (AT) keyboard only: evdev:atkbd matches it on any machine. USB or
# Bluetooth keyboards use different scancodes and would need their own entry.
printf '%s\n' "# Written by setup-handy-trigger-key.sh $1 (see it for why)." \
  "evdev:atkbd:dmi:*" " KEYBOARD_KEY_$SCANCODE=$TARGET" | sudo tee "$HWDB" >/dev/null
reload
echo "Wrote $HWDB. In Handy, set the shortcut by pressing the key (it now sends F14)."
