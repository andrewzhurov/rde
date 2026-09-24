#!/usr/bin/env bash
# Keyboard as on haus: CapsLock acts as Ctrl, and key repeat delay/rate.
#
# delay and repeat-interval are what the Settings > Accessibility > Typing >
# Repeat Keys "Delay" and "Speed" sliders set; both are in milliseconds and
# lower is faster.
#
# haus also has the 'altwin:swap_alt_win' xkb option, likely for its Apple
# keyboard: with it, Ctrl/Super/Alt sit in the same order left of Space as on
# a PC keyboard.  Don't add it on PC keyboards.

set -euo pipefail

add_xkb_option() {
    local opt=$1 cur
    cur=$(gsettings get org.gnome.desktop.input-sources xkb-options)
    case $cur in
        *"'$opt'"*) return 0 ;;
        "@as []" | "[]") cur="['$opt']" ;;
        *) cur="${cur%]}, '$opt']" ;;
    esac
    gsettings set org.gnome.desktop.input-sources xkb-options "$cur"
}

add_xkb_option ctrl:nocaps

gsettings set org.gnome.desktop.peripherals.keyboard repeat true
gsettings set org.gnome.desktop.peripherals.keyboard delay 235
gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 19
