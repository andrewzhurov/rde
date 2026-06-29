# Guix Home Recovery Guide — keeper@Ubuntu

Emergency rollback instructions when `guix home reconfigure` leaves the
`keeper` user in a broken state (can't log in, broken shell, missing
environment, etc.).

**Save a copy of this file on your phone or another device.**

---

## Current System Facts

| Item            | Value                                                    |
| --------------- | -------------------------------------------------------- |
| OS              | Ubuntu, GDM (GNOME Display Manager), X11 session         |
| User            | `keeper` (uid 1000), has `sudo`, login shell `/bin/bash` |
| Recovery user   | `user2` (uid 1001), login shell `/bin/bash`, **no sudo** |
| Root            | password likely locked (Ubuntu default)                  |
| System bash     | `/bin/bash` (Ubuntu, always available)                   |
| Guix bash       | `~/.guix-home/profile/bin/bash` (may break)              |
| `.profile`      | Sources `~/.guix-home/setup-environment` on every login  |
| Display manager | GDM on TTY1                                              |
| Free TTYs       | TTY3–TTY6 (Ctrl+Alt+F3 through F6)                       |

### Why login can break

`~/.profile` runs:

```
HOME_ENVIRONMENT=$HOME/.guix-home
. $HOME_ENVIRONMENT/setup-environment    ← if this fails, login fails
$HOME_ENVIRONMENT/on-first-login
```

A bad generation can make `setup-environment` error out, which kills
the login shell before you get a prompt.

---

## Recovery Procedure

### Step 1: Get to a TTY

Press **Ctrl + Alt + F3** (or F4, F5, F6) to switch to a text console.
GDM runs on TTY1; your graphical session is on TTY2.

You'll see:

```
Ubuntu 24.04 LTS hostname tty3

hostname login: _
```

### Step 2: Log in as keeper (bypassing .profile)

Even if `.profile` is broken, you CAN still log in — the login shell
is system `/bin/bash`, not the Guix one. The trick is to interrupt
`.profile` quickly.

**Method A — Fast Ctrl+C** (simplest):

1. Type `keeper` and press Enter
2. Type your password and press Enter
3. **Immediately spam Ctrl+C** as soon as you see any output — this
   interrupts the broken `setup-environment` before it kills the shell
4. You should land at a `$` prompt (bare bash, no Guix env)

**Method B — GRUB recovery mode** (if Ctrl+C doesn't work):

1. Reboot the machine
2. Hold **Shift** during boot (or press **Esc** repeatedly) to get GRUB
3. Select **Advanced options for Ubuntu** → **(recovery mode)**
4. Select **root — Drop to root shell prompt**
5. Run `mount -o remount,rw /` (recovery mode mounts read-only)
6. Now you have a root shell — go to Step 3 below

**Method C — From user2** (if set up, see Preparation section below):

1. **Ctrl+Alt+F4** → log in as `user2`
2. `sudo -i` to get root
3. Go to Step 3 below

### Step 3: Do the rollback

Once you have any shell (as keeper with bare bash, or as root):

```bash
# Use full paths — Guix may not be in PATH.

# 1. See all generations (most recent first):
/bin/ls -lt /var/guix/profiles/per-user/keeper/guix-home-*-link | head -5

# Output:
#   guix-home-44-link → /gnu/store/…-home   ← current (broken)
#   guix-home-43-link → /gnu/store/…-home   ← previous (probably good)
#   guix-home-42-link → /gnu/store/…-home   ← older

# 2. Switch to the previous generation (replace 43 with your number):
/bin/ln -sfn guix-home-43-link /var/guix/profiles/per-user/keeper/guix-home

# 3. Update ~/.guix-home to match:
/bin/ln -sfn "$(/bin/readlink -f /var/guix/profiles/per-user/keeper/guix-home)" \
             /home/keeper/.guix-home

# 4. Verify:
/bin/ls -la /home/keeper/.guix-home
/bin/ls -la /var/guix/profiles/per-user/keeper/guix-home
# Both should resolve to the same /gnu/store/…-home
```

### Step 4: Test the fix

```bash
# Source the rolled-back environment:
export HOME_ENVIRONMENT=/home/keeper/.guix-home
. $HOME_ENVIRONMENT/setup-environment

# If no errors, optionally run activation (restarts shepherd etc.):
/home/keeper/.guix-home/activate
```

### Step 5: Return to graphical session

Press **Ctrl + Alt + F2** to go back to GDM / your desktop.
Log out and log back in (or reboot).

---

## One-Liner Emergency Rollback

If you know the previous generation number (e.g. 43), the entire
rollback is two commands:

```bash
/bin/ln -sfn guix-home-43-link /var/guix/profiles/per-user/keeper/guix-home
/bin/ln -sfn "$(/bin/readlink -f /var/guix/profiles/per-user/keeper/guix-home)" /home/keeper/.guix-home
```

That's it. Log out, log back in.

---

## Preparation: Set Up user2 For Recovery

**Do this NOW, before you need it.** `user2` exists but currently has
no password and no sudo.

```bash
# As keeper (who has sudo):

# Set a password for user2
sudo passwd user2

# Give user2 sudo access for emergencies
sudo usermod -aG sudo user2

# Verify: Ctrl+Alt+F4, log in as user2, type 'sudo whoami' → root
```

With this done, if keeper can't log in at all:

1. Ctrl+Alt+F4 → log in as `user2`
2. `sudo /bin/ln -sfn guix-home-43-link /var/guix/profiles/per-user/keeper/guix-home`
3. `sudo /bin/ln -sfn "$(/bin/readlink -f /var/guix/profiles/per-user/keeper/guix-home)" /home/keeper/.guix-home`
4. Ctrl+Alt+F3 → log in as keeper → should work now

---

## How Guix Home Generations Work

Every `guix home reconfigure` creates a new **generation** — an immutable
snapshot stored in `/gnu/store/…-home`. Previous generations are never
deleted unless you explicitly run `delete-generations`.

### Symlink chain

```
~/.guix-home                                          (1)
  → /var/guix/profiles/per-user/keeper/guix-home      (2)
    → /var/guix/profiles/per-user/keeper/guix-home-N-link   (3)
      → /gnu/store/…-home                             (4)
```

1. **~/.guix-home** — `.profile` sources `setup-environment` from here
2. **/var/guix/profiles/per-user/keeper/guix-home** — "current" pointer
3. **guix-home-N-link** — one per generation (N = 1, 2, 3, …)
4. **/gnu/store/…-home** — immutable store item

### What each generation contains

```
/gnu/store/…-home/
├── activate           # Guile script: sets env, updates symlinks, (re)starts shepherd
├── setup-environment  # Shell script: sourced by .profile, sets PATH/XDG/etc.
├── on-first-login     # Shell script: runs once after generation switch
├── profile/           # Symlink to the Guix profile (all packages)
├── files/             # Managed dotfiles
├── channels.scm       # Channel snapshot used to build this generation
├── configuration.scm  # The home-environment config
└── provenance         # Build metadata
```

---

## Prevention: Before Reconfigure

### Always build first (no side effects)

```bash
guix home build ./src/andrewzhurov/configs.scm
```

If this succeeds, reconfigure is very likely safe.

### Test in a container (full isolation)

```bash
guix home container ./src/andrewzhurov/configs.scm
```

Runs the full home environment in a Linux namespace container:

- Separate PID/mount/user namespaces — nothing touches the host
- Full activation runs (shepherd, symlinks, env vars)
- Drops you into a login shell inside the container
- Exit to destroy — nothing persisted

### Note your current generation

```bash
readlink /var/guix/profiles/per-user/keeper/guix-home
# e.g. guix-home-44-link
```

Write down the number before reconfiguring.

---

## Agent Testing Workflow (Isolated)

Agents MUST NOT run `guix home reconfigure`. Instead:

### 1. Build only (no activation)

```bash
guix home build ./src/andrewzhurov/configs.scm
```

### 2. Container test (full isolation)

```bash
guix home container ./src/andrewzhurov/configs.scm
```

### 3. Channel-based testing for Guix fork changes

```bash
# 1. Commit changes to local fork
cd ~/gits/guix && git add -A && git commit -m "description"

# 2. Create channels file pointing to local fork
# 3. Build via time-machine --disable-authentication -C channels.scm
```

---

## Key Paths Reference

| Path                                                  | Purpose                                            |
| ----------------------------------------------------- | -------------------------------------------------- |
| `~/.guix-home`                                        | Symlink to current home generation                 |
| `~/.profile`                                          | Login entry; sources `setup-environment`           |
| `/var/guix/profiles/per-user/keeper/guix-home`        | Current generation pointer                         |
| `/var/guix/profiles/per-user/keeper/guix-home-N-link` | Per-generation symlinks                            |
| `/gnu/store/…-home/activate`                          | Guile activation script                            |
| `/gnu/store/…-home/setup-environment`                 | Shell env setup (PATH, XDG, etc.)                  |
| `/gnu/store/…-home/profile`                           | Guix profile with all 336 packages                 |
| `/bin/bash`                                           | System bash — always works, even if Guix is broken |
| `/bin/ln`, `/bin/ls`, `/bin/readlink`                 | System coreutils — always available                |

## Important Notes

- **Generations persist** until you run `guix home delete-generations`.
- **`guix gc`** can delete old store items. Don't run it right after
  breaking things — keep recent generations as fallbacks.
- **Symlink swap is atomic** — power failure mid-rollback can't corrupt.
- **`guix home switch-generation`** only works for current user
  (hardcoded to `$USER`). Cross-user recovery = manual `ln -sfn`.
- **GDM** runs on TTY1. Graphical session on TTY2.
  TTYs 3–6 are free for text console recovery.

## Worst Case: Nothing Works

If even GRUB recovery doesn't help:

1. Boot from a USB live image (Ubuntu installer)
2. Mount the root partition
3. Do the `ln -sfn` symlink swap on the mounted filesystem
4. Reboot from disk

The Guix store is at `/gnu/store/`.
Generations are at `/var/guix/profiles/per-user/keeper/`.
