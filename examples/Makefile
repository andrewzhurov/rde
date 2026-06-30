CHANNELS_FILE=./env/guix/rde-configs/env/guix/channels.scm
CHANNELS_FILE_LOCAL_RDE=./env/guix/rde-configs/env/guix/channels-ci.scm
CHANNELS_FILE_LATEST_GUIX=./env/guix/rde-configs/env/guix/channels-ci-latest-guix.scm
GUIXTM=GUILE_LOAD_PATH="../src:../tests:./src" guix time-machine -L ./env/guix -C ${CHANNELS_FILE}
GUIXTM_LOCAL_RDE=guix time-machine -L ./env/guix -C ${CHANNELS_FILE_LOCAL_RDE}
GUIXTM_LATEST_GUIX=guix time-machine -L ./env/guix -C ${CHANNELS_FILE_LATEST_GUIX}

GUIX=$(GUIXTM) --

# Local rde module dir, passed to `guix home/system` subcommands so that
# our local edits to (rde ...) modules override the rde channel's compiled
# copies.  GUILE_LOAD_PATH alone is not enough: the channel ships
# precompiled .go files that shadow the source found via GUILE_LOAD_PATH.
#
# NB: do NOT add `-L ../tests' here.  guix discovers and loads every module
# under the load path whose name starts with (rde ...); tests/rde/*.scm
# declare such module names (e.g. (rde test-runners)) and pull in `gider',
# which is not on the load path, causing `test-runner-default: unbound
# variable' during home reconfigure.  The config needs nothing from ../tests.
RDE_LOAD_PATH=-L ../src

SRC_DIR=./src
CONFIGS=${SRC_DIR}/andrewzhurov/configs.scm
# https://ci.guix.gnu.org
SUBSTITUTE_URLS=--substitute-urls='https://substitutes.nonguix.org https://bordeaux.guix.gnu.org https://ci.guix.trop.in'
PULL_EXTRA_OPTIONS=--allow-downgrades --url="https://codeberg.org/guix/guix-mirror.git" --disable-authentication --substitute-urls='https://substitutes.nonguix.org https://bordeaux.guix.gnu.org https://ci.guix.trop.in'

ROOT_MOUNT_POINT=/mnt

VERSION=latest

auth-nonguix:
	sudo guix archive --authorize < ./src/andrewzhurov/files/keys/nonguix-key.pub

repl: ares
	${GUIX} repl \
	-L ../files/emacs/gider/src --listen=tcp:37146

g:
	${GUIX} shell -L ./env/guix -L ./env/dev guile guile-ares-rs ${SUBSTITUTE_URLS} \
	-e '(@ (rde-configs env dev packages) guix-package)' \
	-e '(@ (rde-configs env dev packages) channels-package)' \
	-- guile

ares: env/sync
	${GUIX} shell -L ./env/guix -L ./env/dev guile guile-ares-rs ${SUBSTITUTE_URLS} \
	-e '(@ (rde-configs env dev packages) guix-package)' \
	-e '(@ (rde-configs env dev packages) channels-package)' \
	-- guile \
	-L ./env/guix \
	-L ./env/dev \
	-c \
"(begin (use-modules (guix gexp)) #;(load gexp reader macro globally) \
((@ (ares server) run-nrepl-server)))"

env/guix/rde/env/guix/channels.scm: ../env/guix/rde/env/guix/channels.scm
	mkdir -p $(@D)
	chmod +w $@ | true # allow write for override
	echo -e ";;; This is an automatic copy of RDE's channels.scm" > $@
	echo -e ";;; Do not edit it manually\n" >> $@
	cat $< >> $@
	chmod -w $@ # disallow write so we don't accidentally write to it

env/sync: env/guix/rde/env/guix/channels.scm
guix: env/sync

guix-pull:
	guix pull -L ./env/guix -C ${CHANNELS_FILE}

ixy/home/build: guix
	RDE_TARGET=ixy-home ${GUIX} home \
	build ${CONFIGS}

t450/home/reconfigure: guix
	RDE_TARGET=t450-home ${GUIX} home \
	reconfigure ${CONFIGS} --allow-downgrades --substitute-urls='https://substitutes.nonguix.org https://bordeaux.guix.gnu.org https://ci.guix.trop.in' #    https://ci.guix.gnu.org

t450/system/reconfigure: auth-nonguix guix
	sudo RDE_TARGET=t450-system ${GUIX} system \
	reconfigure ${CONFIGS} --allow-downgrades --substitute-urls='https://substitutes.nonguix.org https://bordeaux.guix.gnu.org https://ci.guix.trop.in' # https://ci.guix.gnu.org

haus/home/reconfigure: guix
	RDE_TARGET=haus-home ${GUIX} home \
	reconfigure ${CONFIGS} --allow-downgrades --substitute-urls='https://substitutes.nonguix.org https://bordeaux.guix.gnu.org https://ci.guix.trop.in' #    https://ci.guix.gnu.org

haus/system/reconfigure: auth-nonguix guix
	sudo RDE_TARGET=haus-system ${GUIX} system \
	reconfigure ${CONFIGS} --allow-downgrades --substitute-urls='https://substitutes.nonguix.org https://bordeaux.guix.gnu.org https://ci.guix.trop.in' # https://ci.guix.gnu.org

keeper/home/build: guix
	RDE_TARGET=keeper-home ${GUIX} home \
	build ${RDE_LOAD_PATH} ${CONFIGS}

keeper/home/reconfigure: guix
	RDE_TARGET=keeper-home ${GUIX} home \
	reconfigure ${RDE_LOAD_PATH} ${CONFIGS} --allow-downgrades ${SUBSTITUTE_URLS}

cow-store:
	sudo herd start cow-store ${ROOT_MOUNT_POINT}

haus/system/init: guix
	RDE_TARGET=haus-system ${GUIX} system \
	init ${CONFIGS} ${ROOT_MOUNT_POINT} --allow-downgrades --substitute-urls='https://substitutes.nonguix.org https://bordeaux.guix.gnu.org https://ci.guix.trop.in' # https://ci.guix.gnu.org

t450/system/init: guix
	RDE_TARGET=t450-system ${GUIX} system \
	init ${CONFIGS} ${ROOT_MOUNT_POINT} --allow-downgrades --substitute-urls='https://substitutes.nonguix.org https://bordeaux.guix.gnu.org https://ci.guix.trop.in' # https://ci.guix.gnu.org

target:
	mkdir -p target

live/image/build: guix
	RDE_TARGET=live-system ${GUIX} system image --image-type=iso9660 \
	${CONFIGS}

target/rde-live.iso: guix target
	RDE_TARGET=live-system ${GUIX} system image --image-type=iso9660 \
	${CONFIGS} -r target/rde-live-tmp.iso
	mv -f target/rde-live-tmp.iso target/rde-live.iso

target/release:
	mkdir -p target/release

# TODO: Prevent is rebuilds.
release/rde-live-x86_64: target/rde-live.iso target/release
	cp -df $< target/release/rde-live-${VERSION}-x86_64.iso
	gpg -ab target/release/rde-live-${VERSION}-x86_64.iso

minimal-emacs: guix
	${GUIX} shell --pure -Df ./src/rde-configs/minimal-emacs.scm \
	-E '.*GTK.*|.*XDG.*|.*DISPLAY.*' \
	--rebuild-cache -- emacs -q \
	--eval "(load \"~/.config/emacs/early-init.el\")"
	#--eval "(require 'feature-loader-portable)"

minimal/home/build: guix
	${GUIX} home build ./src/rde-configs/minimal.scm

minimal/home/build/ci:
	${GUIXTM_LOCAL_RDE} -- \
	home build ./src/rde-configs/minimal.scm

minimal/home/build/ci-no-auth:
	${GUIXTM_LOCAL_RDE} \
	--disable-authentication -- \
	home build ./src/rde-configs/minimal.scm

minimal/home/build/ci-latest-guix:
	${GUIXTM_LATEST_GUIX} -- \
	home build ./src/rde-configs/minimal.scm

minimal/home/build/ci-latest-guix-no-auth:
	${GUIXTM_LATEST_GUIX} \
	--disable-authentication -- \
	home build ./src/rde-configs/minimal.scm

cloud-rde: guix
	${GUIX} system image -e '(@ (rde-configs hosts cloud) cloud-image)'

clean-target:
	rm -rf ./target

clean: clean-target
