#!/usr/bin/env bash
# based on https://www-grepular-com.translate.goog/Sandbox_Rust_Development_with_Rust_Analyzer?_x_tr_sl=en&_x_tr_tl=ru&_x_tr_hl=en&_x_tr_pto=op,wapp
set -e

IMAGE_NAME="andrewzhurov/rust-env"

# Build the image if it does not exist
if true; then #[[ $(docker images --filter "reference=$IMAGE_NAME" -q) == "" ]]; then
    docker build -q -t "$IMAGE_NAME" . -f-<<EOF
 # latest nightly as of 2022-08-23
 # https://hub.docker.com/layers/rustlang/rust/nightly/images/sha256-6b9ed77d73268500dcebbd0b6b300cb7435026968681eb20691c85e204eaa355?context=explore
 FROM rustlang/rust@sha256:6b9ed77d73268500dcebbd0b6b300cb7435026968681eb20691c85e204eaa355

 RUN curl -L https://github.com/rust-lang/rust-analyzer/releases/download/2022-08-15/rust-analyzer-x86_64-unknown-linux-gnu.gz | gunzip -c - > /rust-analyzer
 RUN chmod +x /rust-analyzer

 ENTRYPOINT ["$@"]
EOF

fi

# cargo check yells about no write permission showing as error in emacs on save, so it's rw instead of a more safe ro
docker run \
  -u $(stat -c '%u:%g' .) \
  -i --rm \
  -v "$PWD:$PWD:rw" \
  --workdir "$PWD" \
  "$IMAGE_NAME" # "$@"
