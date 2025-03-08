FROM rustlang/rust:nightly

RUN curl -L https://github.com/rust-lang/rust-analyzer/releases/download/2022-08-15/rust-analyzer-x86_64-unknown-linux-gnu.gz | gunzip -c - > /rust-analyzer
RUN chmod +x /rust-analyzer

ENTRYPOINT ["/rust-analyzer"]
