(define-module (rde nrepl))

(use-modules (system repl server))

(spawn-server (make-tcp-server-socket))
