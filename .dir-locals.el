((nil . ((cider-clojure-cli-global-options . "-A:dev:cider")
         (cider-default-cljs-repl . shadow)
         (cider-preferred-build-tool . shadow-cljs)
         (cider-shadow-default-options . "app")
         (cider-shadow-cljs-global-options . "-A:dev:cider")
         (eval . (progn
                   (make-variable-buffer-local 'cider-jack-in-nrepl-middlewares)
                   (add-to-list 'cider-jack-in-nrepl-middlewares "shadow.cljs.devtools.server.nrepl/middleware"))))))
