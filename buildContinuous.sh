while inotifywait -qq -r -e modify .; do cabal configure --enable-executable-profiling; cabal build; echo "Done"; done
