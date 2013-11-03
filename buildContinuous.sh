while inotifywait -qq -r -e modify .; do sleep 2; cabal clean; cabal configure --enable-executable-profiling; cabal build; echo "Done"; done
