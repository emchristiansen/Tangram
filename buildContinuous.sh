while inotifywait -qq -r -e modify .; do cabal build; echo "Done"; done
