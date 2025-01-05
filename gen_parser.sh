rm -rf src/Parser
cd src
bnfc -m --functor --haskell -d ./../Parser.cf
make