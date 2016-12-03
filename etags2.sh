find ./src ./test -name "*.clj" ! -name "*.cljs" ! -name "*.cljc" -print | etags -
