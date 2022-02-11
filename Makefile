clj_cmd = env clj

.PHONY: build
build:
	mkdir -p target
	$(clj_cmd) -X:depstar uberjar :jar target/pitoco.jar :sync-pom true :version '"0.2.0"' :exclude '["clojure/.*", "lambdaisland/.*", "medley/.*", "babashka/.*", "metosin/.*", "spec-provider/.*", "tlc2/.*", "com/.*", "javax/.*", "org/.*", "pcal/.*", "tla2sany/.*", "tla2tex/.*", "util/.*", "malli/.*", "jogamp/.*", "natives/.*", "processing/.*", "cljsjs/.*", "icon/.*", "fipp/.*", "edamame/.*", "javassist/.*", "arrudeia/.*", "tick/.*", "com/wsscode/.*", "com/athaydes/.*", "cheshire/.*", "rawhttp/.*", "cljs/.*", "cognitect/.*", "goog/.*", "public/.*"]'

.PHONY: deploy
deploy:
	mvn deploy:deploy-file -Dfile=target/pitoco.jar -DpomFile=pom.xml -DrepositoryId=clojars -Durl=https://clojars.org/repo/
