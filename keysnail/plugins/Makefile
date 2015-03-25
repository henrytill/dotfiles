COMMON_PLUGINS     = Tanything bmany hok pinboard
DARWIN_PLUGINS     = option-as-meta

URL_Tanything      = https://raw.githubusercontent.com/myuhe/KeySnail_Plugin/master/Tanything.ks.js
URL_bmany          = https://raw.githubusercontent.com/mooz/keysnail/master/plugins/bmany.ks.js
URL_hok            = https://raw.githubusercontent.com/mooz/keysnail/master/plugins/hok.ks.js
URL_pinboard       = https://raw.githubusercontent.com/deadcyclo/keysnail-plugins/master/pinboard/pinboard.ks.js
URL_option-as-meta = https://dl.dropboxusercontent.com/u/313738/keysnail/option-as-meta.ks.js

FETCH = curl -O

all: plugins

ifeq ($(shell uname),Darwin)
plugins: $(COMMON_PLUGINS:=.ks.js) $(DARWIN_PLUGINS:=.ks.js)
else
plugins: $(COMMON_PLUGINS:=.ks.js)
endif

option-as-meta.ks.js:
	$(FETCH) $(URL_option-as-meta)
	sed -i '' 's/KeySnail.Key/key/g' $@

%.ks.js:
	$(FETCH) $(URL_$*)

clean:
	rm *.ks.js
