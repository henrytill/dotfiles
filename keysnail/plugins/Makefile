COMMON_PLUGINS     = Tanything bmany hok pinboard
DARWIN_PLUGINS     = option-as-meta

URL_Tanything      = https://raw.githubusercontent.com/myuhe/KeySnail_Plugin/master/Tanything.ks.js
URL_bmany          = https://raw.githubusercontent.com/mooz/keysnail/master/plugins/bmany.ks.js
URL_hok            = https://raw.githubusercontent.com/mooz/keysnail/master/plugins/hok.ks.js
URL_pinboard       = https://raw.githubusercontent.com/deadcyclo/keysnail-plugins/master/pinboard/pinboard.ks.js
URL_option-as-meta = https://dl.dropboxusercontent.com/u/313738/keysnail/option-as-meta.ks.js

FETCH = curl -O

ifeq ($(shell uname),Darwin)
  PLUGINS = $(COMMON_PLUGINS:=.ks.js) $(DARWIN_PLUGINS:=.ks.js)
else
  PLUGINS = $(COMMON_PLUGINS:=.ks.js)
endif

all: $(PLUGINS)

option-as-meta.ks.js:
	@echo fetching $@
	@$(FETCH) $(URL_option-as-meta)
	@echo patching $@
	@sed -i '' 's/KeySnail.Key/key/g' $@

%.ks.js:
	@echo fetching $@
	@$(FETCH) $(URL_$*)

clean:
	@echo cleaning
	@rm $(PLUGINS)

.PHONY: all clean
