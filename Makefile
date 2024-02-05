copydev:
	cp -v Portfolio.js* ~/Public/Portfolio/s/
	cp -v index.html ~/Public/Portfolio/

copyprod: Portfolio.min.js
	cp -v $^ ~/Public/Portfolio/s/
	sed 's/folio\.js/folio\.min\.js/' index.html > ~/Public/Portfolio/index.html

deployapp: Portfolio.min.js
        scp $^ cloud01:Portfolio/s/

copyrt:
	cp -v *.min.js ~/Public/Portfolio/s/

rtl.js:
	ln -s ~/SDK/pas2js/packages/rtl/rtl.js ./

Runtime.min.js: rtl.js
	cat $^ js/*.js | minify --type js > $@

Portfolio.min.js: Portfolio.js
	cat $^ | minify --type js > $@

all: Runtime.min.js Portfolio.min.js

clean:
	rm *.min.js
