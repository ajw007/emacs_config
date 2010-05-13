wget http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el
git clone git://gitorious.org/magit/mainline.git magit
cvs -d :pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot co slime
darcs get http://code.haskell.org/haskellmode-emacs
wget http://www.emacswiki.org/emacs/download/etags-select.el
wget http://www.xsteve.at/prg/emacs/psvn.el
wget http://www.emacswiki.org/emacs/download/anything.el
wget http://www.emacswiki.org/emacs/download/anything-config.el

YAS_VERSION=0.6.1c
wget http://yasnippet.googlecode.com/files/yasnippet-${YAS_VERSION}.tar.bz2 && tar xvfj yasnippet-${YAS_VERSION}.tar.bz2 && ln -s yasnippet-${YAS_VERSION} yasnippet && rm yasnippet-${YAS_VERSION}.tar.bz2

git clone git://repo.or.cz/org-mode.git && cd org-mode && make && cd ..

THEME_VERSION=6.6.0
wget http://www.very-clever.com/download/nongnu/color-theme/color-theme-${THEME_VERSION}.tar.gz && tar xvfz color-theme-${THEME_VERSION}.tar.gz && ln -s color-theme-${THEME_VERSION} color-theme && rm color-theme-${THEME_VERSION}.tar.gz


