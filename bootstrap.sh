mkdir -p external
cd external

wget http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el
git clone git://gitorious.org/magit/mainline.git magit
cvs -d :pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot co slime
#darcs get http://code.haskell.org/haskellmode-emacs
wget http://www.emacswiki.org/emacs/download/etags-select.el
wget http://www.xsteve.at/prg/emacs/psvn.el
wget http://www.emacswiki.org/emacs/download/anything.el
wget http://www.emacswiki.org/emacs/download/anything-config.el
wget http://launchpad.net/python-mode/trunk/5.1.0/+download/python-mode.el 
wget http://ipython.scipy.org/dist/ipython.el
wget http://mumble.net/~campbell/emacs/paredit.el
wget http://www.emacswiki.org/emacs/download/sml-modeline.el
wget http://nschum.de/src/emacs/highlight-symbol/highlight-symbol.el
wget http://www.python.org/emacs/winring/winring.el
wget http://www.bookshelf.jp/elc/list-register.el
wget http://autopair.googlecode.com/svn/tags/REL_0_3/autopair.el

git clone git://repo.or.cz/org-mode.git && cd org-mode && make && cd ..

YAS_VERSION=0.6.1c
wget http://yasnippet.googlecode.com/files/yasnippet-${YAS_VERSION}.tar.bz2 && tar xvfj yasnippet-${YAS_VERSION}.tar.bz2 && ln -s yasnippet-${YAS_VERSION} yasnippet && rm yasnippet-${YAS_VERSION}.tar.bz2

COLOR_THEME_VERSION=6.6.0
wget http://www.very-clever.com/download/nongnu/color-theme/color-theme-${COLOR_THEME_VERSION}.tar.gz && tar xvfz color-theme-${COLOR_THEME_VERSION}.tar.gz && ln -s color-theme-${COLOR_THEME_VERSION} color-theme && rm color-theme-${COLOR_THEME_VERSION}.tar.gz

BREADCRUMB_VERSION=1.1.3
wget http://downloads.sourceforge.net/project/breadcrumbemacs/Breadcrumb%20for%20Emacs/${BREADCRUMB_VERSION}/breadcrumb-${BREADCRUMB_VERSION}.zip && unzip breadcrumb-${BREADCRUMB_VERSION}.zip && rm breadcrumb-${BREADCRUMB_VERSION}.zip

SESSION_VERSION=2.2a
wget http://downloads.sourceforge.net/project/emacs-session/session/${SESSION_VERSION}/session-${SESSION_VERSION}.tar.gz && tar xvfz session-${SESSION_VERSION}.tar.gz && rm session-${SESSION_VERSION}.tar.gz


