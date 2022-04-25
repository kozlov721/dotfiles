import socket
config.load_autoconfig(True)

c.auto_save.session = True
config.set('content.cookies.accept', 'all', 'chrome-devtools://*')
config.set('content.cookies.accept', 'all', '*')
config.set('content.cookies.accept', 'all', 'devtools://*')

config.set('content.headers.accept_language', '',
           'https://matchmaker.krunker.io/*')

config.set(
    'content.headers.user_agent',
    'Mozilla/5.0 ({os_info}) AppleWebKit/{webkit_version} (KHTML, like Gecko) {upstream_browser_key}/{upstream_browser_version} Safari/{webkit_version}',
    'https://web.whatsapp.com/')

config.set('content.headers.user_agent',
           'Mozilla/5.0 ({os_info}; rv:90.0) Gecko/20100101 Firefox/90.0',
           'https://accounts.google.com/*')

config.set(
    'content.headers.user_agent',
    'Mozilla/5.0 ({os_info}) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99 Safari/537.36',
    'https://*.slack.com/*')

config.set('content.images', True, 'chrome-devtools://*')
config.set('content.images', True, 'devtools://*')

config.set('content.javascript.enabled', True, 'chrome-devtools://*')
config.set('content.javascript.enabled', True, 'devtools://*')
config.set('content.javascript.enabled', True, 'chrome://*/*')
config.set('content.javascript.enabled', True, 'qute://*/*')

config.set('content.notifications.enabled', False,
           'https://www.facebook.com')
config.set('content.notifications.enabled', False,
           'https://www.reddit.com')

config.set('qt.highdpi', False)

c.content.pdfjs = True

c.scrolling.smooth = True

c.tabs.last_close = 'ignore'

c.url.default_page = 'https://google.com'


c.url.searchengines = {
    'DEFAULT': 'https://google.com/search?q={}',
    'm':       'https://google.com/maps/search/{}',
    'y':       'https://youtube.com/results?search_query={}',
    'a':       'https://wiki.archlinux.org/index.php?search={}',
    'r':       'https://reddit.com/r/{}/',
    'w':       'https://en.wikipedia.org/wiki/{}',
    'd':       'https://duckduckgo.com/?q={}'
}

c.url.start_pages = 'https://google.com'

if socket.gethostname() == 'martin-hnwx9x':
    zd = 150
    ds = 11
else:
    zd = 140
    ds = 12
c.zoom.default = f'{zd}%'
c.fonts.default_size = f'{ds}pt'
c.zoom.levels = [
    '25%', '33%', '50%', '67%', '75%',
    '90%', '100%', '110%', '125%', '135%',
    '140%', '150%', '175%', '200%', '250%',
    '300%', '400%', '500%'
    ]

c.content.blocking.whitelist = ['https://www.googleadservices.com/*']
c.content.autoplay = False
c.fonts.completion.category = 'bold default_size default_family'

# ------------- Bindings ----------------

config.unbind('d')
config.unbind('D')
config.unbind('u')

config.bind('<Alt-j>',     'run-with-count 10 scroll down')
config.bind('<Alt-k>',     'run-with-count 10 scroll up')
config.bind('<Ctrl-d>',    'run-with-count 20 scroll down')
config.bind('<Ctrl-u>',    'run-with-count 20 scroll up')
config.bind('J',           'tab-prev')
config.bind('K',           'tab-next')
config.bind('X',           'undo')
config.bind('x',           'tab-close')
config.bind('<Alt-h>',     'back -t')
config.bind('<Alt-l>',     'forward -t')
config.bind('do',          'download-open')
config.bind('dc',          'download-clear')
config.bind('dr',          'download-retry')
config.bind(',r',          'config-source')

# --------------- Theme ----------------

config.source('themes/onedark.py')
selected_fg_color = '#111111'
selected_bg_color = '#cccccc'

c.colors.tabs.selected.even.bg = selected_bg_color
c.colors.tabs.selected.even.fg = selected_fg_color
c.colors.tabs.selected.odd.bg = selected_bg_color
c.colors.tabs.selected.odd.fg = selected_fg_color

c.colors.webpage.darkmode.enabled = False
c.colors.webpage.darkmode.threshold.background = 205
c.colors.webpage.darkmode.threshold.text = 150
c.colors.webpage.darkmode.algorithm = 'lightness-hsl'
# c.colors.webpage.preferred_color_scheme = "dark"
# c.colors.webpage.bg = '#111111'

if c.colors.webpage.darkmode.enabled:
    c.content.user_stylesheets = [
        '~/.config/qutebrowser/css/darculized/darculized-all-sites.css'
    ]
