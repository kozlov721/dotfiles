import os

# Documentation:
#   qute://help/configuring.html
#   qute://help/settings.html

config.load_autoconfig(False)

c.auto_save.session = True

config.set('content.cookies.accept', 'all', 'chrome-devtools://*')

config.set('content.cookies.accept', 'all', 'devtools://*')

# Value to send in the `Accept-Language` header. Note that the value
# read from JavaScript is always the global value.
# Type: String
config.set('content.headers.accept_language', '', 'https://matchmaker.krunker.io/*')

config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}) AppleWebKit/{webkit_version} (KHTML, like Gecko) {upstream_browser_key}/{upstream_browser_version} Safari/{webkit_version}', 'https://web.whatsapp.com/')

config.set('content.headers.user_agent',
           'Mozilla/5.0 ({os_info}; rv:90.0) Gecko/20100101 Firefox/90.0', 'https://accounts.google.com/*')

config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99 Safari/537.36', 'https://*.slack.com/*')

# Load images automatically in web pages.
# Type: Bool
config.set('content.images', True, 'chrome-devtools://*')

# Load images automatically in web pages.
# Type: Bool
config.set('content.images', True, 'devtools://*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'chrome-devtools://*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'devtools://*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'chrome://*/*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'qute://*/*')

# Allow websites to show notifications.
# Type: BoolAsk
# Valid values:
#   - true
#   - false
#   - ask
config.set('content.notifications.enabled',
           False, 'https://www.facebook.com')

# Allow websites to show notifications.
# Type: BoolAsk
# Valid values:
#   - true
#   - false
#   - ask
config.set('content.notifications.enabled',
           False, 'https://www.reddit.com')

c.content.pdfjs = True

c.scrolling.smooth = True

c.tabs.last_close = 'ignore'

c.url.default_page = 'https://google.com'

c.url.searchengines = {
    'DEFAULT': 'https://www.google.com/search?q={}',
    'y':       'https://www.youtube.com/results?search_query={}',
    'a':       'https://wiki.archlinux.org/index.php?search={}',
    'r':       'https://www.reddit.com/r/{}/'
}

c.url.start_pages = 'https://google.com'

c.zoom.default = '140%'

c.zoom.levels = [
    '25%', '33%', '50%', '67%', '75%', '90%', '100%',
    '110%', '125%', '135%', '140%', '150%', '175%',
    '200%', '250%', '300%', '400%', '500%'
]

c.colors.webpage.darkmode.enabled = False
c.colors.webpage.darkmode.threshold.background = 205
c.colors.webpage.darkmode.threshold.text = 150
c.colors.webpage.darkmode.algorithm = 'lightness-hsl'
if not os.path.isfile('/var/local/change_theme/light_on.lck'):
    c.colors.webpage.darkmode.enabled = True
    c.colors.webpage.bg = '#111111'
    c.content.user_stylesheets = ['~/.config/qutebrowser/style.css']

# To prevent youtube to play videos before I focus them
c.content.autoplay = False

c.fonts.default_size = '9pt'

c.fonts.completion.category = 'bold default_size default_family'

config.bind('<Alt+j>', 'run-with-count 10 scroll down')
config.bind('<Alt+k>', 'run-with-count 10 scroll up')
config.bind('J',       'tab-prev')
config.bind('K',       'tab-next')
config.bind('X',       'undo')
config.bind('x',       'tab-close')
config.bind('<Alt-h>', 'back -t')
config.bind('<Alt-l>', 'forward -t')
config.unbind('d');
config.unbind('u');

c.content.blocking.whitelist = ['https://www.googleadservices.com/*']

config.source('themes/onedark.py')
selected_fg_color = '#111111'
selected_bg_color = '#cccccc'

c.colors.tabs.selected.even.bg = selected_bg_color
c.colors.tabs.selected.even.fg = selected_fg_color
c.colors.tabs.selected.odd.bg  = selected_bg_color
c.colors.tabs.selected.odd.fg  = selected_fg_color

