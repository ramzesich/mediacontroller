import httplib
import wx

PORT = 6666
PLAY_ACTION = "play\n\n"
STOP_ACTION = "stop\n\n"
PING_ACTION = "ping\n\n"
PING_INTERVAL = 10

LOGO_ICON = 'images/logo.png'
TRAY_LOGO_ICON = 'images/logo.xpm'
TRAY_LOGO_ONLINE_ICON = 'images/logo_online.xpm'
TRAY_LOGO_OFFLINE_ICON = 'images/logo_offline.xpm'
MAIN_FRAME_SIZE = (220, 90)
BUTTON_SIZE = (100, wx.DefaultSize.GetHeight())
SERVER_DIALOG_SIZE = (200, 90)
SERVER_DIALOG_INPUT_SIZE = (180, wx.DefaultSize.GetHeight())
SQLITE_DB = 'settings.db'


#### players ####

def _http_request(host, method, uri):
    conn = httplib.HTTPConnection(host)
    conn.request(method, uri)
    conn.close()

def vlc_play():
    _http_request('localhost:8080', 'GET', '/requests/status.xml?command=pl_pause')
    #print "vlc play"

def vlc_stop():
    _http_request('localhost:8080', 'GET', '/requests/status.xml?command=pl_stop')
    #print "vlc stop"

def mpc_play():
    print "mpc play"

def mpc_stop():
    print "mpc stop"

PLAYERS = {
    'VLC': {
        'play': vlc_play,
        'stop': vlc_stop,
    },
    'MPC': {
        'play': mpc_play,
        'stop': mpc_stop,
    },
}
