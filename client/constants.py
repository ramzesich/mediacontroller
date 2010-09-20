import wx

PORT = 6666
PLAY_ACTION = "play\n\n"
STOP_ACTION = "stop\n\n"

LOGO_ICON = 'images/logo.png'
TRAY_LOGO_ICON = 'images/logo.xpm'
MAIN_FRAME_SIZE = (220, 90)
BUTTON_SIZE = (100, wx.DefaultSize.GetHeight())
SERVER_DIALOG_SIZE = (200, 90)
SERVER_DIALOG_INPUT_SIZE = (180, wx.DefaultSize.GetHeight())
SQLITE_DB = 'settings.db'

PLAYERS = {
    'VLC': {
        'play': {
            'host': '',
            'method': '',
            'uri': '',
        },
        'stop': {
            'host': '',
            'method': '',
            'uri': '',
        },
    },
    'MPC': {
        'play': {
            'host': '',
            'method': '',
            'uri': '',
        },
        'stop': {
            'host': '',
            'method': '',
            'uri': '',
        },
    },
}
