import constants as const
import sys
import wx


class MainFrame(wx.Frame):
    def __init__(self):
        wx.Frame.__init__(self, None, -1, "Media Controller", size = const.MAIN_FRAME_SIZE, style = wx.CAPTION | wx.CLOSE_BOX)
        
        self.panel = wx.Panel(self, -1)
        
        self.SetIcon(wx.Icon('images/logo.png', wx.BITMAP_TYPE_PNG))
        self.CreateMenuBar()
        self.CreateControls()
        self.SetLayout()
    
    def CreateMenuBar(self):
        menu = wx.MenuBar()
        settings = wx.Menu()
        
        player = wx.Menu()
        for item in const.PLAYERS.keys():
            menu_item = wx.MenuItem(player, wx.NewId(), item, kind = wx.ITEM_RADIO)
            player.AppendItem(menu_item)
        
        server = wx.MenuItem(settings, wx.NewId(), "Server")
        quit = wx.MenuItem(settings, wx.NewId(), "Quit")
        
        settings.AppendMenu(wx.NewId(), "Player", player)
        settings.AppendItem(server)
        settings.AppendSeparator()
        settings.AppendItem(quit)
        
        settings.Bind(wx.EVT_MENU, self.OnServerMenu, id = server.GetId())
        settings.Bind(wx.EVT_MENU, self.OnQuitMenu, id = quit.GetId())
        
        menu.Append(settings, "&Settings")
        self.SetMenuBar(menu)
    
    def OnPlayerMenu(self, event):
        pass
    
    def OnServerMenu(self, event):
        pass
    
    def OnQuitMenu(self, event):
        pass
    
    def CreateControls(self):
        bitmap_play = wx.Image("images/play.png", wx.BITMAP_TYPE_PNG).ConvertToBitmap()
        bitmap_stop = wx.Image("images/stop.png", wx.BITMAP_TYPE_PNG).ConvertToBitmap()
        self.button_play = wx.BitmapButton(self.panel, -1, bitmap_play, size = const.CONTROL_BUTTON_SIZE)
        self.button_stop = wx.BitmapButton(self.panel, -1, bitmap_stop, size = const.CONTROL_BUTTON_SIZE)
    
    def SetLayout(self):
        vbox = wx.BoxSizer(wx.VERTICAL)
        hbox = wx.BoxSizer(wx.HORIZONTAL)
        
        hbox.Add(self.button_play, 0, wx.ALIGN_CENTER | wx.ALL, 4)
        hbox.Add(self.button_stop, 0, wx.ALIGN_CENTER | wx.ALL, 4)
        vbox.Add(hbox, 0, wx.ALIGN_CENTER)
        
        self.panel.SetSizer(vbox)
        self.Layout()
        self.Centre()
        self.Show()


















