import common
import sqlite3


class SQLite:
    def __init__(self):
        self.db = sqlite3.connect(common.SQLITE_DB)
        self.cursor = self.db.cursor()
        
        self.cursor.execute("""
            create table if not exists settings
            (id integer, server text, player text, online boolean)
        """)
        self.db.commit()
        self.cursor.execute("""
            select * from settings
        """)
        if not self.cursor.fetchall():
            v_id = 1
            v_server = ''
            v_player = len(common.PLAYERS.keys()) > 0 and common.PLAYERS.keys()[0] or ''
            v_online = False
            self.cursor.execute("""
                insert into settings (id, server, player, online)
                values (?, ?, ?, ?)
            """, (v_id, v_server, v_player, v_online))
            self.db.commit()
    
    def SetServer(self, server):
        self.SetField('server', server)
    
    def GetServer(self):
        return self.GetField('server')
    
    def SetPlayer(self, player):
        self.SetField('player', player)
    
    def GetPlayer(self):
        return self.GetField('player')
    
    def SetOnline(self, online):
        self.SetField('online', online)
    
    def GetOnline(self):
        return self.GetField('online')
    
    def SetField(self, field, value):
        self.cursor.execute("""
            update settings
            set %s = ?
            where id = 1
        """ % field, (value,))
        self.db.commit()
    
    def GetField(self, field):
        self.cursor.execute("""
            select %s
            from settings
            where id = 1
        """ % field)
        row = self.cursor.fetchone()
        if len(row) > 0:
            return row[0]
        else:
            return ""
