from distutils.core import setup

import common
import sys, os
import py2exe
import glob

datafiles = []

imagefiles = []
imagefiles.extend(glob.glob('images\\*.png'))

jarfiles = []

localization_files = []

origIsSystemDLL = py2exe.build_exe.isSystemDLL

def isSystemDLL(pathname):
    if os.path.basename(pathname).lower() in ("msvcp71.dll", "gdiplus.dll"):
        return 0
    return origIsSystemDLL(pathname)

py2exe.build_exe.isSystemDLL = isSystemDLL


setup(
    name = 'Media Controller',
    version = '1.00',
    description = 'remote media player controller for cooperative video viewing over the Internet',
    author = 'shemsu',
    options = {
        'py2exe': {'packages' : [], 'includes': [], 'skip_archive': 1}
    },
    data_files = [('./images', imagefiles)],    
    packages = [],
    windows = [{'script': 'mediacontroller.py', 'icon_resources': [(1, common.LOGO_ICON)]}]
)
