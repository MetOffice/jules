import re
import sys

if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
else:
    from metomi.rose.upgrade import MacroUpgrade


class vn76_vn77(MacroUpgrade):
    """Version bump macro"""
    
    BEFORE_TAG = "vn7.6"
    AFTER_TAG = "vn7.7"
    
    def upgrade(self, config, meta_config=None):
        # Nothing to do        
        return config, self.reports
        

