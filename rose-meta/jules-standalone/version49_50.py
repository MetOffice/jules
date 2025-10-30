import sys
if sys.version_info[0] == 2:
    from rose.upgrade import MacroUpgrade
else:
    from metomi.rose.upgrade import MacroUpgrade


class vn49_vn50(MacroUpgrade):

    """Upgrade macro from JULES by Author"""

    BEFORE_TAG = "vn4.9"
    AFTER_TAG = "vn5.0"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        return config, self.reports


'''
Do not edit any lines below this point - this is the template.
'''

class vn49_txxx(MacroUpgrade):

    """Upgrade macro from JULES by Author"""

    BEFORE_TAG = "vn4.9_txxx"
    AFTER_TAG = "vn4.9_txxx"

    def upgrade(self, config, meta_config=None):
        """Upgrade a JULES runtime app configuration."""

        # Add settings
        return config, self.reports
