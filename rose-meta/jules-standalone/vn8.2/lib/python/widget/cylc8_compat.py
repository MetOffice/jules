# -*- coding: utf-8 -*-
# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************
"""
This module contains a wrapper around rose.config_editor widgets to allow
Cylc 8 compatability.

"""

from __future__ import print_function
import sys

if sys.version[0] == '2':
    from rose.config_editor.valuewidget.combobox import ComboBoxValueWidget
    from rose.config_editor.pagewidget.table import PageArrayTable

    class ComboBoxValueWidget(ComboBoxValueWidget):
        """
        Standard ComboBoxValueWidget
        """

    class PageArrayTable(PageArrayTable):
        """
        Standard PageArrayTable
        """

else:
    print("[WARN] standard rose edit widgets ignored in Python 3", file=sys.stderr)

    class ComboBoxValueWidget():
        """
        Dummy ComboBoxValueWidget
        """

    class PageArrayTable():
        """
        Dummy PageArrayTable
        """
