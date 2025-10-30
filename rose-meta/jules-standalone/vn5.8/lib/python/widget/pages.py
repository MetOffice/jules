import pygtk
pygtk.require('2.0')
import gtk
import gobject
import pango
import webbrowser
import sys
import optparse

import rose


class PageWithVariableTable(gtk.VBox):
    """Page widget that displays all widgets normally except for a table of variable information"""
    
    def __init__(self, active, latent, var_ops, modes, arg_str=None):
        super(PageWithVariableTable, self).__init__(False, 1)
        
        self.__drawing_suspended = True
        
        self.__var_ops = var_ops
        
        # Parse the given arguments to work out which variables to include in the table
        parser = optparse.OptionParser()
        parser.add_option('-n', '--nvar', action = "store",
                          type = "string", dest = "nvar", default = "nvars")
        (args, vars) = parser.parse_args(arg_str.split() if arg_str else [])
        
        # Store the names for easy looking up later
        self.__var_names = [args.nvar] + vars
        
        # Work out which variables we will explicitly manage and which variables we will defer
        # to the default page        
        self.__nvar = None
        self.__vars = []
        for var in active[:]:
            if var.name == args.nvar:
                self.__nvar = var
                active.remove(var)
        for var in latent[:]:
            if var.name == args.nvar:
                self.__nvar = var
                latent.remove(var)
        for var_name in vars:
            for var in active[:]:
                if var.name == var_name:
                    self.__vars.append(var)
                    active.remove(var)
            for var in latent[:]:
                if var.name == var_name:
                    self.__vars.append(var)
                    latent.remove(var)
                    
        # Correct the sizes of all the variables to match the current value of self.__nvar
        for var in self.__vars:
            value = self.__python_list_for_variable(var)
            self.__var_ops.set_var_value(var, ",".join([self.__value_to_string(var, v) for v in value]))
        
        # Build the default table for all the other variables and insert it as the first element
        self.__table = rose.config_editor.pagewidget.table.PageTable(active, latent, var_ops, modes, arg_str)
        self.pack_start(self.__table)
        
        # Add another panel to contain the table
        self.__table_panel = gtk.VBox(False, 0)
        self.pack_start(self.__table_panel)
        
        toolbar = gtk.HBox()
        
        error_panel = gtk.HBox()
        self.__error_icon = gtk.image_new_from_stock(gtk.STOCK_DIALOG_ERROR, gtk.ICON_SIZE_MENU)
        self.__error_icon.set_padding(5, 3)
        self.__error_icon.show()
        error_panel.pack_start(self.__error_icon, expand = False, fill = False)
        self.__error_label = gtk.Label()
        self.__error_label.set_alignment(xalign=0, yalign=0.5)
        self.__error_label.show()
        error_panel.pack_start(self.__error_label, expand = True, fill = True)
        error_panel.show()
        toolbar.pack_start(error_panel, expand = True, fill = True)
        
        add_button = gtk.Button(stock=gtk.STOCK_ADD)
        add_button.connect("clicked", self.__handle_add_row)
        toolbar.pack_end(add_button, expand = False, fill = False)
        
        self.__table_panel.pack_start(toolbar)
        add_button.show()
        toolbar.show()    
        
        # Build the initial state of our store
        col_types = []
        renderers = []
        col_idx = 0
        for var in self.__vars:
            if 'values' in var.metadata:
                col_types.append(str)
                values_store = gtk.ListStore(str)
                for item in var.metadata['values']:
                    values_store.append([item])
                renderer = gtk.CellRendererCombo()
                renderer.set_property('has-entry', False)
                renderer.set_property('editable', True)
                renderer.set_property('model', values_store)
                renderer.set_property("text-column", 0)
                renderer.connect("edited", self.__handle_text_edit, var, col_idx)
            else:
                type = var.metadata.get('type', None)
                if type == 'integer':
                    col_types.append(int)
                    renderer = gtk.CellRendererSpin()
                    renderer.set_property('editable', True)
                    renderer.set_property('adjustment', 1)
                    renderer.connect("edited", self.__handle_text_edit, var, col_idx)
                elif type == 'boolean':
                    col_types.append(bool)
                    renderer = gtk.CellRendererToggle()
                    renderer.set_property('active', 0)
                    renderer.connect("toggled", self.__handle_toggle, var, col_idx)
                elif type == 'logical':
                    col_types.append(bool)
                    renderer = gtk.CellRendererToggle()
                    renderer.set_property('active', 0)
                    renderer.connect("toggled", self.__handle_toggle, var, col_idx)
                elif type == 'real':
                    col_types.append(float)
                    renderer = gtk.CellRendererText()
                    renderer.set_property('editable', True)
                    renderer.connect("edited", self.__handle_text_edit, var, col_idx)
                else:
                    col_types.append(str)
                    renderer = gtk.CellRendererText()
                    renderer.set_property('editable', True)
                    renderer.connect("edited", self.__handle_text_edit, var, col_idx)
            renderer.set_property("xpad", 8)
            renderer.set_property("ypad", 5)
            renderers.append(renderer)
            col_idx += 1

        self.__store = gtk.ListStore(*col_types)
            
        treeview = gtk.TreeView(self.__store)
        self.__columns = []
        for col in range(len(self.__vars)):
            var = self.__vars[col]
            title = var.metadata.get('title', var.name)
            # We have to create a separate label widget for the column headers to enable tooltips
            column = gtk.TreeViewColumn('', renderers[col], text = col, active = col)
            column_header = gtk.Label(title)
            column_header.modify_font(pango.FontDescription("bold"))
            column_header.show()
            column.set_widget(column_header)
            column.connect("clicked", self.__handle_header_click, var)
            if 'description' in var.metadata:
                tooltips = gtk.Tooltips()
                tooltips.set_tip(column_header, var.metadata['description'])
            self.__columns.append(column)
            treeview.append_column(column)
        treeview.set_headers_clickable(True)
            
        table_viewport = gtk.Viewport()
        table_viewport.set_shadow_type(gtk.SHADOW_OUT)
        table_viewport.add(treeview)
        self.__table_panel.pack_start(table_viewport)
        treeview.show()
        table_viewport.show()
        
        self.__table_panel.show()
        
        # Add the right click context menu to the treeview body
        treeview.connect("button-press-event", self.__handle_mouse_click)
        # Add the right click context menu to the treeview headers
        for col in range(len(self.__vars)):
            var = self.__vars[col]
            button = self.__columns[col].get_widget()
            while not isinstance(button, gtk.Button):
                button = button.get_parent()
            button.connect("button-press-event", self.__handle_header_button_press, var)
        
        self.__drawing_suspended = False
        self.__redraw()
        
        self.show()
        
        
    def add_variable_widget(self, var):
        if var.name not in self.__var_names:
            self.__table.add_variable_widget(var)

    def reload_variable_widget(self, var):
        if var.name not in self.__var_names:
            self.__table.reload_variable_widget(var)

    def remove_variable_widget(self, var):
        if var.name not in self.__var_names:
            self.__table.remove_variable_widget(var)
    
    def update_ignored(self):
        # Refresh the variables in our store
        self.__redraw()
        self.__table.update_ignored()       
        
        
    def __redraw(self):
        """Redraws the variable table from scratch"""
        
        if self.__drawing_suspended:
            return
        
        # If the nvar variable is ignored, hide the whole table
        if self.__nvar.ignored_reason:
            self.__table_panel.hide()
            return
        else:
            self.__table_panel.show()
        
        store_data = []
        for col in range(len(self.__vars)):
            var = self.__vars[col]
            values = self.__python_list_for_variable(var)
            store_data.append(values)
            # Check if the column needs ignoring
            self.__columns[col].set_visible(var.ignored_reason == {})

        self.__store.clear()
        for row in range(int(self.__nvar.value)):
            self.__store.append([store_data[col][row] for col in range(len(self.__vars))])
            
        # Check for any errors we need to show
        self.__error_icon.hide()
        self.__error_label.hide()
        if self.__nvar.error:
            self.__error_label.set_label("%s: %s" % (self.__nvar.name, self.__nvar.error['type']))
            self.__error_icon.show()
            self.__error_label.show()
        
        
    def __default_value_for_variable(self, var):
        """Gets the default Python value for the given variable"""

        default = ''
        # Check if we need to correct from the strings we already have
        if 'values' in var.metadata:
            default = var.metadata['values'][0]
        else:
            type = var.metadata.get('type', None)
            if type == 'integer':
                default = 0
            elif type == 'boolean' or type == 'logical':
                default = False
            elif type == 'real':
                default = 0.0
        return default
    
    
    def __value_from_string(self, var, value):
        """Takes a string value and parses it to the correct type for the variable"""
        
        if 'values' not in var.metadata:
            type = var.metadata.get('type', None)
            if type == 'integer':
                return int(value)
            elif type == 'boolean':
                return value == 'true'
            elif type == 'logical':
                return value == '.true.'
            elif type == 'real':
                return float(value)
            elif type == 'character':
                return rose.config_editor.util.text_for_character_widget(value)
        return value
        
        
    def __value_to_string(self, var, value):
        """Takes the given value and creates a suitable string in the context of the given variable"""
        
        if 'values' not in var.metadata:
            type = var.metadata.get('type', None)
            if type == 'boolean':
                return 'true' if value else 'false'
            elif type == 'logical':
                return '.true.' if value else '.false.'
            elif type == 'character':
                return rose.config_editor.util.text_from_character_widget(value)
        return str(value)
        
        
    def __python_list_for_variable(self, var):
        """Convert the value of the variable to a Python list appropriate for its type"""
        
        values = var.value.split(',') if var.value else []
        values = [self.__value_from_string(var, value) for value in values]
        default = self.__default_value_for_variable(var)
        # Expand with the default value if required
        nrows = int(self.__nvar.value)
        return (values + [default] * nrows)[:nrows]
        
        
    def __handle_add_row(self, button):
        """Adds a new row to the variable table"""
        
        self.__drawing_suspended = True
        
        # Update the count
        self.__var_ops.set_var_value(self.__nvar, str(int(self.__nvar.value) + 1))
        # For each variable, add the default onto the end and set a new value
        for var in self.__vars:
            # Since the count has been updated, this will get an array of the new size
            # with the last element populated with default - exactly what we want
            new_value = self.__python_list_for_variable(var)
            self.__var_ops.set_var_value(var, ",".join([self.__value_to_string(var, v) for v in new_value]))
            
        self.__drawing_suspended = False
        self.__redraw()
        
     
    def __handle_text_edit(self, cell, row, new_text, var, col):
        """Triggered when text is edited"""
        
        row = int(row)
        value = self.__python_list_for_variable(var)
        value[row] = self.__value_from_string(var, new_text)
        self.__var_ops.set_var_value(var, ",".join([self.__value_to_string(var, v) for v in value]))
        self.__redraw()
        
    
    def __handle_toggle(self, cell, row, var, col):
        """Triggered when a boolean is toggled"""
        
        row = int(row)
        value = self.__python_list_for_variable(var)
        value[row] = not value[row]
        self.__var_ops.set_var_value(var, ",".join([self.__value_to_string(var, v) for v in value]))
        self.__redraw()


    def __handle_mouse_click(self, treeview, event):
        """Handles a mouse click to see if we need to display a context menu"""
        
        if event.button == 3:
            x = int(event.x)
            y = int(event.y)
            time = event.time
            pthinfo = treeview.get_path_at_pos(x, y)
            if pthinfo is not None:
                row, col, cellx, celly = pthinfo
                treeview.grab_focus()
                treeview.set_cursor(row, col, 0)
                # Create the context menu to pop up
                ctx_menu = gtk.Menu()
                clone_item = gtk.ImageMenuItem(gtk.STOCK_COPY)
                clone_item.connect("activate", self.__handle_clone_row, row)
                clone_item.show()
                ctx_menu.attach(clone_item, 0, 1, 0, 1)
                remove_item = gtk.ImageMenuItem(gtk.STOCK_REMOVE)
                remove_item.connect("activate", self.__handle_remove_row, row)
                remove_item.show()
                ctx_menu.attach(remove_item, 0, 1, 1, 2)
                ctx_menu.popup(None, None, None, event.button, time)
        
    
    def __handle_clone_row(self, item, row):
        row = int(row[0])
        
        self.__drawing_suspended = True
        
        # Update the count
        self.__var_ops.set_var_value(self.__nvar, str(int(self.__nvar.value) + 1))
        for var in self.__vars:
            # Since the count has been updated, this will get an array of the new size
            value = self.__python_list_for_variable(var)
            # So we just correct the last element
            value[-1] = value[row]
            self.__var_ops.set_var_value(var, ",".join([self.__value_to_string(var, v) for v in value]))
            
        self.__drawing_suspended = False
        self.__redraw()
            
        
    def __handle_remove_row(self, item, row):
        row = int(row[0])
        
        self.__drawing_suspended = True
        
        # Remove the given row from each variable
        for var in self.__vars:
            # This is a list at the old size, since the count hasn't been updated yet
            value = self.__python_list_for_variable(var)
            del value[row]
            self.__var_ops.set_var_value(var, ",".join([self.__value_to_string(var, v) for v in value]))
        # Update the count
        self.__var_ops.set_var_value(self.__nvar, str(int(self.__nvar.value) - 1))
            
        self.__drawing_suspended = False
        self.__redraw()
        
        
    def __handle_header_click(self, column, var):
        if 'url' in var.metadata:
            # For a left mouse click, launch the help for the variable
            webbrowser.open(var.metadata['url'], new=True, autoraise=True)


    def __handle_header_button_press(self, button, event, var):
        """Handles a mouse click on column headers to see if we need to display a context menu"""
        
        if event.button == 3:
            # Create the context menu to pop up
            ctx_menu = gtk.Menu()
            clone_item = gtk.MenuItem('Clone item')
            clone_item.show()
            ctx_menu.attach(clone_item, 0, 1, 0, 1)
            remove_item = gtk.MenuItem('Remove item')
            remove_item.show()
            ctx_menu.attach(remove_item, 0, 1, 1, 2)
            ctx_menu.popup(None, None, None, event.button, event.time)
        
    