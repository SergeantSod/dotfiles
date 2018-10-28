from libqtile.widget import Image
#import gtk


from enum import IntEnum

class Button(IntEnum):
    left   = 1
    middle = 2
    right  = 3
    up     = 4
    down   = 5

class IconButton(Image):
  def __init__(self, icon_name='start-here', width=32, **config):
    file_name = self.__lookup_icon_file(icon_name, width)
    super(IconButton, self).__init__(filename=file_name, width=width, **config)

  def __lookup_icon_file(self, icon_name, size):
    return '/usr/share/icons/mate/32x32/apps/utilities-system-monitor.png'
    # icon_theme = gtk.icon_theme_get_default()
    # icon_info  = icon_theme.lookup_icon(icon_name, size, gtk.ICON_LOOKUP_NO_SVG)
    # if icon_info is not None:
    #   return icon_info.get_filename()

class CloseButton(IconButton):

  def button_press(self, x, y, button):
    window = self.bar.screen.group.currentWindow
    if (window and button == Button.left):
      window.cmd_kill()

class ShiftButton(IconButton):

  def button_press(self, x, y, button):
    layout = self.bar.screen.group.layout
    if (layout):
      if (button in [Button.left, Button.up]):
        layout.cmd_down()
      if (button in [Button.right, Button.down]):
        layout.cmd_up()

class LayoutButton(IconButton):

  def button_press(self, x, y, button):
      if (button in [Button.left, Button.up]):
          self.qtile.cmd_next_layout()
      if (button in [Button.right, Button.down]):
          self.qtile.cmd_prev_layout()
