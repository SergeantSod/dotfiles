# -*- coding: utf-8 -*-
from libqtile.config import Key, Screen, Group
from libqtile.manager import Click, Drag
from libqtile.command import lazy
from libqtile import layout, bar, widget, hook
from custom_widgets import ShiftButton, CloseButton, LayoutButton

def leftest_window_in(group):
  return min(group.windows, key=lambda window: (window.x, window.y))

def focus_master(qtile):
  group = qtile.currentGroup
  master = leftest_window_in(group)
  group.focus(master, True)

def minimize(qtile):
  qtile.currentWindow.enableminimize()
  focus_master(qtile)

def unminimise_all(qtile):
  for w in qtile.currentGroup.windows:
    if w.minimized:
      w.disablefloating()

mod = "mod4"
terminal_command = "roxterm"
keys = [
    Key([mod, "shift"], "j",
        lazy.layout.down()),
    Key([mod,"shift"], "k",
        lazy.layout.up()),
    Key([mod], "l",
        lazy.layout.increase_ratio()),
    Key([mod], "h",
        lazy.layout.decrease_ratio()),
    Key([mod], "comma",
        lazy.layout.increase_nmaster()),
    Key([mod], "period",
        lazy.layout.decrease_nmaster()),
    Key([mod], "k",
        lazy.layout.next()),
    Key([mod], "j",
        lazy.layout.previous()),
    Key([mod], "w",
        lazy.to_screen(0)),
    Key([mod], "e",
        lazy.to_screen(1)),
    Key([mod], "space",
        lazy.nextlayout()),
    Key([mod, "shift"], "space",
        lazy.prevlayout()),
    Key([mod], "y",
        lazy.window.kill()),
    Key([mod], "t",
        lazy.window.disable_floating()),
    Key([mod, "shift"], "t",
        lazy.window.enable_floating()),
    Key([mod], "Left",
        lazy.screen.prevgroup()),
    Key([mod], "Right",
        lazy.screen.nextgroup()),
    Key(["control","shift"], "q", lazy.restart()),
    Key([mod], "m", lazy.function(focus_master)),
    Key([mod], "x", lazy.function(minimize)),
    Key([mod], "a", lazy.function(unminimise_all)),
    Key([mod], "Return", lazy.spawn(terminal_command))
]

mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
        start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
        start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.disable_floating()),
    Click([mod], "Button4", lazy.layout.down()),
    Click([mod], "Button5", lazy.layout.up())
]

fg_color = '#5D97D3' #'#5DAAFF'

border = dict(
    border_focus=  fg_color,
    border_normal= '#000000', #'#808080',
    border_width=  2,
    )

layouts = [
        layout.Tile(shift_windows=True, name="Tall", **border),
        layout.RatioTile(name="Grid",fancy=True,**border),
        layout.Max(name="Max")
    ]

floating_layout = layout.Floating(auto_float_types = set(['notification', 'splash', 'toolbar', 'utility']), **border)

groups = [
    Group('1'),
    Group('2'),
    Group('3'),
    Group('4'),
    Group('5'),
    Group('6'),
    Group('7'),
    Group('8'),
    Group('9')
    ]

for index, group in enumerate(groups):
    keys.append(
        Key([mod], str(index+1), lazy.group[group.name].toscreen())
    )
    keys.append(
        Key([mod, "shift"], str(index+1), lazy.window.togroup(group.name))
    )

screens = [
    Screen(
        bottom=bar.Bar(
            [
                LayoutButton(icon_name='stock_new-window'),
                widget.CurrentLayout(width=40),
                widget.Sep(),
                widget.GroupBox(invert_mouse_wheel=True, borderwidth=1, highlight_method='block', this_current_screen_border=fg_color),
                widget.Sep(),
                widget.TaskList(borderwidth=1, highlight_method='block', border=fg_color, max_title_width=300),
                widget.Sep(),
                ShiftButton(icon_name='rotate'),
                CloseButton(icon_name='exit')
            ],
            29,
        ),
    ),
]

follow_mouse_focus = True

@hook.subscribe.client_new
def static_windows(window):
  if (window.name in ['synapse', 'Kupfer']):
    window.floating = True
    window.cmd_bring_to_front()
