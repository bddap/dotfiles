import Clutter from 'gi://Clutter';
import Gio from 'gi://Gio';

import {Extension} from 'resource:///org/gnome/shell/extensions/extension.js';
import * as Main from 'resource:///org/gnome/shell/ui/main.js';

const INTERFACE = `
<node>
  <interface name="org.gnome.Shell.Extensions.Dictate">
    <method name="Type">
      <arg type="s" direction="in" name="text"/>
    </method>
  </interface>
</node>`;

export default class Dictate extends Extension {
    enable() {
        this._keyboard = Clutter.get_default_backend().get_default_seat()
            .create_virtual_device(Clutter.InputDeviceType.KEYBOARD_DEVICE);
        this._exported = Gio.DBusExportedObject.wrapJSObject(INTERFACE, this);
        this._exported.export(Gio.DBus.session, '/org/gnome/Shell/Extensions/Dictate');
    }

    disable() {
        this._exported.unexport();
        this._exported = null;
        this._keyboard = null;
    }

    Type(text) {
        if (Main.inputMethod.currentFocus) {
            Main.inputMethod.commit(text);
            return;
        }
        // Without an input-method focus (X11 clients, GTK under GTK_IM_MODULE=ibus) only key events reach the window.
        for (const character of text) {
            const keyval = Clutter.unicode_to_keysym(character.codePointAt(0));
            const time = Clutter.get_current_event_time() * 1000;
            this._keyboard.notify_keyval(time, keyval, Clutter.KeyState.PRESSED);
            this._keyboard.notify_keyval(time, keyval, Clutter.KeyState.RELEASED);
        }
    }
}
