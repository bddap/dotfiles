import Clutter from 'gi://Clutter';
import Gio from 'gi://Gio';
import GLib from 'gi://GLib';
import IBus from 'gi://IBus';
import Meta from 'gi://Meta';
import Shell from 'gi://Shell';

import {Extension} from 'resource:///org/gnome/shell/extensions/extension.js';
import {getIBusManager} from 'resource:///org/gnome/shell/misc/ibusManager.js';
import * as Main from 'resource:///org/gnome/shell/ui/main.js';

const KEY = 'Alt_R';
const KEY_MODIFIER = Clutter.ModifierType.MOD1_MASK;
const DAEMON = '@dictate@';

export default class Dictate extends Extension {
    enable() {
        this._spawn();
        this._action = global.display.grab_accelerator(KEY, Meta.KeyBindingFlags.NONE);
        Main.wm.allowKeybinding(Meta.external_binding_name_for_action(this._action), Shell.ActionMode.NORMAL);
        this._activated = global.display.connect('accelerator-activated', (display, action) => {
            if (action === this._action && !this._held) {
                this._held = true;
                this._send('press');
                this._released();
            }
        });
        // A modifier's release fails to match the binding its press matched, so the release is read from the keyboard state.
        this._keymap = Clutter.get_default_backend().get_default_seat().get_keymap();
        this._stateChanged = this._keymap.connect('state-changed', () => this._released());
    }

    disable() {
        this._keymap.disconnect(this._stateChanged);
        global.display.disconnect(this._activated);
        Main.wm.allowKeybinding(Meta.external_binding_name_for_action(this._action), Shell.ActionMode.NONE);
        global.display.ungrab_accelerator(this._action);
        this._held = false;
        if (this._respawn)
            GLib.source_remove(this._respawn);
        this._respawn = 0;
        const daemon = this._daemon;
        this._daemon = null;
        daemon.force_exit();
    }

    _released() {
        const [, , mods] = global.get_pointer();
        if (this._held && !(mods & KEY_MODIFIER)) {
            this._held = false;
            this._send('release');
        }
    }

    _send(event) {
        this._daemon.get_stdin_pipe().write_all(new TextEncoder().encode(`${event}\n`), null);
    }

    _spawn() {
        this._respawn = 0;
        const daemon = Gio.Subprocess.new([DAEMON], Gio.SubprocessFlags.STDIN_PIPE | Gio.SubprocessFlags.STDOUT_PIPE);
        this._daemon = daemon;
        const transcripts = new Gio.DataInputStream({base_stream: daemon.get_stdout_pipe()});
        const next = () => transcripts.read_line_async(GLib.PRIORITY_DEFAULT, null, (stream, result) => {
            const [line] = stream.read_line_finish_utf8(result);
            if (line === null || this._daemon !== daemon)
                return;
            // The shell is IBus's panel: a panel commit is text, never a key event, and reaches the focused input context however the window talks to IBus.
            getIBusManager()._panelService?.commit_text(IBus.Text.new_from_string(`${line} `));
            next();
        });
        next();
        daemon.wait_async(null, () => {
            if (this._daemon === daemon)
                this._respawn = GLib.timeout_add_seconds(GLib.PRIORITY_DEFAULT, 1, () => {
                    this._spawn();
                    return GLib.SOURCE_REMOVE;
                });
        });
    }
}
