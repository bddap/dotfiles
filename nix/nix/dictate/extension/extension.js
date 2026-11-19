import Gio from 'gi://Gio';
import GLib from 'gi://GLib';
import IBus from 'gi://IBus';

import {Extension} from 'resource:///org/gnome/shell/extensions/extension.js';
import {getIBusManager} from 'resource:///org/gnome/shell/misc/ibusManager.js';

const DAEMON = '@dictate@';

export default class Dictate extends Extension {
    enable() {
        this._spawn();
    }

    disable() {
        if (this._respawn)
            GLib.source_remove(this._respawn);
        this._respawn = 0;
        const daemon = this._daemon;
        this._daemon = null;
        daemon.force_exit();
    }

    _spawn() {
        this._respawn = 0;
        const daemon = Gio.Subprocess.new([DAEMON], Gio.SubprocessFlags.STDOUT_PIPE);
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
