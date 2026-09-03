#!/usr/bin/env python3
import os
import re
import sys
import threading
import traceback
from collections.abc import Callable, Iterable, Sequence
from typing import TYPE_CHECKING, Protocol

import numpy as np
from numpy.typing import ArrayLike, NDArray
import gi

gi.require_version("Gdk", "4.0")
gi.require_version("Gtk", "4.0")
gi.require_version("Gst", "1.0")
from gi.repository import Gdk, Gio, GLib, GObject, Gst, Gtk

if TYPE_CHECKING:
    from kokoro.pipeline import KPipeline

KOKORO_REPO = "hexgrad/Kokoro-82M"
KOKORO_CONFIG = "@kokoro_config@"
KOKORO_MODEL = "@kokoro_model@"
VOICE = "@kokoro_voice@"
SAMPLE_RATE = 24000
DEFAULT_SPEED = 2.0
APP_ID = "app.tts_read"
SILENCE = bytes(SAMPLE_RATE // 20 * 4)
FRAME = SAMPLE_RATE * 30 // 1000
TOLERANCE = SAMPLE_RATE * 10 // 1000

Span = tuple[int, int]
Word = tuple[int, int, float, float]
Audio = NDArray[np.float32]
Chunk = tuple[Audio, list[Word]]
Highlight = Span | None

EMPTY_CHUNK: Chunk = (np.zeros(0, np.float32), [])


class Token(Protocol):
    @property
    def text(self) -> str: ...
    @property
    def start_ts(self) -> float | None: ...
    @property
    def end_ts(self) -> float | None: ...


class Result(Protocol):
    @property
    def tokens(self) -> Sequence[Token] | None: ...
    @property
    def audio(self) -> ArrayLike | None: ...


def sentence_spans(text: str) -> list[Span]:
    spans = []
    for line in re.finditer(r"[^\n]+", text):
        for m in re.finditer(r"\S.*?(?:[.!?]+[\"”’)\]]*(?=\s|$)|$)", line.group()):
            spans.append((line.start() + m.start(), line.start() + m.end()))
    return spans


def collect(results: Iterable[Result], text: str) -> Chunk:
    audio: list[Audio] = []
    words: list[Word] = []
    offset, cursor = 0.0, 0
    for r in results:
        for t in r.tokens or []:
            if t.start_ts is None or t.end_ts is None or not any(c.isalnum() for c in t.text):
                continue
            at = text.find(t.text, cursor)
            if at < 0:
                continue
            cursor = at + len(t.text)
            words.append((at, cursor, offset + t.start_ts, offset + t.end_ts))
        chunk = np.asarray(r.audio, dtype=np.float32)
        audio.append(chunk)
        offset += len(chunk) / SAMPLE_RATE
    return (np.concatenate(audio) if audio else np.zeros(0, np.float32)), words


def stretch(audio: Audio, speed: float) -> Audio:
    n = int(len(audio) / speed)
    if speed == 1.0 or n < FRAME:
        return audio
    hop = FRAME // 2
    window = np.hanning(FRAME).astype(np.float32)
    out = np.zeros(n, np.float32)
    weight = np.zeros(n, np.float32)
    last = len(audio) - FRAME
    frames = [(k, min(int(k * speed), last)) for k in range(0, n - FRAME, hop)] + [(n - FRAME, last)]
    previous = 0
    for k, p in frames:
        if k and TOLERANCE <= p < last:
            target = audio[previous + hop : previous + hop + FRAME]
            region = audio[p - TOLERANCE : p + TOLERANCE + FRAME]
            p = min(p + int(np.argmax(np.correlate(region, target, "valid"))) - TOLERANCE, last)
        out[k : k + FRAME] += audio[p : p + FRAME] * window
        weight[k : k + FRAME] += window
        previous = p
    out /= np.maximum(weight, 1e-3)
    return out


def locate(
    t: int, origin: int, spans: Sequence[Span], starts: dict[int, int], chunks: Sequence[Chunk | None]
) -> tuple[float, Highlight]:
    for j in range(origin, len(spans)):
        start = starts.get(j)
        if start is None or t < start:
            return j, None
        chunk = chunks[j]
        assert chunk is not None
        audio, words = chunk
        duration = len(audio) * Gst.SECOND // SAMPLE_RATE
        if t < start + duration:
            s = (t - start) / Gst.SECOND
            a = spans[j][0]
            word = next(((a + w0, a + w1) for w0, w1, t0, t1 in words if t0 <= s < t1), None)
            return j + (t - start) / duration, word
    return len(spans), None


class Engine:
    def __init__(self) -> None:
        import torch
        from kokoro.model import KModel
        from kokoro.pipeline import KPipeline

        torch.set_num_threads(max(1, min(8, (os.cpu_count() or 2) // 2)))
        model = KModel(repo_id=KOKORO_REPO, config=KOKORO_CONFIG, model=KOKORO_MODEL).eval()
        self.pipeline: KPipeline = KPipeline(lang_code="a", repo_id=KOKORO_REPO, model=model)
        self.pipeline.load_voice(VOICE)

    def synth(self, text: str, speed: float) -> Chunk:
        audio, words = collect(self.pipeline(text, voice=VOICE), text)
        return stretch(audio, speed), [(a, b, t0 / speed, t1 / speed) for a, b, t0, t1 in words]


class Player:
    def __init__(self, engine: Engine, text: str, speed: float, on_done: Callable[[str | None], None]) -> None:
        self.engine, self.text, self.speed, self.on_done = engine, text, speed, on_done
        self.spans = sentence_spans(text)
        self.chunks: list[Chunk | None] = [None] * len(self.spans)
        self.lock = threading.Lock()
        self.generation = 0
        self.playing = self.done = False
        self.origin = self.next_push = self.pushed_ns = 0
        self.starts: dict[int, int] = {}
        pipeline = Gst.parse_launch(
            "appsrc name=src format=time block=true max-bytes=%d"
            " ! audio/x-raw,format=F32LE,rate=%d,channels=1,layout=interleaved"
            " ! audioconvert ! audioresample ! autoaudiosink" % (SAMPLE_RATE * 4 // 5, SAMPLE_RATE)
        )
        assert isinstance(pipeline, Gst.Pipeline)
        self.pipeline = pipeline
        src = pipeline.get_by_name("src")
        assert src is not None
        self.src = src
        self.src.connect("need-data", self._pump)
        self.bus = self.pipeline.get_bus()
        self.bus.add_signal_watch()
        self.handlers = [
            self.bus.connect("message::eos", self._eos),
            self.bus.connect("message::error", self._error),
        ]
        self.seek(0)

    def seek(self, sentence: int) -> None:
        self.pipeline.set_state(Gst.State.NULL)
        with self.lock:
            self.generation += 1
            self.chunks = [None if c is EMPTY_CHUNK else c for c in self.chunks]
            self.origin = self.next_push = max(0, min(sentence, len(self.spans) - 1))
            self.starts = {}
            self.pushed_ns = 0
            self.done = False
        threading.Thread(target=self._synthesize, args=(self.origin, self.generation), daemon=True).start()
        self.play()

    def set_speed(self, speed: float) -> None:
        if speed == self.speed:
            return
        sentence = int(self.position()[0])
        with self.lock:
            self.speed = speed
            self.chunks = [None] * len(self.spans)
            self.origin = 0
            self.starts = {}
            self.generation += 1
        if not self.done:
            self.seek(sentence)

    def _synthesize(self, start: int, generation: int) -> None:
        for j in range(start, len(self.spans)):
            with self.lock:
                if generation != self.generation:
                    return
                if self.chunks[j] is not None:
                    continue
                a, b = self.spans[j]
            try:
                chunk = self.engine.synth(self.text[a:b], self.speed)
            except Exception:
                traceback.print_exc()
                chunk = EMPTY_CHUNK
            with self.lock:
                if generation != self.generation:
                    return
                self.chunks[j] = chunk

    def _pump(self, src: Gst.Element, length: int) -> None:
        with self.lock:
            if self.next_push >= len(self.spans):
                self.src.emit("end-of-stream")
                return
            chunk = self.chunks[self.next_push]
            if chunk is None:
                data = SILENCE
            else:
                data = chunk[0].tobytes()
                self.starts[self.next_push] = self.pushed_ns
                self.next_push += 1
                if not data:
                    return
            buf = Gst.Buffer.new_wrapped(data)
            buf.pts = self.pushed_ns
            buf.duration = len(data) // 4 * Gst.SECOND // SAMPLE_RATE
            self.pushed_ns += buf.duration
        self.src.emit("push-buffer", buf)

    def _eos(self, bus: Gst.Bus, msg: Gst.Message) -> None:
        self._done(None)

    def _error(self, bus: Gst.Bus, msg: Gst.Message) -> None:
        self._done(msg.parse_error()[0].message)

    def _done(self, error: str | None) -> None:
        self.playing = False
        self.done = True
        self.on_done(error)

    def play(self) -> None:
        self.playing = True
        self.pipeline.set_state(Gst.State.PLAYING)

    def pause(self) -> None:
        self.playing = False
        self.pipeline.set_state(Gst.State.PAUSED)

    def close(self) -> None:
        with self.lock:
            self.generation += 1
            self.chunks = []
        self.pipeline.set_state(Gst.State.NULL)
        self.bus.remove_signal_watch()
        for handler in self.handlers:
            self.bus.disconnect(handler)
        self.src.disconnect_by_func(self._pump)

    def position(self) -> tuple[float, Highlight]:
        ok, t = self.pipeline.query_position(Gst.Format.TIME)
        with self.lock:
            return locate(t if ok else 0, self.origin, self.spans, self.starts, self.chunks)


class Window(Gtk.ApplicationWindow):
    def __init__(self, app: "App") -> None:
        super().__init__(application=app, title="Read Aloud", default_width=640, default_height=360)
        self.app = app
        self.player: Player | None = None
        self.pending: str | None = None
        self.want_read = False
        self.lit: Highlight = None
        self.speed_timer = 0
        self.set_hide_on_close(True)

        self.view = Gtk.TextView(
            editable=False,
            cursor_visible=False,
            wrap_mode=Gtk.WrapMode.WORD_CHAR,
            left_margin=12,
            right_margin=12,
            top_margin=12,
            bottom_margin=12,
        )
        self.buffer = self.view.get_buffer()
        self.tag = self.buffer.create_tag("current", background="#ffd54f", foreground="#000000")
        self.mark = self.buffer.create_mark(None, self.buffer.get_start_iter(), True)
        click = Gtk.GestureClick(button=Gdk.BUTTON_PRIMARY)
        click.connect("released", self._clicked)
        self.view.add_controller(click)
        scroller = Gtk.ScrolledWindow(child=self.view, vexpand=True)

        self.play_button = Gtk.Button(icon_name="media-playback-pause-symbolic")
        self.play_button.connect("clicked", self._play_clicked)
        self.progress = Gtk.ProgressBar(hexpand=True, valign=Gtk.Align.CENTER)
        self.speed_spin = Gtk.SpinButton.new_with_range(0.5, 3.0, 0.1)
        self.speed_spin.set_digits(1)
        self.speed_spin.set_value(DEFAULT_SPEED)
        self.speed_spin.connect("value-changed", self._speed_changed)

        controls = Gtk.Box(spacing=6, margin_start=6, margin_end=6, margin_top=6, margin_bottom=6)
        for w in (self.play_button, self.progress, self.speed_spin):
            controls.append(w)
        box = Gtk.Box(orientation=Gtk.Orientation.VERTICAL)
        box.append(scroller)
        box.append(controls)
        self.set_child(box)

        keys = Gtk.EventControllerKey()
        keys.connect("key-pressed", self._key)
        self.add_controller(keys)
        self.connect("close-request", self._close_request)
        self.connect("notify::is-active", self._activated)
        self.view.add_tick_callback(self._tick)

    def read_primary(self) -> None:
        self.want_read = True
        if self.is_active():
            self._read()

    def _activated(self, window: Gtk.Window, pspec: GObject.ParamSpec) -> None:
        if self.want_read and self.is_active():
            self._read()

    def _read(self) -> None:
        self.want_read = False
        self.get_primary_clipboard().read_text_async(None, self._got_text)

    def _got_text(self, clipboard: Gdk.Clipboard, result: Gio.AsyncResult) -> None:
        try:
            text = clipboard.read_text_finish(result) or ""
        except GLib.Error:
            text = ""
        self.start(text.strip())

    def start(self, text: str) -> None:
        self.stop()
        self.buffer.set_text(text or "(nothing selected)")
        self.progress.set_fraction(0)
        if not text:
            return
        if self.app.failure is not None:
            self.set_title("Read Aloud — voice failed to load")
            self.buffer.set_text(self.app.failure)
            return
        if self.app.engine is None:
            self.pending = text
            self.set_title("Read Aloud — loading voice…")
            return
        self.set_title("Read Aloud")
        self.player = Player(self.app.engine, text, self.speed_spin.get_value(), self._on_done)
        self.play_button.set_icon_name("media-playback-pause-symbolic")

    def engine_changed(self) -> None:
        if self.pending:
            text, self.pending = self.pending, None
            self.start(text)

    def stop(self) -> None:
        if self.player:
            self.player.close()
            self.player = None
        self.pending = None
        self.lit = None
        self.buffer.remove_tag(self.tag, self.buffer.get_start_iter(), self.buffer.get_end_iter())

    def toggle(self) -> None:
        if not self.player:
            return
        if self.player.playing:
            self.player.pause()
        elif self.player.done:
            self.player.seek(0)
        else:
            self.player.play()
        self.play_button.set_icon_name("media-playback-%s-symbolic" % ("pause" if self.player.playing else "start"))

    def _on_done(self, error: str | None) -> None:
        self.play_button.set_icon_name("media-playback-start-symbolic")
        if error:
            self.set_title(f"Read Aloud — {error}")

    def _play_clicked(self, button: Gtk.Button) -> None:
        self.toggle()

    def _close_request(self, window: Gtk.Window) -> bool:
        self.stop()
        return False

    def _clicked(self, gesture: Gtk.GestureClick, n_press: int, x: float, y: float) -> None:
        if not self.player or self.buffer.get_has_selection():
            return
        bx, by = self.view.window_to_buffer_coords(Gtk.TextWindowType.WIDGET, int(x), int(y))
        found, it = self.view.get_iter_at_location(bx, by)
        if found:
            offset = it.get_offset()
            self.player.seek(max((j for j, (a, _) in enumerate(self.player.spans) if a <= offset), default=0))
            self.play_button.set_icon_name("media-playback-pause-symbolic")

    def _speed_changed(self, spin: Gtk.SpinButton) -> None:
        if self.speed_timer:
            GLib.source_remove(self.speed_timer)
        self.speed_timer = GLib.timeout_add(300, self._apply_speed)

    def _apply_speed(self) -> bool:
        self.speed_timer = 0
        if self.player:
            self.player.set_speed(self.speed_spin.get_value())
            if self.player.playing:
                self.play_button.set_icon_name("media-playback-pause-symbolic")
        return GLib.SOURCE_REMOVE

    def _key(self, controller: Gtk.EventControllerKey, keyval: int, keycode: int, state: Gdk.ModifierType) -> bool:
        if keyval == Gdk.KEY_Escape:
            self.close()
        elif keyval == Gdk.KEY_space:
            self.toggle()
        else:
            return False
        return True

    def _tick(self, widget: Gtk.Widget, clock: Gdk.FrameClock) -> bool:
        if self.player:
            pos, word = self.player.position()
            self.progress.set_fraction(pos / len(self.player.spans))
            if word != self.lit:
                self.lit = word
                self.buffer.remove_tag(self.tag, self.buffer.get_start_iter(), self.buffer.get_end_iter())
                if word:
                    a, b = (self.buffer.get_iter_at_offset(i) for i in word)
                    self.buffer.apply_tag(self.tag, a, b)
                    self.buffer.move_mark(self.mark, a)
                    self.view.scroll_mark_onscreen(self.mark)
        return GLib.SOURCE_CONTINUE


class App(Gtk.Application):
    def __init__(self) -> None:
        super().__init__(application_id=APP_ID)
        self.engine: Engine | None = None
        self.failure: str | None = None
        self.window: Window | None = None

    def do_startup(self) -> None:
        Gtk.Application.do_startup(self)
        Gst.init(None)
        self.hold()
        threading.Thread(target=self._load_engine, daemon=True).start()

    def _load_engine(self) -> None:
        try:
            engine = Engine()
        except Exception:
            GLib.idle_add(self._engine_loaded, None, traceback.format_exc())
            return
        GLib.idle_add(self._engine_loaded, engine, None)

    def _engine_loaded(self, engine: Engine | None, failure: str | None) -> bool:
        self.engine, self.failure = engine, failure
        if self.window:
            self.window.engine_changed()
        return GLib.SOURCE_REMOVE

    def do_activate(self) -> None:
        if self.window is None:
            self.window = Window(self)
        self.window.present()
        self.window.read_primary()


if __name__ == "__main__":
    App().run(sys.argv)
