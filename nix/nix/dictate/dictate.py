#!/usr/bin/env python3
import ctypes
import os
import queue
import signal
import subprocess
import sys
import threading

import numpy as np
from sherpa_onnx.offline_recognizer import OfflineRecognizer

MODEL = "@model@"
SAMPLE_RATE = 16000
MIN_SECONDS = 0.3
PR_SET_DUMPABLE = 4


def load() -> OfflineRecognizer:
    recognizer: OfflineRecognizer = OfflineRecognizer.from_transducer(
        encoder=f"{MODEL}/encoder.int8.onnx",
        decoder=f"{MODEL}/decoder.int8.onnx",
        joiner=f"{MODEL}/joiner.int8.onnx",
        tokens=f"{MODEL}/tokens.txt",
        model_type="nemo_transducer",
        num_threads=min(8, os.cpu_count() or 1),
    )
    return recognizer


def transcribe(recognizer: OfflineRecognizer, audio: bytes) -> str:
    samples = np.frombuffer(audio[: len(audio) // 2 * 2], np.int16).astype(np.float32) / 32768
    # The model hears "Yeah." in the click of a tapped key.
    if len(samples) < MIN_SECONDS * SAMPLE_RATE:
        return ""
    stream = recognizer.create_stream()
    stream.accept_waveform(SAMPLE_RATE, samples)
    recognizer.decode_stream(stream)
    text: str = stream.result.text
    return " ".join(text.split())


class Recording:
    def __init__(self) -> None:
        self.process = subprocess.Popen(
            ["pw-record", "--raw", "--rate", str(SAMPLE_RATE), "--channels", "1", "--format", "s16", "-"],
            stdin=subprocess.DEVNULL,
            stdout=subprocess.PIPE,
        )
        self.audio = b""
        self.reader = threading.Thread(target=self.drain)
        self.reader.start()

    def drain(self) -> None:
        assert self.process.stdout is not None
        self.audio = self.process.stdout.read()

    def stop(self) -> bytes:
        self.process.send_signal(signal.SIGINT)
        try:
            self.process.wait(timeout=5)
        except subprocess.TimeoutExpired:
            self.process.kill()
        self.reader.join()
        return self.audio


def main() -> None:
    # Same-user processes could otherwise write key events or transcripts through /proc/<pid>/fd.
    ctypes.CDLL(None).prctl(PR_SET_DUMPABLE, 0)
    transcripts = os.fdopen(os.dup(sys.stdout.fileno()), "w", encoding="utf-8")
    # The shell commits every line of stdout to the focused window, so nothing else may write there.
    os.dup2(sys.stderr.fileno(), sys.stdout.fileno())
    recognizer = load()
    utterances: queue.Queue[bytes] = queue.Queue()

    def transcribe_in_order() -> None:
        while True:
            text = transcribe(recognizer, utterances.get())
            if text:
                print(text, file=transcripts, flush=True)

    threading.Thread(target=transcribe_in_order, daemon=True).start()
    recording: Recording | None = None
    for event in sys.stdin:
        if event == "press\n" and recording is None:
            recording = Recording()
        elif event == "release\n" and recording is not None:
            utterances.put(recording.stop())
            recording = None
    if recording is not None:
        recording.stop()


if __name__ == "__main__":
    main()
