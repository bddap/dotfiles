#!/usr/bin/env python3
import os
import signal
import subprocess
import sys
import threading
import unicodedata

import numpy as np
from sherpa_onnx.offline_recognizer import OfflineRecognizer

MODEL = "@model@"
SAMPLE_RATE = 16000
MIN_SECONDS = 0.3


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
    return text.strip()


def plain(text: str) -> str:
    return "".join(" " if c.isspace() else c for c in text if c.isspace() or unicodedata.category(c)[0] != "C")


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
        self.process.wait()
        self.reader.join()
        return self.audio


def main() -> None:
    transcripts = os.fdopen(os.dup(sys.stdout.fileno()), "w")
    # The shell commits every line of stdout to the focused window, so nothing else may write there.
    os.dup2(sys.stderr.fileno(), sys.stdout.fileno())
    recognizer = load()
    recording: Recording | None = None
    for event in sys.stdin:
        if event == "press\n" and recording is None:
            recording = Recording()
        elif event == "release\n" and recording is not None:
            text = plain(transcribe(recognizer, recording.stop()))
            recording = None
            if text:
                print(text, file=transcripts, flush=True)
    if recording is not None:
        recording.stop()


if __name__ == "__main__":
    main()
