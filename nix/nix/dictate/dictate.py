#!/usr/bin/env python3
import os
import signal
import subprocess
import sys
import wave

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


def transcribe(recognizer: OfflineRecognizer, path: str) -> str:
    with wave.open(path, "rb") as recording:
        samples = np.frombuffer(recording.readframes(recording.getnframes()), np.int16).astype(np.float32) / 32768
    # The model hears "Yeah." in the click of a tapped key.
    if len(samples) < MIN_SECONDS * SAMPLE_RATE:
        return ""
    stream = recognizer.create_stream()
    stream.accept_waveform(SAMPLE_RATE, samples)
    recognizer.decode_stream(stream)
    text: str = stream.result.text
    return text.strip()


def type_text(text: str) -> None:
    subprocess.run(
        [
            "gdbus", "call", "--session", "--dest", "org.gnome.Shell",
            "--object-path", "/org/gnome/Shell/Extensions/Dictate",
            "--method", "org.gnome.Shell.Extensions.Dictate.Type", "--", text,
        ],
        stdout=subprocess.DEVNULL,
    )


def main() -> None:
    recognizer = load()
    path = os.path.join(os.environ["XDG_RUNTIME_DIR"], "dictate.wav")
    recording: subprocess.Popen[bytes] | None = None
    while True:
        with open(sys.argv[1]) as events:
            for event in events:
                if event == "+dictate\n" and recording is None:
                    # A release that beats the recorder's start must not find the last utterance.
                    if os.path.exists(path):
                        os.unlink(path)
                    recording = subprocess.Popen(
                        ["pw-record", "--rate", str(SAMPLE_RATE), "--channels", "1", "--format", "s16", path]
                    )
                elif event == "-dictate\n" and recording is not None:
                    recording.send_signal(signal.SIGINT)
                    recording.wait()
                    recording = None
                    if os.path.exists(path):
                        text = transcribe(recognizer, path)
                        if text:
                            type_text(text + " ")
        # The key's release is lost with the stream that would have carried it.
        if recording is not None:
            recording.kill()
            recording.wait()
            recording = None


if __name__ == "__main__":
    main()
