import unittest
import wave

import dictate


def sample(seconds: float | None = None) -> bytes:
    with wave.open(f"{dictate.MODEL}/test_wavs/0.wav", "rb") as recording:
        frames = recording.getnframes() if seconds is None else int(seconds * dictate.SAMPLE_RATE)
        return recording.readframes(frames)


class Dictate(unittest.TestCase):
    def test_transcribes_the_sample_shipped_with_the_model(self) -> None:
        text = dictate.transcribe(dictate.load(), sample())
        self.assertEqual(text, "Well, I don't wish to see it any more, observed Phebe, turning away her eyes. It is certainly very like the old portrait.")

    def test_a_tap_types_nothing(self) -> None:
        self.assertEqual(dictate.transcribe(dictate.load(), sample(0.2)), "")


if __name__ == "__main__":
    unittest.main()
