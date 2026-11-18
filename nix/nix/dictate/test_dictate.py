import os
import tempfile
import unittest
import wave

import dictate


class Dictate(unittest.TestCase):
    def test_transcribes_the_sample_shipped_with_the_model(self) -> None:
        text = dictate.transcribe(dictate.load(), f"{dictate.MODEL}/test_wavs/0.wav")
        self.assertEqual(text, "Well, I don't wish to see it any more, observed Phebe, turning away her eyes. It is certainly very like the old portrait.")

    def test_a_tap_types_nothing(self) -> None:
        path = os.path.join(tempfile.mkdtemp(), "tap.wav")
        with wave.open(f"{dictate.MODEL}/test_wavs/0.wav", "rb") as sample, wave.open(path, "wb") as tap:
            tap.setparams(sample.getparams())
            tap.writeframes(sample.readframes(int(0.2 * dictate.SAMPLE_RATE)))
        self.assertEqual(dictate.transcribe(dictate.load(), path), "")


if __name__ == "__main__":
    unittest.main()
