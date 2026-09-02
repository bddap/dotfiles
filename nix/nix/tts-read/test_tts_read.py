import unittest
from dataclasses import dataclass
from typing import ClassVar

import numpy as np

import tts_read


@dataclass
class Token:
    text: str
    start_ts: float | None
    end_ts: float | None


@dataclass
class Result:
    tokens: list[Token]
    audio: tts_read.Audio


def token(text: str, start: float | None, end: float | None) -> Token:
    return Token(text, start, end)


def result(tokens: list[Token], seconds: float) -> Result:
    return Result(tokens, np.zeros(int(seconds * tts_read.SAMPLE_RATE), np.float32))


class SentenceSpans(unittest.TestCase):
    def test_spans_cover_sentences_in_order(self) -> None:
        text = "First one.  Second, with a comma! Third?\nA line without a terminator\n\n  Version 2.0 shipped (quoted.) \"Yes.\""
        spans = tts_read.sentence_spans(text)
        self.assertEqual(
            [text[a:b] for a, b in spans],
            ["First one.", "Second, with a comma!", "Third?", "A line without a terminator", "Version 2.0 shipped (quoted.)", '"Yes."'],
        )


class Collect(unittest.TestCase):
    def test_repeated_word_maps_to_successive_occurrences(self) -> None:
        text = "the cat and the dog."
        _, words = tts_read.collect([result([token("the", 0.0, 0.1), token("cat", 0.1, 0.3), token("and", 0.3, 0.4), token("the", 0.4, 0.5), token("dog", 0.5, 0.8), token(".", 0.8, 0.9)], 1.0)], text)
        self.assertEqual([(a, b) for a, b, _, _ in words], [(0, 3), (4, 7), (8, 11), (12, 15), (16, 19)])

    def test_later_chunks_are_offset_by_earlier_audio(self) -> None:
        text = "one two"
        audio, words = tts_read.collect([result([token("one", 0.0, 0.5)], 2.0), result([token("two", 0.0, 0.5)], 1.0)], text)
        self.assertEqual(len(audio), 3 * tts_read.SAMPLE_RATE)
        self.assertEqual([(t0, t1) for _, _, t0, t1 in words], [(0.0, 0.5), (2.0, 2.5)])

    def test_unmatched_token_does_not_shift_later_words(self) -> None:
        text = "Version 2.0 shipped."
        _, words = tts_read.collect([result([token("Version", 0.0, 0.3), token("two point oh", 0.3, 0.6), token("shipped", 0.6, 0.9)], 1.0)], text)
        self.assertEqual([(a, b) for a, b, _, _ in words], [(0, 7), (12, 19)])

    def test_tokens_without_timestamps_are_skipped(self) -> None:
        _, words = tts_read.collect([result([token("a", None, None), token("b", 0.0, 0.1)], 1.0)], "a b")
        self.assertEqual([(a, b) for a, b, _, _ in words], [(2, 3)])


class Stretch(unittest.TestCase):
    def tone(self, seconds: float, hz: float) -> tts_read.Audio:
        t = np.arange(int(seconds * tts_read.SAMPLE_RATE)) / tts_read.SAMPLE_RATE
        return np.sin(2 * np.pi * hz * t).astype(np.float32)

    def peak_hz(self, audio: tts_read.Audio) -> float:
        spectrum = np.abs(np.fft.rfft(audio))
        return float(np.argmax(spectrum) * tts_read.SAMPLE_RATE / len(audio))

    def test_speed_scales_length_and_keeps_pitch(self) -> None:
        tone = self.tone(1.0, 440.0)
        for speed in (0.5, 1.5, 2.0, 3.0):
            out = tts_read.stretch(tone, speed)
            self.assertEqual(len(out), int(len(tone) / speed))
            self.assertAlmostEqual(self.peak_hz(out), 440.0, delta=6.0)
            self.assertLess(np.abs(out).max(), 1.5)

    def test_speed_one_is_identity(self) -> None:
        tone = self.tone(0.2, 440.0)
        self.assertIs(tts_read.stretch(tone, 1.0), tone)


class Locate(unittest.TestCase):
    spans: ClassVar[list[tts_read.Span]] = [(0, 9), (10, 19)]
    chunks: ClassVar[list[tts_read.Chunk | None]] = [
        (np.zeros(2 * tts_read.SAMPLE_RATE, np.float32), [(0, 3, 0.0, 1.0), (4, 9, 1.0, 2.0)]),
        (np.zeros(1 * tts_read.SAMPLE_RATE, np.float32), [(0, 5, 0.0, 0.5)]),
    ]
    starts: ClassVar[dict[int, int]] = {0: 0, 1: int(3e9)}

    def test_silence_gap_between_chunks_lights_nothing(self) -> None:
        self.assertEqual(tts_read.locate(int(2.5e9), 0, self.spans, self.starts, self.chunks), (1, None))

    def test_word_is_found_relative_to_its_chunk_start(self) -> None:
        pos, word = tts_read.locate(int(3.2e9), 0, self.spans, self.starts, self.chunks)
        self.assertAlmostEqual(pos, 1.2)
        self.assertEqual(word, (10, 15))
        self.assertEqual(tts_read.locate(int(1.5e9), 0, self.spans, self.starts, self.chunks)[1], (4, 9))

    def test_origin_skips_sentences_before_the_seek_point(self) -> None:
        self.assertEqual(tts_read.locate(int(0.25e9), 1, self.spans, {1: 0}, self.chunks), (1.25, (10, 15)))

    def test_empty_chunk_is_stepped_over(self) -> None:
        chunks = [(np.zeros(0, np.float32), []), self.chunks[1]]
        self.assertEqual(tts_read.locate(int(0.2e9), 0, self.spans, {0: 0, 1: 0}, chunks), (1.2, (10, 15)))

    def test_unpushed_chunk_waits_and_end_is_past_last_chunk(self) -> None:
        pos, word = tts_read.locate(int(0.5e9), 0, self.spans, {0: 0}, self.chunks)
        self.assertAlmostEqual(pos, 0.25)
        self.assertEqual(word, (0, 3))
        self.assertEqual(tts_read.locate(int(2.5e9), 0, self.spans, {0: 0}, self.chunks), (1, None))
        self.assertEqual(tts_read.locate(int(9e9), 0, self.spans, self.starts, self.chunks), (2, None))


class Synthesis(unittest.TestCase):
    engine: ClassVar[tts_read.Engine]

    @classmethod
    def setUpClass(cls) -> None:
        cls.engine = tts_read.Engine()

    def test_word_timestamps_are_monotonic_and_cover_the_sentence(self) -> None:
        text = "The quick brown fox jumps over the lazy dog."
        audio, words = self.engine.synth(text, 2.0)
        self.assertEqual([text[a:b] for a, b, _, _ in words], text[:-1].split())
        self.assertGreater(len(audio) / tts_read.SAMPLE_RATE, 1.0)
        self.assertGreaterEqual(words[0][2], 0.0)
        for (_, b0, _, t1), (a1, _, t0, _) in zip(words, words[1:]):
            self.assertLessEqual(b0, a1)
            self.assertLessEqual(t1, t0 + 1e-6)
        for _, _, t0, t1 in words:
            self.assertLess(t0, t1)
        self.assertLessEqual(words[-1][3], len(audio) / tts_read.SAMPLE_RATE + 0.05)

    def test_speed_halves_audio_and_word_times(self) -> None:
        text = "Speed is applied after synthesis by time stretching, zorblax."
        slow, slow_words = self.engine.synth(text, 1.0)
        fast, fast_words = self.engine.synth(text, 2.0)
        self.assertEqual(len(fast), len(slow) // 2)
        self.assertEqual([(a, b) for a, b, _, _ in fast_words], [(a, b) for a, b, _, _ in slow_words])
        for (_, _, s0, s1), (_, _, f0, f1) in zip(slow_words, fast_words):
            self.assertAlmostEqual(f0, s0 / 2)
            self.assertAlmostEqual(f1, s1 / 2)


if __name__ == "__main__":
    unittest.main()
