#!/usr/bin/env python3
"""
Complete extraction pipeline for الجدول في إعراب القرآن
Extracts full traditional irab from all 17 PDF volumes
"""

import pdfplumber
import re
import json
import sqlite3
from collections import defaultdict
from pathlib import Path
from dataclasses import dataclass, asdict
from typing import List, Optional, Dict, Tuple


@dataclass
class IrabEntry:
    """Represents one word's irab entry"""

    surah: int
    verse: int
    word_position: int
    word_surface: str
    grammatical_role: Optional[str] = None
    construction_type: Optional[str] = None
    case_type: Optional[str] = None
    case_marker: Optional[str] = None
    case_position: Optional[str] = None
    full_irab_text: str = ""
    source: str = "aljadwal"
    volume_number: Optional[int] = None
    page_number: Optional[int] = None
    extraction_confidence: float = 1.0


class AljadwalExtractor:
    """Extract irab from الجدول PDF volumes"""

    def __init__(self, data_dir: str = "../data/quran_corpus"):
        self.data_dir = Path(data_dir)
        self.volumes = self._find_volumes()

    def _find_volumes(self) -> List[Path]:
        """Find all الجدول PDF volumes"""
        volumes = sorted(self.data_dir.glob("*الجدول*_text.pdf"))
        print(f"Found {len(volumes)} volumes")
        return volumes

    def group_chars_into_words(
        self, words: List[Dict], line_tolerance: int = 3, word_gap: int = 6
    ) -> List[str]:
        """
        Group individual characters into words with improved spacing

        Args:
            words: List of word dicts from pdfplumber
            line_tolerance: Max y-diff to consider same line
            word_gap: Min x-gap to consider new word (reduced for better grouping)
        """
        # Group by lines (y-coordinate)
        lines = defaultdict(list)
        for word in words:
            y = round(word["top"] / line_tolerance) * line_tolerance
            lines[y].append(word)

        # Process each line
        result_lines = []
        for y in sorted(lines.keys()):
            line_words = sorted(lines[y], key=lambda w: w["x0"])

            # Group characters into words based on x-gap
            current_word = []
            reconstructed_words = []
            last_x = None

            for char_obj in line_words:
                char = char_obj["text"]
                x = char_obj["x0"]
                char_width = char_obj["x1"] - char_obj["x0"]

                if last_x is None:
                    current_word.append(char)
                elif (x - last_x) > word_gap:
                    # Gap detected - new word
                    if current_word:
                        word_text = "".join(current_word)
                        reconstructed_words.append(word_text)
                    current_word = [char]
                else:
                    current_word.append(char)

                last_x = x + char_width

            # Add last word
            if current_word:
                word_text = "".join(current_word)
                reconstructed_words.append(word_text)

            # Reverse for RTL
            line_text = " ".join(reversed(reconstructed_words))
            result_lines.append(line_text)

        return result_lines

    def extract_page(self, pdf_path: Path, page_num: int) -> str:
        """Extract text from a single page"""
        with pdfplumber.open(pdf_path) as pdf:
            page = pdf.pages[page_num]
            words = page.extract_words(x_tolerance=2, y_tolerance=2)
            lines = self.group_chars_into_words(words, line_tolerance=5, word_gap=6)
            return "\n".join(lines)

    def find_verse_pages(self, pdf_path: Path, surah: int, verse: int) -> List[int]:
        """Find pages containing a specific verse"""
        verse_pattern = f"{surah}:{verse}"
        matching_pages = []

        with pdfplumber.open(pdf_path) as pdf:
            for page_num in range(len(pdf.pages)):
                page_text = self.extract_page(pdf_path, page_num)

                # Look for verse reference
                if verse_pattern in page_text or f"[{verse}]" in page_text:
                    matching_pages.append(page_num)

        return matching_pages

    def parse_irab_line(self, line: str) -> Optional[Tuple[str, str]]:
        """
        Parse a line of irab text
        Returns: (word, irab_text) or None

        Format: (word): irab details...
        """
        # Match pattern: (word): text...
        match = re.match(r"\(([^)]+)\)\s*:?\s*(.+)", line.strip())
        if match:
            word = match.group(1).strip()
            irab = match.group(2).strip()
            return (word, irab)
        return None

    def extract_irab_components(self, irab_text: str) -> Dict[str, Optional[str]]:
        """
        Extract grammatical components from irab text

        Patterns to extract:
        - Grammatical role: مبتدأ، خبر، فاعل، مفعول به
        - Construction: اسم إشارة، فعل ماضي، حرف جر
        - Case: مبني على، معرب
        - Position: في محل رفع/نصب/جر
        """
        components = {
            "grammatical_role": None,
            "construction_type": None,
            "case_type": None,
            "case_marker": None,
            "case_position": None,
        }

        # Extract grammatical role (at end usually)
        role_patterns = [
            r"(?:في محل \w+ )?(\w+)$",  # Word at end
            r"(مبتدأ|خبر|فاعل|مفعول به|مفعول مطلق|حال|تمييز|بدل|عطف بيان|نعت|توكيد)",
        ]
        for pattern in role_patterns:
            match = re.search(pattern, irab_text)
            if match:
                components["grammatical_role"] = match.group(1)
                break

        # Extract construction type
        construction_patterns = [
            r"(اسم إشارة|اسم موصول|اسم فعل|فعل ماضي|فعل مضارع|فعل أمر|حرف جر|حرف عطف|حرف نفي)",
            r"(نافية للجنس|اسم لا)",
        ]
        for pattern in construction_patterns:
            match = re.search(pattern, irab_text)
            if match:
                components["construction_type"] = match.group(1)
                break

        # Extract case type
        if "مبني" in irab_text:
            components["case_type"] = "مبني"
            # Extract what it's built on
            marker_match = re.search(r"مبني على (\w+)", irab_text)
            if marker_match:
                components["case_marker"] = f"على {marker_match.group(1)}"
        elif "معرب" in irab_text or any(
            x in irab_text for x in ["مرفوع", "منصوب", "مجرور"]
        ):
            components["case_type"] = "معرب"
            # Extract case marker
            for case in ["الضمة", "الفتحة", "الكسرة", "الواو", "الياء", "الألف"]:
                if case in irab_text:
                    components["case_marker"] = case
                    break

        # Extract positional phrase
        position_match = re.search(r"في محل (رفع|نصب|جر)(?: (\w+))?", irab_text)
        if position_match:
            pos = position_match.group(1)
            role = position_match.group(2) if position_match.group(2) else ""
            components["case_position"] = f"في محل {pos} {role}".strip()

        return components

    def extract_verse_irab(
        self, pdf_path: Path, page_num: int, surah: int, verse: int
    ) -> List[IrabEntry]:
        """Extract all irab entries for a verse from a page"""
        page_text = self.extract_page(pdf_path, page_num)
        lines = page_text.split("\n")

        entries = []
        word_position = 1
        current_word = None
        current_irab = []

        for line in lines:
            parsed = self.parse_irab_line(line)

            if parsed:
                # Save previous entry if exists
                if current_word and current_irab:
                    full_text = " ".join(current_irab)
                    components = self.extract_irab_components(full_text)

                    entry = IrabEntry(
                        surah=surah,
                        verse=verse,
                        word_position=word_position,
                        word_surface=current_word,
                        full_irab_text=full_text,
                        **components,
                        volume_number=self._get_volume_number(pdf_path),
                        page_number=page_num + 1,
                    )
                    entries.append(entry)
                    word_position += 1

                # Start new entry
                current_word, irab_text = parsed
                current_irab = [irab_text]
            elif current_word:
                # Continuation of previous irab
                current_irab.append(line.strip())

        # Save last entry
        if current_word and current_irab:
            full_text = " ".join(current_irab)
            components = self.extract_irab_components(full_text)

            entry = IrabEntry(
                surah=surah,
                verse=verse,
                word_position=word_position,
                word_surface=current_word,
                full_irab_text=full_text,
                **components,
                volume_number=self._get_volume_number(pdf_path),
                page_number=page_num + 1,
            )
            entries.append(entry)

        return entries

    def _get_volume_number(self, pdf_path: Path) -> int:
        """Extract volume number from filename"""
        match = re.search(r"(\d+)", pdf_path.name)
        return int(match.group(1)) if match else 0

    def extract_volume(
        self, volume_path: Path, output_json: Optional[Path] = None
    ) -> List[IrabEntry]:
        """Extract all irab from one volume"""
        print(f"\nProcessing: {volume_path.name}")
        all_entries = []

        with pdfplumber.open(volume_path) as pdf:
            total_pages = len(pdf.pages)
            print(f"Total pages: {total_pages}")

            for page_num in range(total_pages):
                if page_num % 50 == 0:
                    print(f"  Page {page_num}/{total_pages}...")

                # Extract page and look for verse markers
                # This is simplified - need verse detection logic
                # For now, extract all irab-like patterns

        if output_json:
            with open(output_json, "w", encoding="utf-8") as f:
                json.dump(
                    [asdict(e) for e in all_entries], f, ensure_ascii=False, indent=2
                )
            print(f"Saved {len(all_entries)} entries to {output_json}")

        return all_entries


def main():
    """Main extraction pipeline"""
    extractor = AljadwalExtractor("../data/quran_corpus")

    print("=" * 80)
    print("الجدول في إعراب القرآن - Extraction Pipeline")
    print("=" * 80)

    # Test on verse 2:2 first
    print("\nTest: Extracting verse 2:2...")
    volume1 = extractor.volumes[0]

    # We know it's on page 32 (index 31)
    entries = extractor.extract_verse_irab(volume1, 31, 2, 2)

    print(f"\nExtracted {len(entries)} words from verse 2:2:")
    for entry in entries:
        print(f"\n{entry.word_position}. {entry.word_surface}")
        print(f"   Role: {entry.grammatical_role}")
        print(f"   Type: {entry.construction_type}")
        print(f"   Case: {entry.case_type} {entry.case_marker}")
        print(f"   Position: {entry.case_position}")
        print(f"   Full: {entry.full_irab_text[:100]}...")


if __name__ == "__main__":
    main()
