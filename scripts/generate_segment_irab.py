#!/usr/bin/env python3
"""
Generate segment-level إعراب following Quranic Corpus methodology
Each morphological segment gets its own i'rab entry

Reference: https://corpus.quran.com/
"""

import sqlite3
import json
from typing import Dict, List, Optional
from dataclasses import dataclass


@dataclass
class SegmentIrab:
    """I'rab for a single morphological segment"""

    segment_surface: str
    grammatical_role: Optional[str]
    construction_type: Optional[str]
    case_type: str
    case_marker: Optional[str]
    case_position: Optional[str]
    full_irab_text: str
    confidence: float


class SegmentIrabGenerator:
    """Generate i'rab for each segment following Quranic Corpus approach"""

    def __init__(self, db_path: str = "../backend/aralex.db"):
        self.conn = sqlite3.connect(db_path)
        self.conn.row_factory = sqlite3.Row

    def get_word_segments(
        self, surah: int, verse: int, word_position: int
    ) -> List[Dict]:
        """Get segments for a specific word"""
        cursor = self.conn.cursor()
        cursor.execute(
            """
            SELECT segments_json FROM quranic_words
            WHERE surah = ? AND verse = ? AND word_position = ?
        """,
            (surah, verse, word_position),
        )

        row = cursor.fetchone()
        if not row:
            return []

        segments = json.loads(row["segments_json"])
        return segments

    def analyze_segment(
        self,
        segment: Dict,
        segment_position: int,
        all_segments: List[Dict],
        word_context: List[Dict],
        word_position_in_verse: int,
    ) -> SegmentIrab:
        """Analyze a single segment and generate i'rab"""

        surface = segment.get("dtoSurface", "")
        pos = segment.get("dtoPos", "")
        features = segment.get("dtoFeatures", [])

        # Extract morphological features
        case = self._extract_case(features)
        root = self._extract_root(features)
        lemma = self._extract_lemma(features)

        # Determine segment role based on position and features
        role = self._infer_segment_role(
            segment,
            segment_position,
            all_segments,
            word_context,
            word_position_in_verse,
        )

        # Determine construction type
        construction = self._infer_segment_construction(
            segment, features, segment_position, all_segments
        )

        # Determine case type and marker
        case_type, case_marker = self._determine_case_type(pos, features, case)

        # Determine positional phrase
        case_position = self._determine_position(role, case)

        # Build full i'rab text
        full_text = self._build_segment_irab(
            surface,
            pos,
            construction,
            role,
            case_type,
            case_marker,
            case_position,
            root,
            features,
        )

        # Calculate confidence
        confidence = self._calculate_confidence(segment, role)

        return SegmentIrab(
            segment_surface=surface,
            grammatical_role=role,
            construction_type=construction,
            case_type=case_type,
            case_marker=case_marker,
            case_position=case_position,
            full_irab_text=full_text,
            confidence=confidence,
        )

    def _extract_case(self, features: List[Dict]) -> Optional[str]:
        """Extract case marking"""
        for feat in features:
            if feat.get("tag") == "CaseDTO":
                case_map = {"Nominative": "NOM", "Accusative": "ACC", "Genitive": "GEN"}
                return case_map.get(feat.get("contents"))
        return None

    def _extract_root(self, features: List[Dict]) -> Optional[str]:
        """Extract root"""
        for feat in features:
            if feat.get("tag") == "RootDTO":
                return feat.get("contents")
        return None

    def _extract_lemma(self, features: List[Dict]) -> Optional[str]:
        """Extract lemma"""
        for feat in features:
            if feat.get("tag") == "LemmaDTO":
                return feat.get("contents")
        return None

    def _infer_segment_role(
        self,
        segment: Dict,
        seg_pos: int,
        all_segments: List[Dict],
        word_context: List[Dict],
        word_pos_in_verse: int,
    ) -> Optional[str]:
        """Infer grammatical role for this specific segment"""

        pos = segment.get("dtoPos", "")
        features = segment.get("dtoFeatures", [])
        surface = segment.get("dtoSurface", "")

        # PREFIXES - check feature tags
        is_prefix = any(f.get("tag") == "PrefixDTO" for f in features)

        if is_prefix or seg_pos == 1:  # First segment often a prefix
            # Vocative particle
            if surface in ["يَا", "يَٰٓ", "يَٰ", "أَ", "أَيَا"]:
                return "حرف نداء"

            # Prepositions
            lemma = self._extract_lemma(features)
            if lemma in [
                "ب",
                "بِ",
                "ل",
                "لِ",
                "ك",
                "كَ",
                "في",
                "فِي",
                "من",
                "مِن",
                "إلى",
                "على",
                "عن",
            ]:
                return "حرف جر"

            # Conjunction
            if lemma in ["و", "وَ", "ف", "فَ"]:
                return "حرف عطف"

            # Determiner (ال)
            if any(f.get("tag") == "DeterminerDTO" for f in features):
                return "أداة تعريف"

            # Negation
            if any(f.get("tag") == "NegationDTO" for f in features):
                return "حرف نفي"

        # SUFFIXES - check for pronouns
        is_suffix = any(f.get("tag") == "SuffixDTO" for f in features)
        is_pronoun = any(f.get("tag") == "PronounDTO" for f in features)

        if is_suffix or is_pronoun:
            return "ضمير متصل"

        # STEM/MAIN SEGMENT - full analysis based on context
        if pos == "Noun":
            case = self._extract_case(features)

            # Check if this is part of vocative (after يا)
            if seg_pos > 1 and len(all_segments) >= seg_pos:
                prev_seg = all_segments[seg_pos - 2]  # 0-indexed
                prev_surface = prev_seg.get("dtoSurface", "")
                if prev_surface in ["يَا", "يَٰٓ", "يَٰ"]:
                    return "منادى"

            # Check if after preposition (مجرور)
            if case == "GEN":
                # Check previous segments in same word
                for i in range(seg_pos - 1):
                    prev_seg = all_segments[i]
                    prev_lemma = self._extract_lemma(prev_seg.get("dtoFeatures", []))
                    if prev_lemma in ["ب", "ل", "ك", "في", "من", "إلى", "على", "عن"]:
                        return "اسم مجرور"
                return "مضاف إليه"  # Genitive without explicit preposition

            # Subject/predicate based on position
            if word_pos_in_verse == 1 and case == "NOM":
                return "مبتدأ"
            elif case == "NOM":
                # Could be فاعل, خبر, etc. - need more context
                return "مرفوع"
            elif case == "ACC":
                return "منصوب"

        elif pos == "Verb":
            # Check verb type from features
            for feat in features:
                tag = feat.get("tag", "")
                if tag == "PerfectDTO":
                    return "فعل ماضي"
                elif tag == "ImperfectDTO":
                    return "فعل مضارع"
                elif tag == "ImperativeDTO":
                    return "فعل أمر"
            return "فعل"

        elif pos == "Particle":
            return "حرف"

        return None

    def _infer_segment_construction(
        self,
        segment: Dict,
        features: List[Dict],
        seg_pos: int,
        all_segments: List[Dict],
    ) -> Optional[str]:
        """Infer construction type for segment"""

        pos = segment.get("dtoPos", "")

        # Check specific feature tags
        for feat in features:
            tag = feat.get("tag", "")

            if tag == "ActiveParticipleDTO":
                return "اسم فاعل"
            elif tag == "PassiveParticipleDTO":
                return "اسم مفعول"
            elif tag == "ProperNounDTO":
                return "علم"
            elif tag == "PronounDTO":
                return "ضمير"
            elif tag == "DemonstrativeDTO":
                return "اسم إشارة"
            elif tag == "PrepositionDTO":
                return "حرف جر"
            elif tag == "ConjunctionDTO":
                return "حرف عطف"
            elif tag == "NegationDTO":
                return "حرف نفي"
            elif tag == "DeterminerDTO":
                return "أل التعريف"

        # General POS-based
        if pos == "Noun":
            return "اسم"
        elif pos == "Verb":
            return "فعل"
        elif pos == "Particle":
            return "حرف"

        return None

    def _determine_case_type(
        self, pos: str, features: List[Dict], case: Optional[str]
    ) -> tuple[str, Optional[str]]:
        """Determine if مبني or معرب"""

        # Particles always مبني
        if pos == "Particle":
            return ("مبني", "على السكون")

        # Pronouns always مبني
        if any(f.get("tag") == "PronounDTO" for f in features):
            return ("مبني", "على السكون")

        # Verbs - check type
        if pos == "Verb":
            for feat in features:
                if feat.get("tag") == "PerfectDTO":
                    return ("مبني", "على الفتح")
                elif feat.get("tag") == "ImperativeDTO":
                    return ("مبني", "على السكون")
            return ("معرب", None)  # مضارع usually معرب

        # Nouns with case marking are معرب
        if pos == "Noun" and case:
            if case == "NOM":
                return ("معرب", "بالضمة")
            elif case == "ACC":
                return ("معرب", "بالفتحة")
            elif case == "GEN":
                return ("معرب", "بالكسرة")

        return ("معرب", None)

    def _determine_position(
        self, role: Optional[str], case: Optional[str]
    ) -> Optional[str]:
        """Determine positional phrase (في محل...)"""

        case_map = {"NOM": "رفع", "ACC": "نصب", "GEN": "جر"}

        if case and role:
            case_ar = case_map.get(case)
            if case_ar:
                return f"في محل {case_ar}"

        return None

    def _build_segment_irab(
        self,
        surface: str,
        pos: str,
        construction: Optional[str],
        role: Optional[str],
        case_type: str,
        case_marker: Optional[str],
        case_position: Optional[str],
        root: Optional[str],
        features: List[Dict],
    ) -> str:
        """Build full i'rab text for this segment"""

        parts = []

        # Segment surface
        parts.append(f"«{surface}»")

        # Construction type
        if construction:
            parts.append(construction)

        # Case type
        parts.append(case_type)

        # Case marker
        if case_marker:
            parts.append(case_marker)

        # Position or role
        if case_position and role:
            parts.append(f"{case_position} {role}")
        elif role:
            parts.append(role)
        elif case_position:
            parts.append(case_position)

        # Root reference
        if root:
            parts.append(f"من الجذر: {root}")

        return " ".join(parts)

    def _calculate_confidence(self, segment: Dict, role: Optional[str]) -> float:
        """Calculate confidence score"""
        confidence = 0.7

        features = segment.get("dtoFeatures", [])

        # Higher if has root
        if any(f.get("tag") == "RootDTO" for f in features):
            confidence += 0.1

        # Higher if role inferred
        if role:
            confidence += 0.1

        # Higher if many features
        if len(features) > 3:
            confidence += 0.1

        return min(confidence, 1.0)

    def generate_word_irab(
        self, surah: int, verse: int, word_position: int, word_context: List[Dict]
    ) -> List[SegmentIrab]:
        """Generate i'rab for all segments in a word"""

        segments = self.get_word_segments(surah, verse, word_position)
        irab_list = []

        for i, segment in enumerate(segments, 1):
            irab = self.analyze_segment(
                segment, i, segments, word_context, word_position
            )
            irab_list.append(irab)

        return irab_list

    def save_segment_irab(
        self,
        surah: int,
        verse: int,
        word_position: int,
        segment_position: int,
        irab: SegmentIrab,
    ):
        """Save segment i'rab to database"""
        cursor = self.conn.cursor()

        cursor.execute(
            """
            INSERT OR REPLACE INTO irab_details (
                surah, verse, word_position, segment_position,
                segment_surface, grammatical_role, construction_type,
                case_type, case_marker, case_position,
                full_irab_text, source, confidence
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """,
            (
                surah,
                verse,
                word_position,
                segment_position,
                irab.segment_surface,
                irab.grammatical_role,
                irab.construction_type,
                irab.case_type,
                irab.case_marker,
                irab.case_position,
                irab.full_irab_text,
                "inferred",
                irab.confidence,
            ),
        )

        self.conn.commit()


def main():
    """Test on verse 5:15"""
    print("=" * 80)
    print("Segment-Level إعراب Generator (Following Quranic Corpus)")
    print("=" * 80)

    generator = SegmentIrabGenerator()

    # Test on verse 5:15, word 1 (يَٰٓأَهْلَ)
    print("\nTesting verse 5:15, word 1: يَٰٓأَهْلَ\n")

    irab_list = generator.generate_word_irab(5, 15, 1, [])

    for i, irab in enumerate(irab_list, 1):
        print(f"Segment {i}: {irab.segment_surface}")
        print(f"  الدور: {irab.grammatical_role or 'غير محدد'}")
        print(f"  النوع: {irab.construction_type or 'غير محدد'}")
        print(f"  الإعراب: {irab.full_irab_text}")
        print(f"  الثقة: {irab.confidence:.1%}\n")

    # Save to database
    for i, irab in enumerate(irab_list, 1):
        generator.save_segment_irab(5, 15, 1, i, irab)

    print("✅ Saved to database!")


if __name__ == "__main__":
    main()
