#!/usr/bin/env python3
"""
Generate إعراب from existing morphology data
Rule-based inference for immediate 80% solution
"""

import sqlite3
import json
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass


@dataclass
class MorphologyData:
    """Morphology from corpus"""

    surface: str
    root: Optional[str]
    lemma: Optional[str]
    pos: str  # Noun, Verb, Particle
    features: List[str]


@dataclass
class IrabInferred:
    """Inferred irab"""

    word_surface: str
    grammatical_role: Optional[str]
    construction_type: Optional[str]
    case_type: str
    case_marker: Optional[str]
    case_position: Optional[str]
    full_irab_text: str
    confidence: float


class IrabGenerator:
    """Generate irab from morphology using linguistic rules"""

    def __init__(self, db_path: str = "../backend/aralex.db"):
        self.conn = sqlite3.connect(db_path)
        self.conn.row_factory = sqlite3.Row

    def get_verse_morphology(self, surah: int, verse: int) -> List[Dict]:
        """Get morphology for a verse"""
        cursor = self.conn.cursor()
        cursor.execute(
            """
            SELECT word_position, surface_form, root, lemma, pos, segments_json
            FROM quranic_words
            WHERE surah = ? AND verse = ?
            ORDER BY word_position
        """,
            (surah, verse),
        )

        results = []
        for row in cursor.fetchall():
            segments = json.loads(row["segments_json"])

            # Extract features from segments
            all_features = []
            for seg in segments:
                all_features.extend(seg.get("dtoFeatures", []))

            results.append(
                {
                    "word_position": row["word_position"],
                    "surface": row["surface_form"],
                    "root": row["root"],
                    "lemma": row["lemma"],
                    "pos": row["pos"],
                    "segments": segments,
                    "features": all_features,
                }
            )

        return results

    def infer_irab(
        self, word_data: Dict, context: List[Dict], position: int
    ) -> IrabInferred:
        """Infer irab for a word given its morphology and context"""

        surface = word_data["surface"]
        pos = word_data["pos"]
        features = word_data["features"]
        root = word_data["root"]

        # Extract case from features
        case = self._extract_case(features)
        gender = self._extract_gender(features)
        number = self._extract_number(features)

        # Infer grammatical role based on POS and position
        role = self._infer_role(word_data, context, position)

        # Infer construction type
        construction = self._infer_construction(word_data, features)

        # Determine case type (مبني vs معرب)
        case_type, case_marker = self._determine_case_type(pos, features, case)

        # Determine positional phrase
        case_position = self._determine_position(role, case)

        # Build full irab text
        full_text = self._build_full_irab(
            surface,
            pos,
            construction,
            role,
            case_type,
            case_marker,
            case_position,
            gender,
            number,
            root,
            word_data,
        )

        # Confidence based on how certain we are
        confidence = self._calculate_confidence(word_data, role)

        return IrabInferred(
            word_surface=surface,
            grammatical_role=role,
            construction_type=construction,
            case_type=case_type,
            case_marker=case_marker,
            case_position=case_position,
            full_irab_text=full_text,
            confidence=confidence,
        )

    def _extract_case(self, features: List[Dict]) -> Optional[str]:
        """Extract case from features"""
        for feat in features:
            if feat.get("tag") == "CaseDTO":
                case_map = {"Nominative": "NOM", "Accusative": "ACC", "Genitive": "GEN"}
                return case_map.get(feat.get("contents"), None)
        return None

    def _extract_gender(self, features: List[Dict]) -> Optional[str]:
        """Extract gender"""
        for feat in features:
            if feat.get("tag") == "GenderDTO":
                return feat.get("contents")  # Masculine/Feminine
        return None

    def _extract_number(self, features: List[Dict]) -> Optional[str]:
        """Extract number"""
        for feat in features:
            if feat.get("tag") == "NumberDTO":
                num_map = {"Singular": "مفرد", "Dual": "مثنى", "Plural": "جمع"}
                return num_map.get(feat.get("contents"), None)
        return None

    def _infer_role(
        self, word_data: Dict, context: List[Dict], position: int
    ) -> Optional[str]:
        """Infer grammatical role"""
        pos = word_data["pos"]
        features = word_data["features"]
        case = self._extract_case(features)
        surface = word_data["surface"]
        lemma = word_data.get("lemma")

        # CHECK FOR VOCATIVE CONSTRUCTION (يا + noun)
        segments = word_data.get("segments", [])
        if len(segments) >= 2:
            first_seg = segments[0]
            second_seg = segments[1]

            # Check if first segment is يا vocative particle
            first_surface = first_seg.get("dtoSurface", "")
            is_vocative_particle = first_surface in ["يَا", "يَٰٓ", "يَٰ", "أَ", "أَيَا"]

            # Check if second segment is a noun with case
            second_pos = second_seg.get("dtoPos", "")
            second_features = second_seg.get("dtoFeatures", [])
            second_has_case = any(f.get("tag") == "CaseDTO" for f in second_features)

            if is_vocative_particle and second_pos == "Noun" and second_has_case:
                return "منادى"  # This is a vocative construction

        # Check for specific markers and special constructions
        for feat in features:
            tag = feat.get("tag", "")
            contents = feat.get("contents", "")

            # Demonstratives (أسماء الإشارة)
            if tag == "DemonstrativeDTO" or (
                tag == "NumberDTO" and contents == "Dual" and position == 0
            ):
                # ذلك, هذا, etc. - check if مبتدأ
                if position == 0 or (position == 1 and context[0]["pos"] == "Particle"):
                    return "مبتدأ"
                return "اسم إشارة"

            # Verb-related
            if tag in ["PerfectDTO", "ImperfectDTO", "ImperativeDTO"]:
                return "فعل"

            # Particle families
            if tag == "NegationDTO":
                # Special case: لا النافية للجنس
                if position > 0 and position < len(context) - 1:
                    next_word = (
                        context[position + 1] if position + 1 < len(context) else None
                    )
                    if (
                        next_word
                        and next_word["pos"] == "Noun"
                        and self._extract_case(next_word["features"]) == "ACC"
                    ):
                        return "حرف نفي للجنس"
                return "حرف نفي"
            if tag == "ConjunctionDTO":
                return "حرف عطف"
            if tag == "PrepositionDTO":
                return "حرف جر"

        # Special case for word after لا النافية للجنس
        if position > 0:
            prev = context[position - 1]
            for feat in prev["features"]:
                if feat.get("tag") == "NegationDTO" and case == "ACC":
                    return "اسم لا"

        # Check for preposition constructions FIRST (before general POS handling)
        segments = word_data.get("segments", [])
        if len(segments) >= 2:
            first_seg_features = segments[0].get("dtoFeatures", [])
            has_prep_tag = any(
                f.get("tag") == "PrepositionDTO" for f in first_seg_features
            )
            has_prep_lemma = any(
                f.get("tag") == "LemmaDTO"
                and f.get("contents")
                in [
                    "في",
                    "فِي",
                    "ل",
                    "لِ",
                    "ب",
                    "بِ",
                    "من",
                    "مِن",
                    "إلى",
                    "إِلى",
                    "على",
                    "عَلى",
                    "عن",
                    "عَن",
                ]
                for f in first_seg_features
            )
            has_pronoun = any(f.get("tag") == "PronounDTO" for f in features)
            last_seg_is_noun = segments[-1].get("dtoPos") == "Noun"

            if (has_prep_tag or has_prep_lemma) and (has_pronoun or last_seg_is_noun):
                return "جار ومجرور"

        # Infer from POS + case + position
        if pos == "Noun":
            if position == 0:  # First word
                if case == "NOM" or case is None:
                    return "مبتدأ"
            elif case == "NOM":
                # Look at previous words
                if position > 0:
                    prev = context[position - 1]
                    # After demonstrative → خبر or بدل
                    if position == 1:
                        return "خبر"
                    # After verb → فاعل
                    elif prev["pos"] == "Verb":
                        return "فاعل"
                    # After another nominative noun → خبر
                    elif (
                        prev["pos"] == "Noun"
                        and self._extract_case(prev["features"]) == "NOM"
                    ):
                        return "خبر"
            elif case == "ACC":
                # After verb → مفعول به
                if position > 0:
                    prev = context[position - 1]
                    if prev["pos"] == "Verb":
                        return "مفعول به"
                    # Check if after لا
                    for feat in prev["features"]:
                        if feat.get("tag") == "NegationDTO":
                            return "اسم لا"
            elif case == "GEN":
                # After preposition or in إضافة
                if position > 0:
                    prev = context[position - 1]
                    # Check if previous word has preposition
                    has_prep = any(
                        f.get("tag") == "PrepositionDTO"
                        for f in prev.get("features", [])
                    )
                    # Also check if starts with preposition segment
                    prev_segments = prev.get("segments", [])
                    starts_with_prep = len(prev_segments) > 0 and any(
                        f.get("tag") == "PrepositionDTO"
                        for f in prev_segments[0].get("dtoFeatures", [])
                    )

                    if prev["pos"] == "Particle" or has_prep or starts_with_prep:
                        return "اسم مجرور"
                    else:
                        return "مضاف إليه"

            # Indefinite nominative in later position → could be خبر ثان
            if case == "NOM" and position > 2:
                is_indefinite = any(
                    f.get("tag") == "DefinitenessDTO"
                    and f.get("contents") == "Indefinite"
                    for f in features
                )
                if is_indefinite:
                    return "خبر ثان"

        elif pos == "Verb":
            return "فعل"

        elif pos == "Particle":
            # Particle words are typically just حرف
            return "حرف"

        # Special handling for preposition constructions
        segments = word_data.get("segments", [])
        if len(segments) >= 2:
            # Check if first segment is preposition (by tag or lemma)
            first_seg_features = segments[0].get("dtoFeatures", [])
            has_prep_tag = any(
                f.get("tag") == "PrepositionDTO" for f in first_seg_features
            )
            has_prep_lemma = any(
                f.get("tag") == "LemmaDTO"
                and f.get("contents")
                in [
                    "في",
                    "فِي",
                    "ل",
                    "لِ",
                    "ب",
                    "بِ",
                    "من",
                    "مِن",
                    "إلى",
                    "إِلى",
                    "على",
                    "عَلى",
                    "عن",
                    "عَن",
                ]
                for f in first_seg_features
            )

            # Check if word overall contains a pronoun or noun
            has_pronoun = any(
                f.get("tag") == "PronounDTO" for f in word_data.get("features", [])
            )
            last_seg_is_noun = segments[-1].get("dtoPos") == "Noun"

            if (has_prep_tag or has_prep_lemma) and (has_pronoun or last_seg_is_noun):
                return "جار ومجرور"

        return None

    def _infer_construction(
        self, word_data: Dict, features: List[Dict]
    ) -> Optional[str]:
        """Infer construction type"""
        surface = word_data["surface"]
        segments = word_data.get("segments", [])

        # Check for vocative construction (يا + noun)
        if len(segments) >= 2:
            first_seg = segments[0]
            first_surface = first_seg.get("dtoSurface", "")
            is_vocative = first_surface in ["يَا", "يَٰٓ", "يَٰ", "أَ", "أَيَا"]

            second_seg = segments[1]
            second_pos = second_seg.get("dtoPos", "")

            if is_vocative and second_pos == "Noun":
                return "أسلوب نداء"

        # Check for preposition construction (prep + pronoun/noun)
        if len(segments) >= 2:
            first_seg_features = segments[0].get("dtoFeatures", [])
            has_prep_tag = any(
                f.get("tag") == "PrepositionDTO" for f in first_seg_features
            )
            has_prep_lemma = any(
                f.get("tag") == "LemmaDTO"
                and f.get("contents")
                in [
                    "في",
                    "فِي",
                    "ل",
                    "لِ",
                    "ب",
                    "بِ",
                    "من",
                    "مِن",
                    "إلى",
                    "إِلى",
                    "على",
                    "عَلى",
                    "عن",
                    "عَن",
                ]
                for f in first_seg_features
            )
            has_pronoun = any(
                f.get("tag") == "PronounDTO" for f in word_data.get("features", [])
            )
            last_seg_is_noun = segments[-1].get("dtoPos") == "Noun"

            if (has_prep_tag or has_prep_lemma) and (has_pronoun or last_seg_is_noun):
                return "جار ومجرور"

        for feat in features:
            tag = feat.get("tag", "")
            contents = feat.get("contents", "")

            if tag == "PerfectDTO":
                return "فعل ماضي"
            elif tag == "ImperfectDTO":
                return "فعل مضارع"
            elif tag == "ImperativeDTO":
                return "فعل أمر"
            elif tag == "ActiveParticipleDTO":
                return "اسم فاعل"
            elif tag == "PassiveParticipleDTO":
                return "اسم مفعول"
            elif tag == "ProperNounDTO":
                return "علم"
            elif tag == "PronounDTO":
                return "ضمير"
            elif tag == "DemonstrativeDTO":
                return "اسم إشارة"
            elif tag == "NegationDTO":
                return "حرف نفي"
            elif tag == "PrepositionDTO":
                return "حرف جر"
            elif tag == "ConjunctionDTO":
                return "حرف عطف"

        # Check for demonstratives by pattern (ذلك, هذا, etc.)
        if word_data["pos"] == "Noun" and any(
            f.get("tag") == "NumberDTO" and f.get("contents") == "Dual"
            for f in features
        ):
            # Could be demonstrative
            if surface in ["ذَٰلِكَ", "هَٰذَا", "تِلْكَ", "هَٰؤُلَاءِ"]:
                return "اسم إشارة"

        # Based on POS
        if word_data["pos"] == "Noun":
            return "اسم"
        elif word_data["pos"] == "Verb":
            return "فعل"
        elif word_data["pos"] == "Particle":
            return "حرف"

        return None

    def _determine_case_type(
        self, pos: str, features: List[Dict], case: Optional[str]
    ) -> Tuple[str, Optional[str]]:
        """Determine if مبني or معرب and case marker"""

        # Particles are always مبني
        if pos == "Particle":
            return ("مبني", "على السكون")

        # Verbs - check for مبني markers
        if pos == "Verb":
            for feat in features:
                if feat.get("tag") == "PerfectDTO":
                    return ("مبني", "على الفتح")
                elif feat.get("tag") == "ImperativeDTO":
                    return ("مبني", "على السكون")
            return ("معرب", None)  # مضارع usually معرب

        # Nouns - check for مبني categories
        if pos == "Noun":
            for feat in features:
                tag = feat.get("tag", "")
                # Pronouns are مبني
                if tag == "PronounDTO":
                    return ("مبني", "على السكون")
                # Demonstratives are مبني
                elif tag == "DemonstrativeDTO":
                    return ("مبني", "على السكون")
                # Dual demonstratives could be مبني على الكسر
                elif tag == "NumberDTO" and feat.get("contents") == "Dual":
                    # Check if demonstrative - they're often مبني على الكسر
                    return ("مبني", "على الكسر")

            # معرب - determine marker by case
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

        # مبني nouns need positional phrase
        case_map = {"NOM": "رفع", "ACC": "نصب", "GEN": "جر"}

        if case and role:
            case_ar = case_map.get(case)
            if case_ar:
                return f"في محل {case_ar}"

        return None

    def _build_full_irab(
        self,
        surface: str,
        pos: str,
        construction: Optional[str],
        role: Optional[str],
        case_type: str,
        case_marker: Optional[str],
        case_position: Optional[str],
        gender: Optional[str],
        number: Optional[str],
        root: Optional[str],
        word_data: Dict,
    ) -> str:
        """Build full irab text in traditional style"""

        # SPECIAL HANDLING FOR VOCATIVE CONSTRUCTIONS (يا + noun)
        segments = word_data.get("segments", [])
        if role == "منادى" and len(segments) >= 2:
            first_seg = segments[0]
            second_seg = segments[1]

            first_surface = first_seg.get("dtoSurface", "")
            second_surface = second_seg.get("dtoSurface", "")

            # Extract case from second segment (the noun)
            second_features = second_seg.get("dtoFeatures", [])
            noun_case = self._extract_case(second_features)

            # Build two-part analysis
            particle_part = (
                f"«{first_surface}» حرف نداء مبني على السكون لا محل له من الإعراب"
            )

            if noun_case == "ACC":
                noun_part = f"«{second_surface}» منادى منصوب وعلامة نصبه الفتحة"
            elif noun_case == "NOM":
                noun_part = f"«{second_surface}» منادى مبني على الضم في محل نصب"
            else:
                noun_part = f"«{second_surface}» منادى"

            if root:
                noun_part += f" من الجذر: {root}"

            return f"{particle_part}، {noun_part}"

        parts = []

        # Construction type
        if construction:
            parts.append(construction)

        # Case type
        parts.append(case_type)

        # Case marker
        if case_marker:
            parts.append(case_marker)

        # Position
        if case_position and role:
            parts.append(f"{case_position} {role}")
        elif role:
            parts.append(role)
        elif case_position:
            parts.append(case_position)

        # Additional features
        if number:
            parts.append(number)

        # Root reference
        if root:
            parts.append(f"من الجذر: {root}")

        return " ".join(parts)

    def _calculate_confidence(self, word_data: Dict, role: Optional[str]) -> float:
        """Calculate confidence score"""
        confidence = 0.7  # Base

        # Higher if we have root
        if word_data.get("root"):
            confidence += 0.1

        # Higher if role was inferred
        if role:
            confidence += 0.1

        # Higher if we have many features
        if len(word_data.get("features", [])) > 3:
            confidence += 0.1

        return min(confidence, 1.0)

    def generate_verse_irab(self, surah: int, verse: int) -> List[IrabInferred]:
        """Generate irab for entire verse"""
        words = self.get_verse_morphology(surah, verse)
        results = []

        for i, word in enumerate(words):
            irab = self.infer_irab(word, words, i)
            results.append(irab)

        return results

    def save_to_database(self, surah: int, verse: int, irab_list: List[IrabInferred]):
        """Save generated irab to database"""
        cursor = self.conn.cursor()

        for i, irab in enumerate(irab_list, 1):
            cursor.execute(
                """
                INSERT OR REPLACE INTO irab_details (
                    surah, verse, word_position, word_surface,
                    grammatical_role, construction_type, case_type,
                    case_marker, case_position, full_irab_text,
                    source, confidence
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
                (
                    surah,
                    verse,
                    i,
                    irab.word_surface,
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
    """Test on verse 2:2"""
    print("=" * 80)
    print("Rule-Based إعراب Generator")
    print("=" * 80)

    generator = IrabGenerator()

    # Test on verse 2:2
    print("\nGenerating إعراب for verse 2:2...")
    irab_list = generator.generate_verse_irab(2, 2)

    print(f"\nGenerated {len(irab_list)} word entries:\n")

    for i, irab in enumerate(irab_list, 1):
        print(f"{i}. {irab.word_surface}")
        print(f"   الدور: {irab.grammatical_role or 'غير محدد'}")
        print(f"   النوع: {irab.construction_type or 'غير محدد'}")
        print(f"   الحالة: {irab.case_type}")
        if irab.case_marker:
            print(f"   العلامة: {irab.case_marker}")
        if irab.case_position:
            print(f"   المحل: {irab.case_position}")
        print(f"   الإعراب الكامل: {irab.full_irab_text}")
        print(f"   الثقة: {irab.confidence:.1%}\n")

    # Save to database
    print("Saving to database...")
    generator.save_to_database(2, 2, irab_list)
    print("✅ Saved successfully!")


if __name__ == "__main__":
    main()
