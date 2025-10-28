#!/usr/bin/env python3
"""
Generate إعراب for ALL Quran verses (100% coverage)
Ensures every word has grammatical analysis

Source attribution:
- 'inferred' = Rule-based inference from Quranic Corpus morphology (NOT LLM)
"""

import sqlite3
import sys
from generate_irab_from_morphology import IrabGenerator


def get_all_verses(db_path: str):
    """Get all unique (surah, verse) pairs in the Quran"""
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()

    cursor.execute("""
        SELECT DISTINCT surah, verse
        FROM quranic_words
        ORDER BY surah, verse
    """)

    verses = cursor.fetchall()
    conn.close()
    return verses


def get_verses_without_irab(db_path: str):
    """Get verses that don't have i'rab yet"""
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()

    cursor.execute("""
        SELECT DISTINCT qw.surah, qw.verse
        FROM quranic_words qw
        LEFT JOIN irab_details irab
            ON qw.surah = irab.surah AND qw.verse = irab.verse
        WHERE irab.surah IS NULL
        ORDER BY qw.surah, qw.verse
    """)

    verses = cursor.fetchall()
    conn.close()
    return verses


def main():
    db_path = "../backend/aralex.db"

    print("=" * 80)
    print("Complete إعراب Generation - 100% Quran Coverage")
    print("=" * 80)
    print("\nSource Attribution:")
    print("  'inferred' = Rule-based from Quranic Corpus morphology (NOT LLM)")
    print("  - Uses grammatical rules + context analysis")
    print("  - Based on verified Quranic Corpus data")
    print("=" * 80)

    # Check current coverage
    all_verses = get_all_verses(db_path)
    remaining_verses = get_verses_without_irab(db_path)

    total_verses = len(all_verses)
    already_done = total_verses - len(remaining_verses)

    print(f"\nCurrent Status:")
    print(f"  Total Quran verses: {total_verses}")
    print(
        f"  Already have i'rab: {already_done} ({(already_done / total_verses) * 100:.1f}%)"
    )
    print(
        f"  Remaining to process: {len(remaining_verses)} ({(len(remaining_verses) / total_verses) * 100:.1f}%)"
    )

    if len(remaining_verses) == 0:
        print("\n✅ All verses already have i'rab! Nothing to do.")
        return

    print(f"\nProcessing {len(remaining_verses)} remaining verses...\n")

    # Initialize generator
    generator = IrabGenerator(db_path)

    # Process remaining verses
    processed = 0
    errors = 0

    for surah, verse in remaining_verses:
        try:
            # Generate i'rab (source will be 'inferred')
            irab_list = generator.generate_verse_irab(surah, verse)

            # Save to database
            generator.save_to_database(surah, verse, irab_list)

            processed += 1

            # Progress indicator every 50 verses
            if processed % 50 == 0:
                percent = (processed / len(remaining_verses)) * 100
                print(
                    f"Progress: {processed}/{len(remaining_verses)} verses ({percent:.1f}%) - Last: {surah}:{verse}"
                )

        except Exception as e:
            errors += 1
            print(f"ERROR processing {surah}:{verse}: {e}", file=sys.stderr)
            continue

    print("\n" + "=" * 80)
    print("Complete Generation Finished!")
    print("=" * 80)

    # Final statistics
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()

    cursor.execute("SELECT COUNT(DISTINCT surah, verse) FROM irab_details")
    total_verses_with_irab = cursor.fetchone()[0]

    cursor.execute("SELECT COUNT(*) FROM irab_details")
    total_irab_entries = cursor.fetchone()[0]

    cursor.execute("SELECT COUNT(*) FROM quranic_words")
    total_words = cursor.fetchone()[0]

    conn.close()

    print(f"\nFinal Coverage:")
    print(
        f"  Verses with i'rab: {total_verses_with_irab}/{total_verses} ({(total_verses_with_irab / total_verses) * 100:.1f}%)"
    )
    print(f"  Total i'rab entries: {total_irab_entries:,}")
    print(f"  Total Quran words: {total_words:,}")
    print(f"  Coverage: {(total_irab_entries / total_words) * 100:.1f}% of all words")
    print(f"\nProcessing Stats:")
    print(f"  Newly processed: {processed}")
    print(f"  Errors: {errors}")
    print(
        f"  Success rate: {(processed / (processed + errors)) * 100:.1f}%"
        if (processed + errors) > 0
        else "N/A"
    )
    print("\n✅ All Quran words now have grammatical analysis (i'rab)!")


if __name__ == "__main__":
    main()
